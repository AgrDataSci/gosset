#' Get ClimMob data
#'
#' Fetch the trial data from a ClimMob project using your API key
#'
#' @param project a character for the project id
#' @param ... additional arguments passed to methods
#' @inheritParams getProjectsCM
#' @return A data frame with the project data
#' \item{id}{the participant's package id}
#' \item{moment}{the data collection moment}
#' \item{variable}{the variable name}
#' \item{value}{the value for each variable}
#' @examples
#' \dontrun{
#' # This function will not work without your API key  
#' library("gosset")
#' library("jsonlite")
#' library("httr")
#' 
#' my_key <- "my_api_key"
#' my_project <- "my_climmob_project"
#' 
#' data <- getDataCM(key = my_key, project = my_project)
#' }
#' 
#' @seealso \code{\link{getProjectsCM}}
#' @export
getDataCM <- function(key = NULL, project = NULL, ...){
  
  dots <- list(...)
  
  # if the raw .json is required
  raw <- dots[["raw"]]
  if (is.null(raw)) { raw <- FALSE }
  
  if (!is.null(key)) {
    url <- "https://climmob.net/climmob3/api/readDataOfProject?Body={}&Apikey={}"
    
    cmdata <- httr::GET(url = url,
                        query = list(Body = paste0('{"project_cod":"', project, '"}'),
                                     Apikey = key),
                        httr::accept_json())
    
    cmdata <- httr::content(cmdata, as = "text")
    
    cmdata <- jsonlite::fromJSON(cmdata)
  }
  
  # if a .json data is provided instead of a CliMob key
  if (is.null(key)) {
    cmdata <- dots[["data"]]
  }
  
  
  # check if the given project has data
  # if not then return a warning message
  if (length(cmdata) < 7) {
    pstring <- paste0("'",project,"'")
    return(
      cat(
        "\nProject", pstring, "was found but has no associated data. \n"
      )
    )
  }
  
  if (!raw) {
    cmdata <- .extractFromjson(cmdata, ...)
  }
  
  return(cmdata)
  
}

.extractFromjson <- function(data, tidynames = TRUE, pivot.wider = FALSE){
  
  # currently the json file is structured with
  # data[[1]] 'specialfields', the assessment questions
  # data[[2]] 'project', the project details
  # data[[3]] 'registry', the questions during participant registration
  # data[[4]] 'importantfields', 
  # data[[5]] 'assessments', the survey in trial data assessment
  # data[[6]] 'packages', the packages info
  # data[[7]] 'data', the trial data assessment
  
  # get the names of assessments questions
  assess_q <- data[[1]]
  
  assess_q <- assess_q[,2]
  
  overallvslocal <- grepl("overallchar", assess_q)
  
  rank_q <- assess_q[!overallvslocal]
  
  overallvslocal <- assess_q[overallvslocal]
  
  # get variables names from participant registration
  regs <- data[[3]]
  ##regs_codes <- do.call("rbind", regs$lkptables$values)
  regs <- regs[[1]]
  
  regs_name <- paste0("REG_", regs[,1])
  
  # get variables names from assessments
  assess <- data[[5]]
  assess <- do.call("rbind", assess$fields)
  assess <- data.frame(assess, stringsAsFactors = FALSE)
  assess <- assess[!duplicated(assess[,1]), ]
  
  assess_id <- paste0("ASS", data[[5]]$code)
  assess_name <- data[[5]]$desc
  
  # trial data
  trial <- data[[7]]
  
  looknames <- assess[,1]
  
  looknames <- c(regs_name,
                 paste(rep(assess_id, each = length(looknames)), 
                       looknames, sep = "_"))
  
  trial <- lapply(looknames, function(x){
    i <- names(trial) %in% x
    y <- trial[i]
    y
  })
  
  trial <- do.call("cbind", trial)
  
  # split farmgeolocation info
  # check if geografic location is available
  geoTRUE <- grepl("farmgoelocation|ubicacion", names(trial))
  
  # if is available, then split the vector as lon lat
  if(any(geoTRUE)){
    
    geo_which <- which(geoTRUE)
    
    geo <- trial[geo_which]
    
    trial <- trial[!geoTRUE]
    
    for (i in seq_along(geo_which)){
      newname <- names(geo[i])
      newname <- gsub("_farmgoelocation|_ubicacion", "", newname)
      newname <- paste0(newname, c("_lat","_lon"))
      
      lonlat <- geo[i]
      
      lonlat[is.na(lonlat)] <- c("NA NA NA NA")
      
      lonlat <- tidyr::separate(lonlat, 1,
                                newname, 
                                sep = " ", 
                                remove = TRUE,
                                extra = "drop") 
      
      lonlat[lonlat == "NA"] <- NA
      
      lonlat <- lonlat[c(2,1)]
      
      trial <- cbind(trial, lonlat)
      
    }
  }
  
  
  # replace numbers in trial results by LETTERS
  trial[, rank_q] <-
    apply(trial[, rank_q], 2, function(x) {
      LETTERS[as.integer(x)]
    })
  
  # replace numbers in question about overall vs local 
  if (length(overallvslocal) > 1) {
    trial[, overallvslocal] <-
      apply(trial[, overallvslocal], 2, function(x) {
        ifelse(x == "1", "Better", 
               ifelse(x == "2", "Worse", x))
      })
  }
  
  # replace any possible code in participant registration
  # NOT SUPPORTED YET
  
  # reshape it into a long format 
  # put pack id as first colunm
  packid <- grepl("REG_qst162", names(trial))
  
  trial <- cbind(trial[packid], trial[!packid])
  
  trial <- tidyr::gather(trial, 
                         key = "variable",
                         value = "value", 
                         names(trial)[2:ncol(trial)])
  
  trial$moment <- "registration"
  
  # remove possible space in assess name
  assess_name <- gsub(" ", "", assess_name)
  
  # add which moment the data was taken
  for (i in seq_along(assess_id)) {
    trial$moment <- ifelse(grepl(assess_id[i], trial$variable),
                          tolower(assess_name[i]),
                          trial$moment)
    
  }
  
  names(trial)[1] <- "id"
  
  # comparisons and package
  comps <- data[[6]]$comps
  
  comps <- lapply(comps, function(x) {
    x <- unique(unlist(x$technologies))
    x <- x[-1]
    names(x) <- paste0("item_", LETTERS[1:length(x)])
    x
  })
  
  comps <- do.call("rbind", comps)
  
  comps <- as.data.frame(comps, stringsAsFactors = FALSE)
  
  pack <- cbind(data[[6]][2:3], comps)
  
  pack <- tidyr::gather(pack, 
                        key = "variable",
                        value = "value", 
                        names(pack)[2:ncol(pack)])
  
  pack$moment <- "package"
  
  names(pack)[1] <- "id"
  
  trial <- rbind(pack, trial)
  
  # check if ids from ODK names are required to be removed 
  if (tidynames){
    
    trial$variable <- gsub("REG_", "", trial$variable)
    
    for (i in seq_along(assess_id)) {
      trial$variable <- gsub(paste0(assess_id[i], "_"), "", trial$variable)
    }
    
    ovl <- which(grepl("perf_overallchar", trial$variable))
    
    trial[ovl, 2] <- sapply(trial[ovl, 2], function(x) {
      x <- strsplit(x, split = "_")
      x <- paste0("item_", LETTERS[as.integer(x[[1]][3])], "_vs_local")
      x
    })
    
    trial$variable <- gsub("char_", "", trial$variable)
    
    trial$variable <- gsub("stmt_", "pos", trial$variable)
    
    trial$variable <- gsub("clm_", "survey_", trial$variable)
    
    trial$variable[trial$variable == "farmername"] <- "participant_name"
    
  }
  
  output <- tibble::as_tibble(trial)
  
  output <- output[c(1,4,2,3)]
  
  # remove some ODK variables
  output <- output[!grepl("originid|rowuuid|qst163", output[[3]]), ]
  
  # reorder rows and make sure that packages and registration comes first
  assess_name <- sort(tolower(assess_name))
  output$moment <- factor(output$moment, levels = c("package",
                                                    "registration",
                                                    assess_name))
  
  output <- dplyr::arrange(output, moment)
  
  output$id <- as.integer(output$id)
  
  output <- dplyr::arrange(output, id)
  
  # if required, put the data in wide format
  if (pivot.wider) {
    output$variable <- paste(output$moment, output$variable, sep = "_")
    
    variable_levels <- unique(output$variable)
    
    id <- unique(output$id)
    
    output <- output[,-2]
    
    output <- tidyr::spread(output, variable, value)
    
    output <- dplyr::mutate(output, id = id)
    
    output <- output[c("id", variable_levels)]
  }
  
  return(output)
}
