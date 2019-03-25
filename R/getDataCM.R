#' Get ClimMob data
#'
#' Fetch the trial data from a ClimMob project using your API key
#'
#' @param project a character for the project id
#' @param ... additional arguments passed to methods
#' @inheritParams getProjectsCM
#' @return A data frame with the project data
#' @examples
#' **## Not run:**
#' 
#' # This function will not work without your API key  
#' library("gosset")
#' library("jsonlite")
#' library("httr")
#' 
#' my_key <- "my_api_key"
#' my_project <- "my_climmob_project"
#' 
#' data <- getDataCM(key = my_key, project = my_project)
#' 
#' End(**Not run**)
#' 
#' @seealso \code{\link{getProjectsCM}}
#' @export
getDataCM <- function(key = NULL, project = NULL, ...){

  if (!is.null(key)) {
    url <- "https://climmob.net/climmob3/api/readDataOfProject?Body={}&Apikey={}"
    
    cmdata <- httr::GET(url = url,
                        query = list(Body = paste0('{"project_cod":"', project, '"}'),
                                     Apikey = key),
                        httr::accept_json())
    
    cmdata <- httr::content(cmdata, as = "text")
    
    cmdata <- jsonlite::fromJSON(cmdata)
  }
  
  if (is.null(key)) {
    dots <- list(...)
    cmdata <- dots[["data"]]
  }
  
  # check if the given project has data
  # if not then return a warning message
  if (length(cmdata) < 7) {
    return(
      cat(
        "\nNo data found for project: '",
        project,
        "'\nPlease check https://climmob.net/climmob3/ for details. \n"
      )
    )
  }
  
  cmdata <- .extractFromjson(cmdata, ...)
  
  return(cmdata)
  
}

.extractFromjson <- function(data, tidynames = TRUE, pivot.wider = FALSE){
  
  # currently the json file is structured with
  # data[[1]] 'specialfields', the assessment questions
  # data[[2]] 'project', the project details
  # data[[3]] 'registry', the ODK fields, not important here
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
  
  # get variables names from assessments
  assess <- data[[5]]
  assess <- do.call("rbind", assess$fields)
  assess <- data.frame(assess, stringsAsFactors = FALSE)
  assess <- assess[!duplicated(assess[,1]), ]
  
  assess_id <- paste0("ASS", data[[5]]$code)
  assess_name <- data[[5]]$desc
  
  # trial data
  trial <- data[[7]]
  
  looknames <- c("qst162", assess[,1])
  
  looknames <- c(paste("REG", looknames, sep = "_"),
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
  
  # reshape it into a long format 
  trial <- tidyr::gather(trial, 
                         key = "variable",
                         value = "value", 
                         names(trial)[2:ncol(trial)])
  
  trial$class <- "registration"
  
  # add class, which moment the data was taken
  for (i in seq_along(assess_id)) {
    trial$class <- ifelse(grepl(assess_id[i], trial$variable),
                          assess_name[i],
                          trial$class)
    
  }
  
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
  
  pack$class <- "package"
  
  names(pack)[1] <- "id"
  
  output <- rbind(pack, trial)
  
  output <- tibble::as_tibble(output)
  
  output <- output[c(1,4,2,3)]
  
  # reorder rows 
  output <- dplyr::arrange(output, id)
  
  # fix some variable names
  output$variable[output$variable == "farmername"] <- "participant_name"
  
  output <- output[!grepl("surveyid|originid|rowuuid|qst163", output[[3]]), ]
  
  output$variable <- gsub("clm_", "survey_", output[[3]])
  
  # if required, put the data in wide format
  if (pivot.wider) {
    output$variable <- paste(output$class, output$variable, sep = "_")
    
    output <- output[,-2]
    
    output <- tidyr::spread(output, variable, value)
  }
  
  return(output)
}
