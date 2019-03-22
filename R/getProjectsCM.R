#' Get ClimMob projects 
#'
#' Fetch the list of ClimMob projects using your API key
#'
#' @param key a character for the ClimMob user API key
#' @return A data frame with the ClimMob projects 
#' @examples
#' **## Not run:**
#' 
#' # This function will not work without your API key
#' 
#' my_key <- "my_api_key"
#' 
#' getProjectsCM(key = my_key)
#' 
#' End(**Not run**)
#' 
#' @import httr
#' @import jsonlite
#' @seealso \url{https://climmob.net/climmob3/}
#' @export
getProjectsCM <- function(key = NULL){

  url <- "https://climmob.net/climmob3/api/readProjects?Apikey="
  
  data <- httr::GET(url = url, 
                    query = list(Apikey = key), 
                    httr::accept_json())

  data <- httr::content(data, as = "text")
  
  data <- jsonlite::fromJSON(data)
  
  data <- data[,c("project_cod","project_name",
                 "project_active","project_creationdate")]

  names(data) <- c("project_id","name","status","creation_date")

  data <- tibble::as_tibble(data)
  
  return(data)

}