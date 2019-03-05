# library(httr)
# library(rvest)
# library(jsonlite)
# 
# 
# 
# list_projects <- function(key = NULL){
# 
#   url <- "https://climmob.net/climmob3/api/readProjects?Apikey="
# 
#   result <- httr::GET(url = url, query = list(Apikey = key), httr::accept_json())
# 
#   result <- httr::content(result, "parsed", encoding = "UTF-8")
# 
#   nodes <- rvest::html_node(result, css = "body")
# 
#   text <- rvest::html_text(nodes)
# 
#   data <- jsonlite::fromJSON(text)
# 
#   data
# 
# }
# 
# x <- list_projects("d39a3c66-5822-4930-a9d4-50e7da041e77")
# str(x)



