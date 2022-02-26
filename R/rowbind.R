#' Combine R objects by rows
#' 
#' Combine R objects when number and names of columns do not match 
#' 
#' @family utility functions
#' @param x a R object, typically a data.frame, matrix or list
#' @param y a matrix, a data.frame (or any other object that can
#' be coerced to data.frame)
#' @param ... additional arguments passed to methods
#' @return a data.frame with the combined data
#' @examples
#'  
#' # two data frames
#' rowbind(airquality, mtcars)
#' 
#' # a list of data frames
#' l <- list(airquality, mtcars)
#' rowbind(l)
#' 
#' @export
rowbind <- function(x, ...){
  UseMethod("rowbind")
}

#' @rdname rowbind
#' @export
rowbind.default <- function(x, y, ...){
  
  nm <- union(dimnames(x)[[2]], dimnames(y)[[2]])
  
  dt <- data.frame(matrix(NA, 
                          ncol = length(nm),
                          nrow = 0,
                          dimnames = list(NULL, nm)),
                   stringsAsFactors = FALSE)
  
  x <- list(x, y)
  
  # if dataset has different variables, this loop looks for the common variables
  # and merge the dataset
  for (i in seq_along(x)) {
    
    # take the data
    y <- x[[i]]
    
    # select the variable available in the data
    in_y <- nm %in% dimnames(y)[[2]]
    
    # if any missing variable, then add it as NAs
    if (any(!in_y)) {
      
      miss <- nm[!in_y]
      
      miss <- data.frame(matrix(NA, 
                                ncol = length(miss),
                                nrow = nrow(y),
                                dimnames = list(1:nrow(y), miss)),
                         stringsAsFactors = FALSE)
      
      y <- cbind(y, miss)
      
    }
    
    y <- y[, nm]
    
    # bind with the main data
    dt <- rbind(dt, y)
    
  }
  
  rownames(dt) <- seq_len(dim(dt)[[1]])
  
  return(dt)
  
}

#' @rdname rowbind
#' @method rowbind list
#' @export
rowbind.list <- function(x, ...){
  
  # look for the names across the list
  nm <- unlist(lapply(x, function(z){
    dimnames(z)[[2]]
    }))
  
  nm <- unique(nm)
  
  dt <- data.frame(matrix(NA, 
                          ncol = length(nm),
                          nrow = 0,
                          dimnames = list(NULL, nm)),
                   stringsAsFactors = FALSE)
  
  # the dataset has different variables, this loop looks for the common variables
  # and merge the dataset
  for (i in seq_along(x)) {
    
    # take the data
    y <- x[[i]]
    
    # select the variable available in the data
    in_y <- nm %in% dimnames(y)[[2]]
    
    # if any missing variable, then add it as NAs
    if (any(!in_y)) {
      
      miss <- nm[!in_y]
      
      miss <- data.frame(matrix(NA, 
                                ncol = length(miss),
                                nrow = nrow(y),
                                dimnames = list(1:nrow(y), miss)),
                         stringsAsFactors = FALSE)
      
      y <- cbind(y, miss)
      
    }
    
    y <- y[, nm]
    
    # bind with the main data
    dt <- rbind(dt, y)
    
  }
  
  rownames(dt) <- seq_len(dim(dt)[[1]])
  
  return(dt)
  
}

