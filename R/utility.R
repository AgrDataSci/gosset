# Find integer values in a vector with decimals
.is_wholenumber <- function(x) {
  x <- as.vector(t(x))
  x <- x %% 1 == 0
  return(x)
}

# Find a decimal number in a vector with integers
.is_decimal <- function(x) {
  x <- as.vector(t(x))
  x <- !x %% 1 == 0
  return(x)
}

# Test a grouped_rankings object
.is_grouped_rankings <- function(object) {
  
  return(class(object) == "grouped_rankings")
  
}

# Test a rankings object
.is_rankings <- function(object) {
  
  return(class(object) == "rankings")
  
}

# Test a paircomp object
.is_paircomp <- function(object) {
  
  return(class(object) == "paircomp")
  
}


# logical function for > greater 
.is_greater <- function(x, y) {
  x > y
}


# logical function for < lower
.is_lower <- function(x, y) {
  x < y
}


# Validate a tibble object
.is_tibble <- function(object) {
  
  c("tbl_df") %in% class(object)
  
}


# Round to the nearest base value
.round5 <- function(x, base.value) {
  
  base.value * round( x / base.value )
  
}


# Checks data in functions favourite and contests
.check_data <- function(data = NULL, 
                        items = NULL, 
                        input = NULL) {
  # keep only target columns in data
  if (!is.null(data)) {
    data <- tibble::as_tibble(data)
    items <- names(data[, items])
    input <- names(data[, input])
    data <- data[, c(items, input)]
  }
  
  # if 'items' and 'input' are provided as data.frame
  # put all together as 'data'
  if (is.null(data)) {
    data <- tibble::as_tibble(cbind(items, input))
    items <- names(items)
    input <- names(input)
  }
  
  # if 'data' is an object of class tbl_df
  # convert to "data.frame"
  if (.is_tibble(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  
  # check for NAs within data
  nalist <- apply(data, 2, is.na)
  nalist <- !apply(nalist, 1, any)
  
  # apply vector to data
  data <- data[nalist, ]
  
  if (!any(nalist)) {
    cat("Suppressing", sum(!nalist), "rows with missing data \n")
  }
  
  return(list(data = data, items = items, input = input))
}

