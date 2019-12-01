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
  
  return(any(class(object) == "grouped_rankings"))
  
}

# Test a rankings object
.is_rankings <- function(object) {
  
  return(any(class(object) == "rankings"))
  
}

# Test a paircomp object
.is_paircomp <- function(object) {
  
  return(any(class(object) == "paircomp"))
  
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

# rank decimal numbers
.rank_decimal <- function(object, id = NULL, ...){
  
  dots <- list(...)
  
  bindwith <- dots[["bindwith"]]
  
  isdf <- "data.frame" %in% class(object)
  
  if (!isdf) {
    object <- as.data.frame(object)
    names(object) <- "rank"
  }
  
  if (!is.null(bindwith)) {
    object <- cbind(object, bindwith)
  }
  
  if(is.null(id)) {
    id <- rep(1, nrow(object))
  }
  
  object <- cbind(id = id, object)
  
  object <- split(object, id)
  
  object <- lapply(object, function(x) {
    x$rank <- rank((x$rank - 1) * -1, na.last = "keep")
    return(x)
  })
  
  object <- do.call("rbind", object)
  
  object <- tibble::as_tibble(object) 
  
  object[,c("id","rank")] <- lapply(object[,c("id","rank")], 
                                    as.integer)
  
  return(object)
  
}

# check if objects has the same class
.same_class <- function(x, y) {
  
  class_x <- class(x)
  
  any(class(x) %in% class(y))

}

# check order of players based on their performance in contests
.player_order <- function(data, split, value) {
  
  # get sums of victories to order items from higher to lower
  p_order <- split(data, data[, split])
  
  p_order <- lapply(p_order, function(z) {
    sum(z[, value])
  })
  
  p_order <- sort(unlist(p_order))
  
  # get order
  p_order <- rev(names(p_order))
  
  return(p_order)
  
}

# decode rankings to avoid many dependencies
.decode_ranking <- function(items, rankings) {
  
  nc <- ncol(rankings)
  nr <- nrow(rankings)
  
  rankings <- split(rankings, rownames(rankings))
  
  index <- lapply(rankings, function(y) {
    
    order(y, na.last = NA)
    
  })
  
  index <- do.call("rbind", index)
  
  ranks <- matrix(NA, nrow = nr, ncol = nc)
  
  for (z in seq_len(nc)) {
    
    ranks[, z ] <- items[cbind(1:nr, index[, z])]
    
  }
  
  return(ranks)
  
}



