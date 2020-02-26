#' Plackett-Luce rankings from numeric values
#'
#' Group and coerce numeric values into Plackett-Luce rankings.
#' 
#' @author Kauê de Sousa
#' @family rank functions
#' @inheritParams rank_tricot
#' @param id a data frame or index of \code{data} indicating the 
#'  ids for "long" \code{data}
#' @param ascending logical, only for floating point numbers, to 
#'  compute rankings from lower to higher values 
#' @param ... additional arguments passed to methods
#' @return a PlackettLuce "rankings" object, which is a matrix 
#'  of dense rankings
#' @seealso \code{\link[PlackettLuce]{rankings}} 
#' @examples
#' # A matrix with 10 rankings of 5 items (A, B, C, D, E)
#' # with numeric values as "rank"
#' set.seed(123)
#' df <- cbind(id = rep(1:10, each = 5),
#'             items = rep(LETTERS[1:5], times = 10),
#'             input = runif(50, 1, 3))
#' 
#' # return an object of class 'rankings'
#' R <- rank_numeric(df,
#'                   items = 2,
#'                   input = 3,
#'                   id = 1)
#' 
#' # rankings can be computed in ascending order
#' R <- rank_numeric(df,
#'                   items = 2,
#'                   input = 3,
#'                   id = 1,
#'                   ascending = TRUE)
#' 
#' 
#' # return an object of class 'grouped_rankings'
#' R <- rank_numeric(df,
#'                   items = 2,
#'                   input = 3,
#'                   id = 1,
#'                   grouped.rankings = TRUE)
#' 
#' #..............................................
#' #..............................................
#'  
#' # Rankings with 5 items randomly assigned
#' 
#' i <- as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(i) <- paste0("Item",1:5)
#' 
#' r <- as.data.frame(matrix(NA, nrow = 10, ncol = 5))
#' names(r) <- paste0("Position_Item",1:5)
#' 
#' for(s in 1:10) {
#'   i[s,] <- sample(LETTERS[1:5])
#'   r[s,] <- sample(1:5)
#' }
#' data <- cbind(i, r)
#' R <- rank_numeric(data = data,
#'                   items = c(1:5),
#'                   input = c(6:10))
#'  
#' @importFrom tibble tibble
#' @importFrom tidyr gather spread
#' @importFrom PlackettLuce as.rankings group
#' @export
rank_numeric <- function(data = NULL, items = NULL,
                         input = NULL, id = NULL, 
                         group = FALSE, ascending = FALSE, 
                         ...) {
  
  
  # keep only target columns in data
  if (is.null(data)) {
    stop("argument 'data' is missing with no default \n")
  }
  
  if (class(data)[1] != "data.frame") {
    data <- data.frame(data, stringsAsFactors = FALSE)
  }
  
  # get nrow in object
  n <- nrow(data)
  
  # get the items in data
  items <- data[items]
  
  if (is.null(id)) {
    id <- data.frame(id = rownames(data))
  } else {
    id <- data[, id]
  }
  
  # make sure that input are numeric
  data[input] <- lapply(data[input], as.numeric)
  
  r <- data[input]
  
  # fix names in r
  names(r) <- paste0("PosItem", 1:ncol(r))
  
  # get the number of possible rankings
  nrank <- ncol(r)
  
  # if data is oriented in the wide format
  if (nrank > 1) {
    
    # if there is any NA in items
    # add a pseudo-item which will be removed later
    if (sum(is.na(items)) > 0)  {
      for (p in seq_len(nrank)) {
        items[is.na(items[p]), p] <- paste0("pseudoitem", p)
      }
    }
    
    # add 0 if there is any missing ranking in r
    if (sum(is.na(r)) > 0)  {
      r[is.na(r)] <- 0
    }
    
    # combine items with rankings
    r <- cbind(id, items, r)
    
    # put rankings into a long format 
    r <- tidyr::gather(r, 
                       key = "variable",
                       value = "value",
                       names(r)[2:ncol(r)])
    
    # this vector checks which rows are the ranks and which 
    # are the item name
    pr <- grepl("PosItem", r[[2]])
    
    # create separate vectors and then merge it
    id <- r[pr,"id"]
    item <- r[!pr,"value"]
    rank <- as.numeric(r[pr,"value"])
    
    r <- data.frame(id, item, rank, stringsAsFactors = FALSE)
    
    names(r) <- c("id","item","rank")
    
    # if pseudo-item were added, it is removed now
    rmitem <- !r[["item"]] %in% paste0("pseudoitem", 1:nrank)
    r <- r[rmitem, ]
    
  }
  
  # if is in long format then
  if (nrank == 1) {
    # combine vectors
    r <- cbind(id, items, r)
    names(r) <- c("id","item","rank")
  }
  
  # if values in rankings are numeric 
  # then we group it by ids and convert it 
  # into integer ranks
  # the highest value is the best item
  # negative values are allowed
  # they are added in the last place
  if (any(.is_decimal(r[["rank"]]))) {
    
    r <- .rank_decimal(r$rank, id = r$id, bindwith = r$item)
    
    names(r)[3] <- "item"
    
    # if the rankings are required to be ascending 
    if (ascending) {
      
      r <- .asc_rank(r)
      
    }
    
  }
  
  # reshape data into wide format
  r <- tidyr::spread(r, item, rank)
  
  # replace possible NA's with zeros (0) as required for PlackettLuce
  r[is.na(r)] <- 0
  
  # arrange observations by ids
  r <- r[order(r$id), ]
  
  # drop id
  r <- r[ ,-match("id", names(r))]
  
  # dataframe into matrix
  r <- as.matrix(r)
  
  
  # make a PlackettLuce rankings
  R <- PlackettLuce::as.rankings(r)
  
  # and a PlackettLuce grouped_rankings
  n <- nrow(R)
  G <- PlackettLuce::group(R, index = seq_len(n))
    
  # return a grouped_rankings if required
  if (group) {
    R <- G
  }
  
  return(R)
  
}

#' Compute an ascending rank
#' @param object a data.frame internaly formated by rank_numeric
#' @return the object with rankings in the ascending order, 
#' meaning that lower is better
#' @examples 
#' set.seed(123)
#' r <- data.frame(id = rep(1:3, each = 4),
#'                 items = rep(LETTERS[1:4], times = 3),
#'                 rank = as.numeric(runif(12, 1, 3)),
#'                 stringsAsFactors = FALSE)
#' 
#' r <- gosset:::.rank_decimal(r$rank, id = r$id, bindwith = r$item)
#' 
#' r <- gosset:::.asc_rank(r)
#' @noRd
.asc_rank <- function(object){
  
  object <- split(object, object$id)
  
  object <- lapply(object, function(x){
    
    x$rank <- x$rank * -1
    
    x$rank <- rank(x$rank, na.last = "keep")
    
    x
    
  })
  
  object <- do.call("rbind", object)
  
  return(object)
  
}

#' Rank decimal numbers
#' @param object a vector with floating point numbers
#' @param id optional, a vector with ids to group values
#' @param bindwith optional, a data.frame to cbind with ranked values
#' @return A data frame with ranked floating point numbers
#' @examples 
#' # without id
#' .rank_decimal(c(0.2, -1.2, 2.3, 0.2, 0.4, -3.3))
#' 
#' # with id
#' .rank_decimal(c(0.2, -1.2, 2.3, 0.2, 0.4, -3.3),
#'               id = c(1,1,1,2,2,2))
#'
#' @importFrom tibble as_tibble
#' @noRd
.rank_decimal <- function(object, id = NULL, bindwith = NULL){
  
  isdf <- is.data.frame(object)
  
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
