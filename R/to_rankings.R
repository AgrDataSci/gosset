#' Plackett-Luce rankings
#'
#' Create an object of class "rankings" from a dataframe or matrix
#'
#' @param data a data frame with columns specified by items and rankings. Data can be in both wide and long format.
#' An id is required if long format.
#' @param items a data frame or vector representing the item names
#' @param rankings a data frame or vector representing the rankings
#' @param type the type of data input. Options are 'tricot' and 'rank'
#' @param ... further arguments passed to PlackettLuce::rankings
#' @return a "rankings" object, which is a matrix of dense rankings 
#' @seealso \code{\link[PlackettLuce]{rankings}}
#' @examples
#' 
#' library("PlackettLuce")
#' 
#' # A matrix with 10 rankings of 5 items (A, B, C, D, E)
#' # with numeric values as "rank"
#' set.seed(123)
#' df <- cbind(id = rep(1:10, each = 5),
#'             items = rep(LETTERS[1:5], times = 10),
#'             rankings = runif(50, 1, 3))
#' df
#' 
#' # return a 'rankings' object
#' to_rankings(df,
#'             items = 2,
#'             rankings = 3,
#'             id = 1)
#' 
#' # return a 'grouped_rankings' object
#' to_rankings(df,
#'             items = 2,
#'             rankings = 3,
#'             id = 1,
#'             grouped.rankings = TRUE)
#' 
#' ##################################
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
#' 
#' cbind(i, r) 
#' 
#' 
#' to_rankings(items = i,
#'             rankings = r)
#' 
#' ###################################
#' 
#' # breadwheat data, which is an object ordered in the 'tricot' format
#' # each observer compares 3 varieties randomly distributed from a list of 16 
#' data("breadwheat", package = "gosset")
#' 
#' to_rankings(breadwheat,
#'             items = c("variety_a","variety_b","variety_c"),
#'             rankings = c("best","worst"),
#'             type = "tricot")
#' 
#' ######################
#' 
#' # beans data which is an object ordered in the 'tricot'format
#' # each observer compares 3 varieties randomly distributed from a list of 11
#' # and additionally compares these 3 varieties with their local variety
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' # comparison with local item is added as an additional rankings, then
#' # each of the 3 varieties are compared separately with the local item,
#' # it return a object four times larger (in rows) than the input data
#' # combining this with covariates from other dataset is easy since 
#' # the function keeps an internal id
#' to_rankings(beans,
#'             items = c(1:3),
#'             rankings = c(4:5),
#'             local = c(6:8),
#'             type = "tricot",
#'             grouped.rankings = TRUE)
#' 
#' @import tidyverse
#' @importFrom reshape2 melt
#' @export

to_rankings <- function(data = NULL, items = NULL, rankings = NULL, type = NULL, ...) {
  
  if (is.null(data)) {
    data <- cbind(items, rankings)
    items <- names(items)
    rankings <- names(rankings)
  }
  
  if (!is.data.frame(data)){
    data <- data.frame(data, stringsAsFactors = FALSE)
  }
  
  if (is.null(items)) {
    stop("argument 'items' is missing with no default \n")
  }
  
  if (is.null(rankings)) {
    stop("argument 'rankings' is missing with no default \n")
  }

  if (is.null(type)) {
    type <- "rank"
  }
  
  if (!type %in% c("tricot", "rank")) {
    stop("type ", type, " is not a valid method. Options are 'rank' and 'tricot' \n")
  }
  
  # get nrow in object
  n <- nrow(data)

  # get the items in data
  items <- data[items]
  names(items) <- paste0("Item", 1:ncol(items))
  
  # get extra arguments
  dots <- list(...)
  
  if (type == "rank") {
    
    # take or create an id
    id <- dots[["id"]]
    if(is.null(id)){
      id <- tibble::tibble(id = rownames(data))
    }else{
      id <- data[id]
    }
    
    # make sure that rankings are numeric
    data[rankings] <- lapply(data[rankings], as.numeric)
    
    R <- .fromrank(id, items, r = data[rankings])
    
    if ("grouped.rankings" %in% names(dots)) {
      n <- nrow(R)
      
      R <- PlackettLuce::grouped_rankings(R, index = seq_len(n))
    }
    
    } 
    
  if (type == "tricot") {
    
    # check if comparison with a local item is required
    if("local" %in% names(dots)){
      local.rankings <- dots[["local"]]
      local.rankings <- data[local.rankings]
    } else {
      local.rankings <- NULL
    }
    
    R <- .fromtricot(items, r = data[rankings], local.rankings)
    
    if ("grouped.rankings" %in% names(dots)) {
      
      if (!is.null(local.rankings)) {
        R <- PlackettLuce::grouped_rankings(R, index = rep(seq_len(n), 4))
      }else{
        R <- PlackettLuce::grouped_rankings(R, index = seq_len(n))
      }
    }
    
  }

  return(R)

}


# organise rankings into a PlackettLuce ranking object
.fromrank <- function(id, items, r){
  # fix names in r data 
  names(r) <- paste0("PosItem",1:ncol(r))
  # get the number of possible rankings
  nrank <- ncol(r)
  
  # if data is oriented in the wide format
  if (nrank > 1) {
    
    # if there is any NA in items
    # add a pseudo-item which will be removed later
    if (sum(is.na(items)) > 0)  {
      for (i in seq_len(nrank)) {
        items[is.na(items[i]), i] <- paste0("pseudoitem",i)
      }
    }
    
    # add 0 if there is any missing ranking in r
    if (sum(is.na(r)) > 0)  {
      r[is.na(r)] <- 0
    }
    
    # combine items with rankings
    r <- cbind(id, items, r)
    
    # convert data into long format
    r <- cbind(reshape2::melt(r[c(paste0("Item", 1:nrank), "id")], 
                              id.vars = "id"),
               reshape2::melt(r[c(paste0("PosItem", 1:nrank), "id")], 
                              id.vars = "id"))
    
    # select only id, items and rankings
    r <- r[c(1,3,6)]
    names(r) <- c("id","item","rank")
    
    # if pseudo-item were added, it is removed now
    rmitem <- !r[["item"]] %in% paste0("pseudoitem", 1:nrank)
    r <- r[rmitem,]
    
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
  if (any(is_decimal(r[["rank"]]))) {
    r <- num2rank(r)
  }
  
  # reshape data into wide format
  r <- tidyr::spread(r, item, rank)
  
  # replace possible NA's with zeros (0) as required for PlackettLuce
  r[is.na(r)] <- 0
  
  # arrange observations by ids
  r <- dplyr::arrange(r, as.integer(id))
  
  # drop id
  r <- r[ ,-match("id", names(r))]
  
  # dataframe into matrix
  R <- as.matrix(r)
  
  # make a PlackettLuce ranking
  R <- PlackettLuce::as.rankings(R)
  
  return(R)
  
} 


# organise tricot rankings into a PlackettLuce ranking object
.fromtricot <- function(items, r, local.rankings){
  
  n <- nrow(items)
  
  # fix names in r
  # first column must always be the best and the 
  # second the worst
  names(r) <- c("best", "worst")
  
  # rankings can be LETTERS (A, B, C) or integer (1, 2, 3)
  # convert it to factor and then to integer in case values are LETTERS
  # keep as integer to allow us to impute the middle-ranked item
  # (a strict ranking is assumed here, so the sum of each row should always be 6)
  r <- within(r,{ 
    best = as.integer(as.factor(best))
    worst = as.integer(as.factor(worst))
    middle = 6 - best - worst
  })
  
  # if there is any NA in items and observations with only two items
  # add a pseudo-item which will be removed later
  if (sum(is.na(items)) > 0)  {
    items[is.na(items)] <- "pseudoitem"
  }
  
  # combine items with rankings
  r <- cbind(items, r)
  
  # convert items in a matrix
  items <- as.matrix(items)
  
  # then convert the itemsIDs to the item names
  r <- within(r,{
    best = items[cbind(seq_len(n), best)]
    worst = items[cbind(seq_len(n), worst)]
    middle = items[cbind(seq_len(n), middle)]
  })
  
  # get vector with item names
  itemnames <- sort(unique(unlist(data.frame(items, stringsAsFactors = FALSE ))))
  
  # convert the orderings of the items given to each observer to 
  # sub-rankings of the full set of varieties:
  R <- PlackettLuce::as.rankings(r[c("best","middle","worst")], 
                                 input = "ordering", 
                                 labels = itemnames)
  
  # if pseudo-item were added, it is removed
  if ("pseudoitem" %in% itemnames) {
    R <- R[,-match("pseudoitem", itemnames)]
  }
  
  # if comparison with local is required then use it
  if (!is.null(local.rankings)) {
    
    # get local.rankings in a separeted object
    lr <- local.rankings
    names(lr) <- paste0("localVSitem", 1:3)
    
    #add lr to r
    r <- cbind(r, lr)
    
    # treat the paired comparisons as additional rankings.
    # first we can convert the orderings of the trial varieties to 
    # sub-rankings of the full set of items including the local 
    # as an additional item, so that we can add the paired comparisons 
    # shortly the comparisons with the local item are stored 
    # in another set of columns
    
    # add local to itemnames
    itemnames <- c("Local", itemnames)
    
    paired <- list()
    
    for (id in 1:3) {
      ordering <- matrix("Local", nrow = n, ncol = 2)
      worse <- r[[paste0("localVSitem", id)]] == "Worse"
      ## name of winner
      ordering[!worse, 1] <- r[[paste0("Item", id)]][!worse]
      ## name of loser
      ordering[worse, 2] <- r[[paste0("Item", id)]][worse]
      paired[[id]] <- ordering
    }
    # again we convert these orderings to sub-rankings of the full set of items
    # and combine them with the rankings of order three:
    paired <- lapply(paired, function(x) {
      x <- PlackettLuce::as.rankings(x, input = "ordering", labels = itemnames)
    })
    
    R <- rbind(R, paired[[1]], paired[[2]], paired[[3]])
    
  }
  
  return(R)
  
}





