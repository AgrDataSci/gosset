#' Plackett-Luce rankings
#'
#' Create an object of class "rankings" from a dataframe or matrix
#'
#' @param data a data frame with columns specified by items and rankings. Data can be in both wide and long format.
#' An id is required if long format.
#' @param items a data frame or vector representing the item names
#' @param rankings a data frame or vector representing the rankings
#' @param type optional, the type of data input, 'rank' is set by default
#' @param ... other arguments passed to methods
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
#' # it return a object 1 + n_c (number of comparisons) larger (in rows) than the input data
#' # combining this with covariates from other dataset is easy since 
#' # the function keeps an internal id
#' # argument 'add.rank' must be passed as a dataframe
#' to_rankings(data = beans,
#'             items = c(1:3),
#'             rankings = c(4:5),
#'             type = "tricot",
#'             add.rank = beans[c(6:8)],
#'             grouped.rankings = TRUE)
#' 
#' @import tidyverse
#' @export
to_rankings <- function(data = NULL, items = NULL,
                        rankings = NULL, type = NULL, ...) {
  
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
    
    r <- .pivot_default(id, i = items, r = data[rankings])
    
    # make a PlackettLuce ranking
    R <- PlackettLuce::as.rankings(r)
    
    if ("grouped.rankings" %in% names(dots)) {
      n <- nrow(R)
      
      R <- PlackettLuce::grouped_rankings(R, index = seq_len(n))
    }
    
    } 
    
  if (type == "tricot") {
    
    ncomp <- ncol(items)
    
    if (ncomp == 3) {
      r <- .pivot_triadic(i = items, r = data[rankings])
    }
    
    if (ncomp >= 4) {
      r <- .pivot_tetra(i = items, r = data[rankings])
    }
    
    # get names of all items
    itemnames <- sort(unique(unlist(r)))
    
    # convert it into a PlackettLuce rank
    R <- PlackettLuce::as.rankings(r, input = "ordering", labels = itemnames)
    
    # if pseudo-item were added, it is removed
    if (any(grepl("pseudoitem", itemnames))) {
      R <- R[, !grepl("pseudoitem", itemnames)]
    }
    
    # check if comparison with a local item is required
    local <- dots[["add.rank"]]
    if (!is.null(local)) {
      # add comparisons with local rankings
      R <- .additional_rankings(i = items, R = R, add = local)
    }
 
    # this is used in ClimMob
    if ("all.data" %in% names(dots)) {
      R <- list(r, R)
    } 
    
    # and into a grouped_rankings
    if ("grouped.rankings" %in% names(dots)) {
      if (!is.null(local)) {
        R <- PlackettLuce::grouped_rankings(R, index = rep(seq_len(n), (1 + ncomp) ))
      }
      if (is.null(local)) {
        R <- PlackettLuce::grouped_rankings(R, index = seq_len(n))
      }
    }
    
  }
  
  return(R)

}


# organise numbered rankings
.pivot_default <- function(id, i, r){
  
  # fix names in r data
  names(r) <- paste0("PosItem", 1:ncol(r))
  
  # get the number of possible rankings
  nrank <- ncol(r)
  
  # if data is oriented in the wide format
  if (nrank > 1) {
    
    # if there is any NA in items
    # add a pseudo-item which will be removed later
    if (sum(is.na(i)) > 0)  {
      for (p in seq_len(nrank)) {
        i[is.na(i[p]), p] <- paste0("pseudoitem", p)
      }
    }
    
    # add 0 if there is any missing ranking in r
    if (sum(is.na(r)) > 0)  {
      r[is.na(r)] <- 0
    }
    
    # combine items with rankings
    r <- cbind(id, i, r)
    
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
    r <- r[rmitem, ]
    
  }
  
  # if is in long format then
  if (nrank == 1) {
    # combine vectors
    r <- cbind(id, i, r)
    names(r) <- c("id","item","rank")
  }
  
  # if values in rankings are numeric 
  # then we group it by ids and convert it 
  # into integer ranks
  # the highest value is the best item
  # negative values are permited
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
  r <- as.matrix(r)
  
  return(r)
  
} 

# this function deals with object in the triadic approach
# in ClimMob when three items are tested by each participant
# i, is a dataframe with items
# r, is a dataframe with rankings 
.pivot_triadic <- function(i, r) {
  
  n <- nrow(i)
  
  # fix names in rankings
  # first column must be the best item
  # and the second the worst
  names(r) <- c("best", "worst")
  # rankings can be LETTERS (A, B, C) or integer (1, 2, 3)
  # convert it to factor and then to integer to prevent cases when they are LETTERS
  # keep as integer to allow us to impute the middle-ranked item
  # (a strict ranking is assumed here, so the sum of each row should always be 6)
  r <- within(r, {
    best = as.integer(as.factor(best))
    worst = as.integer(as.factor(worst))
    middle = 6 - best - worst
  })
  
  # if there is any NA in items and observations with only two items
  # add a stopper pseudo-item which will be removed later
  if (sum(is.na(i)) > 0)  {
    i[is.na(i)] <- "pseudoitem"
  }
  
  # combine items with rankings
  r <- cbind(i, r)
  
  # convert items into a matrix
  i <- as.matrix(i)
  
  # then replace rankings integers with their respective item names
  r <- within(r, {
    best = i[cbind(seq_len(n), best)]
    worst = i[cbind(seq_len(n), worst)]
    middle = i[cbind(seq_len(n), middle)]
  })
  
  r <- r[, c("best", "middle", "worst")]
  
  return(r)
  
}

# this function deals with object in the tetra approach
# in ClimMob when four or more items are tested by each participant
# i, is a dataframe with items
# r, is a dataframe with rankings 
.pivot_tetra <- function(i, r) {
  
  ncomp <- ncol(i)
  
  # set names in items to mach with rankings
  names(i) <- LETTERS[1:ncomp]
  
  # rankings can be LETTERS (A, B, C) or integer (1, 2, 3)
  # convert it to factor and then to integer to prevent cases when they are LETTERS
  r <- t(apply(r, 1, function(x) {
    x <- as.factor(x)
    x <- as.integer(x)
    x
  }))
  
  # if there is any NA in items and observations with only two items
  # add a stopper pseudo-item which will be removed later
  if (sum(is.na(i)) > 0)  {
    for (p in seq_len(ncomp)) {
      i[is.na(i[p]), p] <- paste0("pseudoitem",p)
    }
  }
  
  # add 0 if there is any missing ranking in r
  if (sum(is.na(r)) > 0)  {
    r[is.na(r)] <- 0
  }
  
  # combine items with rankings
  r <- cbind(i, r)
  
  # items into a matrix
  i <- as.matrix(i)
  
  # then replace rankings integers with their respective item names
  r <- t(apply(r, 1, function(x) {
    y <- as.integer(x[grepl("Pos", names(r))])
    x <- x[y]
  }))
  
  # set standar names in rankings data
  dimnames(r)[[2]] <- paste0("Pos", 1:ncomp)
  
  return(r)
  
}

# this function adds additional ranks, generally when a local item 
# is tested against the tricot items
# i, is a dataframe with items
# R, is an object of class rankings from PlackettLuce
# add, is a dataframe with additional rankings characters 
## indication whether the tricot items performed "Better" or "Worse" 
## compared to the local item
.additional_rankings <- function(i, R, add){
  
  n <- nrow(add)
  
  ncomp <- ncol(i)
  
  # convert it into characters
  add[1:ncol(add)] <- lapply(add[1:ncol(add)], as.character)
  
  add <- as.matrix(add)
  
  i <- as.matrix(i)
  
  # treat this comparisons as additional rankings.
  # first we can convert the orderings of the items to 
  # sub-rankings of the full set of items including the additional items 
  # so that we can add the paired comparisons 
  # the comparisons with the additional items are stored 
  # in another set of columns
  
  # make sure that values in add are integers 
  # where 1 means Better and 2 means Worse
  add <- t(apply(add, 1, function(x) {
    x <- as.factor(x)
    x <- as.integer(x)
    x
  }))
  
  # add local to itemnames
  itemnames <- dimnames(R)[[2]]
  itemnames <- unique(c("Local", itemnames))
  
  paired <- list()
  
  for (id in 1:ncomp) {
    ordering <- matrix("Local", nrow = n, ncol = 2)
    worse <- add[, id] == 2
    ## name of winner
    ordering[!worse, 1] <- i[, id][!worse]
    ## name of loser
    ordering[worse, 2] <- i[, id][worse]
    paired[[id]] <- ordering
  }
  
  # we then convert these orderings to sub-rankings of the full set of items
  # and combine them with the rankings
  paired <- lapply(paired, function(x) {
    x <- PlackettLuce::as.rankings(x, input = "ordering", labels = itemnames)
  })
  
  R <- rbind(R, do.call("rbind", paired))  
  
  return(R)
  
}