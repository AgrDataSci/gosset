#' Plackett-Luce rankings
#'
#' Create an object of class "rankings" from a dataframe or matrix
#'
#' @param data a data frame with columns specified by items and input values to rank. Data can be in both wide and long format.
#' An id is required if long format. 
#' @param items a data frame or index of \code{data} specifying the column(s) containing the item names
#' @param input a data frame or index of \code{data} specifying the column(s) containing the values to be ranked
#' @param ... additional arguments passed to methods
#' @return a PlackettLuce "rankings" object, which is a matrix of dense rankings 
#' @seealso \code{\link[PlackettLuce]{rankings}}
#' @examples
#'  
#' # A matrix with 10 rankings of 5 items (A, B, C, D, E)
#' # with numeric values as "rank"
#' set.seed(123)
#' df <- cbind(id = rep(1:10, each = 5),
#'             items = rep(LETTERS[1:5], times = 10),
#'             input = runif(50, 1, 3))
#' 
#' # return an object of class 'rankings'
#' R <- to_rankings(df,
#'                  items = 2,
#'                  input = 3,
#'                  id = 1)
#' 
#' # rankings can be computed in ascending order
#' R <- to_rankings(df,
#'                  items = 2,
#'                  input = 3,
#'                  id = 1, 
#'                  ascending = TRUE)
#' 
#' 
#' # return an object of class 'grouped_rankings'
#' R <- to_rankings(df,
#'                  items = 2,
#'                  input = 3,
#'                  id = 1,
#'                  grouped.rankings = TRUE)
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
#' R <- to_rankings(items = i,
#'                  rankings = r)
#'
#' ###################################
#' 
#' # breadwheat data, which is an object ordered in the 'tricot' format
#' # each observer compares 3 varieties randomly distributed from a list of 16 
#' data("breadwheat", package = "gosset")
#'   
#' R <- to_rankings(breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"),
#'                  type = "tricot")
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
#' R <- to_rankings(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  type = "tricot",
#'                  add.rank = beans[c(6:8)],
#'                  grouped.rankings = TRUE)
#' 
#' @importFrom dplyr arrange bind_cols group_by mutate  
#' @import tibble
#' @import tidyr
#' @export
to_rankings <- function(data = NULL, items = NULL,
                        input = NULL, ...) {
  
  # get extra arguments
  dots <- list(...)
  
  rankings <- dots[["rankings"]]
  
  if(!is.null(rankings)) {
    warning("Argument 'rankings' is deprecated, use 'input' instead \n")
    
    input <- rankings
    
  }
  
  
  if (is.null(data)) {
    data <- cbind(items, input)
    items <- names(items)
    input <- names(input)
  }
  
  if (!is.data.frame(data)){
    data <- data.frame(data, stringsAsFactors = FALSE)
  }
  
  if (is.null(items)) {
    stop("argument 'items' is missing with no default \n")
  }
  
  if (is.null(input)) {
    stop("argument 'input' is missing with no default \n")
  }

  # get nrow in object
  n <- nrow(data)

  # get the items in data
  items <- data[items]
  
  # the type of data 
  type <- dots[["type"]]
  if (is.null(type)) { type = "rank" }
  # the ids (a vector) for objects of type "rank"
  id <- dots[["id"]] 
  # the comparisons with an additional input, if required
  add.rank <- dots[["add.rank"]] 
  # if a grouped_input is required
  grouped.rankings <- dots[["grouped.rankings"]]
  grouped.rankings <- isTRUE(grouped.rankings)
  # if all data is required
  all.data <- dots[["all.data"]]
  all.data <- isTRUE(all.data)
  # if the rank must be computed in ascending order
  ascending <- dots[["ascending"]]
  if (is.null(ascending)) {ascending <- FALSE}
  
  # deal with data of type "rank"
  if (type == "rank") {
    
    if(is.null(id)){
      id <- tibble::tibble(id = rownames(data))
    }else{
      id <- data[id]
    }
    
    # make sure that input are numeric
    data[input] <- lapply(data[input], as.numeric)
    
    r <- .pivot_default(id, i = items, r = data[input], ascending)
    
    # make a PlackettLuce rankings
    R <- PlackettLuce::as.rankings(r)
    
    # and a PlackettLuce grouped_rankings
    n <- nrow(R)
    G <- PlackettLuce::grouped_rankings(R, index = seq_len(n))
  
  }
  
  # deal with data of type "tricot"  
  if (type == "tricot") {
    
    ncomp <- ncol(items)
    
    # with 3 comparisons
    if (ncomp == 3) {
      rrank <- data[input]
      
      if (any(rrank[,1] == rrank[,2])) {
        stop("to_rankings cannot handle ties in objects of type 'tricot'\n")
      }
      
      r <- .pivot_triadic(i = items, r = data[input])
    }
    
    # with 4 or more comparisons
    if (ncomp >= 4) {
      r <- .pivot_tetra(i = items, r = data[input])
    }
    
    # get names of all items
    itemnames <- sort(unique(as.vector(r)))
    
    # convert it into a PlackettLuce rank
    R <- PlackettLuce::as.rankings(r, input = "ordering", labels = itemnames)
    
    # if pseudo-item were added, it is removed
    pseudo <- grepl("pseudoitem", itemnames) 
    if (any(pseudo)) {
      R <- R[, !pseudo]
    }
    
    # check if additional rankings are required
    if (!is.null(add.rank)) {
      # add comparisons with local rankings
      R <- .additional_rankings(i = items, R = R, add = add.rank)
    }
    
    # and into a grouped_rankings
    gi <- rep(seq_len(n), (nrow(R) / n))
    G <- PlackettLuce::grouped_rankings(R, index = gi)
 
  }
  
  # check if all data is required
  if (all.data) {
    R <- list(PLranking = R, PLgrouped = G, myrank = r)
  }
  
  # return a grouped_rankings if required
  if (grouped.rankings) {
    R <- G
  }
  
  return(R)

}


# organise numbered rankings
.pivot_default <- function(id, i, r, ascending){
  
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

    r <- num2rank(r$rank, id = r$id, bindwith = r$item)
    
    names(r)[3] <- "item"
    
  }
  
  # if the rankings are required to be ascending 
  if (ascending) {
    
    r <- .asc_rank(r)
  
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
  
  # rankings should be LETTERS (A, B, C), 
  # we must convert it into factor and then into integer 
  # it allow us to impute the middle-ranked item
  # (a strict ranking is assumed here, so the sum of each row should always be 6)
  r <- within(r, {
    best = as.integer(factor(best, levels = LETTERS[1:3]))
    worst = as.integer(factor(worst, levels = LETTERS[1:3]))
    middle = as.integer(6 - best - worst)
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
  
  r <- as.matrix(r)
  
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
  
  # rankings should be LETTERS (A, B, C) 
  # convert it to factor and then to integer 
  r <- t(apply(r, 1, function(x) {
    x <- factor(x, levels = LETTERS[1:ncomp])
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
  
  # set standar names in rankings data
  dimnames(r)[[2]] <- paste0("Pos", 1:ncomp)
  
  # combine items with rankings
  r <- cbind(i, r)
  
  # items into a matrix
  i <- as.matrix(i)
  
  # then replace rankings integers with their respective item names
  r <- t(apply(r, 1, function(x) {
    y <- as.integer(x[grepl("Pos", names(r))])
    x <- x[y]
  }))
  
  r <- as.matrix(r)
  
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
  
  # treat these comparisons as additional rankings.
  # first we convert the orderings of the items to 
  # sub-rankings of the full set of items including the additional items 
  # so we add the paired comparisons
  
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
  
  for (p in seq_len(ncomp)) {
    ordering <- matrix("Local", nrow = n, ncol = 2)
    worse <- add[, p] == 2
    # name of winner
    ordering[!worse, 1] <- i[, p][!worse]
    # name of loser
    ordering[worse, 2] <- i[, p][worse]
    paired[[p]] <- ordering
  }
  
  # we then convert these orderings to sub-rankings of the full set of items
  # and combine them with the rankings
  paired <- lapply(paired, function(x) {
    x <- PlackettLuce::as.rankings(x, input = "ordering", labels = itemnames)
  })
  
  R <- rbind(R, do.call("rbind", paired))  
  
  return(R)
  
}


# compute a ascending rank 
.asc_rank <- num2rank <- function(object){
  
  object <- dplyr::mutate(object, 
                          rank = rank * -1)
  
  object <- dplyr::mutate(dplyr::group_by(object, id),
                          rank = rank(rank, na.last = "keep"))
  
  
  object <- tibble::as_tibble(object)
  
  return(object)
  
}




