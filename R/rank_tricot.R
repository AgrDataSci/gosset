#' Build Plackett-Luce rankings from tricot dataset 
#'
#' Create an object of class "rankings" from tricot data. Tricot stands 
#' for "triadic comparison of technologies". Is a methodology to carry out 
#' large agronomic field experiments allowing the comparison between many 
#' alternative technologies, in many different environments. Each participant
#' evaluates a set of three randomised technologies from a larger set. A 
#' comparison with a local item can be added as additional rankings with the 
#' argument \code{additional.rank}.
#'
#' @author Kauê de Sousa and Jacob van Etten, with ideas from Heather Turner
#' @family rank functions
#' @param data a data.frame with columns specified by items and input values
#' @param items a character or numerical vector for indexing the column(s) 
#' containing the item names in \code{data} 
#' @param input a character or numerical vector for indexing the column(s) 
#' containing the values in \code{data} to be ranked 
#' @param group logical, if \code{TRUE} return an object of class "grouped_rankings"
#' @param additional.rank optional, a data frame for the comparisons between 
#' tricot items and the local item
#' @param ... additional arguments passed to methods. See details
#' @return a PlackettLuce "rankings" object, which is a matrix of dense rankings 
#' @seealso \code{\link[PlackettLuce]{rankings}}, \code{\link{breadwheat}}
#' @references 
#' 
#' van Etten J., et al. (2016). Experimental Agriculture, 55(S1), 275–296.
#' \doi{https://doi.org/10.1017/S0014479716000739}
#' 
#' @details 
#' full.output: logical, to return a list with a "rankings", 
#' a "grouped_rankings" and the ordered items
#' 
#' @examples
#' 
#' # using breadwheat data
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'rankings' from PlackettLuce
#' R <- rank_tricot(data = breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"))
#'                  
#' ############################################################
#'   
#' # beans data where each observer compares 3 varieties randomly distributed
#' # from a list of 11 and additionally compares these 3 varieties
#' # with their local variety
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' # first build rankings with only tricot items
#' # and return an object of class 'rankings'
#' R <- rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5))
#' head(R)
#' 
#' ############################################################
#' 
#' # pass the comparison with local item as an additional rankings, then
#' # each of the 3 varieties are compared separately with the local item
#' # and return an object of class grouped_rankings
#' G <- rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  group = TRUE,
#'                  additional.rank = beans[c(6:8)])
#' 
#' head(G)
#' 
#' @importFrom PlackettLuce as.rankings group
#' @export
rank_tricot <- function(data, items, input, 
                        group = FALSE, additional.rank = NULL, 
                        ...) {
  
  # if tibble coerce into a data.frame
  if (.is_tibble(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  
  items <- data[, items]
  
  input <- data[, input]
  
  # get nrow
  n <- nrow(data)
  
  # get extra arguments
  dots <- list(...)
  
  # if all data is required
  full.output <- dots[["full.output"]]

  # check number of comparisons to decide which way data will 
  # be handled, if type is tricot or if it contains more comparisons
  ncomp <- ncol(items)
  
  # with 3 comparisons
  if (ncomp == 3) {
    
    n <- nrow(items)
    
    # check for more than two missing labels in items
    mi <- rowSums(apply(items, 2, is.na))
    if (any(mi > 1)) {
      stop("Cannot handle more than 2 NAs per row in 'items', 
           more than 2 NAs where found in rows ", 
           paste(which(mi > 1), collapse = ", "), "\n")
    }
    
    # if there is one NA  per row in items and observations 
    # with only two items add a pseudo-item which will be removed later
    if (any(mi == 1))  {
      items[is.na(items)] <- "pseudoitem"
    }
    
    # data frame with items as matrix
    im <- as.matrix(items)
    
    # get the names of items
    itemnames <- unique(as.vector(im))
    
    # a Sparse matrix where rows are the observations 
    # and columns the item names
    r <- matrix(0, nrow = n, ncol = length(itemnames))
    colnames(r) <- itemnames
    
    # run over the rows filling the rankings that were observed 
    for(j in seq_len(n)){ 
      
      r[j, im[j,]] <- .setorder(as.vector(unlist(input[j,])))
    
    }
    
    R <- PlackettLuce::as.rankings(r)
    
    # if full output is required, for internal use
    # put r into the ordering format
    if (isTRUE(full.output)) {
      r2 <- matrix("", nrow = n, ncol = 3)
      colnames(r2) <- c("best", "middle", "worst")
      r[r==0] <- NA
      for(j in seq_len(n)) {
        jr <- sort(r[j, !is.na(r[j, ])])
        if (sum(jr == 2) > 1) {
          names(jr)[jr == 2] <- paste(names(jr[jr == 2]), collapse = ", ")
        }
        r2[j, ] <- names(jr)
      }
      r <- r2
    }
    
  }
  
  # with 4 or more comparisons
  if (ncomp >= 4) {
    
    r <- .pivot_tetra(i = items, r = input)
    
    # get item names 
    itemnames <- sort(unique(as.vector(r)))
    
    # make a PlackettLuce rankings
    R <- PlackettLuce::as.rankings(r, input = "ordering", items = itemnames)
    
  }
  
  # if pseudo-item were added, it is removed
  pseudo <- grepl("pseudoitem", itemnames) 
  if (any(pseudo)) {
    R <- R[, !pseudo]
  }
  
  # check if additional rankings are required
  if (!is.null(additional.rank)) {
    # add comparisons with local rankings
    R <- .additional_rankings(i = items, R = R, add = additional.rank)
  }
  
  # and into a grouped_rankings
  gi <- rep(seq_len(n), (nrow(R) / n))
  G <- PlackettLuce::group(R, index = gi)
  
  # check if all data is required
  if (isTRUE(full.output)) {
    R <- list(PLranking = R, PLgrouped = G, myrank = r)
  }
  
  # return a grouped_rankings if required
  if (group) {
    R <- G
  }
  
  return(R)
  
}

#' Set the order of tricot rankings
#' @param x a vector of length 3 with the tricot letters A, B, C,
#'  a Tie will be assigned the value 2
#' @examples 
#' x <- c("C", "Tie")
#' gosset:::.setorder(x)
#' 
#' x <- c("A", "B")
#' gosset:::.setorder(x)
#' @noRd
.setorder <- function(x){
  s <- rep(2, times = 3) #default value is 2
  L <- LETTERS[1:3]
  
  # works backwards from C to A to give most importance 
  # to item(s) listed as better
  s[L %in% strsplit(x[2], split="")] <- 3
  s[L %in% strsplit(x[1], split="")] <- 1
  
  #avoid skipped numbers
  # s <- order(s)
  return(s)
}

#' Tetra rankings
#'  This function deals with object in the tetra approach
#'  in ClimMob when four or more items are tested by each participant
#' @param i is a dataframe with items
#' @param r is a dataframe with rankings 
#' @noRd
.pivot_tetra <- function(i, r){
  
  # fix names in r data
  names(r) <- paste0("PosItem", 1:ncol(r))
  
  # get the number of possible rankings
  nrank <- ncol(r)
  # number of rows
  n <- nrow(r)
  
  # handle NAs
  r[r==0] <- NA
  
  for (z in seq_len(nrank)) {
    rm <- is.na(i[, z]) | is.na(r[, z])
    i[rm , z] <- NA
    r[rm , z] <- NA
  }
  
  # check for more than two missing labels in items
  mi <- rowSums(t(apply(i, 1, is.na)))
  mi <- nrank - mi
  if( any(mi <= 1) ) {
    stop("Cannot handle more than 2 NAs per row in 'items' \n")
  }
  
  # if there is accepted NAs in items  
  # put a label as pseudoitem which will be removed later
  if ( any(mi < nrank) )  {
    i <- t(apply(i, 1, function(p) {
      pi <- length(p[is.na(p)])
      p[is.na(p)] <- paste0("pseudoitem", 1:pi)
      p
    }))
  }
  
  # add a very high value if there is accepted NAs in rankings
  if (any(mi < nrank))  {
    r[is.na(r)] <- 999999
  }
  
  decoded <- .decode_ranking(i, r)
  
  return(decoded)
  
} 


#' this function adds additional ranks, generally when a local item 
#' is tested against the tricot items
#' i, is a dataframe with items
#' R, is an object of class rankings from PlackettLuce
#' add, is a dataframe with additional rankings characters 
#' indication whether the tricot items performed "Better" or "Worse" 
#' compared to the local item
#' @noRd
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
  add <- apply(add, 2, function(x) {
    x <- ifelse(x == "Better" | x == 1, 1,
                ifelse(x == "Worse" | x == 2, 2, NA))
    x
  })
  
  # stop if any NA
  if (any(is.na(add))) {
    "NAs are not allowed in additional rankings"
  }
  
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
    x <- PlackettLuce::as.rankings(x, input = "ordering", items = itemnames)
  })
  
  paired <- do.call("rbind", paired)
  
  R <- rbind(R, paired)  
  
  return(R)
  
}
