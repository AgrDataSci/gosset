#' Build Plackett-Luce rankings from tricot dataset 
#'
#' Create an object of class "rankings" from tricot data. Tricot stands 
#' for "triadic comparison of technology options". Is an approach to carry out 
#' large decentralized agronomic field experiments as incomplete blocks. 
#' Each incomplete block contains a set of three randomised technologies 
#' out of a larger set.
#'
#' @author Kauê de Sousa and Jacob van Etten, with ideas from Heather Turner
#' @family rank functions
#' @param data a data.frame with columns specified by items and input values
#' @param items a character or numerical vector for indexing the column(s) 
#' containing the item names in \code{data} 
#' @param input a character or numerical vector for indexing the column(s) 
#' containing the values in \code{data} to be ranked 
#' @param group logical, if \code{TRUE} return an object of class "grouped_rankings"
#' @param validate.rankings logical, if \code{TRUE} implements a check on ranking consistency 
#'  looking for possible ties, NA or letters other than A, B, C. These entries are set to 0
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
#' library("PlackettLuce")
#' data("beans", package = "PlackettLuce")
#' 
#' # Using a subset of the bean data
#' beans = beans[1:5, 1:5]
#' beans[1, 1] = NA
#' beans[3, 4:5] = NA
#' beans[5, 5] = "Tie"
#' 
#' # The default approach do not validate rankings
#' # accepting any entry used in the argument input
#' R1 = rank_tricot(beans,
#'                  items = c(1:3),
#'                  input = c(4:5), 
#'                  group = FALSE)
#' 
#' # Using validate.rankings = TRUE, the rankings
#' # are only considered for those entries without 
#' # NAs, Ties and with any of the letters A, B, C
#' # this do not affect the lenght of the final ranking 
#' R2 = rank_tricot(beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  validate.rankings = TRUE,
#'                  group = FALSE)
#' 
#' coef(PlackettLuce(R1))
#' 
#' coef(PlackettLuce(R2))
#' 
#' ############################################################
#' 
#' # pass the comparison with local item as an additional rankings, then
#' # each of the 3 varieties are compared separately with the local item
#' # and return an object of class grouped_rankings
#' 
#' data("beans", package = "PlackettLuce")
#' 
#' G = rank_tricot(data = beans,
#'                  items = c(1:3),
#'                  input = c(4:5),
#'                  group = TRUE,
#'                  additional.rank = beans[c(6:8)])
#' 
#' head(G)
#' 
#' @importFrom PlackettLuce as.rankings group
#' @export
rank_tricot = function(data, 
                       items, 
                       input, 
                       group = FALSE,
                       validate.rankings = FALSE,
                       additional.rank = NULL, 
                       ...) {
  
  # if tibble coerce into a data.frame
  if (.is_tibble(data)) {
    data = as.data.frame(data, stringsAsFactors = FALSE)
  }
  
  items = data[, items]
  
  input = data[, input]
  
  # get nrow
  n = nrow(data)
  
  # get extra arguments
  dots = list(...)
  
  # if all data is required
  full.output = dots[["full.output"]]

  n = nrow(items)
  
  # check for more than two missing labels in items
  mi = rowSums(apply(items, 2, is.na))
  if (any(mi > 1)) {
    stop("Cannot handle more than 2 NAs per row in 'items', 
           more than 2 NAs where found in rows ", 
         paste(which(mi > 1), collapse = ", "), "\n")
  }
  
  # if there is one NA per row in items and observations 
  # with only two items add a pseudo-item which will be removed later
  if (any(mi == 1))  {
    items[is.na(items)] = "pseudoitem"
  }
  
  # validate rankings, and set to 0 if required
  keep = .validate_rankings(input)
  
  out = which(keep == FALSE)

  # data frame with items as matrix
  im = as.matrix(items)
  
  # get the names of items
  itemnames = unique(as.vector(im))
  
  # a Sparse matrix where rows are the observations 
  # and columns the item names
  r = matrix(0, nrow = n, ncol = length(itemnames))
  colnames(r) = itemnames
  
  # run over the rows filling the rankings that were observed 
  for(j in seq_len(n)){ 
    
    r[j, im[j,]] = .setorder(as.vector(unlist(input[j,])))
    
  }
  
  R = PlackettLuce::as.rankings(r)
  
  # if ranking validation was required, rankings that did not passed the 
  # validation are set to 0, this does not affect the final length 
  # of the rankings
  if (isTRUE(validate.rankings)) {
    
    R[!keep] = 0
    
  }
  
  if (length(out) > 0) {
    messag = paste0("Ties, NA's or letters different than A, B, C, were identified in rows ", 
                    paste(out, collapse = ", "), "\n")
    if (isFALSE(validate.rankings)) {
      messag = paste(messag, "Use validate.rankings = TRUE to ignore these entries\n")
    }
    warning(messag)
  }
  
  # if full output is required, for internal use
  # put r into the ordering format
  if (isTRUE(full.output)) {
    r2 = matrix("", nrow = n, ncol = 3)
    colnames(r2) = c("best", "middle", "worst")
    r[r==0] = NA
    for(j in seq_len(n)) {
      jr = sort(r[j, !is.na(r[j, ])])
      if (sum(jr == 2) > 1) {
        names(jr)[jr == 2] = paste(names(jr[jr == 2]), collapse = ", ")
      }
      r2[j, ] = names(jr)
    }
    r = r2
  }
  
  # if pseudo-item were added, it is removed
  pseudo = grepl("pseudoitem", itemnames) 
  if (any(pseudo)) {
    R = R[, !pseudo]
  }
  
  # check if additional rankings are required
  if (!is.null(additional.rank)) {
    # add comparisons with local rankings
    R = .additional_rankings(i = items, R = R, add = additional.rank)
  }
  
  # and into a grouped_rankings
  gi = rep(seq_len(n), (nrow(R) / n))
  G = PlackettLuce::group(R, index = gi)
  
  # check if all data is required
  if (isTRUE(full.output)) {
    R = list(PLranking = R, PLgrouped = G, myrank = r)
  }
  
  # return a grouped_rankings if required
  if (group) {
    R = G
  }
  
  return(R)
  
}


#' Validate rankings
#' 
#' This check ranking consistency making sure that 
#' no NAs or ties are mantained in the final PlackettLuce ranking
#' 
#' @param x data.frame with two columns indicating the tricot rankings
#' @noRd
.validate_rankings = function(x) {
  
  ABC <- apply(x, 1, function(y) {
    all(y %in% LETTERS[1:3])
  })
  
  noNA <- apply(x, 1, function(y) {
    all(!is.na(y))
  })
  
  noDups <- apply(x, 1, function(y) {
    all(!duplicated(y))
  })
  
  keep <- as.vector(ABC & noNA & noDups)
  
  return(keep)
  
}
 

#' Set the order of tricot rankings
#' 
#' This function set the indices to place the order of best worst 
#' technologies indicates in the tricot approach 
#' 
#' @param x a vector of length 2 with the LETTERS A, B or C, or Tie
#' first element in the vector indicates the best technology, 
#' second element indicates the worst technology
#' @examples 
#' x = c("C", "Tie")
#' gosset:::.setorder(x)
#' 
#' x = c("A", "B")
#' gosset:::.setorder(x)
#' @noRd
.setorder = function(x){
  # default value is 2
  s = rep(2, times = 3) 
  L = LETTERS[1:3]
  
  # works backwards from C to A to give most importance 
  # to item(s) listed as better
  s[L %in% strsplit(x[2], split = "")] = 3
  s[L %in% strsplit(x[1], split = "")] = 1
  
  return(s)
  
}

#' this function adds additional ranks, generally when a local item 
#' is tested against the tricot items
#' i, is a dataframe with items
#' R, is an object of class rankings from PlackettLuce
#' add, is a dataframe with additional rankings characters 
#' indication whether the tricot items performed "Better" or "Worse" 
#' compared to the local item
#' @noRd
.additional_rankings = function(i, R, add){
  
  n = nrow(add)
  
  ncomp = ncol(i)
  
  # convert it into characters
  add[1:ncol(add)] = lapply(add[1:ncol(add)], as.character)
  
  add = as.matrix(add)
  
  i = as.matrix(i)
  
  # treat these comparisons as additional rankings.
  # first we convert the orderings of the items to 
  # sub-rankings of the full set of items including the additional items 
  # so we add the paired comparisons
  
  # the comparisons with the additional items are stored 
  # in another set of columns
  
  # make sure that values in add are integers 
  # where 1 means Better and 2 means Worse
  add = apply(add, 2, function(x) {
    x = ifelse(x == "Better" | x == 1, 1,
                ifelse(x == "Worse" | x == 2, 2, NA))
    x
  })
  
  # stop if any NA
  if (any(is.na(add))) {
    "NAs are not allowed in additional rankings"
  }
  
  # add local to itemnames
  itemnames = dimnames(R)[[2]]
  itemnames = unique(c("Local", itemnames))
  
  paired = list()
  
  for (p in seq_len(ncomp)) {
    ordering = matrix("Local", nrow = n, ncol = 2)
    worse = add[, p] == 2
    # name of winner
    ordering[!worse, 1] = i[, p][!worse]
    # name of loser
    ordering[worse, 2] = i[, p][worse]
    paired[[p]] = ordering
  }
  
  # we then convert these orderings to sub-rankings of the full set of items
  # and combine them with the rankings
  paired = lapply(paired, function(x) {
    x = PlackettLuce::as.rankings(x, input = "ordering", items = itemnames)
  })
  
  paired = do.call("rbind", paired)
  
  R = rbind(R, paired)  
  
  return(R)
  
}
