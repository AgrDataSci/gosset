#' Binary rankings from pairwise contests
#'
#' Binary comparisons from a ranking object. Ties are not 
#' taken into account, then they are added as NA's.
#'
#' @author Kauê de Sousa
#' @family rank functions
#' @param object an object of class \code{rankings}, \code{grouped_rankings}
#'  or \code{paircomp}
#' @param drop.null logical, an optional argument to remove null contests 
#' @param disaggregate logical, if \code{TRUE} binaries are disaggregated 
#'  by individual contests
#' @return A data.frame with binary rank of pairwise contests:
#' \item{player1}{a factor with n levels for the first player 
#'  in the contests}
#' \item{player2}{a factor with n levels (same as player1) for 
#'  the second player in the contests}
#' \item{win1}{number of times player1 wins against player2}
#' \item{win2}{number of times player2 wins against player1}
#' @references 
#' Turner H. & Firth D. (2012). 
#' Journal of Statistical Software, 48(9), 1–21. 
#' \doi{https://doi.org/10.18637/jss.v048.i09}
#' 
#' @examples 
#' library("PlackettLuce")
#' 
#' R = matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 3, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' R = as.rankings(R)
#' 
#' rank_binomial(R)
#' 
#' rank_binomial(R, disaggregate = TRUE)
#' 
#' @export
rank_binomial = function(object, 
                         drop.null = FALSE, 
                         disaggregate = FALSE) {
  
  if (isFALSE(disaggregate)) {
    
    if (.is_grouped_rankings(object) | .is_rankings(object)) {
      object = rank_paircomp(object)
    }
    
    # take all possible combinations between items
    p_labels = t(.combn2(labels(object), 2))
    
    # paired comparisons into dataframe
    object = cbind(p_labels, data.frame(summary(object))[1:2])
    
    # > win1 and < win2 
    #select vectors to keep on the dataframe
    names(object) = c("player1","player2","win1","win2")
    
    # rename row names in object
    row.names(object) = 1:nrow(object)
    
    # remove combinations without comparisons
    if (drop.null) {
      drop = object$win1 == 0 & object$win2 == 0
      object = object[!drop, ]
    }
    
  }
  
  if (isTRUE(disaggregate)) {
    
    n = dim(object)[[1]]
    
    # matrix with rankings
    if (.is_rankings(object)) {
      R = object[1:n, , as.rankings = FALSE]
    }
    if (.is_grouped_rankings(object)) {
      R = object[1:n, , as.grouped_rankings = FALSE]
    }
    
    players = dimnames(R)[[2]]
    
    # zeros into NAs
    R[R == 0] = NA
    
    # make pairwise comparisons
    cc = .combn2(players, 2)
    
    # get the rankings as pair comparisons
    # ties are not considered and will be NA's
    pair = apply(cc, 2, function(x){
      
      # take the first item in the comparison
      i = x[1]
      # and the second one
      j = x[2]
      
      # combine the rankings for these two items
      # with i as first and j as the second colunm
      p = cbind(R[, i], R[, j])
      
      # if i is lower than j, add 1, this means that i beats j
      # if i is higher than j, add 0, this means that j beats i
      # if none of these options, add NA
      p = ifelse(p[, 1] < p[, 2], 1, 
                 ifelse(p[, 1] > p[, 2] , 0, NA))
      
    })
    
    contests = NULL
    
    for(i in seq_len(n)) {
      x = as.data.frame(cbind(id = i, t(cc), pair[i, ]),
                        stringsAsFactors = FALSE)
      x$V5 = ifelse(x$V4 == 0, 1,
                    ifelse(x$V4 == 1, 0, NA))
      
      x = x[!is.na(x$V4), ]
      
      contests = rbind(contests, x)
    }
    
    names(contests) = c("id","player1","player2","win1","win2")
    
    contests[,c("id","win1","win2")] = lapply(contests[,c("id","win1","win2")], 
                                              as.integer)
    object = contests
  }
  
  # take the name of all items
  players = factor(
    sort(unique(as.vector(unlist(object[,c("player1", "player2")]))))
  )
  
  # run over object looking for corresponding combination and 
  # add the results into the new dataframe object_bin
  object$player1 = factor(object$player1, levels = players)
  object$player2 = factor(object$player2 , levels = players)
  
  class(object) = union("gosset_df", class(object))
  
  return(object)
  
}
