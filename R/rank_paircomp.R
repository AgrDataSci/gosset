#' Pairwise comparison rankings
#'
#' Pairwise comparisons from a PlackettLuce ranking object. 
#'  Ties are not taken into account, then coerced to NA's.
#'
#' @author Kauê de Sousa and Jacob van Etten
#' @family rank functions
#' @param object an object of class "rankings" or "grouped_rankings" 
#' which is a matrix of dense rankings. 
#' @return an object of class "paircomp" which is a matrix 
#'  of pairwise comparisons
#' @seealso \code{\link[PlackettLuce]{rankings}}, 
#' \code{\link[psychotools]{paircomp}}
#' @references 
#' Strobl C., Wickelmaier F. & Zeileis A. (2011). 
#' Journal of Educational and Behavioral Statistics, 36(2), 135–153.
#' doi:10.3102/1076998609359791
#' 
#' @examples 
#'  
#' library("PlackettLuce")
#' 
#' R = matrix(c(1, 2, 0, 0,
#'               4, 1, 2, 3,
#'               2, 4, 3, 1,
#'               1, 2, 3, 0,
#'               2, 1, 1, 0,
#'               1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' R = as.rankings(R)
#' 
#' PC = set_paircomp(R)
#' 
#' #############################################
#' 
#' # using breadwheat data
#' data("breadwheat", package = "gosset")
#' 
#' # convert the tricot rankings from breadwheat data
#' # into a object of class 'rankings' from PlackettLuce
#' R = rank_tricot(breadwheat,
#'                  items = c("variety_a","variety_b","variety_c"),
#'                  input = c("overall_best","overall_worst"))
#' 
#' 
#' PC = set_paircomp(R)
#' 
#' @importFrom psychotools paircomp
#' @importFrom utils combn
#' @export
set_paircomp = function(object){
  
  R = object
  
  # check wich kind of input is given and convert it into a
  # matrix with rankings
  if (.is_rankings(R)) {
    R = R[1:length(R), , as.rankings = FALSE]
  }
  if (.is_grouped_rankings(R)) {
    R = R[1:length(R), , as.grouped_rankings = FALSE]
  }
  
  # zeros into NA's
  R[R == 0] = NA
  
  # take name of items
  items = dimnames(R)[[2]]
  
  # make pairwise comparisons
  cc = .combn2(items, 2)
  
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
    # if i is higher than j, add -1, this means that j beats i
    # if none of these options, add NA
    p = ifelse(p[, 1] < p[, 2], 1, ifelse(p[, 1] > p[, 2] , -1, NA))
    
  })
  
  # convert this matrix into a paircomp object
  pair = psychotools::paircomp(pair, labels = as.character(items))
  
  return(pair)
}
