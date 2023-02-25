#' Add pseudo-rank to missing values 
#' 
#' @param object a matrix or PlackettLuce rank
#' @param ... additional arguments passed to methods
#' @return a matrix or PlackettLuce rank
#' @examples 
#' library("PlackettLuce")
#' R = matrix(c(1, 2, 0, 0,
#'               4, 1, 0, 3,
#'               2, 1, 0, 3,
#'               1, 2, 0, 0,
#'               2, 1, 0, 0,
#'               1, 0, 0, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "orange", "pear")
#' 
#' # summary(PlackettLuce(R))
#' 
#' R = pseudo_rank(R)
#' 
#' summary(PlackettLuce(R))
#' @importFrom PlackettLuce as.rankings
#' @export
pseudo_rank = function(object, ...) {
  
  keepclass = class(object)[1]
  
  object = as.matrix(object)
  
  do = dim(object)
  
  sumR = colSums(object)
  
  # find the missing values
  missR = as.vector(which(sumR == 0))
  
  if (length(missR) == 0) {
    
    if (keepclass == "rankings") {
      object = PlackettLuce::as.rankings(object)
    }
    
    return(object)
    
  }
  
  # check for n times the items are tested to balance variance
  tested = apply(object, 2, function(x){sum(x != 0)})
  tested = floor(mean(tested[-missR]))
  
  # input the pseudo-ranking to the missing values to always loose
  # against the worst
  set.seed(21014422)
  s = sort(sample(1:do[1], tested))
  for (i in seq_along(missR)) {
    
    object[s, ] = t(apply(object[s, ], 1, function(x){
      x[missR[i]] = max(x) + 1 
      x
    }))
    
  }
  
  if (keepclass == "rankings") {
    object = PlackettLuce::as.rankings(object)
  }
  
  return(object)
  
}
