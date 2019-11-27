#' Kendall Tau correlation coefficient
#' 
#' Compute Kendall Tau correlation coefficient between two matrices
#' 
#' @param x a matrix with rankings or model coefficients 
#' @param y a matrix with rankings or model coefficients
#' @return The Kendall correlation coefficient and the Effective N, which 
#' is the equivalent N needed if all items were compared to all items. 
#' Used for significance testing.
#' @examples
#' 
#' # check the correlation between observed rankings 
#' # and the predicted coefficients from PlackettLuce
#' 
#' library("PlackettLuce")
#' 
#' R <- matrix(c(1, 2, 4, 3,
#'               1, 4, 2, 3,
#'               1, 2, 4, 3,
#'               1, 2, 4, 3,
#'               1, 3, 4, 2,
#'               1, 4, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) <- LETTERS[1:4]
#' 
#' G <- group(as.rankings(R), 1:6)
#' 
#' mod <- pltree(G ~ 1, data = G)
#' 
#' preds <- predict(mod)
#' 
#' kendallTau(R, preds)
#' 
#' @seealso \code{\link[stats]{cor}}
#' @export
kendallTau <- function(x, y){
  
  nc <- ncol(x)
  
  # if any decimal in x or y transform this to rankings
  # decimals will be computed as descending rankings
  # where the highest values are the "best" 
  # negative values are placed as least positions
  if (any(.is_decimal(x))) {
    
   x <- t(apply(x, 1, function(X){
      t(.rank_decimal(X)["rank"])
    }))
  
  }
  
  if (any(.is_decimal(y))) {
    
    y <- t(apply(y, 1, function(X){
      t(.rank_decimal(X)["rank"])
    }))
    
  }
  
  
  # Function bellow produces a matrix with two rows
  # First row: Kendall tau value for that row
  # Second row: how many pairs entered the comparison as a weight
  # To make the predicted probability of winning match the ranks (1 = best), 
  # we take the negative probability
  tau_N <- apply(cbind(x, y), 1, function(X) {
                   
                   obr <- X[1:nc]
                   prr <- X[(nc + 1):(2 * nc)]
                   
                   keep <- !is.na(obr) & !is.na(prr)
                   
                   obr <- obr[keep]
                   
                   prr <- prr[keep]
                   
                   tau_cor <- stats::cor(prr, 
                                         obr, 
                                         method = "kendall", 
                                         use = "pairwise.complete.obs")
                   
                   n <- sum(obr > 0, na.rm = TRUE)
                   
                   weight <- n * (n - 1) / 2
                   
                   return(c(tau_cor, weight))
                   
                 }
  )
  
  # Extract the values from the matrix
  tau <- tau_N[1,]
  N <- tau_N[2,]
  
  # N are very small per observer, so simple averaging will have very little bias
  # No need for z transformation at this stage
  # And z tranformation would always give -Inf or Inf with N=2
  tau_average <- sum(tau * N) / sum(N)
  
  # Effective N is the equivalent N needed if all were compared to all
  # N_comparisons = ((N_effective - 1) * N_effective) / 2
  # This is used for significance testing later
  N_effective <- 0.5 + sqrt(.25 + 2 * sum(N)) 
  
  result <- t(as.matrix(c(tau_average, N_effective)))
  
  dimnames(result)[[2]] <- c("kendallTau", "N_effective")
  
  result <- tibble::as_tibble(result)
  
  return(result)
  
}
