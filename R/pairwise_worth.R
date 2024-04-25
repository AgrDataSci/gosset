#' Compute pairwise probabilities
#' 
#' Implements the Luce's Choice Axiom to calculate pairwise probabilities 
#' in a set of choice probabilities. The Luce's Choice Axiom states
#' that the probability of selecting one item over another from a pool of
#' many items is not affected by the presence or absence of other items
#' in the pool. 
#' 
#' @param object a named numeric vector with probabilities
#' @param relative.probs logical, TRUE to return matrix with relative
#'  probs (prob - 0.5), otherwise true values are returned
#' @param ... additional arguments passed to methods
#' @return a matrix with pairwise probabilities
#' @examples
#' 
#' library("PlackettLuce")
#' library("ggplot2")
#' 
#' R = matrix(c(1, 2, 3, 0,
#'              4, 1, 2, 3,
#'              2, 1, 3, 4,
#'              1, 2, 3, 0,
#'              2, 1, 3, 0,
#'              1, 0, 3, 2), nrow = 6, byrow = TRUE)
#' colnames(R) = c("apple", "banana", "grape", "pear")
#' 
#' mod = PlackettLuce(R)
#' 
#' coefs = coefficients(mod, log = FALSE)
#' 
#' pair_worth = pairwise_probs(coefs)
#' 
#' pair_worth
#' 
#' # plot the results
#' lvls = dimnames(pair_worth)[[1]]
#' 
#' pair_dat = data.frame(player1 = rep(lvls, times = length(lvls)), 
#'                       player2 = rep(lvls, each = length(lvls)),
#'                       worth = as.vector(pair_worth))
#' 
#' pair_dat
#' 
#' pair_dat$player1 = factor(pair_dat$player1, levels = lvls)
#' 
#' pair_dat$player2 = factor(pair_dat$player2, levels = rev(lvls))
#' 
#' pair_dat$worth = round(pair_dat$worth, 2)
#' 
#' ggplot(pair_dat, 
#'        aes(x = player2, 
#'            y = player1,
#'            fill = worth,
#'            label = worth)) +
#'   geom_tile(show.legend = FALSE) + 
#'   geom_text() +
#'   scale_fill_gradient2(low = "#b2182b", 
#'                        high = "#2166ac", 
#'                        na.value = "white") +
#'   scale_x_discrete(position = "top") +
#'   theme_bw() +
#'   theme(axis.text = element_text(color = "grey10"),
#'         strip.text.x = element_text(color = "grey10"),
#'         axis.text.x = element_text(angle = 90, hjust = 0),
#'         panel.grid = element_blank()) +
#'   labs(x = "", 
#'        y = "",
#'        fill = "")
#' 
#' @export
pairwise_probs = function(object, relative.probs = TRUE, ...) {

  lvls = names(object)
  
  pair_worth = matrix(NA, 
                      nrow = length(lvls), 
                      ncol = length(lvls),
                      dimnames = list(rev(lvls), rev(lvls)))
  
  for(i in seq_len(ncol(pair_worth))) {
    
    for(j in seq_len(nrow(pair_worth))) {
      
      col_i = dimnames(pair_worth)[[1]][i]
      row_j = dimnames(pair_worth)[[2]][j]
      
      if(col_i == row_j) next
      
      w = object[col_i] / (object[col_i] + object[row_j])
      
      w = round(w, 2)
      
      pair_worth[row_j, col_i] = w
      
    }
    
  }
  
  if (isTRUE(relative.probs)) {
    
    pair_worth = pair_worth - 0.5
    
  }
  
  pair_worth = round(pair_worth, 3)
  
  pair_worth = t(pair_worth)
  
  return(pair_worth)
  
}
