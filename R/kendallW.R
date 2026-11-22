#' Kendall's W (coefficient of concordance)
#' 
#' Compute Kendall's W, also known as coefficient of concordance among observed rankings
#' and those predicted by the Plackett-Luce model.
#' 
#' Kendall's W is computed for each ranking (row) between the observed 
#' and the predicted ranks, with the correction for ties, and then 
#' averaged across rankings. Only the items that the participant ranked 
#' are used. Items with a 0 (not ranked) or NA in the observed rankings, 
#' or NA in the predicted rankings, are dropped from that row, and rows 
#' with fewer than two items left are skipped.
#'  
#' Kendall's W values range from 0 (no agreement) to 1 (full agreement)
#' 
#' @author David Brown and Jacob van Etten
#' @family goodness-of-fit functions
#' @param x an object of class \code{pltree}, or a matrix of predicted
#'  rankings for the default method
#' @param y a matrix of observed rankings, for the default method
#' @param newdata data for predictions
#' @param ... additional arguments passed to methods
#' @return Kendall's W (coefficient of concordance) 
#' @examples
#' pred = matrix(c(1, 2, 3, 4,
#'                 2, 1, 3, 4,
#'                 1, 3, 2, 4), nrow = 3, byrow = TRUE)
#' 
#' # 0 marks items the participant did not rank
#' obs = matrix(c(1, 2, 3, 0,
#'                1, 2, 0, 3,
#'                0, 2, 1, 3), nrow = 3, byrow = TRUE)
#' 
#' kendallW(pred, obs)
#' @export
kendallW = function(x, ...) {
  
  UseMethod("kendallW")
  
}

#' @rdname kendallW 
#' @export
kendallW.default = function(x, y, ...) {
  
  pred_ranks = as.matrix(x)
  
  obs_ranks = as.matrix(y)
  
  if (!all(dim(pred_ranks) == dim(obs_ranks))) {
    stop("x and y must have the same dimensions")
  }
  
  kw = vapply(seq_len(nrow(pred_ranks)), function(i) {
    
    pred = as.numeric(pred_ranks[i, ])
    
    obs = as.numeric(obs_ranks[i, ])
    
    keep = !is.na(pred) & !is.na(obs) & obs != 0
    
    if (sum(keep) < 2L) return(NA_real_)
    
    .kendall_w(cbind(pred[keep], obs[keep]))
    
  }, numeric(1L))
  
  if (all(is.na(kw))) return(NA_real_)
  
  mean(kw, na.rm = TRUE)
  
}

#' Kendall's W with correction for ties
#' @param ratings a matrix with items in rows and raters in columns
#' @return Kendall's W, or NA when it is not defined
#' @noRd
.kendall_w = function(ratings) {
  
  n = nrow(ratings)
  
  m = ncol(ratings)
  
  ranks = apply(ratings, 2, rank)
  
  S = sum((rowSums(ranks) - m * (n + 1) / 2)^2)
  
  ties = sum(apply(ratings, 2, function(r) {
    t = table(r)
    sum(t^3 - t)
  }))
  
  denom = m^2 * (n^3 - n) - m * ties
  
  if (denom <= 0) return(NA_real_)
  
  12 * S / denom
  
}


#' @rdname kendallW
#' @method kendallW pltree
#' @export
kendallW.pltree = function(x, newdata = NULL, ...) {
  
  if(class(x)[1] != "pltree"){
    stop("x is not an object of class pltree")
  }
  
  obs_ranks = x[[1]]$data[[1]]
  
  obs_ranks_mtx = obs_ranks[1:length(obs_ranks), , as.grouped_rankings = FALSE]
  
  
  if(is.null(newdata)){
    
    
    pred_ranks = predict(x, type = "rank")
    
  }
  
  if(!is.null(newdata)){
    
    
    pred_ranks = predict(x, newdata = newdata, type = "rank")
    
  }
  
  kendallW(pred_ranks, obs_ranks_mtx)
  
}

