#' Kendall's W (coefficient of concordance)
#' 
#' Compute Kendall's W, also known as coefficient of concordance among observed rankings
#' and those predicted by the Plackett-Luce model.
#' 
#' It is as wrapper of the function DescTools::KendallW, adapted to compute the Kendall's W
#' on the observed and predicted rankings.
#'  
#' Kendall's W values range between 0 (no agreement) to 1 (full agreement)
#' 
#' @author David Brown, Kauê de Sousa and Jacob van Etten
#' @family goodness-of-fit functions
#' @param x an object of class \code{pltree}
#' @param y an object of class \code{pltree}
#' @param newdata data for predictions
#' @param ... additional arguments passed to methods
#' @return Kendall's W (coefficient of concordance) 
#' @details 
#' @importFrom DescTools KendallW
#' @export
kendallW <- function(x, ...) {
  
  UseMethod("kendallW")
  
}

#' @rdname kendallW 
#' @export
kendallW.default <- function(x, y, ...) {
  
  pred_ranks <- x
  
  obs_ranks <- y
  
  kw <- mean(unlist(lapply(X = seq_len(nrow(pred_ranks)),
                           FUN = function(X){
                             DescTools::KendallW(data.frame("pred" = pred_ranks[X, ],
                                                            "obs" = obs_ranks[X, ]),
                                                 correct = TRUE,
                                                 test = TRUE)$estimate
                           })))
  
  return(kw)
  
}


#' @rdname kendallW
#' @method kendallW pltree
#' @export
kendallW.pltree <- function(x, newdata = NULL, ...) {
  
  if(class(x)[1] != "pltree"){
    stop("x is not an object of class pltree")
  }
  
   obs_ranks <- x[[1]]$data[[1]]
  
   obs_ranks_mtx <- obs_ranks[1:length(obs_ranks), , as.grouped_rankings = FALSE]
   
  
  if(is.null(newdata)){
    
    
    pred_ranks <- predict(x, type = "rank")
    
  }
   
   if(!is.null(newdata)){
     
     
     pred_ranks <- predict(x, newdata = newdata, type = "rank")
     
   }
   
   kendallW(pred_ranks, obs_ranks_mtx)
  
}

