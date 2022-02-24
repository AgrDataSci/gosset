library("gosset")
library("PlackettLuce")

#' @param object a matrix with ranking data (no ties)
#' @param coeff optional, a matrix with coefficients from a fitted model using \var{object}
logLik2 <- function(object, coeff = NULL) {
  
  dimo <- dim(object)
  
  if(is.null(dimo)){
    object <- as.matrix(t(object))
    dimo <- dim(object)
  }
  
  # This function assumes that all coefficients
  # are equal to get a true null estimates
  if (is.null(coeff)) {
    coeff <- matrix(0, nrow = dimo[1], ncol = dimo[2])
  }
  
  input <- array(c(object, coeff), dim = c(dimo, 2))
  
  LL <- apply(input, 1, function(x) {
    
    x <- x[x[, 1] != 0, ]
    # Put coefficients in the right order
    v <- x[order(x[, 1], na.last = NA), 2]
    l <- 0L
    # From Hunter MM(2004) The Annals of Statistics, Vol. 32, 
    # No. 1, 384-406, page 397
    for (i in seq_along(v)) {
      l <- l + v[i] - log(sum(exp(v[(i):(length(v))])))
    }
    
    return(l)
    
  })
  
  sum(LL)
  
}


# a toy matrix with no ties
R <- matrix(c(1, 2, NA, NA,
              4, 1, 2, 3,
              2, 1, 4, 3,
              1, 2, 3, NA,
              2, 1, 3, NA,
              1, NA, 3, 2),
            nrow = 6, byrow = TRUE)

dimnames(R)[[2]] <- c("banana", "apple", "grape", "pear")

mod <- PlackettLuce(R)

# same in all the three functions
mod$null.loglik
gosset:::.logLikNull(R)
logLik2(R)

# add coefficients
coefs <- matrix(coefficients(mod, log = TRUE),
                nrow = 6, ncol = 4, byrow = TRUE)


logLik(mod)
logLik2(R, coefs)

gosset:::.getpseudoR2(logLik2(R), logLik2(R, coefs), dim(R)[1])




# now test with the bean data
example("beans", package = "PlackettLuce")

G <- rank_tricot(beans,
                 c("variety_a", "variety_b", "variety_c"),
                 c("best", "worst"),
                 group = TRUE)

d <- cbind(G, beans)

pl <- pltree(G ~ maxTN + season, data = d, alpha = 1)

pl


logLik(pl)

logLik(pl, newdata = d[1:5,])


# null ranking
R <- G[1:length(G),, as.grouped_rankings = FALSE]

#R <- R[,-which(dimnames(R)[[2]] == "Local")]
#keep <- apply(R,1,sum) > 3
#R <- R[keep,]

# mod <- PlackettLuce(R)
# 
# # different logLikNULL because it takes the additional rankings
# mod$null.loglik
# logLik2(R)
# 
# nodes <- as.vector(predict(pl, type = "node"))

# coeffs <- coefficients(pl, log = TRUE)
# coeffs <- t(as.matrix(coeffs))
# coeffs <- coeffs[as.integer(nodes), ]

coefs <- predict(pl, vcov = FALSE)

logLik2(R); logLik2(R, coefs); logLik(pl)


logLik.pltree <- function(object, newdata = NULL, ...){
  
  
  
  return(cat("''"))  
  
}




# with LL from PlackettLuce
gosset:::.getpseudoR2(mod$null.loglik, logLik(pl), nrow(d))

# with our function
gosset:::.getpseudoR2(logLik2(R), logLik2(R, coeffs), dim(R)[1])








