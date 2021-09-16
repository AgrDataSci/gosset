#' @param method a character for the method to compute logLik 
#'  options are "Hunter" or "Turner"
#' @param newdata optionally, a data frame in which to look for variables 
#' @method logLik pltree
#' @rdname pseudoR2
#' @importFrom stats predict
#' @importFrom partykit predict.modelparty nodeids
#' @export
logLik.pltree <- function(object, newdata = NULL, method = "Turner", ...) {
  
  if (method == "Turner") {
    
    if (is.null(newdata)) {
        return(NextMethod(object, ...))
    }
    
    response <- as.character(formula(object)[[2L]])
    if (!response %in% colnames(newdata)) 
      stop("`newdata` must include response")
    f <- formula(object)
    environment(f) <- parent.frame()
    newdata <- model.frame(f, data = newdata, ...)
    node <- partykit::predict.modelparty(object, newdata = newdata, 
                                         type = "node")
    cf <- coef(object, log = FALSE)
    if (is.null(dim(cf))) 
      cf <- t(as.matrix(cf))
    nodes <- partykit::nodeids(object, terminal = TRUE)
    dots <- object$info$dots
    G <- model.response(newdata)
    w <- model.weights(newdata)
    if (is.null(w)) 
      w <- rep.int(1L, length(G))
    LL <- df <- numeric(length(nodes))
    for (i in seq_along(nodes)) {
      id <- node == nodes[i]
      if (sum(id)) {
        fit <- suppressWarnings(do.call("plfit", c(list(y = G[id, 
        ], start = cf[i, ], weights = w[id], maxit = 0), 
        dots)))
        LL[i] <- -fit$objfun
      }
    }
    
    return(sum(LL))
      
  }
  
  if (is.null(newdata)) {
    dat <- object$data
  }
  
  if (!is.null(newdata)) {
    
    dat <- newdata 
    
  }
  
  whichG <- unlist(lapply(dat, class))
  
  whichG <- which(whichG %in% "grouped_rankings")
  
  G <- dat[, whichG]
  
  G <- G[1:length(G), , as.grouped_rankings = FALSE]
  
  coeff <- stats::predict(object, newdata = dat, vcov = FALSE, ...)
  
  
  dimo <- dim(G)
  
  # Compute logLik
  input <- array(c(G, coeff), dim = c(dimo, 2))
  
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
  
  LL <- sum(LL)
  
  # This function assumes that all coefficients
  # are equal to get a true null estimates
  coeffNULL <- matrix(0, nrow = dimo[1], ncol = dimo[2])
  
  inputNULL <- array(c(G, coeffNULL), dim = c(dimo, 2))
  
  LLnull <- apply(inputNULL, 1, function(x) {
    
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
  
  LLnull <- sum(LLnull)
  
  result <- c(LL, LLnull)
  
  names(result) <- c("logLik", "logLikNULL")
  
  return(result)
  
}



#' AIC from a Bradley-Terry model
#' code adapted from PlackettLuce repository
#' Turner et al (2020)
#' @method AIC bttree
#' @importFrom methods addNextMethod asMethodDefinition assignClassDef
#' @importFrom stats AIC coef formula logLik model.response model.weights
#' @importFrom partykit nodeids predict.modelparty
#' @importFrom psychotools btmodel estfun.btmodel itempar
#' @export
AIC.bttree <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  # create model.frame from newdata
  response <- as.character(stats::formula(object)[[2]])
  
  if (!response %in% colnames(newdata)) {
    stop("'newdata' must include response")
  }
  
  f <- stats::formula(object)
  
  environment(f) <- parent.frame()
  
  newdata <- stats::model.frame(f, data = newdata, ...)
  
  # predict node for each grouped ranking
  node <- partykit::predict.modelparty(object,
                                       newdata = newdata,
                                       type = "node")
  
  # set up to refit models based on newdata
  cf <- psychotools::itempar(object)
  
  if (is.null(dim(cf))) { 
    cf <- t(as.matrix(cf))
  }
  
  nodes <- partykit::nodeids(object, terminal = TRUE)
  
  dots <- object$info$dots
  
  G <- stats::model.response(newdata)
  
  w <- stats::model.weights(newdata)
  
  if (is.null(w)) {
    w <- rep.int(1, length(G))
  }
  
  LL <- df <- numeric(length(nodes))
  
  for (i in seq_along(nodes)) {
    # fit model with coef fixed to get logLik
    # suppress warning due to fixing maxit
    id <- node == nodes[i]
    if (sum(id)) {
      fit <- suppressWarnings(
        do.call(".btfit",
                c(list(y = G[id,],
                       start = cf[i,],
                       weights = w[id]),
                  dots)))
      LL[i] <- -fit$objfun
    }
  }
  # compute AIC based on total log likelihood of data
  # and df of original model fit
  -2 * sum(LL) + 2 * attr(stats::logLik(object), "df")
}


# formula logLik model.frame model.response model.weights 
# deviance from a Bradley-Terry model
# code adapted from PlackettLuce repository
# Turner et al (2020)
# https://github.com/hturner/PlackettLuce/blob/master/R/pltree.R
#' @method deviance bttree
#' @export
deviance.bttree <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  # create model.frame from newdata
  
  response <- as.character(formula(object)[[2]])
  
  if (!response %in% colnames(newdata)) {
    stop("`newdata` must include response")
  }
  
  f <- stats::formula(object)
  
  environment(f) <- parent.frame()
  
  newdata <- stats::model.frame(f, data = newdata, ...)
  
  # predict node for each grouped ranking
  node <- partykit::predict.modelparty(object,
                                       newdata = newdata,
                                       type = "node")
  
  # set up to refit models based on newdata
  cf <- psychotools::itempar(object)
  
  if (is.null(dim(cf))) { 
    cf <- t(as.matrix(cf))
  }
  
  nodes <- partykit::nodeids(object, terminal = TRUE)
  
  dots <- object$info$dots
  
  G <- stats::model.response(newdata)
  
  w <- stats::model.weights(newdata)
  
  if (is.null(w)) {
    w <- rep.int(1, length(G))
  }
  
  LL <- df <- numeric(length(nodes))
  
  for (i in seq_along(nodes)){
    # fit model with coef fixed to get logLik
    # suppress warning due to fixing maxit
    id <- node == nodes[i]
    if (sum(id)) {
      fit <- suppressWarnings(
        do.call(".btfit",
                c(list(y = G[id,],
                       start = cf[i,],
                       weights = w[id]),
                  dots)))
      LL[i] <- -fit$objfun
    }
  }
  # compute deviance based on total log likelihood of data
  -2 * sum(LL)
}

# Compute deviance on a validation sample
#
# Glue code for calling btmodel()
# code from psychotree repository
# https://github.com/cran/psychotree/blob/master/R/bttree.R
# Strobl et al (2011)
.btfit <- function(y, x = NULL, start = NULL, weights = NULL, offset = NULL,
                   cluster = NULL, ..., estfun = FALSE, object = FALSE)
{
  
  if (!(is.null(x) || NCOL(x) == 0L)) {
    warning("x not used")
  }
  
  if(!is.null(offset)) {
    warning("offset not used")
  }
  
  rval <- psychotools::btmodel(y, weights = weights, ..., vcov = object)
  
  rval <- list(
    coefficients = rval$coefficients,
    objfun = -rval$loglik,
    estfun = if (estfun) psychotools::estfun.btmodel(rval) else NULL,
    object = if (object) rval else NULL
  )
  
  return(rval)
}

# deviance from a PlackettLuce model
# compute AIC in a validation sample and remove the degrees of freedom
# https://github.com/hturner/PlackettLuce/issues/32
#' @method deviance pltree
#' @export
deviance.pltree <- function(object, newdata = NULL, ...) {
  
  # compute deviance
  object <- logLik(object, newdata = newdata, ...)
  
  # and the deviance 
  result <- object * - (2)
  
  return(as.vector(result[1]))
  
}


# compute AIC with a validation sample
# code from https://freakonometrics.hypotheses.org/20158
#' @method AIC gnm
#' @export
AIC.gnm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # get the AIC
  stats::AIC(result)

}

# code from https://freakonometrics.hypotheses.org/20158
#' @method deviance gnm
#' @export
deviance.gnm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # take the AIC
  aic <- stats::AIC(result)
  
  # and the degree of freedom
  df <- attr(stats::logLik(result), "df")
  
  # remove the degrees of freedom in AIC 
  # and return the deviance
  d <- aic - (2 * df)
  
  d
  
}

# compute AIC with a validation sample
# code from https://freakonometrics.hypotheses.org/20158
#' @method AIC glm
#' @export
AIC.glm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # get the AIC
  stats::AIC(result)
  
}

# code from https://freakonometrics.hypotheses.org/20158
#' @method deviance glm
#' @export
deviance.glm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # take the AIC
  aic <- stats::AIC(result)
  
  # and the degree of freedom
  df <- attr(stats::logLik(result), "df")
  
  # remove the degrees of freedom in AIC 
  # and return the deviance
  d <- aic - (2 * df)
  
  d
  
}

# compute AIC with a validation sample
# code from https://freakonometrics.hypotheses.org/20158
#' @method AIC lm
#' @export
AIC.lm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # get the AIC
  stats::AIC(result)
  
}

# code from https://freakonometrics.hypotheses.org/20158
#' @method deviance lm
#' @export
deviance.lm <- function(object, newdata = NULL, ...) {
  
  if (is.null(newdata)) {
    return(NextMethod(object, ...))
  }
  
  result <- .refit_glm(object = object, newdata = newdata)
  
  # take the AIC
  aic <- stats::AIC(result)
  
  # and the degree of freedom
  df <- attr(stats::logLik(result), "df")
  
  # remove the degrees of freedom in AIC 
  # and return the deviance
  d <- aic - (2 * df)
  
  d
  
}

# add function refit 
.refit_glm <- function(object, newdata = NULL) {
  
  # get the full call with all arguments 
  args <- as.list(object$call)[-1]
  
  # replace data by newdata
  args[["data"]] <- newdata
  
  # add the formula
  args[["formula"]] <- stats::formula(object)
  
  # add extra arguments constrain and constrainTo
  args <- c(args,
            contrain = "*",
            contrainTo = stats::coef(object))
  
  # make the call to gnm using the arguments defined above
  m <- do.call("gnm", args)
}
