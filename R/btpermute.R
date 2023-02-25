#' Variable selection with Permuted Inclusion Criterion
#' 
#' Method of forward variable selection based on deviance for Bradley-Terry
#'  models using pairwise ranking data. The selection procedure consists of two steps, 
#'  first, permuting the variables from the original \code{predictors} with 
#'  \code{n.iteractions}, then performing a forward selection to retain the
#'  \code{predictors} with highest contribution to the model, see details.
#' 
#' @family model selection functions
#' @author Jonathan Steinke and Kauê de Sousa
#' @param contests a data frame with pairwise binary contests with these variables
#'  'id','player1','player2','win1','win2'; in that order. The id should be equivalent 
#'  to the index of each row in \code{predictors}
#' @param predictors a data frame with player-specific variables with row indices that 
#'  should match with the ids in \code{contests}. An id is not required, only the 
#'  predictor variables, the ids are the index for each row
#' @param n.iterations integer, number of iterations to compute
#' @param seed integer, the seed for random number generation. If NULL (the default), 
#' \pkg{gosset} will set the seed randomly
#' @param ... additional arguments passed to \pkg{BradleyTerry2} methods
#' @return an object of class \code{gosset_btpermute} with the final \code{BTm()} model,
#' selected variables, seeds (random numbers) used for permutations and deviances 
#' @seealso \code{\link{rank_binomial}}, \code{\link[BradleyTerry2]{BTm}}
#' @details 
#' The selection procedure consists of two steps. In the first step, \code{btpermute} 
#'  adds to the set of original (candidate) \code{predictors} variables
#'  an additional set of 'fake', permuted variables. This set of permuted 
#'  \code{predictors} is created 
#'  by assigning to each ranking the variables from another, randomly selected 
#'  ranking. The permuted variables are not expected to have any predictive 
#'  power for pairwise rankings. In the second step, \code{btpermute} adds 
#'  predictors to the Bradley-Terry model in a forward selection procedure. 
#'  Each \code{predictors} (real and permuted) is added to the null model 
#'  individually, and \code{btpermute} retains which variable reduces model 
#'  deviance most strongly. The two-step process is replicated \code{n} times 
#'  with argument \code{n.iterations}. At each iteration, a new random permutation 
#'  is generated and all variables are tested. Replicability can be controlled 
#'  using argument \code{seed}. Across the n \code{n.iterations}, the function 
#'  identifies the predictor that appeared most often as the most deviance-reducing 
#'  one. When this is a real variable, it is constantly added to the model and 
#'  the forward selection procedure moves on – again creating new permutations, 
#'  adding real and fake variables individually, and examining model deviance. 
#'  Variable selection stops when a permuted variable is found to be most 
#'  frequently the most deviance-reducing \code{predictors} across \code{n.iterations}. 
#'  In turn, variable selection continuous as long as any real variable has stronger 
#'  explanatory power for pairwise rankings than the random variables.
#'  
#' @references 
#' Lysen, S. (2009) Permuted inclusion criterion: A variable selection technique. 
#' University of Pennsylvania
#' \url{https://repository.upenn.edu/edissertations/28/}
#' @examplesIf interactive()
#' 
#' require("BradleyTerry2")
#' 
#' data("kenyachoice", package = "gosset")
#' 
#' mod <- btpermute(contests = kenyachoice$contests,
#'                  predictors = kenyachoice$predictors,
#'                  n.iterations = 10,
#'                  seed = 1)
#' 
#' mod
#' 
#' @importFrom BradleyTerry2 BTm
#' @importFrom stats deviance formula
#' @export
btpermute <- function(contests = NULL,
                      predictors = NULL,
                      n.iterations = 15, 
                      seed = NULL, 
                      ...){
  
  # define/set a seed to ensure reproducibility 
  if (is.null(seed)) {
    seed <- runif(1, 1, 99999)
  }
  set.seed(seed)
  seeds <- as.integer(runif(n.iterations, 1, 99999))
  
  # contests should be a data.frame with 5 columns
  # if not, then stop
  if (isFALSE(dim(contests)[[2]] == 5)) {
    stop("'contests' should be a data.frame with 5 columns with these variables:
        c('id','player1','player2','win1','win2')\n")
  }
  
  # change names in contest
  origContsnames <- names(contests)
  names(contests) <- c("ObsID","Item1","Item2","Win1","Win2")
  
  # check if players are factors
  not_factor <- any(c(!is.factor(contests$Item1), !is.factor(contests$Item2)))
  
  if (isTRUE(not_factor)) {
    stop("'player1' and 'player2' should be factors with the same levels\n")
  }
  
  # check if players are factors with same level
  items <- unique(contests$Item1)
  items2 <- unique(contests$Item2)
  
  not_factor <- any(levels(items) != levels(items2))
  
  if (isTRUE(not_factor)) {
    stop("'player1' and 'player2' should be factors with the same levels\n")
  }
  
  dat <- list()
  dat[[1]] <- contests 
  dat[[2]] <- predictors
  dat[[3]] <- as.data.frame(diag(length(items)))
  
  # 'items' need to be assigned to the .GlobalEnv
  # to be processed by BTm
  #assign("items", items, envir = .GlobalEnv)
  items <<- items
  Win1 <- contests$Win1
  Win2 <- contests$Win2
  Item1 <- contests$Item1
  Item2 <- contests$Item2
  
  nvar <- dim(predictors)[[2]]
  
  nobs <- dim(predictors)[[1]]
  
  origVarnames <- names(dat[[2]])
  origVarnames <- c(origVarnames, rep("Permuted variable", times = nvar))
  
  Vars <- sprintf("Var%1d", seq_len(nvar))
  names(dat[[2]]) <- Vars
  
  rownames(dat[[3]]) <- items
  colnames(dat[[3]]) <- items
  
  permVars <- sprintf("PV%1d", seq_len(nvar))
  allVars <- c(Vars,permVars)
  
  # Calculating deviance of BT models
  #first item in the item list is the reference item for BT models
  refItem <- levels(items)[1]
  
  # List of formula "extensions" to be added to the
  # BTm models as variables are sequentially added.
  nperm <- n.iterations + 1
  
  BTmformula <- as.list(paste0("~ .. + ", 
                               sprintf("Var%1d", seq_len(nperm)), 
                               "[ObsID] * items[..]"))
  BTmformula <- lapply(BTmformula, stats::as.formula)
  
  # permutations formulas
  BTmformulaPV <- as.list(paste0("~ .. + ", 
                                 sprintf("PV%1d", seq_len(nperm)), 
                                 "[ObsID] * items[..]"))
  BTmformulaPV <- lapply(BTmformulaPV, stats::as.formula)
  
  Covariates <- character()
  deviances <- list()
  selectedCovIndice <- integer()
  d.VarOverview <- list()
  selectedCovariates <- character()
  
  
  for (l in seq_len(nvar)){
    
    ## this piece of code ensures that the loop terminates whenever a permuted 
    ## variable would be selected for the model
    if (l == 1) {
      selectedCovIndex <- 0
    }else{
      selectedCovIndex <- match(Covariates[l-1], allVars)
    }
    
    selectedCovIndice[l-1] <- selectedCovIndex
    
    # for the first variable, selectedCovIndex == 0
    # thus, no variable is removed from the dataset 
    # if > nvar, then FirstCovariate is a PV and we stop here (break)
    # exclude selected variable along with its permuted one
    ifelse(
      selectedCovIndex == 0,
      allVars2 <- allVars,
      ifelse(
        selectedCovIndex > 0 & selectedCovIndex <= nvar,
        allVars2 <- allVars[-c(selectedCovIndice,nvar + selectedCovIndice)],
        break)
    )
    # Selecting covariates
    
    # deviances at first round (to identify the first variable to include)
    devl <-  numeric(length = length(allVars2)) 
    names(devl) <- allVars2
    
    deviances[[l]] <- devl
    
    # First, determining deviances of BTm models with individual *real* variables
    varIndice <- seq_len(nvar)
    
    indexCovariate <- match(Covariates, Vars)
    if (length(indexCovariate) > 0) {
      varIndice <- varIndice[-indexCovariate]
    }
    
    counter <- 0
    
    if (length(Covariates) == 0) {
      for (i in varIndice){
        counter <- counter + 1
        deviances[[l]][counter] <- stats::deviance(  
          BradleyTerry2::BTm(
            outcome = cbind(Win1,Win2),
            player1 = Item1,
            player2 = Item2,
            formula = BTmformula[[i]],
            refcat  = refItem,
            data    = dat,
            ...)
        )
      }
    }
    
    if (length(Covariates) > 0) {
      for (i in varIndice) {
        # the model sequentially tests all real variables (BTmformula[i])
        # except for those that are already included in Covariates
        # these are necessarily included in the model anyway
        updatedFormula <- stats::as.formula(paste(
          c(BTmformula[[i]], c(BTmformula[match(Covariates,allVars)])),
          collapse="+")
        )
        
        counter <- counter + 1
        
        deviances[[l]][counter] <- stats::deviance(  
          BradleyTerry2::BTm(
            outcome = cbind(Win1,Win2),
            player1 = Item1,
            player2 = Item2,
            formula = updatedFormula,
            refcat  = refItem,
            data    = dat,
            ...)
        )
      }
    }
    
    # Second, determining deviances of models with individual *permuted* variables
    # stores the variable that caused greates deviance reduction, 
    # for each iteration in the loop
    
    d.Var <- character(length = n.iterations) 
    
    devsPerIteration <- list()
    
    message("Selecting variable #",l,"...\n")
    
    for (j in seq_len(n.iterations)){
      # permute = randomize rows, and create a new 'fake' data set
      set.seed(seeds[j])
      permRowOrder <- sample(rownames(dat[[2]]), replace = FALSE)
      
      permutedData <- dat[[2]][permRowOrder, ]
      
      names(permutedData) <- permVars
      
      rownames(permutedData) <- seq_len(nobs)
      
      # add the permuted data to the original data space
      datPIC <- dat
      datPIC[[2]] <- cbind.data.frame(dat[[2]], permutedData)
      
      # data permuted, now fit models
      # adds another half-filled vector to the devsPerIteration list at each iteration
      devsPerIteration[[j]] <- deviances[[l]] 
      
      # again, omit permuted variables that correspond to already-included Covariates
      for (k in ((length(varIndice)+1):length(allVars))[1:length(varIndice)]) { 
        # we can omit the same index (indexCovariate) because here we start at nvar+1
        if (length(Covariates) == 0) {
          # for the first Covariate (if length(Covariates)==0))
          # the full dataset, including real and fake variables
          devsPerIteration[[j]][k] <- stats::deviance( 
            BradleyTerry2::BTm(
              outcome = cbind(Win1,Win2),
              player1 = Item1,
              player2 = Item2,
              formula = BTmformulaPV[[k-length(varIndice)]], 
              refcat  = refItem,
              data    = datPIC, 
              ...) 
          )
          
        }
        
        if (length(Covariates) > 0) {
          updatedFormula <- stats::formula(paste(
            c(BTmformulaPV[[k - length(varIndice)]], 
              c(BTmformula[match(Covariates, allVars)])),
            collapse="+"))
          # when there is already on Covariate and we are adding to that 
          # ((if length(Covariates)!=0)))
          devsPerIteration[[j]][k] <- stats::deviance( 
            BradleyTerry2::BTm(
              outcome = cbind(Win1,Win2),
              player1 = Item1,
              player2 = Item2,
              formula = updatedFormula,  
              refcat  = refItem,
              data    = datPIC,
              ...)
          )
        }
        
      }
      # variable name corresponding to the smallest deviance value
      d.Var[j] <- names(devsPerIteration[[j]])[devsPerIteration[[j]] %in% 
                                                 min(devsPerIteration[[j]])] 
    }
    
    #when a PV appears as often as a real variable, process stops
    Covariates[l] <- names(table(d.Var))[table(d.Var) %in% max(table(d.Var))] 
    
    selectedCovariates[l] <- origVarnames[match(Covariates[l], allVars)]
  }
  
  selected <- selectedCovariates[-which(selectedCovariates == "Permuted variable")]
  
  message("Selected ", toString(selected), "\n")
  
  finalmodel <- stats::as.formula(paste(BTmformula[match(selected, origVarnames)], 
                                        collapse = " + "))
  
  
  repl <- match(selected, origVarnames)
  finalcall <- deparse(finalmodel, width.cutoff = 500)
  for(i in seq_along(repl)) {
    finalcall <- gsub(paste0("Var",repl[i]), selected[i], finalcall)
  }
  
  finalmodel <- stats::as.formula(finalcall)
  
  names(dat[[2]]) <- names(predictors)
  
  # fit final model with the selected variables
  PICmodel <- 
    BradleyTerry2::BTm(
      outcome = cbind(Win1, Win2),
      player1 = Item1,
      player2 = Item2,
      formula = finalmodel,
      refcat = refItem,
      data = dat, 
      ...)
  
  result <- list(model = PICmodel, 
                 formula = finalcall,
                 selected = as.character(selected),
                 seeds = seeds,
                 deviances = deviances)
  
  class(result) <- union("gosset_btpermute", class(result))
  
  return(result)
}

#' @method print gosset_btpermute
#' @export
print.gosset_btpermute <- function(x, ...) {
  cat("Model formula:\n")
  cat(x[["formula"]], "\n \n")
  cat("Permutation estimates: \n")
  p <- unlist(x[["model"]][c("aic","deviance",
                             "null.deviance",
                             "df.residual","df.null")])
  names(p)[1] <- "AIC"
  print(p)
}
