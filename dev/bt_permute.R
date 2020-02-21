library("BradleyTerry2")

load("data/gossetJonathanData.rda")

#########

#items <- KenyaItems ### function permic will only work if "items" is defined beforehand! Ridiculous...
iter <- 100
contests <- KenyaChoiceData
predictors <- KenyaFarmerData

#########
bt_permute <- function(contests = NULL, predictors = NULL, iter = 100, ...){

  #### User must specify number of iterations (permutations), (nIterations)
  #### a factor of the items that were ranked,                (items) ## but see the problem mentioned above
  #### the observer's choices in specific (pairwise) format,  (choices)
  #### and observer characteristics.                          (variables)

  # change names in contest
  names(contests) <- c("ObsID","Item1","Item2","Win1","Win2")
  # take items names
  items <- levels(contests$Item1)
  items2 <- levels(contests$Item2)

  # not_factor <- c(!is.factor(contests$Item1), !is.factor(contests$Item2))
  # 
  # if (any(not_factor)) {
  #   stop("Items should be a factor with the same levels")
  # }
  # 
  # if (items != items2) {
  #   stop("Items should be a factor with the same levels")
  # }

  # take number of predictors
  nvar <- ncol(predictors)

  if(nvar > 100) {
    stop("Subscript out of bounds. Cannot handle more than 100 predictors")
  }

  # put input data into a list BradleyTerry2::BTm
  dat <- list()
  dat[[1]] <- contests
  dat[[2]] <- predictors
  dat[[3]] <- as.data.frame(diag(length(items)))

  # take original names of predictors
  origVarnames <- names(dat[[2]])
  origVarnames <- c(origVarnames, rep("Permuted variable", times = nvar))

  # create generalized names for predictors
  Vars <- sprintf("Var%1d", 1:nvar)
  names(dat[[2]]) <- Vars

  # create generalized names for permuted predictors
  permVars <- sprintf("PV%1d", 1:nvar)

  # combine both vectors
  allVars <- c(Vars, permVars)

  # make a matrix with equal rows and cols names
  # for items
  rownames(dat[[3]]) <- items
  colnames(dat[[3]]) <- items


  # calculating deviance of BT models
  # first item in the item list is the reference for BT models
  refItem <- as.character(items[1])

  # List of formula "extensions" to be added to the
  # BTm models as variables are sequentially added.
  BTmformula <- as.list(paste0("~ .. + ", Vars, "[ObsID] * items[..]"))
  BTmformula <- lapply(BTmformula, as.formula)

  # permutations formulas
  BTmformulaPV <- as.list(paste0("~ .. + ", permVars, "[ObsID] * items[..]"))
  BTmformulaPV <- lapply(BTmformulaPV, as.formula)

  Covariates <- character()
  deviances <- list()
  selectedCovIndice <- integer()
  d_VarOverview <- list()
  selectedCovariates <- character()


  # this piece of code ensures that the loop terminates
  ## whenever a permuted variable would be selected for the model
  for (l in 1:nvar){

    if (l == 1) {
      selectedCovIndex <- 0
    } else {
      selectedCovIndex <- match(Covariates[l-1],allVars)
      }

    selectedCovIndice[l-1] <- selectedCovIndex

    # for the first variable, selectedCovIndex == 0
    # thus, no variable is removed from the dataset
    if (selectedCovIndex == 0) {
      allVars2 <- allVars
    }

    # if > nvar, then FirstCovariate is a PV and we stop here (break)
    # exclude selected variable along with its permuted one
    if (selectedCovIndex > 0 & selectedCovIndex <= nvar) {
      allVars2 <- allVars[-c(selectedCovIndice,nvar+selectedCovIndice)]
    }
    # else break

    # Selecting covariates
    # deviances at first round (to identify the first variable to include)
    devl <-  rep(0, length(allVars2))
    names(devl) <- allVars2

    deviances[[l]] <- devl

    # First, determining deviances of BTm models with individual *real* variables
    varIndice <- 1:nvar

    indexCovariate <- match(Covariates, Vars)

    if (length(indexCovariate) > 0) {
      varIndice <- varIndice[-indexCovariate]
      }

    # subset the formulas
    f <- BTmformula[varIndice]

    if (length(Covariates) > 0) {
      # the model sequentially tests all real variables (BTmformula[i])
      # except for those that are already included in Covariates
      # these are necessarily included in the model anyway
      # update formula
      f <- lapply(f, function(X){
        formula(paste(
          c(X,
            c(BTmformula[match(Covariates,allVars)])),
          collapse = "+"))
      })

    }

    # calculate deviances
    dev <- lapply(f, function(X){
      bt <- BradleyTerry2::BTm(outcome = cbind(Win1,Win2),
                               player1 = Item1,
                               player2 = Item2,
                               formula = X,
                               refcat = refItem,
                               data = dat)
      deviance(bt)
    })

    # add results to the main vector
    deviances[[l]][varIndice] <- unlist(dev)

    # ...................................
    # ...................................
    # Second, determining deviances of models with individual *permuted* variables

    # keeps the variable that caused greates deviance reduction, for each iteration in the loop
    d_Var <- character(length = nIterations)

    devsPerIteration <- list()

    # shows user where the process is at - could also be hashed out
    cat("Selecting variable #", l , "... \n")

    for (m in 1:nIterations){

      permutedData <- matrix(nrow = 0, ncol = nvar)

      # permute = randomize rows, and create a new "fake" data set
      permRowOrder <- sample(rownames(dat[[2]]))
      for (j in 1:nrow(dat[[2]])){
        permutedData <- rbind(permutedData,
                              dat[[2]][permRowOrder[j],])
      }

      colnames(permutedData) <- permVars

      # add the permuted data to the original data space
      datPIC <- dat
      datPIC[[2]] <- cbind.data.frame(dat[[2]],permutedData)

      ### data permuted, now fit models

      # adds another half-filled vector to the devsPerIteration list at each iteration
      devsPerIteration[[m]] <- deviances[[l]]

      # again, omit permuted variables that correspond to already-included Covariates
      # we can omit the same index (indexCovariate) because here we start at nvar+1
      for (k in ((length(varIndice)+1):length(allVars))[1:length(varIndice)] ){


        if (length(Covariates) == 0){

          devsPerIteration[[m]][k] <- deviance(
              BTm(
                outcome=cbind(Win1,Win2),

                player1=Item1,
                player2=Item2,

                formula=BTmformulaPV[[k-length(varIndice)]], #for the first Covariate (if length(Covariates)==0))

                refcat=refItem,

                data=datPIC) #the full dataset, including real and fake variables
            )

          } else {

            updatedFormula <- formula(paste(c(BTmformulaPV[[k-length(varIndice)]],
                                              c(BTmformula[match(Covariates,allVars)])),
                                            collapse="+"))

            devsPerIteration[[t]][k] <- deviance(
              BTm(
                outcome=cbind(Win1,Win2),

                player1=Item1,
                player2=Item2,

                formula=updatedFormula,  #when there is already on Covariate and we are adding to that ((if length(Covariates)!=0)))

                refcat=refItem,

                data=datPIC)
            )
            }

      }

      # variable name corresponding to the smallest deviance value
      d_Var[m] <- names(devsPerIteration[[m]])[devsPerIteration[[m]] %in% min(devsPerIteration[[m]])]
    }


    Covariates[l] <- names(table(d.Var))[table(d.Var) %in% max(table(d.Var))] #when a PV appears as often as a real variable, process stops
    #print(origVarnames[match(Covariates[l],allVars)]) #this is the variable that gets added to the model and removed from the variable space, then the process begins anew

    selectedCovariates[l] <- origVarnames[match(Covariates[l],allVars)]
  }

  ######
  selected <- selectedCovariates[-which(selectedCovariates == "Permuted variable")]
  ######

  #### fit final model with the selected variables
  PICmodel <-

    BTm(
      outcome=cbind(Win1,Win2),

      player1=Item1,
      player2=Item2,

      formula=formula(paste(BTmformula[match(selected,origVarnames)],collapse="+")),

      refcat=as.character(items)[1],

      data=dat
    )


#######

  result <- list(
    selected = as.character(selected),
    deviance = PICmodel$deviance,
    AIC = PICmodel$aic,
    null_deviance = PICmodel$null.deviance,
    fitted_values = PICmodel$fitted.values,
    refcat = PICmodel$refcat,
    df_null = PICmodel$df.null,
    df_residual = PICmodel$df.residual
  )

  #######
    return(result)
}
#######

bt_permute(KenyaChoiceData, KenyaChoiceData, 15)

#######
PICselectionResult <- permic(nIterations = 15,
                             items = KenyaItems,
                             choices = KenyaChoiceData,
                             variables = KenyaFarmerData)
# ^^ This is how I see the function should work. But it won't work unless "items" is already defined in the environment before calling the function

#######

PICselectionResult$selected
#With the Kenyan example data, this will reliably yield the expected result ("InputChanges", "LabourAvailability") with about 100+ iterations.


