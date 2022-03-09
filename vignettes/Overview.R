## ----fetch, message=FALSE, eval=FALSE, echo=TRUE------------------------------
#  library("PlackettLuce")
#  library("gosset")
#  library("climatrends")
#  library("nasapower")
#  
#  data("breadwheat", package = "gosset")
#  
#  head(breadwheat)
#  
#  dat <- breadwheat
#  

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  traits <- c("yield",
#              "grainquality",
#              "germination")
#  
#  # names of colunms with varieties
#  items <- paste0("variety_", letters[1:3])
#  
#  # name of varieties
#  itemnames <- sort(unique(unlist(dat[items])))
#  

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  # build the rankings and put into a list
#  pldat <- list()
#  
#  # run over the rankings of each trait
#  for(i in seq_along(traits)){
#  
#    # select the item names and rankings for the trait in the iteration i
#    d_i <- dat[, c(items, paste0(traits[i], c("_best", "_worst")))]
#    # not observed as NA
#    d_i[d_i == "Not observed"] <- NA
#    # check for ties in the response pos == neg
#    keep <- d_i[,4] != d_i[,5] & !is.na(d_i[,4]) & !is.na(d_i[,5])
#    # keep only the TRUE values out of the later validation
#    d_i <- d_i[keep, ]
#  
#    names(d_i)[4:5] <- c("best", "worst")
#  
#    R_i <- rank_tricot(d_i, items = 1:3, input = c("best", "worst"))
#  
#    pldat[[i]] <- R_i
#  
#  }
#  
#  pldat
#  
#  # fit the PlackettLuce model
#  mod <- lapply(pldat, PlackettLuce)
#  

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  worth_map(mod, labels = traits)

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  temp <- temperature(dat[, c("lon","lat")],
#                      day.one = dat[, "planting_date"],
#                      span = 80)

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  R <- rank_tricot(dat,
#                   items = c("variety_a","variety_b","variety_c"),
#                   input = c("overall_best","overall_worst"),
#                   group = TRUE)
#  
#  pld <- cbind(R, temp)
#  
#  pl <- pltree(R ~ maxNT + maxDT,
#               alpha = 0.1,
#               gamma = TRUE,
#               data = pld)

## ---- message=FALSE, eval=FALSE, echo=TRUE------------------------------------
#  plot(pl)
#  
#  node_rules(pl)
#  
#  top_items(pl, top = 5)
#  
#  worst_regret(pl)
#  
#  worth_map(pl)

