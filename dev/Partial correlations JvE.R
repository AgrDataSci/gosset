############################################
#Second, check correlation between overall preference and other traits.  
# run packages
library("PlackettLuce")
library("gosset")
library("ClimMobTools")
library("ggplot2")
library("tibble")
source("helper_01_functions.R")
library("reshape2")
library("heatmap3")
library("corpcor")
library("GeneNet")

#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix <- function(L){
  
  n_i <- length(L)
  
  #Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) <- names(L)
  colnames(r) <- names(L)
  
  #Prepare the effective N matrix
  N <- matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) <- names(L)
  colnames(N) <- names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij <- kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] <- as.numeric(rNij[1])
      N[i,j] <- as.numeric(rNij[2]) 
      
    }
    
  }
  
  #Make matrices symmetric    
  r[upper.tri(r)] <- t(r)[upper.tri(r)]
  N[upper.tri(N)] <- t(N)[upper.tri(N)]
  
  #Wrap result
  results <- list(Kendall_tau_corr=r,Effective_N=N)
  return(results)
  
}

#Function that returns the names of the ranking (positive and negative) traits
getRankingTraitNames <- function(x, strip=TRUE){
  
  r <- unlist(colnames(x))
  r <- r[grepl("_pos", r) | grepl("_neg", r)]
  if(strip) r <- unlist(unique(strsplit(r, c("_pos", "_neg"))))
  return(r)
  
}

getRankingTraitNames(dat)

#####################################

#read the data
dat <- read.csv("potato-tricot-data-with-covariate_for_manuscript_QS.csv")

#check data structure
str(dat)
head(dat)
tail(dat)
summary(dat)
table_id <- table(dat$id)
duplicated_ID <- names(table_id[table_id>1]) #find duplicate!! 
dat <- dat[!(dat$id %in% duplicated_ID),]
which(table(dat$id) > 1) #check data structure again

#####################################################################################################
#######################################based on scenario2############################################
#######################################Quality scoring base##########################################

# Remove 23 records exceeding QS of 99, due to low data quality.
QS <- which(dat$Total_sum_QS_point.with.BW > 98)
dat <- dat[-c(QS),] #437 farmers data left in the dataset

#Set non-observed to NA
dat[dat == "Not observed"] <- NA

#########################################################################
# Check how many record left in each trait 
tnames_posneg <- getRankingTraitNames(dat, strip=FALSE)
tnames <- getRankingTraitNames(dat, strip=TRUE)
records <- vector(length=length(tnames))
names(records) <- tnames
for(i in 1:length(tnames)){
  
  records[i] <- sum(complete.cases(dat[,grepl(tnames[i], colnames(dat))]))
  
}

#Inspect results
as.data.frame(records)

# Prepare list of rankings for different traits
R <- list()

for(i in seq_along(tnames)){
  
  # select the item names and rankings for the trait in the iteration i
  d_i <- cbind(dat[, c("package_item_A", "package_item_B", "package_item_C")], dat[, grepl(tnames[i], colnames(dat))])

  # put in the matrix
  R[[i]] <- rankTricot(d_i, items = 1:3, input = 4:5)

}

names(R) <- tnames

rN <- kendallTauCorMatrix(R)
r <- rN[[1]]
N <- rN[[2]]

# Test with the full matrix, just to see if it works
pr <- cor2pcor(r)

# Simple way to work with these values is to convert to Pearson's r
r_pearson <- sin(3.141592654*r*.5)

# Is overall preference mainly affected by marketability, taste, or yield?
s1 <- c("postharvest1_yield", "postharvest1_taste", "postharvest2_marketableatstor", "postharvest3_overallper")
pcor_pearson <- cor2pcor(r_pearson[s1, s1])


s1 <- c("postharvest1_yield", "postharvest1_taste", "postharvest2_marketableatstor", "postharvest3_overallper")
r1 <- r[s1, s1]

?cor0.test

