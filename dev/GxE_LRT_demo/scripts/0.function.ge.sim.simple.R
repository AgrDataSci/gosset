library(mvtnorm)
library(PlackettLuce)
library(ClimMobTools)

#' @param n.env integer, the number of environments
#' @param n.geno integer, the number of genotypes
#' @param n.farm.env integer, the number of farms (blocks) per n.env
#' @param sigma.env numeric, the standard deviation of environment 
#' @param cov.env numeric, 
#' @param sigma.plot numeric, the standard deviation of block sites (farms)

ge.sim.simple<-function(n.env,
                        n.geno, 
                        n.farms.env, 
                        sigma.env, 
                        cov.env, 
                        sigma.plot){

# make mockup worth data with genetic gain, multiple environments, 
# partial overlap for different environments, with SEs and GxY

###for now genrate multivariate normal with some covarance matrix to get multy environment data
##set up random environental gradients covering each farm
# n.env=5
# n.geno=10
# n.farms.env<-50

# #all sigma are variances

# ##environmental vcov
# sigma.env=250^2 ##variances
# cov.env=250^2 ##variances
# sigma.plot=700^2

npackages = n.farms.env* n.env

vcov.env <- matrix(cov.env, n.env, n.env)
diag(vcov.env)=sigma.env #covariance matrix   ####


mu.env<-rep(0, n.env)
ge.mat<-rmvnorm(n.geno, mean = mu.env, sigma = vcov.env)
colnames(ge.mat)<-paste("environment",1:n.env,sep="_")
rownames(ge.mat)<-1:n.geno
ge.mat<-data.frame(geno=rownames(ge.mat), ge.mat)


ge.mat.long <- reshape(ge.mat, 
                       direction="long", 
                       idvar="geno", 
                       v.names="environment", 
                       varying=2:ncol(ge.mat))

colnames(ge.mat.long)[colnames(ge.mat.long)=="environment"]<-"trait"
colnames(ge.mat.long)[colnames(ge.mat.long)=="time"]<-"environment"

ge.mat.long$geno<-as.factor(ge.mat.long$geno)
ge.mat.long$geno.environment<-paste(ge.mat.long$geno,ge.mat.long$environment)

####make tricot design and divide farms over environments
tricot.data <- randomise(npackages = npackages, itemnames =  ge.mat$geno)
tricot.data <- data.frame(farm = 1:nrow(tricot.data),
                          environment = rep(1:n.env,each=ceiling(npackages/n.env)), 
                          tricot.data)

##convert to long format and order
tricot.data<-reshape(tricot.data,direction="long",idvar="farm", v.names="item", varying=3:ncol(tricot.data))
tricot.data<-tricot.data[order(tricot.data$farm, tricot.data$environment),]
##change column names
colnames(tricot.data)[match(c("time","item"),colnames(tricot.data))]<-c("rank","geno")
##add geno.environment and assign trait.true
tricot.data$geno.environment<-with(tricot.data,paste(geno, environment))
tricot.data$trait.true<-ge.mat.long$trait[match(tricot.data$geno.environment, ge.mat.long $geno.environment)]
#add plot error
tricot.data$trait.obs<-tricot.data$trait.true+rnorm(nrow(tricot.data),0, sqrt(sigma.plot))

##now change rank for actual observed rank
tricot.data$rank<-unlist(with(tricot.data,tapply(trait.obs, 
                                                 farm,
                                                 function(x) rank(-1*x))))


return(tricot.data)
	
	
}


