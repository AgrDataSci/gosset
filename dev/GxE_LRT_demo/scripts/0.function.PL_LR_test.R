library(PlackettLuce)
library(ClimMobTools)
library(gosset)

PL_LR_test<-function(tricot.data,test.factor){

###first create PL ranks for whole trial
PL.data.all<-rank_numeric(data = tricot.data,
                        items = "geno",
                        input = "rank", 
                        id = "farm", 
                        ascending = FALSE)

##fit PL model
PL.model.all<-PlackettLuce(PL.data.all)

###now iterate over environments, estimate worths, and store models in list
PL.model.list<-c()
DF.resid.sum=0 ##DF
LL.sum=0 ##LL

level.list<-unique(tricot.data[, test.factor])


for(lev in 1:length(level.list)){
tricot.data.lev <- tricot.data[tricot.data[, test.factor] == level.list[lev], ]	

PL.data.lev <- rank_numeric(data = tricot.data.lev,
                            items = "geno",
                            input = "rank", 
                            id = "farm", 
                            ascending = FALSE)
##fit PL model
PL.model.lev <- PlackettLuce(PL.data.lev)

LL.sum = LL.sum + PL.model.lev$loglik
DF.resid.sum = DF.resid.sum + PL.model.lev$df.residual

##write to list
PL.model.list[[lev]]<-PL.model.lev

}

DF.delta= PL.model.all$df.residual-DF.resid.sum
Deviance = -2 * (PL.model.all$loglik - LL.sum)

##Chisq P value
p.val.chisq <- 1 - pchisq(Deviance, DF.delta)

out <- c(Deviance = Deviance ,DF.delta = DF.delta, p.val.chisq = p.val.chisq)

return(out)	

}
