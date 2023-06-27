library(here)
library(car)

#set working directory
setwd(here())

##read functions

function.list<-list.files("scripts") 
function.list<-function.list[grep("0.", function.list)]
function.list<-paste("scripts/",function.list,sep="")

funs<-lapply(function.list,source)
rm(funs)


#####now run simulations with and without GxE and store p values

sim.results.null <- c()
sim.results.gxe <- c()
n.sim=10#00  ###. number of simulations, 1000 takes a bit without parallelisation

for(sim in 1:n.sim){
	
##without gxe

##simulate 
sim.data.null<-ge.sim.simple(n.env=2,
                             n.geno=10, 
                             n.farms.env=100, 
                             sigma.env=250^2, 
                             cov.env=250^2, 
                             sigma.plot=500^2)

##test for gxe
test.null<-PL_LR_test(sim.data.null, test.factor="environment")

sim.results.null<-rbind(sim.results.null, test.null)

##simulate 
sim.data.gxe<-ge.sim.simple(n.env=2, 
                            n.geno=10, 
                            n.farms.env=100, 
                            sigma.env=250^2, 
                            cov.env=150^2, 
                            sigma.plot=500^2)

##test for gxe
test.gxe<-PL_LR_test(sim.data.gxe, test.factor="environment")

sim.results.gxe <-rbind(sim.results.gxe, test.gxe)

print(sim)	
}

sim.results.null<-as.data.frame(sim.results.null)
sim.results.gxe <-as.data.frame(sim.results.gxe)

h.null <- hist(sim.results.null$p.val.chisq, breaks= 20, plot=FALSE)
h.null$counts= h.null$counts/sum(h.null$counts)

h.gxe <- hist(sim.results.gxe$p.val.chisq, breaks= 20, plot=FALSE)
h.gxe$counts= h.gxe$counts/sum(h.gxe$counts)


pdf("results/ims/power_hist.pdf", width=20,height=10)
par(mfrow=c(1,2))
plot(h.null,xlab="p-value",main="null",col="gray",cex.axis=1.5,cex.lab=1.5)
plot(h.gxe,xlab="p-value",main="GxE",col="gray",cex.axis=1.5,cex.lab=1.5)
dev.off()


##plot QQ
pdf("results/ims/power_qq.pdf", width=20,height=10)
par(mfrow=c(1,2))
qqPlot(sim.results.null$p.val.chisq, distribution="unif",ylab="observed",main="null", envelope=F)
qqPlot(sim.results.gxe$p.val.chisq, distribution="unif",ylab="observed",main="GxE", envelope=F)
dev.off()
