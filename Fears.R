

### Fits fears data

library(splines)
library(tidyr)
library(spatstat) 
library("foreign") 


## run first:
source("./Responsefunctions.R")
source("./Difficultyfunctions.R")
source("./ProgramsFixedResp.R")
source("./ProgramsFixedAlphaResp.R")




##### fears  data data set I x P matrix
dat <- read.table("fear", header=TRUE,sep= " ")
###################

I <- dim(dat)[1]
P <- dim(dat)[2]

### transformation data to 0,...,6 
dat0<-dat-1




### selection of difficulty function (lin) and response function (respfct)

## choose lin, that is, difficulty function

lin <- "log1"
lin <- "lin"
lin<- "logit"  


## choose respf, that is response function

respf<- "NV"
respf<- "logistic"   
respf<- "Gumbel"
respf<-"Gompertz"


# choose continuous or discrete
indicator<- "C"
indicator<- "D"



### always run difffct and respfct after selection of lin and respfct:

if(lin =="lin") {diffunct<-diffunctlin
derdiffunct<-derdiffunctlin}
if(lin =="log") {diffunct<-diffunctlog
derdiffunct<-derdiffunctlog}
if(lin =="log1") {diffunct<-diffunctlog1
derdiffunct<-derdiffunctlog1}
if(lin =="logit") {diffunct<-diffunctlogit
derdiffunct<-derdiffunctlogit}

if(respf=="NV"){respfctd <- respfctdNV
respfctc<-respfctcNV
respfctder<-respfctderNV}
if(respf=="logistic"){respfctd <- respfctdlogit
respfctc<-respfctclogit
respfctder<-respfctderlogit}
if(respf=="Gumbel"){respfctd <- respfctdGumbel
respfctc<-respfctcGumbel
respfctder<-respfctderGumbel}
if(respf=="Gompertz"){respfctd <- respfctdGompertz
respfctc<-respfctcGompertz
respfctder<-respfctderGompertz}



########################################
###  if(lin =="logit")  difficulty function has to defined anew
### for continuous symmetric, for discrete assymmetric


## thresholds and adaptation:
min<- 0  
max <- 6
c<- 0.5

minnew<-min-c #*width
maxnew<-max+c  #*width

minnewcat<-min-2*c #*width
maxnewcat<-max+0.01 

if(indicator=="C"){
  
  diffunctlogit <-function(intdiff,slopediff,y){
    #minnew<--0.5
    #maxnew<-6.5
    r<- intdiff+slopediff*log((y-minnew)/(maxnew-y))
    return(r)  }
  derdiffunctlogit <-function(intdiff,slopediff,y){
    #minnew<--0.5
    #maxnew<-6.5
    r<- slopediff*(maxnew-minnew)/((y-minnew)*(maxnew-y))
    return(r)  }
}

if(indicator=="D"){
  diffunctlogit <-function(intdiff,slopediff,y){
    #minnew<--2
    #maxnew<-6.01 
    r<- intdiff+slopediff*log((y-minnewcat)/(maxnewcat-y))
    return(r)   }
  derdiffunctlogit <-function(intdiff,slopediff,y){
    #minnew<--2
    #maxnew<-6.01 
    r<- slopediff*(maxnewcat-minnewcat)/((y-minnewcat)*(maxnewcat-y))
    return(r)  }
}  


if(lin =="logit") {diffunct<-diffunctlogit
derdiffunct<-derdiffunctlogit}

### end only if(lin =="logit")
#########################################


##### Fits

## fixed alpha

ThrGd<-ThreshModFixedResp(dat0,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)

ThrGd

## varying alpha

Thralpha<-ThreshModFixedAlphaResp(dat0,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, 
                                  startind="no",penalpha=0,respf)
Thralpha 


###memorize for plots:

if (indicator="D") Thrdisc<-ThrGd
if (indicator="C") Thrcon<-ThrGd






####################################
#### Plots for fixed alpha (ThrGd)


#####prob

parmatrest <- ThrGd$parmatrix ###
datn<-dat0

#min <- 1 #minnew #min(dat)-0.5
#max <- 7 #maxnew  #max(dat)+0.5

min <- -0.15 #minnew #min(dat)-0.5
max <- 6.15 

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-0.25 # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,1.3)
y <- seq(min,max,(max-min)/70)

prob <- 0*y  ### only dummy
delta<-0*y  ## dummy

for (l in 1:length(y)){prob[l]<- respfctd(theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])}  
#plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])


plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims, ylab="", pch =pcdum[1])

for (i in 2:I){
  
  for (l in 1:length(y)){prob[l]<- respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
    derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
  delta[l]<-diffunct(parmatrest[i,1],parmatrest[i,2],y[l])}  
  
  
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
  #lines(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
  #sum(prob)*((max-min)/70)
}



##difficulty

parmatrest <- ThrGd$parmatrix ###
datn<-dat0


min <- -0.05 #minnew #min(dat)-0.5
max <- 6.05 

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-1 # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,1.3)
y <- seq(min,max,(max-min)/70)

prob <- 0*y  ### only dummy
delta<-0*y  ## dummy

for (l in 1:length(y)){prob[l]<- respfctd(theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])
}  
plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])

for (i in 2:I){
  
  for (l in 1:length(y))prob[l]<- {respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])
    delta[l]<-diffunct(parmatrest[i,1],parmatrest[i,2],y[l])}  
  
  
  lines(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}


##thresholds functions

parmatrest <- ThrGd$parmatrix ###

min <- -0.45 #minnew #min(dat)-0.5
max <- 6.45 

theta <-0 # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,1.3)
y <- seq(min,max,(max-min)/70)

PT <- 0*y  ### only dummy
delta<-0*y  ## dummy

ypl<-y+1 #Plot 1,2,..

for (l in 1:length(y)){PT[l]<- respfctc(theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))}  
plot(ypl,PT,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Person thresholds functions",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])

for (i in 2:I){ for (l in 1:length(y)){PT[l]<- respfctc(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))}  
  
  
  lines(ypl,PT,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}

#### end plots for ThrGd



##### posterior person parameters    continuous

parmatrest<-Thrcon$parmatrix
stdest <- Thrcon$stdmixt
grid <- seq(-5,5,0.05)  ##theta grid to compute estimates

PostFit <- PosteriorEstimates(grid,dat0,I,indicator="C",lin =lin,parmatrest,stdest)
#PosteriorEstimates <-function(grid,dat,I,indicator,lin =lin,parmatrest,stdest)

pers<-seq(1,P,1)
plot(pers,PostFit,main ="Person parameters",xlab ="Persons", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

perssum<- rep(0,P)
for (l in 1:P)perssum[l]<- sum(dat0[,l])

plot(perssum,PostFit,main ="Person parameters",xlab ="Sums of scores", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

cor(perssum,PostFit) ## correlation with total score

#### link to transformed score (run first difficulty functions for continuous)

dattrans<-matrix(0,I,P)
for (i in 1:I){for (p in 1:P){
  dattrans[i,p] <- diffunct(0,1,dat0[i,p])}}

perssumtrans<- rep(0,P)
for (l in 1:P)perssumtrans[l]<- sum(dattrans[,l])

plot(perssumtrans,PostFit,main ="Person parameters (continuous)",xlab ="Sums of transformed scores", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

cor(perssumtrans,PostFit) ## crrelation with total score



##### posterior person parameters    discrete


parmatrest<-Thrdisc$parmatrix
stdest <- Thrdisc$stdmixt
grid <- seq(-5,5,0.05)  ##theta grid to compute estimates

PostFitDiscr <- PosteriorEstimates(grid,dat0,I,indicator="D",lin =lin,parmatrest,stdest)

pers<-seq(1,P,1)
plot(pers,PostFitDiscr,main ="Person parameters",xlab ="Persons", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

perssum<- rep(0,P)
for (l in 1:P)perssum[l]<- sum(dat0[,l])

plot(perssum,PostFitDiscr,main ="Person parameters",xlab ="Sums of scores", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

cor(perssum,PostFitDiscr) 

#### link to transformed score

dattrans<-matrix(0,I,P)
for (i in 1:I){for (p in 1:P){
  dattrans[i,p] <- diffunct(0,1,dat0[i,p])}}

perssumtrans<- rep(0,P)
for (l in 1:P)perssumtrans[l]<- sum(dattrans[,l])

plot(perssumtrans,PostFitDiscr,main ="Person parameters (discrete)",xlab ="Sums of transformed scores", lwd=2,
     ylab="Estimated parameters",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)  ### plot true against posterior

cor(perssumtrans,PostFitDiscr) ## correlation with total score


#### comparison continuous discrete


cor(PostFit,PostFitDiscr) 





