

####### Fits of Self-Regulation data (Mair)


library(MPsychoR)
library(plyr)
library(MASS)
library(pracma)
library(plyr)
library(spatstat)
#library(memisc)

## run first:
source("./Responsefunctions.R")
source("./Difficultyfunctions.R")
source("./ProgramsFixedResp.R")
source("./ProgramsFixedAlphaResp.R")



#### fitting  using  saved data

dat <-read.table("physsums", header=TRUE,sep= " ")

I <- dim(dat)[1]
P <- dim(dat)[2]

pldat<-as.matrix(dat) 
hist(pldat[1,])
hist(pldat[2,])
hist(pldat[3,])

mean(pldat[1,])
mean(pldat[2,])
mean(pldat[3,])

#### choose difficulty function (lin), response function (respf), and distribution continuous/discrete (indicator)

lin <- "lin"  
lin <- "log"
lin <- "log1"
lin <- "logit"

respf<- "NV"
respf<- "logistic"
respf<- "Gumbel"
respf<- "Gompertz"

indicator<- "C"
#indicator<- "D"


### always run (1) and (2) after selection of lin and respfct   

####(1) define function for response function

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

####(2) define function for difficulty function

if(lin =="lin") {diffunct<-diffunctlin
derdiffunct<-derdiffunctlin}
if(lin =="log") {diffunct<-diffunctlog
derdiffunct<-derdiffunctlog}
if(lin =="log1") {diffunct<-diffunctlog1
derdiffunct<-derdiffunctlog1}


########################################
###  if(lin =="logit") compute  min, max of responses are used

## thresholds and adaptation:
min<- 1  
max <- 7
c<- 0.5 

minnew<-min-c #*width
maxnew<-max+c  #*width

diffunctlogit <-function(intdiff,slopediff,y){
  #minnew<-0.7
  #maxnew<-7.3 #7.6
  r<- intdiff+slopediff*log((y-minnew)/(maxnew-y))
  return(r)  
}
derdiffunctlogit <-function(intdiff,slopediff,y){
  #minnew<-0.7
  #maxnew<-7.3 #7.6
  r<- slopediff*(maxnew-minnew)/((y-minnew)*(maxnew-y))
  return(r)  
}

if(lin =="logit") {diffunct<-diffunctlogit
derdiffunct<-derdiffunctlogit}

### end if(lin =="logit")
#########################################



### fit fixed alpha

Thrp<-ThreshModFixedResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",respf)
Thrp


### fit varying alpha
Thralpha<- ThreshModFixedAlphaResp(dat,commonslope="var" ,indicator=indicator, der="der",lin =lin, start, startind="no",
                                   penalpha=0,respf) 
Thralpha  









####################################
#### Plots for fixed alpha

parmatrest <- Thrp$parmatrix ###
datn<-dat

min <- 1 #minnew #min(dat)-0.5
max <- 7 #maxnew  #max(dat)+0.5

min <- 0 #minnew #min(dat)-0.5
max <- 8 

#min <- 0.0 #minnew #min(dat)-0.5
#max <- 8.5

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-3.5  # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,1.3)
y <- seq(min,max,(max-min)/70)



prob <- 0*y  ### only dummy
delta<-0*y  ## dummy

for (l in 1:length(y)){prob[l]<- respfctd(theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])
}  
#plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])
sum(prob)*(y[2]-y[1]) 

plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])

for (i in 2:I){
  
  for (l in 1:length(y))prob[l]<- {respfctd(theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])} 
  
  
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
}


############################
#### Plots for varying alpha

parmatrest <- Thralpha$parmatrix ###
alphas<-Thralpha$alpha
datn<-dat


min <- 4 #minnew #min(dat)-0.5
max <- 7.00 #maxnew  #max(dat)+0.5

#min <- 0.0 #minnew #min(dat)-0.5
#max <- 8.5

x<- seq(min,max,(max-min)/40)
pcdum <-c ('1','2','3','4','5','6')

theta <-3.5 # specify theta

pcdum <-c ('1','2','3','4','5','6')
ylims <- c(0,2.6)
#ylims <- c(0,2.0)
y <- seq(min,max,(max-min)/80)



prob <- 0*y  ### only dummy
delta<-0*y  ## dummy
#### with item slope, parameterization is alpha*theta -delta_oi+delta*a(y)

for (l in 1:length(y)){prob[l]<- respfctd(alphas[1]*theta - diffunct(parmatrest[1,1],parmatrest[1,2],y[l]))*
  derdiffunct(parmatrest[1,1],parmatrest[1,2],y[l])
delta[l]<-diffunct(parmatrest[1,1],parmatrest[1,2],y[l])
}  
#plot(y,delta,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Difficulty",xlab ="y", type="b",lwd=1.2, ylab="", pch =pcdum[1])
sum(prob)*(y[2]-y[1]) 



plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main ="Densities",xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])

titledens<- "Logit difficulty function, theta=3.5"
plot(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5, main =titledens,xlab ="y", type="b",lwd=1.2,ylim=ylims,
     ylab="", pch =pcdum[1])


for (i in 2:I){
  
  for (l in 1:length(y))prob[l]<- {respfctd(alphas[i]*theta - diffunct(parmatrest[i,1],parmatrest[i,2],y[l]))*
      derdiffunct(parmatrest[i,1],parmatrest[i,2],y[l])} 
  
  
  lines(y,prob,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,lty=2,lwd=1.2, type="b",pch =pcdum[i],cex = 1)
  sum(prob)*(y[2]-y[1]) 
}


