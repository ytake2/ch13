
source("script/minNforHDIpower.R")

# Specify desired powers:
desPow = seq(.6,.95,by=0.01)
# Specify generating modes:
genMod = seq(.60,0.95,by=0.01)

# HDI excludes ROPE around nullVal:
# Declare matrix for storing results:
sampSizeMatrix = matrix( NA , nrow=length(desPow) , ncol=length(genMod) ,
                         dimnames=list("Power"=desPow,"Gen.Mode"=genMod) )
# Compute sample size for all combinations of desired powers and modes:
for ( desPowIdx in 1:length(desPow) ) {
  for ( genModIdx in 1:length(genMod)  ) {
    sampSize = minNforHDIpower( genPriorMode=genMod[genModIdx], genPriorN=2000,
                                HDImaxwid=NULL, nullVal=0.5, ROPE=c(0.48,0.52),
                                desiredPower=desPow[desPowIdx] , 
                                audPriorMode=0.5 , audPriorN=2 ,
                                HDImass=0.95 , initSampSize=5 , verbose=FALSE )
    sampSizeMatrix[desPowIdx,genModIdx ] = sampSize
    show(sampSizeMatrix)
  }
}

sampSizeExclNull = sampSizeMatrix
show(sampSizeExclNull)

d1<-as.data.frame(sampSizeExclNull)
library(tidyverse)
names(d1)<-paste0("m",genMod)

d1.long<-gather(d1,gem.mode,samplesize, m0.6:m0.95)
d1.long$power<-rep(genMod,length(genMod))
d1.long$mode<-rep(genMod,each=length(genMod))

head(d1.long)
g<-ggplot(d1.long, aes(x=mode,y=samplesize,group=as.factor(power),color=power))
g+geom_line()

# HDI has maximum width:
# Declare matrix for storing results:
sampSizeMatrix = matrix( NA , nrow=length(desPow) , ncol=length(genMod) ,
                         dimnames=list("Power"=desPow,"Gen.Mode"=genMod) )
# Compute sample size for all combinations of desired powers and modes:
for ( desPowIdx in 1:length(desPow) ) {
  for ( genModIdx in 1:length(genMod)  ) {
    sampSize = minNforHDIpower( genPriorMode=genMod[genModIdx], genPriorN=10,
                                HDImaxwid=0.20, nullVal=NULL, ROPE=NULL,
                                desiredPower=desPow[desPowIdx] , 
                                audPriorMode=0.5 , audPriorN=2 ,
                                HDImass=0.95 , initSampSize=50 , verbose=FALSE )
    sampSizeMatrix[desPowIdx,genModIdx ] = sampSize
    show(sampSizeMatrix)
  }
}
sampSizeMaxHdiWid = sampSizeMatrix
show(sampSizeExclNull)
show(sampSizeMaxHdiWid)



