# 目標: 空値を除外
## シンプルな例 コイントス 


OUTnull=function(    genPriorMode , genPriorN ,
                      nullVal=NULL , 
                      ROPE=c(nullVal-.03, nullVal+.03),
                      desiredPower=0.8 , audPriorMode=0.5 , audPriorN=2 ,
                      HDImass=0.95 , initSampSize=20 , verbose=TRUE){


# modeと集中度をベータ分布のパラメタに変換
genPriorA = genPriorMode * (genPriorN-2) + 1
genPriorB = ( 1.0 - genPriorMode ) * (genPriorN-2) + 1
audPriorA = audPriorMode * (audPriorN-2) + 1
audPriorB = ( 1.0 - audPriorMode ) * (audPriorN-2) + 1
# Initialize loop for incrementing sampleSize:
sampleSize = initSampSize
notPowerfulEnough = TRUE


#
zvec = 0:sampleSize # vector of all possible z values for N flips.
# Compute probability of each z value for data-generating prior:
pzvec = exp( lchoose( sampleSize , zvec )
             + lbeta( zvec + genPriorA , sampleSize-zvec + genPriorB )
             - lbeta( genPriorA , genPriorB ) )
# For each z value, compute posterior HDI: 
# hdiMat will hold HDI limits for each z:
hdiMat = matrix( 0 , nrow=length(zvec) , ncol=2 )
for ( zIdx in 1:length(zvec) ) {
  z = zvec[zIdx]
  hdiMat[zIdx,] = HDIofICDF( qbeta , 
                             shape1 = z + audPriorA ,
                             shape2 = sampleSize - z + audPriorB ,
                             credMass = HDImass )
}

hdiMat<-round(hdiMat,3)
powerHDI=sum(pzvec[hdiMat[,1]>ROPE[2]|hdiMat[,1]>ROPE[2]])

return(powerHDI)

}



genPriorN = 2000 # 2000回投げたコインで
genPriorMode = 0.60 #表になるのが60%
#HDImaxwid=NULL
nullVal=0.50 # 空値は0.50
ROPE=c(nullVal-.03, nullVal+.03) # ROPE .48~.53
desiredPower=0.8 # 目標達成確率
audPriorMode=0.5 #無情報事前分布
audPriorN=2 # 無情報事前分布
HDImass=0.95 # confidence interval level
initSampSize=20 # サンプルサイズの初期値
#verbose=TRUE

x<-2:100

plot(lchoose( sampleSize , zvec ))

plot(choose(sampleSize, zvec)+beta(0:20+genPriorA,(20-0:20)+genPriorB)-beta(genPriorA,genPriorB))

plot(beta(0:20+genPriorA,(20-0:20)+genPriorB))
plot(beta(genPriorA,genPriorB))

genPriorMode<-seq(.60,.90,0.05)

Ns=NULL
for(j in genPriorMode){
  for (i in 20:500){
    
    Ns<-c(Ns,OUTnull(genPriorN = 2000, # 2000回投げたコインで
                     genPriorMode = j, #表になるのが60%
                     #HDImaxwid=NULL
                     nullVal=0.50, # 空値は0.50
                     ROPE=c(nullVal-.03, nullVal+.03), # ROPE .48~.53
                     desiredPower=0.8, # 目標達成確率
                     audPriorMode=0.5, #無情報事前分布
                     audPriorN=2, # 無情報事前分布
                     HDImass=0.95, # confidence interval level
                     initSampSize=i))
  }
}



Ns2<-as.data.frame(cbind(Ns,rep(20:500,length(genPriorMode)),rep(genPriorMode,each=length(20:500))))
                   
                   
head(Ns2)
library(tidyverse)
devtools::install_github('hadley/ggplot2')
library(ggplot2)
library(plotly)

require(ggplot2)

genPriorA = 0.65 * (2000-2) + 1
genPriorB = ( 1.0 - 0.65 ) * (2000-2) + 1
audPriorA = audPriorMode * (audPriorN-2) + 1
audPriorB = ( 1.0 - audPriorMode ) * (audPriorN-2) + 1

x <- seq(0, 1, len = 2000)
p <- qplot(x, geom = "blank")
stat <- stat_function(aes(x = x, y = ..y..), fun = dbeta, colour="red", n = 2000,
                      args = list(shape1 =genPriorN , shape2 = genPriorA))
p + stat


pzvec = exp( plot(lchoose( sampleSize , zvec ))
             + plot(lbeta( zvec + genPriorA , sampleSize-zvec + genPriorB ))
             - plot(lbeta( genPriorA , genPriorB ) )
             
hist(pzvec)


             
             ggplotly(qplot(x=V2,y=Ns,geom="line",group=as.factor(V3),color=as.factor(V3),data=Ns2)+theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
    axis.title = element_text(size = 15), 
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 22), 
    plot.background = element_rect(fill = "aliceblue", 
        colour = NA, size = 0.2)) +labs(title = "Sample size estimation", x = "N", 
    y = "Power")+labs(colour = "PriorMode") + theme(axis.text = element_text(size = 10), 
    plot.background = element_rect(fill = "azure2")))

