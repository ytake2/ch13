rm(list=ls())
source("script/DBDA2E-utilities.R")
source("script/Stan-Ydich-XnomSsubj-MbinomBetaOmegaKappa.R")

library(rstan)
#######理想データ作り###############


# 理想的な仮説を設定:
idealGroupMean = 0.65 # 仮説平均
idealGroupSD = 0.07  # 仮説SD
idealNsubj = 100       # 対象者 対象者を多くすれば、仮説の確信度は高くなる
idealNtrlPerSubj = 100 # 試行数 試行数を多くすれば、仮説の確信度は高くなる

# Generate random theta values for idealized subjects:
betaAB = betaABfromMeanSD( idealGroupMean , idealGroupSD ) #平均とSDをベータ分布のパラメータに変換
theta = rbeta( idealNsubj , betaAB$a , betaAB$b ) # 仮説のパラメータ分布をrbetaで発生
# Transform the theta values to exactly match idealized mean, SD:
theta = ((theta-mean(theta))/sd(theta))*idealGroupSD + idealGroupMean
theta[ theta >= 0.999 ] = 0.999 # must be between 0 and 1
theta[ theta <= 0.001 ] = 0.001 # must be between 0 and 1
# Generate idealized data very close to theta's:
z = round( theta*idealNtrlPerSubj ) 
# Convert to data format needed by JAGS function: 1人100回のコイントスを100名分 yがコイントス結果、sがsubject、theta値に基づいて、成功数を計算
dataMat=matrix(0,ncol=2,nrow=0,dimnames=list(NULL,c("y","s")))
for ( sIdx in 1:idealNsubj ) {
  yVec = c(rep(1,z[sIdx]),rep(0,idealNtrlPerSubj-z[sIdx]))
  dataMat = rbind( dataMat , cbind( yVec , rep(sIdx,idealNtrlPerSubj) ) )
}
idealDatFrm = data.frame(dataMat)

#######理想データにMCMCでパラメタの分布を生成###############

y = as.numeric(idealDatFrm[,"y"])
s = as.numeric(idealDatFrm[,"s"]) # ensures consecutive integer levels

if ( any( y!=0 & y!=1 ) ) { stop("All y values must be 0 or 1.") }
z = aggregate( y , by=list(s) , FUN=sum )$x
N = aggregate( rep(1,length(y)) , by=list(s) , FUN=sum )$x
Nsubj = length(unique(s))
# Specify the data in a list, for later shipment to JAGS:
dataList = list(
  z = z ,
  N = N ,
  Nsubj = Nsubj
)

#-----------------------------------------------------------------------------
# THE MODEL.
modelString = "
data {
int<lower=1> Nsubj ;
int<lower=0> z[Nsubj] ;
int<lower=0> N[Nsubj] ;
}
parameters {
real<lower=0,upper=1> theta[Nsubj] ; // individual prob correct
real<lower=0,upper=1> omega ;        // group mode
real<lower=0> kappaMinusTwo ;        // group concentration minus two
}
transformed parameters {
real<lower=0> kappa ;  
kappa <- kappaMinusTwo + 2 ;
}
model {
omega ~ beta( 1 , 1 ) ;
kappaMinusTwo ~ gamma( 0.01 , 0.01 ) ; // mean=1 , sd=10 (generic vague)
// kappaMinusTwo ~ gamma( 1.105125 , 0.1051249 ) ;  # mode=1 , sd=10 
theta ~ beta( omega*(kappa-2)+1 , (1-omega)*(kappa-2)+1 ) ; // vectorized
for ( s in 1:Nsubj ) {
z[s] ~ binomial( N[s], theta[s] ) ;
}
}
"

model<-rstan::stan_model(model_code=modelString)

#-----------------------------------------------------------------------------
# INTIALIZE THE CHAINS.
# Initial values of MCMC chains based on data:
initsList = function() {
  thetaInit = rep(0,Nsubj)
  for ( sIdx in 1:Nsubj ) { # for each subject
    includeRows = ( s == sIdx ) # identify rows of this subject
    yThisSubj = y[includeRows]  # extract data of this subject
    resampledY = sample( yThisSubj , replace=TRUE ) # resample
    thetaInit[sIdx] = sum(resampledY)/length(resampledY) 
  }
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  meanThetaInit = mean( thetaInit )
  kappaInit = 100 # lazy, start high and let burn-in find better value
  return( list( theta=thetaInit , omega=meanThetaInit , 
                kappaMinusTwo=kappaInit-2 ) )
}
#-----------------------------------------------------------------------------
# RUN THE CHAINS
parameters = c( "theta","omega","kappa") # The parameters to be monitored
burnInSteps = 500            # Number of steps to burn-in the chains
nChains = 4                  # nChains should be 2 or more for diagnostics 
numSavedSteps=2000
nChains=4
thinSteps=20
Nsubj=100
# Get MC sample of posterior:
options(mc.cores = parallel::detectCores())
stanFit <- rstan::sampling(object=model , 
                     data = stan_data,
                     warmup=burnInSteps,
                     chain=nChains,
                     thin=thinSteps,
                     init=initsList)

aaa<-rstan::extract(stanFit)


aaa[1,"omega"]

library(dplyr)

as.matrix(aaa)

stan_plot(stanFit)

aaa[1,0.78]

plot(stanFit)
