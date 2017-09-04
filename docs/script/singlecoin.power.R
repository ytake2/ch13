# stanの準備

model_strings<-'
data{
int N;
int y[N];
}
parameters{
real<lower=0,upper=1> theta;
}
model{
theta ~ beta(1,1);
y ~ bernoulli(theta);
}
'

library(rstan)
model<-stan_model(model_code=model_strings)


# データ生成関数
simData.binom<-function(omega, kappa,sampleN){
  #kappa= 2000 #2000回投げて
  #omega= 0.65 # 65%が表
  
  #sampleN=74 # サンプルサイズを指定
  
  genPriorA = omega * (kappa-2) + 1 #β分布のaパラメタに変換
  genPriorB = ( 1.0 - omega ) * (kappa-2) + 1 # β分布のbパラメタに変換
  
  genTheta<-rbeta(1,genPriorA,genPriorB) #パラメタ値生成
  sampleZ<-rbinom(1,size=sampleN,prob=genTheta) # データ生成
  simulatedData<-c(rep(1,sampleZ),rep(0,sampleN-sampleZ)) # 0,1ベクトルに変換
  return(simulatedData)
  
}


#目標達成関数
goalAchievedForSample <- function(fit,ROPE,W) {
  thetaROPE = ROPE
  thetaHDImaxWid= W
  
  # 95%HDIを算出
  thetaHDI<-as.vector(quantile(rstan::extract(fit)$theta,probs=c(0.025,0.975)))
  
  # 目標: 空値ROPE除外
  goalAchieved =list()
  goalAchieved = c(goalAchieved, 
                   "ExcludeROPE"=(thetaHDI[1]>thetaROPE[2]|thetaHDI[2]<thetaROPE[1]))
  
  # 目標: 精度
  goalAchieved = c(goalAchieved, 
                   "NarrowHDI"=(thetaHDI[2]-thetaHDI[1]<thetaHDImaxWid))
  
  return(goalAchieved)
}






nSimulatedDataSets<-100 #シミュレーション数を指定
goalTally=NULL
for (simIdx in 1:nSimulatedDataSets){

simulatedData<-simData.binom(0.70,2000,74) #仮説パラメタ分布のモード (ω)と集中度 (κ), サンプルサイズ
standata<-list(N=length(simulatedData), y=simulatedData)

options(mc.cores = parallel::detectCores())
fit<-rstan::sampling(model,data=standata, cores = getOption("mc.cores", 4L))

goalAchieved<-goalAchievedForSample(fit,c(0.48,0.52),0.2) #ROPE, widthを指定


if(!exists("goalTally")) {
  goalTally=matrix(nrow=0, ncol=length(goalAchieved))
}

goalTally <- rbind(goalTally, goalAchieved)

}

for (goalIdx in 1:NCOL(goalTally)){
  goalName=colnames(goalTally)[goalIdx]
  goalHits=sum(unlist(goalTally[, goalIdx]))
  goalAttempts=nrow(goalTally)
  goalEst=goalHits/goalAttempts
  show(paste0(goalName,": est.power=",round(goalEst,3)))
}


