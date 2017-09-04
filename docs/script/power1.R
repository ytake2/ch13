library(rstan)
options(mc.cores = parallel::detectCores())
library(tidyverse)
library(ggjoy)

rm(list=ls())


genPriorN = 2000 # 2000回投げたコインで
genPriorMode = 0.70 #表になるのが60%
genPriorA = genPriorMode * (genPriorN-2) + 1 # a = 最頻値*(集中度-2)+1
genPriorB = ( 1.0 - genPriorMode ) * (genPriorN-2) + 1 # b = (1-最頻値)*(集中度-2)+1
Null_value<- 0.50 #空値
ROPE<-c(Null_value-0.02,Null_value+0.02) # ROPE
HDIwidth<-0.2 # 事後分布の幅(精度)

Nrep=74 #シミュレーションデータセットのサンプルサイズ
simN=100 #シミュレーション回数

#事前分布からパラメタをサンプリング

modelstring<-"
data{
real genPriorA;
real genPriorB;
}

parameters{
real<lower=0,upper=1> theta;
}

model{
theta ~ beta(genPriorA,genPriorB);
}
"

mprior<-stan_model(model_code=modelstring)
mprior_fit<-sampling(mprior, data=list(genPriorA=genPriorA,genPriorB=genPriorB),iter=1000,thin=5,chain=3)


priortheta<-rstan::extract(mprior_fit)$theta
rm(mprior_fit)

#生成されたパラメタからデータセットを作成
dvec<-list()
for(i in 1:length(priortheta)){
  dvec[[i]]=sample(x=c(1,0),# Xの取り得る値を指定
           size=Nrep, #いくつの乱数を発生させるか？
           replace = T, # 復元抽出を指定
           prob=c(priortheta[i],1-priortheta[i])) # 各値を取り得る確率。  
}



# 作成されたデータセットからコイントスの確率推定

modelstring1<-"
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
"



m1<-stan_model(model_code=modelstring1)


# データがリスト型なので、リストにループかけてシミュレーション



reslist<-list()
for(i in 1:simN){
  reslist[[i]]<-sampling(m1, data=list(N=Nrep,y=dvec[[i]]),thin=5,iter=1000,chain=3)
}


thetaList<-list()
for (i in 1:simN)
thetaList[[i]]<-rstan::extract(reslist[[i]])$theta

thetaList<-lapply(thetaList,round,digits=3)


library(ggjoy)
alltheta=NULL
for(i in 1:length(thetaList))
alltheta<-c(alltheta,thetaList[[i]])

library(plotly)
df.all<-data.frame(theta=alltheta,prior=rep(round(priortheta[1:simN],digits=3),each=length(thetaList)))
ggplot(df.all, aes(x = theta, y = as.factor(prior),fill=as.factor(prior))) + 
  geom_joy(panel_scaling=F) + theme_bw()+ylab("Power")+scale_fill_discrete(name="Power")+
  geom_vline(xintercept=0.52,col="red",size=5,alpha=0.3)

thetaList_quantile<-lapply(thetaList,quantile,probs=c(0.025,0.975))
quantile_all<-rbind_all(thetaList_quantile)

#ROPE外し率
sum(quantile_all[,2]<ROPE[1]|quantile_all[,1]>ROPE[2])/nrow(quantile_all)
#目標精度の達成率
sum(quantile_all[,2]-quantile_all[,1]<0.2)/nrow(quantile_all)

