# t-test power

library(rstan)
options(mc.cores = parallel::detectCores())

y1<-rnorm(100,20,3)
y2<-rnorm(100,10,3)

modelstrings<-"
parameters{
real mu1; //y1の平均値
real mu2; //y2の平均値
real <lower=0>sigma1; //y1の標準偏差
real <lower=0>sigma2; //y2の標準偏差
}
model{
//y1 ~ normal(mu1,sigma1); 
//y2 ~ normal(mu2,sigma2); 
mu1 ~ normal(20,5); 
mu2 ~ normal(12,4); 
sigma1 ~ cauchy(0,5);
sigma2 ~ cauchy(0,5);
}
generated quantities{
real delta;
delta = mu1-mu2;
}
"

model<-stan_model(model_code=modelstrings)
fit_ttest<-sampling(model,thin=50)

mu1<-rstan::extract(fit_ttest)$mu1
mu2<-rstan::extract(fit_ttest)$mu2
sigma1<-rstan::extract(fit_ttest)$sigma1
sigma2<-rstan::extract(fit_ttest)$sigma2

y1s<-list()
for(i in 1:length(mu1)){
  y1s[[i]]<-rnorm(20,mu1[i],sigma1[i])
}

y2s<-list()
for(i in 1:length(mu2)){
  y2s[[i]]<-rnorm(20,mu2[i],sigma2[i])
}

modelstrings<-"
data{
int N1; 
int N2;
real y1[N1];
real y2[N2];
}
parameters{
real mu1; //y1の平均値
real mu2; //y2の平均値
real <lower=0>sigma1; //y1の標準偏差
real <lower=0>sigma2; //y2の標準偏差
}
model{
y1 ~ normal(mu1,sigma1); 
y2 ~ normal(mu2,sigma2); 
mu1 ~ normal(0,100); 
mu2 ~ normal(0,100); 
sigma1 ~ cauchy(0,5);
sigma2 ~ cauchy(0,5);
}
generated quantities{
real delta;
delta = mu1-mu2;
}
"

model2<-rstan::stan_model(model_code=modelstrings)

reslist<-list()
for(i in 1:length(mu1))
reslist[[i]]<-sampling(model2,data=list(N1=length(y1s[[1]]),N2=length(y2s[[1]]),
                          y1=y1s[[i]],y2=y2s[[i]]))


mcmclist<-list()
for(i in 1:length(mu1))
mcmclist[[i]]<-rstan::extract(reslist[[i]])$delta

library(tidyverse)
mcmclist<-lapply(mcmclist,round,digits=3)
HDI<-lapply(mcmclist,quantile,probs=c(0.025,0.0975))
HDIall<-rbind_all(HDI)
sum(HDIall[,1]>1|HDIall[,2]< -1)/nrow(HDIall)


hist(mcmclist[[12]])


rnorm(20,mu1[1],sigma1[1])
rnorm(20,mu1[2],sigma1[2])
rnorm(20,mu1[3],sigma1[3])

load("test")

hist(p)
