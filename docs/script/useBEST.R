#  for plot goal one

library(BEST)

proData <- BEST::makeData(mu1=20, sd1=5, mu2=18, sd2=5, nPerGrp=200,
                          pcntOut=10, sdOutMult=2.0, rnd.seed=123)
proMCMC1 <- BESTmcmc(proData$y1, proData$y2, numSavedSteps=2000)

proData <- BEST::makeData(mu1=18, sd1=5, mu2=20, sd2=5, nPerGrp=200,
                          pcntOut=10, sdOutMult=2.0, rnd.seed=123)
proMCMC2 <- BESTmcmc(proData$y1, proData$y2, numSavedSteps=2000)

proData <- BEST::makeData(mu1=20, sd1=5, mu2=20, sd2=5, nPerGrp=700,
                          pcntOut=10, sdOutMult=2.0, rnd.seed=125)
proMCMC3 <- BESTmcmc(proData$y1, proData$y2, numSavedSteps=2000)

plot1<-list(proMCMC1,proMCMC2,proMCMC3)

save(plot1,file="forPlot/plot1.Rdata")

?BEST

proMCMC3

hist(proMCMC3$nu)

proData <- BEST::makeData(mu1=20, sd1=7, mu2=18, sd2=7, nPerGrp=200,
                          pcntOut=10, sdOutMult=2.0, rnd.seed=123)
proMCMC4 <- BESTmcmc(proData$y1, proData$y2, numSavedSteps=2000)


plot(proMCMC1,ROPE=c(-0.5,0.5),which="mean")
plot(proMCMC2,ROPE=c(-0.5,0.5),which="mean")
