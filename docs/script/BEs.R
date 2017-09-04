library(BEST)
library(rjags)
# 1. Generate idealised data set:
set.seed(123)
proData <- BEST::makeData(mu1=20, sd1=5, mu2=10, sd2=5, nPerGrp=1000,
                    pcntOut=10, sdOutMult=2.0, rnd.seed=NULL)

# 2. Generate credible parameter values from the idealised data:
proMCMC <- BESTmcmc(proData$y1, proData$y2, numSavedSteps=2000)

N1plan <- N2plan <- 20
powerPro <- BESTpower(proMCMC, N1=N1plan, N2=N2plan,
                      ROPEm=c(-1.5,1.5), ROPEsd=c(-2,2), ROPEeff=c(-0.5,0.5),
                      maxHDIWm=15.0, maxHDIWsd=10.0, maxHDIWeff=1.0, nRep=100,
                      showFirstNrep=5,saveName="test")

ddd<-as.data.frame(powerPro)

library(ggjoy)

ggplot(iris, aes(x = Sepal.Length, y = Species)) +
  geom_joy(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_joy()


?ggjoy::geom_joy()
