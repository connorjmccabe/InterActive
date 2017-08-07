##Simulate linear regression data

options(scipen=20) #Punish scientific notation

b0 <- 40 ##Intercept
b1 <- 15 ###X Slope Effect
b2 <- 11 #Z Slope Effect
b3 <- 10 #X*Z Slope Effect
# n<-150 #Sample Size
n<-500 #Sample Size
mu<-rep(0,2) #Specify means (mean of X and Z are both 0 here)
S<-matrix(c(1,.5,.5,1),nrow=,2,ncol=2) #Specify covariance matrix. The correlation between X and Z is moderate (r=.5). 
sigma <- 50 #error

set.seed(789223) ##Set the seed for random number generation

##Simulation function
  dat <- data.frame(id=rep(1:n, times=1))##Creates a data frame, with an ID variable

  require(MASS)
  rawvars<-mvrnorm(n=n, mu=rep(0,2), Sigma=S) #simulates our predictors from a multivariate normal distribution

  require(stats)
  pvars<-pnorm(rawvars) #Given our simulated data this computes the probability that a normally distributed random number will be less than that number. If we use these values, we can convert our raw scores into different distributions with approxiately the same covariances as our original raw data.

  dat$X<-rawvars[,1]
  dat$Z<-rawvars[,2]

  #Exponentially distributed predictors
  dat$Xexp<-scale(qexp(pvars[,1]))
  dat$Zexp<-scale(qexp(pvars[,2]))

  dat$eij <- rep(rnorm(dat$id, 0, sigma)) ##Simulates the Level 1 residual
  #Create Y based on different conditions:

  #When X and Z are both normal
  dat$y <- (b0) + (b1) * (dat$X) + (b2) * (dat$Z)+ (b3)*(dat$X)*(dat$Z) + dat$eij

  #When both are exponentiated
  dat$yexpXZ <- (b0) + (b1) * (dat$Xexp) + (b2) * (dat$Zexp) + (b3)*(dat$Xexp)*(dat$Zexp) + dat$eij
  
#Estimatte linear models for each
lm.mvnorm<-lm(y ~ X + Z + X*Z, data=dat)
lm.exp<-lm(yexpXZ ~ Xexp + Zexp + Xexp*Zexp, data=dat)

#Compare coefficients for each model. Note that coefficients for each are nearly identical.
coef.compare<-cbind(lm.mvnorm$coef,lm.exp$coef)
colnames(coef.compare)<-c("MV normal","Exponentiated")

coef.compare

#The below code write a .csv file of the data created above. This data file can then be uploaded and analyzed in the interActive application.
# write.csv(dat[c("X","Z","Xexp","Zexp","y","yexpXZ","yquad")],"simexample_n150.csv")
# write.csv(dat[c("X","Z","Xexp","Zexp","y","yexpXZ")],"simexample_n500.csv")