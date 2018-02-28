#Appendix A: Computing the Confidence Interval of a Predicted Value of a Dependent Variable.

#R Code

#NOTE: the ggplot2 package must be installed for this example.

#Equations 2 and 3, in matrix algebra form
#values for x and y, n = 5
x<-c(-1.29,.64,1.2,1.37,-.7)
y<-c(48.8,4.78,81.38,112,67.79)

Xmat<-as.matrix(cbind(rep(1,5),x))
names(Xmat)<-c("(Intercept)","X")

A<-solve((t(Xmat) %*% Xmat)) #This is the inverse of the inner product found in Equation 3
beta <- A %*% t(Xmat) %*% y #linear model coefficients
resids <- y - (Xmat %*% beta) #residualvalues
sig<-sqrt(sum(resids^2) / (nrow(Xmat)-
                             ncol(Xmat))) #residual standard error term found in Equation 3
matxi<-t(as.matrix(c(1,.5)))
SEys <- diag(sig * (sqrt(as.matrix(matxi) %*% A %*% t(as.matrix(matxi))))) #Computation of the standard error of the predicted value of Y, shown in Equation 3
yhat<-matxi%*%beta
tmult <- qt(.975,3) #t-multiplier, with degrees of freedom = n-k-1 = 3
ylower<- yhat - tmult*SEys #lower confidence limit, as in Equation 2
yupper<- yhat + tmult*SEys #upper confidence limit, as in Equation 2
#The following shows that the confidence limits computed above correspond with the edges of the confidence region of the regression line.
#note that the following code requires the ggplot2 package be installed.
df<-as.data.frame(cbind(x,y)) #Put x and y into a dataframe object
require(ggplot2)
ggplot(x=x,y=y) +
  geom_smooth(data=df, aes(x=x,y=y),method =
                "lm", color = "black") +
  annotate("point",x=.5,y=ylower, shape =
             21) +
  annotate("point",x=.5,y=yupper, shape =
             21) +
  xlab("X") + ylab("Y") +
  theme_bw()

