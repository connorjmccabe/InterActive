require(RCurl)

require(ggplot2)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

b0 <- 40 ##Intercept
b1 <- 15 ###X Slope Effect
b2 <- 11 #Z Slope Effect
b3 <- 10 #X*Z Slope Effect
# n<-150 #Sample Size
n<-500 #Sample Size
mu<-rep(0,2) #Specify means
S<-matrix(c(1,.5,.5,1),nrow=2,ncol=2) #Specify covariance matrix
sigma <- 5 #Level 1 error (i.e. time-specific residuals)

set.seed(789223) ##Set the seed for random number generation

##Simulation function
df <- data.frame(id=rep(1:n, times=1))##Creates a dfa frame, with an ID variable
require(MASS)
rawvars<-mvrnorm(n=n, mu=rep(0,2), Sigma=S) #simulates our predictors from a multivariate normal distribution

require(stats)
pvars<-pnorm(rawvars) #Given our simulated dfa this computes the probability that a normally distributed random number will be less than that number. If we use these values, we can convert our raw scores into different distributions with approxiately the same covariances as our original raw dfa.

df$X<-rawvars[,1]
df$Z<-rawvars[,2]

#Exponentially distributed predictors
df$Xexp<-scale(qexp(pvars[,1]))
df$Zexp<-scale(qexp(pvars[,2]))

df$eij <- rep(rnorm(df$id, 0, sigma)) ##Simulates the Level 1 residual
#Create Y based on different conditions:

#When X and Z are both normal
df$y <- (b0) + (b1) * (df$X) + (b2) * (df$Z)+ (b3)*(df$X)*(df$Z) + df$eij


#When both are exponentiated
df$yexpXZ <- (b0) + (b1) * (df$Xexp) + (b2) * (df$Zexp) + (b3)*(df$Xexp)*(df$Zexp) + df$eij

df<-newdata <- df[order(df$Z),]
dfplot<-df

df1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

# A basic graph
lp <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, linetype=sex)) + geom_line(size=1) + theme_bw() + theme(legend.position = c(.53, 0.8), legend.direction = "vertical")

# Change the legend
lp2<-lp + scale_linetype_discrete(name = element_blank(),
                                  breaks=c("Male", "Female"),
                                  labels=c("Low Z (-1 SD)", "High Z (+1 SD)"))


mylegend1<-g_legend(lp2)

require(RColorBrewer)
x<-colorRampPalette(c("white","black"))
y<-colorRampPalette(c("black","white"))

df$level.Z[scale(df$Z)<0]<-"Low"
df$level.Z[scale(df$Z)>0]<-"High"
df$level.Zexp[scale(df$Zexp)<0]<-"Low"
df$level.Zexp[scale(df$Zexp)>0]<-"High"

#User-defined centering and probing functions
center<-function(MOD)
{
  centered<-MOD-mean(MOD,na.rm=TRUE)
  return(centered)
}

probe.low<-function(MOD)
{
  low<-(MOD-mean(MOD,na.rm=TRUE))+sd(MOD, na.rm=TRUE)
  return(low)
}

probe.high<-function(MOD)
{
  high<-(MOD-mean(MOD, na.rm=TRUE))-sd(MOD, na.rm=TRUE)
  return(high)
}

model.mvnorm.low<-lm(y~1 + center(X) + probe.low(Z) + center(X)*probe.low(Z), data=df)
model.mvnorm.mean<-lm(y~1 + center(X) + center(Z) + center(X)*center(Z), data=df)
model.mvnorm.high<-lm(y~1 + center(X) + probe.high(Z) + center(X)*probe.high(Z), data=df)

#Exponentiated

model.XZexp.low<-lm(yexpXZ~1 + center(Xexp) + probe.low(Zexp) + center(Xexp)*probe.low(Zexp), data=df)
model.XZexp.mean<-lm(yexpXZ~1 + center(Xexp) + center(Zexp) + center(Xexp)*center(Zexp), data=df)
model.XZexp.high<-lm(yexpXZ~1 + center(Xexp) + probe.high(Zexp) + center(Xexp)*probe.high(Zexp), data=df)

require(ggplot2)
plot.mvnorm<-ggplot(dfplot, aes(x=X,y=y)) + 
  
  geom_point(aes(color=Z),size = 1.5, alpha = 1) +
  # scale_colour_gradient(low = "#000000", high = "#FFFFFF") +
  scale_colour_gradient2(low = "black", mid= "white", high = "black", guide = guide_colorbar(direction="horizontal")) +
  
  geom_abline(intercept = model.mvnorm.low$coefficients["(Intercept)"], slope = model.mvnorm.low$coefficients["center(X)"], linetype = "dashed", color = "black", size = 1) +
  
  geom_abline(intercept = model.mvnorm.high$coefficients["(Intercept)"], slope = model.mvnorm.high$coefficients["center(X)"], linetype = "solid", color = "black", size = 1) +
  
  ylab("Y") +
  xlab("") +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(-2,-1,0, 1, 2),
                     labels = c("-2 SD","-1 SD", "Mean", "1 SD", "2 SD")) +
  theme(
    text = element_text("Helvetica"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
    panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom", legend.direction = "vertical") +
  theme_bw()
plot.mvnorm
mylegend2<-g_legend(plot.mvnorm)


plot.XZexp<-ggplot(dfplot, aes(Xexp,yexpXZ)) + 
  
  geom_point(aes(color=Zexp),size = 1.5, alpha = 1) + 
  # scale_colour_gradient(low = "#000000", high = "#FFFFFF") +
  scale_colour_gradient2(low = "black", mid= "white", high = "black") +
  
  # geom_abline(intercept = model.XZexp.low$coefficients["(Intercept)"], slope = model.XZexp.low$coefficients["center(Xexp)"], linetype = "dashed", color = "#2C2C2C", size = 1.5) +
  
  geom_abline(intercept = model.XZexp.low$coefficients["(Intercept)"], slope = model.XZexp.low$coefficients["center(Xexp)"], linetype = "dashed", color = "black", size = 1) +
  
  # geom_abline(intercept = model.XZexp.high$coefficients["(Intercept)"], slope = model.XZexp.high$coefficients["center(Xexp)"], linetype = "dashed", color = "#646464", size = 1.5) +
  
  geom_abline(intercept = model.XZexp.high$coefficients["(Intercept)"], slope = model.XZexp.high$coefficients["center(Xexp)"], linetype = "solid", color = "black", size = 1) +
  
  # ylim(0,101) +
  ylab("Y") +
  xlab("") +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(-2,-1,0, 1, 2),
                     labels = c("-2 SD","-1 SD", "Mean", "1 SD", "2 SD")) +
  theme(
    text = element_text("Helvetica"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
    panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom") +
  theme_bw()
plot.XZexp

df$yquad <-(b0) + (b1) * (df$X) + (b2) * (df$Z) + (b3)*(df$X)*(df$X) + df$eij

model.quad.low<-lm(yquad~1 + center(X) + probe.low(Z) + I(center(X)^2), data=df)
model.quad.mean<-lm(yquad~1 + center(X) + center(Z) + I(center(X)^2), data=df)
model.quad.high<-lm(yquad~1 + center(X) + probe.high(Z) + I(center(X)^2), data=df)

model.quadint.low<-lm(yquad~1 + center(X) + probe.low(Z) + center(X)*probe.low(Z), data=df)
model.quadint.mean<-lm(yquad~1 + center(X) + center(Z) + center(X)*center(Z), data=df)
model.quadint.high<-lm(yquad~1 + center(X) + probe.high(Z) + center(X)*probe.high(Z), data=df)

plot.quad<-ggplot(df, aes(x=X,y=yquad)) + 
  
  geom_point(aes(color=Z),size = 1.5, alpha = 1) +
  scale_colour_gradient2(low = "black", mid= "white", high = "black", guide = guide_colorbar(direction="horizontal")) +
  
  geom_abline(intercept = model.quadint.low$coefficients["(Intercept)"], slope = model.quadint.low$coefficients["center(X)"], linetype = "dashed", color = "black", size = 1) +
  
  geom_abline(intercept = model.quadint.high$coefficients["(Intercept)"], slope = model.quadint.high$coefficients["center(X)"], linetype = "solid", color = "black", size = 1) +
  ylab("Y") +
  xlab("") +
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(-2,-1,0, 1, 2),
                     labels = c("-2 SD","-1 SD", "Mean", "1 SD", "2 SD")) +
  theme(
    text = element_text("Helvetica"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
    panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom", legend.direction = "vertical") +
  ylim(-100,200) +
  theme_bw()
plot.quad

plot3a<-plot.mvnorm + ylim(-100,200) + 
  ggtitle("4a") + theme(text = element_text("Helvetica", size = 10)) +
  theme(legend.position="none",
        text=element_text("Helvetica", size = 10))

plot3b<-plot.XZexp + ylim(-100,200) + 
  ggtitle("4b") + theme(text = element_text("Helvetica", size = 10)) +
  theme(legend.position="none",
        text=element_text("Helvetica", size = 10))

plot3c<-plot.quad + 
  ylim(-100,200) + 
  ggtitle("4c") + theme(text = element_text("Helvetica", size = 10)) +
  theme(legend.position="none",
        text=element_text("Helvetica", size = 10))

require(gridExtra)
plot3final<-grid.arrange(arrangeGrob(
  plot3a,plot3b,plot3c,mylegend1, nrow = 1, widths = c(8,8,8,4)),
  mylegend2, nrow=2,heights=c(8, 1))
