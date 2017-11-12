require(RCurl)
df<-read.csv(text=getURL("https://raw.githubusercontent.com/connorjmccabe/InterActive/master/Simulated%20Data/ex1_n150_AMPPS.csv"))

names(df)
df$X<-df$X-mean(df$X)
df$Xmin1<-df$X+1
df$Xplus1<-df$X-1

lm.x<-lm(y~X,df)
lm.xmin1<-lm(y~Xmin1,df)
lm.xplus1<-lm(y~Xplus1,df)
coef(lm.x)
coef(lm.xmin1)

mean(df$y)
qt(.975,lm.x$df.residual)

lm.x$df.residual

confint(lm.x)
ll.xmin1<-confint(lm.xmin1)["(Intercept)","2.5 %"]
ul.xmin1<-confint(lm.xmin1)["(Intercept)","97.5 %"]
ll.x<-confint(lm.x)["(Intercept)","2.5 %"]
ul.x<-confint(lm.x)["(Intercept)","97.5 %"]
ll.xplus1<-confint(lm.xplus1)["(Intercept)","2.5 %"]
ul.xplus1<-confint(lm.xplus1)["(Intercept)","97.5 %"]
confint(lm.xplus1)

coef(lm.x)
require(ggplot2)
confplot<-ggplot(data=df,aes(x=X,y=y)) +
  geom_smooth(method = "lm", color = "black") +
  annotate("point",x=0,y=ll.x, shape = 22) +
  annotate("point",x=0,y=ul.x, shape = 22) +
  annotate("point",x=1,y=ll.xplus1, shape = 22) +
  annotate("point",x=1,y=ul.xplus1, shape = 22) +
  xlab("X") +
  ylab("Y") +
  theme(text=element_text(family="Helvetica",size=12, color="black"),
        legend.position="none",
        panel.background = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(colour="black"),
        axis.title.x=element_text(size=14),
        axis.text.y=element_text(colour="black"),
        axis.title.y=element_text(size=14),
        panel.grid.major = element_line(colour="#C0C0C0"),
        plot.background=element_rect(fill='white')) +
  theme_bw()
ggsave("fig6.pdf",
       confplot, width = 7, height = 4, units = "in")

