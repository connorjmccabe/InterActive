#Figure 3: uncertainty by sample size
require(RCurl)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
n500<-na.omit(read.csv(text=getURL("https://raw.githubusercontent.com/connorjmccabe/InterActive/master/plot_estimates_n500_mvnorm.csv"),header=T))
df150all<-read.csv(text=getURL("https://raw.githubusercontent.com/connorjmccabe/InterActive/master/figure3_uncertainty_plotestimates_n150.csv"), header=T)
df500all<-read.csv(text=getURL("https://raw.githubusercontent.com/connorjmccabe/InterActive/master/figure3_uncertainty_plotestimates_n500.csv"), header=T)

n150<-df150all[which(startsWith(as.character(df150all$level), "-1 SD")==TRUE| startsWith(as.character(df150all$level), "1 SD")==TRUE),]
n500<-df500all[which(startsWith(as.character(df500all$level), "-1 SD")==TRUE| startsWith(as.character(df500all$level), "1 SD")==TRUE),]

lighten <- function(col,
                    pct=0.75,
                    alpha=1){
  if (abs(pct)>1) {
    warning("invalid pct in lighten(); must be between 0 and 1.")
    pcol <- col2rgb(col)/255
  } else {
    col <- col2rgb(col)/255
    if (pct>0) {
      pcol <- col + pct*(1-col)
    } else {
      pcol <- col*pct
    }
  }
  rgb(pcol[1], pcol[2], pcol[3], alpha)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

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

plot.n150<-ggplot() + 
  #Specifies confidence intervals referencing the lower and upper CI values
  geom_ribbon(data=n150, aes(x=focal.seq, ymin = lower, ymax = upper, fill=level), alpha = .25) +
  
  scale_colour_manual(values=rep("black",2)) +
  scale_fill_manual(values=rep(lighten("black"),2)) +
  # 
  #Include fit lines
  geom_line(data = n150, aes(x=focal.seq, y=pe,color=level, linetype = level), size=1) +
  scale_linetype_manual(values = c("dashed","solid")) +
  ylab("Y") +
  xlab("") +
  ylim(-100,200)+
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(-2,-1,0, 1, 2),
                     labels = c("-2 SD","-1 SD", "Mean", "1 SD", "2 SD")) +
  ggtitle("3a: n = 150") +
  theme(
    text = element_text("Helvetica"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
    panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  theme_bw()

plot.n150

plot.n500<-ggplot() + 
  #Specifies confidence intervals referencing the lower and upper CI values
  geom_ribbon(data=n500, aes(x=focal.seq, ymin = lower, ymax = upper, fill=level), alpha = .25) +
  
  scale_colour_manual(values=rep("black",2)) +
  scale_fill_manual(values=rep(lighten("black"),2)) +
  # 
  #Include fit lines
  geom_line(data = n500, aes(x=focal.seq, y=pe,color=level, linetype = level), size=1) +
  scale_linetype_manual(values = c("dashed","solid")) +
  ylab("Y") +
  xlab("") +
  ylim(-100,200)+
  scale_x_continuous(limits = c(-3, 3),
                     breaks = c(-2,-1,0, 1, 2),
                     labels = c("-2 SD","-1 SD", "Mean", "1 SD", "2 SD")) +
  ggtitle("3b: n = 500") +
  theme(
    text = element_text("Helvetica"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
    panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom", legend.direction = "horizontal") +
  theme_bw()

plot.n500

grid.arrange(plot.n150 + theme(text = element_text("Helvetica"),legend.position="none"),
             plot.n500 + theme(text = element_text("Helvetica"),legend.position="none"),
             mylegend1,
             nrow=1, widths = c(8, 8, 3))

