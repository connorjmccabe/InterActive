#InterActive paper Sample plots
require(ggplot2)
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
# A different data set
df1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

# A basic graph
lp <- ggplot(data=df1, aes(x=time, y=total_bill, group=sex, linetype=sex)) + geom_line(size=1) + theme_bw() + theme(legend.position = c(.53, 0.8), legend.direction = "horizontal")

# Change the legend
lp2<-lp + scale_linetype_discrete(name = element_blank(),
                          breaks=c("Male", "Female"),
                          labels=c("Low Z", "High Z"))


mylegend<-g_legend(lp2)

#Figure 1
#SEE sampleplots.xls FOR MORE DETAIL

baseplot.ord<-ggplot() + 
  ylim(0,101) +
  ylab("Y") +
  xlab("") +
  scale_x_continuous(limits = c(-1.5, 1.5),
                     breaks = c(-1, 1),
                     labels = c("Low X","High X")) +
  theme(
        text = element_text("Helvetica"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
        panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom") +
  # geom_vline(xintercept = -2, linetype = "dashed", size = .5) +
  # geom_vline(xintercept = -1, linetype = "dashed", size = .5) +
  # geom_vline(xintercept = 1, linetype = "dashed", size = .5) +
  # geom_vline(xintercept = 2, linetype = "dashed", size= .5) +
  theme_bw()

baseplot.disord<-ggplot() + 
  ylim(0,101) +
  ylab("Y") +
  xlab("") +
  scale_x_continuous(limits = c(-2.5, 2.5),
                     breaks = c(-2, 2),
                     labels = c("Low X","High X")) +
  theme(
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="#CDCDCD",size=0.25),
        panel.background = element_rect(fill = "transparent",colour = "black")) +
  theme(legend.position="bottom") +
  geom_vline(xintercept = -2, linetype = "dashed", size = .5) +
  geom_vline(xintercept = -1, linetype = "dashed", size = .5) +
  geom_vline(xintercept = 1, linetype = "dashed", size = .5) +
  geom_vline(xintercept = 2, linetype = "dashed", size = .5) +
  theme_bw()


# Ordinal plots -----------------------------------------------------------

  synplot<-baseplot.ord + 
  #Z high
    geom_segment(aes(x = -1, y = 25,
          xend = 1, yend = 76),
      linetype = "solid", size = 1) +
  #Z low
  geom_segment(aes(x = -1, y = 24,
                   xend = 1, yend = 34),
               linetype = "dashed", size = 1) +
  theme(
    text = element_text("Helvetica")) +
    ggtitle("1a: Synergistic")

#Y = (25 - 10*Z) + (15 + 5*Z)*X
  buffplot<-baseplot.ord + 
    geom_segment(aes(x = -1, y = 25,
                     xend = 1, yend = 25),
                 linetype = "solid", size = 1) +
    geom_segment(aes(x = -1, y = 31,
                     xend = 1, yend = 79),
                 linetype = "dashed", size = 1) +
    theme(
      text = element_text("Helvetica")) +
    ggtitle("1b: Buffering")
  
  antplot<-baseplot.ord + 
    geom_segment(aes(x = -1, y = 72,
                     xend = 1, yend = 68),
                 linetype = "solid", size = 1) +
    geom_segment(aes(x = -1, y = 28,
                     xend = 1, yend = 72),
                 linetype = "dashed", size = 1) +
    theme(
      text = element_text("Helvetica")) +
    ggtitle("1c: Antagonistic")
  
  # crossoverplot<-baseplot.ord + 
  #   geom_segment(aes(x = -1, y = 75,
  #                    xend = 1, yend = 45),
  #                linetype = "solid", size = 1) +
  #   geom_segment(aes(x = -1, y = 30,
  #                    xend = 1, yend = 80),
  #                linetype = "dashed", size = 1) +
  #   ggtitle("1d. Crossover")
  

  # Disrdinal plots -----------------------------------------------------------
  synplot.disord<-baseplot.disord + 
    geom_segment(aes(x = -2, y = 1,
                     xend = 2, yend = 101),
                 linetype = "solid", size = 1) +
    geom_segment(aes(x = -2, y = 19,
                     xend = 2, yend = 39),
                 linetype = "dashed", size = 1) +
    ggtitle("1d. Synergistic")
  
  buffplot.disord<-baseplot.disord + 
    geom_segment(aes(x = -2, y = 25,
                     xend = 2, yend = 25),
                 linetype = "solid", size = 1) +
    geom_segment(aes(x = -2, y = 7,
                     xend = 2, yend = 91),
                 linetype = "dashed", size = 1) +
    ggtitle("1e. Buffering")
  
  antplot.disord<-baseplot.disord + 
    geom_segment(aes(x = -2, y = 74,
                     xend = 2, yend = 66),
                 linetype = "solid", size = 1) +
    geom_segment(aes(x = -2, y = 6,
                     xend = 2, yend = 94),
                 linetype = "dashed", size = 1) +
    ggtitle("1f. Antagonistic")

require(gridExtra)
g1<-grid.arrange(arrangeGrob(synplot,
                         buffplot,
                         antplot,
                         nrow=1),
                 mylegend, nrow=2,heights=c(10, 1))

ggsave("fig1.pdf",g1,width = 7, height = 4, units = "in")