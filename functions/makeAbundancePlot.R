makeAbundancePlot <- function(abTibble.m){
  axisLabSize = 18
  legendLabSize = 18
  th <- theme_bw(base_size = 24) +
    
    theme(
      legend.background = element_rect(),
      
      # axis text
      axis.text.x = element_text(
        angle = 0,
        size = 18,
        vjust = 1),
      axis.text.y = element_text(
        angle = 0,
        size = 18,
        vjust = 1),
      axis.title = element_text(
        size = axisLabSize),
      
      # legend
      legend.key = element_blank(),
      legend.text = element_text(
        size = legendLabSize),
      title = element_text(
        size = legendLabSize),
      legend.title = element_blank())
  
  gp <- ggplot(data=abTibble.m, aes(x=genotype, y=value, group=genotype))
  gp <- gp + th
  gp <- gp + geom_quasirandom(size=4, width=.1, aes(fill=genotype), stroke=.75, color='black', pch=21, show.legend = TRUE)
  gp <- gp + scale_fill_manual(values =alpha(rainbow(3), 0.1))
  gp <- gp + stat_summary(fun.data = mean_se, geom="errorbar", alpha=1, size=1.25, width=0.1, show.legend = FALSE)
  gp <- gp + stat_summary(fun=mean, geom="point", aes(shape=genotype), stroke=1, color='black', alpha=1, show.legend = FALSE, size=4)
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
  gp <- gp + ylab("Normalized abundance")
  gp <- gp + xlab("")
  gp <- gp + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  gp <- gp + theme(axis.line = element_line(size = .8), panel.border = element_blank(), panel.background = element_blank())
  gp <- gp + theme(axis.text.y=element_text(angle=0, hjust=1), axis.text.x=element_text(angle=30, hjust=1))
  gp <- gp + theme(legend.position = c(0.5,.95))
  gp <- gp + theme(legend.direction = "horizontal")
  #gp <- gp + guides(fill=guide_legend(nrow=2))
  gp <- gp + theme(legend.box.background = element_rect(colour = "black", size=1), legend.margin = margin(.5,.5,.5,.5))
  gp <- gp + theme(axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0)))
  gp
  
  return(gp)
}