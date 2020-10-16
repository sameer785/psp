makeStratifiedCumLfcPlot <- function(tbl){
  linSiz = .75
  axisLabSize = 18
  legendLabSize = 18
  
  th <- theme_bw(base_size = 24) +
    
    theme(
      legend.background = element_rect(),
      
      # axis text
      axis.text.x = element_text(
        angle = 0,
        size = axisLabSize,
        vjust = 1),
      axis.text.y = element_text(
        angle = 0,
        size = axisLabSize,
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
  
  sa <- data.frame(x=tbl$logFC.KO.over.WT, g=tbl$lfcBins)
  gp <- ggplot(sa, aes(x, colour=g))
  gp <- gp + th
  gp <- gp + stat_ecdf(size=linSiz)
  gp <- gp + geom_hline(yintercept=0.5, linetype="dashed")
  gp <- gp + geom_vline(xintercept=0, linetype="dashed")
  gp <- gp + theme(legend.text=element_text(size=18))
  gp <- gp + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #cumPlotCols <- c("#377EB8", "#FAAA33", "#4DAF4A", "#FF7F00", "#E41A1C", "#984EA3")
  #cumPlotCols <- c("#78CDFF", "#6193E8", "#6B7BFF", "#FF14F7", "#FF4935", "#80251B")
  cumPlotCols <- c("#9421FF", "#7C94FF", "#91FF10", "#36FF06", "#FF701D", "#FF1132")
  gp <- gp + scale_colour_manual(values = cumPlotCols)
  gp <- gp + xlab(bquote(~Log[2]~ "fold change"))
  gp <- gp + ylab("Cumulative probability")
  gp <- gp + theme(legend.title=element_blank())
  gp <- gp + theme(legend.position = c(0.95,0.25))
  #gp <- gp + coord_cartesian(xlim=c(-.5,.5))
  gp <- gp + theme(axis.line = element_line(size = .8), panel.border = element_blank(), panel.background = element_blank())
  gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks(n=10))#breaks=c(-.7,-.5,0,.5,.7)
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  gp
  
  
  
}