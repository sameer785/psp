makeScatterPlot <- function(dat, lab){
  axisLabSize = 18
  legendLabSize = 18
  axisLineSize = .8
  xFcLim = 2
  yFcLim = 2
  
  th <- theme(
      # axis text
      axis.text.x = element_text(angle = 0, size = axisLabSize, vjust = 1),
      axis.text.y = element_text(angle = 0, size = axisLabSize, vjust = 1),
      axis.title = element_text(size = axisLabSize),
      axis.line = element_line(size = axisLineSize),
      
      # legend
      legend.background = element_rect(),
      legend.key = element_blank(),
      legend.text = element_text(size = legendLabSize),
      title = element_text(size = legendLabSize),
      legend.title = element_blank(),
      
      #panel
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
      )
  linSiz <- 0.25
  gp <- ggplot(dat, aes(x=x, y=y, colour=as.factor(color), shape=as.factor(color), fill=as.factor(color), size=as.factor(color), alpha=as.factor(color)))
  gp <- gp + th
  gp <- gp + geom_point()
  gp <- gp + geom_hline(yintercept = 0, linetype = "dashed", size=linSiz)
  gp <- gp + geom_vline(xintercept = 0, linetype = "dashed", size=linSiz)
  #
  gp <- gp + geom_hline(yintercept = yFcLim, linetype = "dotted", size=linSiz)
  gp <- gp + geom_hline(yintercept = -yFcLim, linetype = "dotted", size=linSiz)
  #
  gp <- gp + geom_vline(xintercept = xFcLim, linetype = "dotted", size=linSiz)
  gp <- gp + geom_vline(xintercept = -xFcLim, linetype = "dotted", size=linSiz)

  gp <- gp + geom_abline(slope=1, intercept = 0, linetype = "dashed", size=linSiz)
  gp <- gp + scale_size_manual(values=c(3, 3, 3, 3), labels=lab)
  gp <- gp + scale_alpha_manual(values=c(.1, 1,1,1), labels=lab)
  gp <- gp + scale_color_manual(values=c('gray60', 'black', 'black', 'black'), labels=lab)
  gp <- gp + scale_fill_manual(values=c('gray60', 'orange1', 'slategray2', 'orchid'), labels=lab)
  gp <- gp + scale_shape_manual(values = c(16, 21, 21, 23), labels=lab)
  gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + theme(legend.position = c(0.75,0.15))
  return(gp)
}