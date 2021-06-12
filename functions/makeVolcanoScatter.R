makeVolcanoScatter <- function(dat, lab){
  if (length(lab)==4){
    colVal <- c('gray10', 'black', 'black', 'black')
    fillVal <- c('gray60', 'slategray2', 'orange1', 'orchid')
  }
  if (length(lab)==3){
    colVal <- c('black', 'black', 'black')
    fillVal <- c('slategray2', 'orange1', 'orchid')
  }
  if (length(lab)==2){
    colVal <- c('black', 'black')
    fillVal <- c('gray60', 'orange1')
  }
  if (length(lab)==1){
    colVal <- c('black')
    fillVal <- c('orange1')
  }
  
  axisLabSize = 18
  legendLabSize = 18
  axisLineSize = .8
  xFcLim = 2.0
  yFcLim = 2.0
  
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
  pointSize <- 5
  bgDat <- dat %>% filter(color == 0)
  smallDat <- dat %>% filter(color != 0)
  # colour=as.factor(color), shape=as.factor(color), fill=as.factor(color), size=as.factor(color), alpha=as.factor(color))
  #color=='gray60', fill='gray60', size=pointSize, alpha=.1
  gp <- ggplot(smallDat, aes(x=x, y=y))
  gp <- gp + th
  gp <- gp + geom_point(data=bgDat, color="gray10", shape=21, fill="gray60", size=pointSize, alpha=.1)
  gp <- gp + geom_point(aes(x=x,y=y,colour=as.factor(color), shape=as.factor(color), fill=as.factor(color), size=as.factor(color), alpha=as.factor(color)))
  gp <- gp + geom_hline(yintercept = 1, linetype = "dashed", size=linSiz)
  gp <- gp + geom_vline(xintercept = 0, linetype = "dashed", size=linSiz)
  #
  #gp <- gp + geom_hline(yintercept = yFcLim, linetype = "dotted", size=linSiz)
  #gp <- gp + geom_hline(yintercept = -yFcLim, linetype = "dotted", size=linSiz)
  #
  gp <- gp + geom_vline(xintercept = xFcLim, linetype = "dotted", size=linSiz)
  gp <- gp + geom_vline(xintercept = -xFcLim, linetype = "dotted", size=linSiz)
  
  gp <- gp + scale_size_manual(values=c(5, 5, 5, 5), labels=lab)
  gp <- gp + scale_alpha_manual(values=c(1, 1,1,1), labels=lab)
  gp <- gp + scale_color_manual(values=colVal, labels=lab)
  gp <- gp + scale_fill_manual(values=fillVal, labels=lab)
  gp <- gp + scale_shape_manual(values = c(21, 21, 21, 23), labels=lab)
  gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(-5,5))
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(0,2))
  gp <- gp + theme(legend.position = "none")
  return(gp)
}