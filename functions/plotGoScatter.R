plotGoScatter <- function(dat, lab){
  source("~/Google Drive/psp/functions/setTheme.R")
  linSiz <- 0.25
  
  gp <- ggplot(dat, aes(x=X, y=Y, colour=as.factor(color), shape=as.factor(color), fill=as.factor(color), size=as.factor(color), alpha=as.factor(color)))
  gp <- gp + th
  gp <- gp + geom_point()
  gp <- gp + geom_abline(slope=1, intercept = 0, linetype = "dashed", size=linSiz)
  gp <- gp + scale_size_manual(values=c(3, 3, 3, 3), labels=lab)
  gp <- gp + scale_alpha_manual(values=c(.1, 1,1,1), labels=lab)
  gp <- gp + scale_color_manual(values=c('gray60', 'black', 'black', 'black'), labels=lab)
  gp <- gp + scale_fill_manual(values=c('gray60', 'orange1', 'slategray2', 'orchid'), labels=lab)
  gp <- gp + scale_shape_manual(values = c(16, 21, 21, 23), labels=lab)
  gp <- gp + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  gp <- gp + theme(legend.position = c(0.15,0.9))
  gp <- gp + geom_text_repel(aes(label=ifelse(color==3 & (X>0 | Y>0), as.character(str_to_title(substring(pathway,4))), '')),
                             show.legend = FALSE,
                             segment.color='black',
                             segment.size = .5,
                             segment.alpha = .25, size=3)
  # gp <- gp + geom_text_repel(aes(label=ifelse(color==3 & (X < -3 | Y < -2.5 ), as.character(pathway), '')), 
  #                            show.legend = FALSE, 
  #                            segment.color='black', 
  #                            segment.size = .5, 
  #                            segment.alpha = .25, size=3)
  return(gp)
}