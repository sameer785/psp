makeAbundancePlot <- function(abTibble.m){
source("~/Google Drive/psp/functions/setTheme.R")
  gp <- ggplot(data=abTibble.m, aes(x=genotype, y=value, group=genotype))
  gp <- gp + th
  gp <- gp + geom_quasirandom(size=4, width=.1, aes(fill=genotype), stroke=.1, color='black', pch=21, show.legend = FALSE)
  gp <- gp + scale_fill_manual(values =alpha(rainbow(3), 0.1))
  gp <- gp + stat_summary(fun.data = mean_se, geom="errorbar", alpha=1, size=1.25, width=0.1, show.legend = FALSE)
  gp <- gp + stat_summary(fun=mean, geom="point", stroke=1, color='black', alpha=1, show.legend = FALSE, size=4)
  gp <- gp + scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
  gp <- gp + ylab("Normalized abundance")
  gp <- gp + xlab("")
  gp <- gp + theme(legend.position = c(0.5,.95))
  gp
  
  return(gp)
}
