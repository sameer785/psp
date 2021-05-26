makePcaPlot <- function(abundances, color){
  source("~/Google Drive/psp/functions/setTheme.R")
  library(ggrepel)
  set.seed(42)
  
  pca <- prcomp(t(abundances))
  pca.plot <- autoplot(pca, x=1, y=2,
                       data=t(abundances),
                       loadings=FALSE, loadings.label=FALSE,
                       colour=color, group=color, size=5)

  
  pca.plot <- pca.plot + th
  pca.plot <- pca.plot + geom_text_repel(vjust=-1, 
                                   label=rownames(t(abundances)), segment.alpha=0.1, alpha=0.1)
  pca.plot <- pca.plot +  theme(legend.position = "none")
  return(pca.plot)
}
