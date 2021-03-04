makePcaPlot <- function(abundances, color){
  source("~/Google Drive/psp/functions/setTheme.R")
  pca <- prcomp(t(abundances))
  pca.plot <- autoplot(pca, x=1, y=2,
                       data=t(abundances),
                       loadings=FALSE, loadings.label=FALSE,
                       colour=color, group=color, size=5)

  
  pca.plot <- pca.plot + th
  pca.plot <- pca.plot + geom_text(vjust=-1, 
                                   label=rownames(t(abundances)))
  pca.plot <- pca.plot +  theme(legend.position = "none")
  return(pca.plot)
}
