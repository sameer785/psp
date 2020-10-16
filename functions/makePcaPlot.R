makePcaPlot <- function(abundances, color){
  pca <- prcomp(t(abundances))
  pca.plot <- autoplot(pca, x=1, y=2, 
                       data=t(abundances), 
                       loadings=FALSE, loadings.label=FALSE,
                       colour=color)
  
  pca.plot <- pca.plot + geom_text(vjust=-1, 
                                   label=rownames(t(abundances)))
  return(pca.plot)
}
