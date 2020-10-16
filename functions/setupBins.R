setupBins <- function(tbl, cl, brks){
  resHist <- hist(as.matrix(tbl[,cl]), breaks=brks, plot = FALSE)
  lfcBins <- cut(as.matrix(tbl[,cl]), breaks=resHist$breaks, 
                 include.lowest = TRUE, labels=c(1:6))
  tbl$lfcBins <- lfcBins
  return(tbl)
}