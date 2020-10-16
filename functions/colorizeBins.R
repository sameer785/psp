colorizeBins <- function(tbl, cl){
  vplot.color <- brewer.pal(9, "YlGn")
  vplot.col2 <- brewer.pal(9, "YlGnBu")
  #vplot.color<-colorRampPalette(brewer.pal(12,"YlOrRd"))(12)
  kvl <- rep('black', nrow(tbl))
  names(kvl) <- rep('Mid', nrow(tbl))
  kvl[which(tbl$lfcBins==6)] <- vplot.col2[9]
  names(kvl)[which(tbl$lfcBins==6)] <- "6"
  kvl[which(tbl$lfcBins==5)] <- vplot.col2[8]
  names(kvl)[which(tbl$lfcBins==5)] <- '5'
  kvl[which(tbl$lfcBins==4)] <- vplot.col2[7]
  names(kvl)[which(tbl$lfcBins==4)] <- '4'
  kvl[which(tbl$lfcBins==3)] <- vplot.color[4]
  names(kvl)[which(tbl$lfcBins==3)] <- '3'
  kvl[which(tbl$lfcBins==2)] <- vplot.color[3]
  names(kvl)[which(tbl$lfcBins==2)] <- '2'
  kvl[which(tbl$lfcBins==1)] <- vplot.color[2]
  names(kvl)[which(tbl$lfcBins==1)] <- '1'
  
  return(kvl)
}