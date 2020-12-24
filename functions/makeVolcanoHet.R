makeVolcanoHet <- function(tbl, selLab, cAlph, colCust, legPos, qt){
  if(missing(selLab)){
    selLab <- NULL
  }
  if(missing(cAlph)){
    cAlph <- 0.5
  }
  if(missing(colCust)){
    colCust <- NULL
  }
  if(missing(legPos)){
    legPos <- 'none'
  }
  if(qt==2){
    qtx <- "logFC.Het.over.WT"
    qty <- "P.Value.WT.over.Het"
  }
  if(qt==3){
    qtx <- "logFC.KO.over.Het"
    qty <- "P.Value.KO.over.Het"
  }
  
  vPlot <- EnhancedVolcano(tbl,
                           x=qtx,
                           y=qty,
                           lab=tbl$geneSymbol,
                           ylim=c(0,-log10(10e-5)),
                           FCcutoff = 2.5, 
                           gridlines.major = FALSE, 
                           gridlines.minor = FALSE,
                           border = 'partial', 
                           legendPosition = legPos,
                           selectLab = selLab,
                           colAlpha = cAlph,
                           colCustom = colCust,
                           borderWidth = .8,  
                           title = element_blank(), 
                           subtitle = element_blank(),
                           caption = element_blank(),
                           drawConnectors = TRUE, 
                           colConnectors = 'grey50',
                           pCutoff = 10e-3,
                           lengthConnectors = unit(0.01,'npc'),
                           widthConnectors=0.3,
                           typeConnectors = "open"
  )
  vPlot <- vPlot + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +  
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  return(vPlot)
}