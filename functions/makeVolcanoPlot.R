makeVolcanoPlot <- function(tbl, selLab, cAlph, colCust, legPos){
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
  
  vPlot <- EnhancedVolcano(tbl,
                           x="logFC.KO.over.WT",
                           y="P.Value.WT.over.KO",
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
  return(vPlot)
}