makeVolcanoPlot <- function(tbl, xVar, yVar, selLab, cAlph, colCust, legPos){
  if(missing(selLab)){
    selLab <- NULL
  }
  if(missing(cAlph)){
    cAlph <- 0.6
  }
  if(missing(colCust)){
    colCust <- NULL
  }
  if(missing(legPos)){
    legPos <- 'none'
  }
  if(missing(xVar)){
    xVar <- "logFC.KO.over.WT"
  }
  if(missing(yVar)){
    yVar <- "P.Value.WT.over.KO"
  }
  
  vPlot <- EnhancedVolcano(tbl,
                           x=xVar,
                           y=yVar,
                           lab=tbl$geneSymbol,
                           FCcutoff = 2, 
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
                           pCutoff = 10e-2,
                           lengthConnectors = unit(0.01,'npc'),
                           widthConnectors=0.3,
                           typeConnectors = "open",
                           labSize = 4.0,
                           pointSize = 3
  )
  vPlot <- vPlot + scale_x_continuous(breaks = scales::pretty_breaks(n=10))
  vPlot <- vPlot + scale_y_continuous(breaks = scales::pretty_breaks(n=10))

  return(vPlot)
}
