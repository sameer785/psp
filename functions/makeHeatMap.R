makeHeatMap <- function(bigTab, geneList){
  currTab <- bigTab %>% 
    filter(geneSymbol.x %in% geneList ) %>%
    select(geneSymbol.x, starts_with("Trio_PSD_TMT11_Proteome.."), starts_with("Shank3_PSD_TMT11_Proteome.."))
  
  
  joint.sampleSheet <- makeSampleSheet(currTab %>% select(-c(geneSymbol.x)))
  colnames(currTab) <- c('geneSymbol', paste(joint.sampleSheet$exptId, '_', joint.sampleSheet$cleanId, sep = ""))
  currTab <- currTab %>% relocate(geneSymbol, starts_with("Trio_WT"), starts_with("Trio_Het"), starts_with("Trio_KO"), starts_with("Shank3_WT"), starts_with("Shank3_Het"), starts_with("Shank3_KO"))
  
  currTab.tmp <- distinct(currTab, geneSymbol, .keep_all = TRUE)
  currTab.mat <- as.data.frame(currTab.tmp %>% select(-c("geneSymbol")))
  rNames <- currTab.tmp %>% select("geneSymbol")
  rownames(currTab.mat) <- rNames$geneSymbol
  
  hMapLev <- paste(c(rep("Trio",3), rep("Shank3", 3)), c("WT", "Het", "KO"), sep="_")
  currTabHmap <- Heatmap(currTab.mat, 
                        row_names_gp = gpar(fontsize = 7), 
                        cluster_columns = FALSE,
                        column_split = factor(str_sub(colnames(currTab.mat), 1, -2), levels=hMapLev),
                        column_title = NULL
  )
  
  currTab.av <- data.frame(Trio_WT = currTab.tmp %>% select(starts_with("Trio_WT")) %>% rowMeans(na.rm = TRUE),
                      Trio_Het = currTab.tmp %>% select(starts_with("Trio_Het")) %>% rowMeans(na.rm = TRUE),
                      Trio_KO = currTab.tmp %>% select(starts_with("Trio_KO")) %>% rowMeans(na.rm = TRUE),
                      Shank3_WT = currTab.tmp %>% select(starts_with("Shank3_WT")) %>% rowMeans(na.rm = TRUE),
                      Shank3_Het = currTab.tmp %>% select(starts_with("Shank3_Het")) %>% rowMeans(na.rm = TRUE),
                      Shank3_KO = currTab.tmp %>% select(starts_with("Shank3_KO")) %>% rowMeans(na.rm = TRUE)
  )
  rownames(currTab.av) <- rNames$geneSymbol
  
  currTabAvHmap <- Heatmap(currTab.av,
                          row_names_gp = gpar(fontsize = 7), 
                          cluster_columns = FALSE,
                          column_split = factor(c(rep("Trio", 3), rep("Shank3",3)), levels=c("Trio", "Shank3")),
                          column_title = NULL
  )
  
  
  return (c(currTabHmap, currTabAvHmap))
}