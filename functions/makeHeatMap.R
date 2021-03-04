makeHeatMap <- function(bigTab, geneList){
  library(circlize)

  currTab <- bigTab %>% 
    filter(geneSymbol.x %in% geneList ) %>%
    select(geneSymbol.x, starts_with("Trio_PSD_TMT11_Proteome.."), starts_with("Shank3_PSD_TMT11_Proteome.."), starts_with("Glun2A_PSD_TMT16_Proteome.."))
  
  
  joint.sampleSheet <- makeSampleSheet(currTab %>% select(-c(geneSymbol.x)))
  colnames(currTab) <- c('geneSymbol', paste(joint.sampleSheet$exptId, '_', joint.sampleSheet$cleanId, sep = ""))
  currTab <- currTab %>% relocate(geneSymbol, starts_with("Trio_WT"), starts_with("Trio_Het"), starts_with("Trio_KO"), starts_with("Shank3_WT"), starts_with("Shank3_Het"), starts_with("Shank3_KO"), starts_with("Glun2A_WT"),  starts_with("Glun2A_Het"),  starts_with("Glun2A_KO"))
  
  currTab.tmp <- distinct(currTab, geneSymbol, .keep_all = TRUE)
  currTab.mat <- as.data.frame(currTab.tmp %>% select(-c("geneSymbol")))
  rNames <- currTab.tmp %>% select("geneSymbol")
  rownames(currTab.mat) <- rNames$geneSymbol
  col_fun = colorRamp2(c(-3, 0, 3), c("blue", "white", "red"))
  col_fun(seq(-3, 3))
  
  hMapLev <- paste(c(rep("Trio",3), rep("Shank3", 3), rep("Glun2A", 3)), c("WT", "Het", "KO"), sep="_")
  currTabHmap <- Heatmap(currTab.mat, 
                        row_names_gp = gpar(fontsize = 7), 
                        cluster_columns = FALSE,
                        column_split = factor(str_sub(colnames(currTab.mat), 1, -2), levels=hMapLev),
                        column_title = NULL,
                        col=col_fun,
                        heatmap_legend_param = list(title = NULL)
                        
  )
  
  currTab.av <- data.frame(Trio_WT = currTab.tmp %>% select(starts_with("Trio_WT")) %>% rowMeans(na.rm = TRUE),
                      Trio_Het = currTab.tmp %>% select(starts_with("Trio_Het")) %>% rowMeans(na.rm = TRUE),
                      Trio_KO = currTab.tmp %>% select(starts_with("Trio_KO")) %>% rowMeans(na.rm = TRUE),
                      Shank3_WT = currTab.tmp %>% select(starts_with("Shank3_WT")) %>% rowMeans(na.rm = TRUE),
                      Shank3_Het = currTab.tmp %>% select(starts_with("Shank3_Het")) %>% rowMeans(na.rm = TRUE),
                      Shank3_KO = currTab.tmp %>% select(starts_with("Shank3_KO")) %>% rowMeans(na.rm = TRUE),
                      Glun2A_WT = currTab.tmp %>% select(starts_with("Glun2A_WT")) %>% rowMeans(na.rm = TRUE),
                      Glun2A_Het = currTab.tmp %>% select(starts_with("Glun2A_Het")) %>% rowMeans(na.rm = TRUE),
                      Glun2A_KO = currTab.tmp %>% select(starts_with("Glun2A_KO")) %>% rowMeans(na.rm = TRUE)
  )
  rownames(currTab.av) <- rNames$geneSymbol
  hc <- hclust(dist(currTab.av, method="euclidean"))
  
  
  currTabAvHmap <- Heatmap(currTab.av,
                          row_names_gp = gpar(fontsize = 7), 
                          cluster_columns = FALSE,
                          column_split = factor(c(rep("Trio", 3), rep("Shank3",3), rep("Glun2A", 3)), levels=c("Trio", "Shank3", "Glun2A")),
                          column_title = NULL,
                          cluster_rows = hc,
                          col = col_fun,
                          heatmap_legend_param = list(title = NULL)
  )
  
  
  
  lfcTab <- bigTab %>% filter(geneSymbol.x %in% geneList) %>% select(geneSymbol.x,
                              logFC.Het.over.WT.x, 
                              logFC.KO.over.WT.x, 
                              logFC.Het.over.WT.y, 
                              logFC.KO.over.WT.y, 
                              logFC.Het.over.WT, 
                              logFC.KO.over.WT)
  
  colnames(lfcTab) <- c("geneSymbol", "Trio Het/WT", "Trio KO/WT", "Shank3 Het/WT", "Shank3 KO/WT", "Grin2a Het/WT", "Grin2a KO/WT")
  lfcTab.tmp <- distinct(lfcTab, geneSymbol, .keep_all = TRUE)
  lfcTab.mat <- as.data.frame(lfcTab.tmp %>% select(-c("geneSymbol")))
  rNames <- lfcTab.tmp %>% select("geneSymbol")
  rownames(lfcTab.mat) <- rNames$geneSymbol
  
  
  
  lfc.hc <- hclust(dist(lfcTab.mat, method="euclidean"))
  lfcTabHmap <- Heatmap(lfcTab.mat,
                           row_names_gp = gpar(fontsize = 7), 
                           cluster_columns = FALSE,
                           column_split = factor(c(rep("Trio", 2), rep("Shank3",2), rep("Glun2A", 2)), levels=c("Trio", "Shank3", "Glun2A")),
                           column_title = NULL,
                          col = col_fun,
                           cluster_rows = lfc.hc,
                           heatmap_legend_param = list(title = NULL)
  )
  
  tau <- read_xlsx("~/Google Drive/psp/data/tauTgPsdProteomics.xlsx")
  tau <- tau %>% drop_na('Fold-change')
  tau <- tau %>% mutate(Log2_Fold_Change_Tau_tg = log2(`Fold-change`))
  tau <- tau %>% mutate(padj = -log10(`Adjusted p-value`))
  
  nr <- dim(currTab.av)[1]
  nc <- dim(currTab.av) [2]
  df <- data.frame(matrix(nrow=nr, ncol=1, data = "NA"))
  colnames(df) <- c("Symbol")
  df$Symbol <- rownames(currTab.av)
  tau.curr <- left_join(df, tau, by="Symbol")
  tau.curr <- tau.curr[lfc.hc$order,]
  tau.filt <- data.frame(row.names=tau.curr$Symbol, Lfc_Tau_Tg=tau.curr$Log2_Fold_Change_Tau_tg)
  tau.hm <- Heatmap(tau.filt, cluster_columns = FALSE, 
                    cluster_rows=FALSE, 
                    na_col = "grey30", 
                    row_names_gp = gpar(fontsize = 7), 
                    col = col_fun,
                    heatmap_legend_param = list(title = NULL))
  
  return (c(currTabHmap, currTabAvHmap, tau.hm, lfcTabHmap))
}