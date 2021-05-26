doGoAnalysisHuman <- function(tbl, pathway, tStat){
  library(lazyeval)
  tStat <- as.name(tStat)
  # To Convert mouse gene names to human
  # Save only those whose human and mouse names match
  res <- tbl
  
  # Select moderated t-statistic (KO/WT) and Ensembl external gene name
  # For multiple proteins that have same gene name
  # Select row with the largest absolute t-statistic
  psd.res <- res %>% 
    dplyr::select(geneSymbol, tStat) %>% 
    na.omit() %>% 
    distinct() %>%
    rename(tStatistic=tStat) %>% 
    group_by(geneSymbol) %>%
    slice_max(abs(tStatistic))
  
  ranks <- deframe(psd.res)
  pathways.go <- gmtPathways(pathway)
  fgseaRes.psd <- fgsea(pathways=pathways.go, stats=ranks)
  
  fgseaRes.psd <- fgseaRes.psd %>%
    as_tibble() %>%
    arrange(padj, desc(NES))
  
  return (fgseaRes.psd)
  
}