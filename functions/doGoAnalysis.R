doGoAnalysis <- function(tbl, pathway){
  # To Convert mouse gene names to human
  # Save only those whose human and mouse names match
  bm<-read_csv("~/bioChem/ensemblId_mouseGeneName_humanGeneName.csv")
  res <- inner_join(tbl, bm, by=c("geneSymbol"="external_gene_name"))
  
  # Select moderated t-statistic (KO/WT) and Ensembl external gene name
  # For multiple proteins that have same gene name
  # Select row with the largest absolute t-statistic
  psd.res <- res %>% 
    dplyr::select(hsapiens_homolog_associated_gene_name, t.KO.over.WT) %>% 
    na.omit() %>% 
    distinct() %>% 
    group_by(hsapiens_homolog_associated_gene_name) %>% 
    slice_max(abs(t.KO.over.WT))
  
  ranks <- deframe(psd.res)
  pathways.go <- gmtPathways(pathway)
  fgseaRes.psd <- fgsea(pathways=pathways.go, stats=ranks, nperm=1000)
  
  fgseaRes.psd.up <- fgseaRes.psd %>%
    as_tibble() %>%
    arrange(desc(NES), padj) %>%
    dplyr::select(-ES, -nMoreExtreme) %>%
    DT::datatable()
  
  fgseaRes.psd.down <- fgseaRes.psd %>%
    as_tibble() %>%
    arrange(NES, padj) %>%
    dplyr::select(-ES, -nMoreExtreme) %>%
    DT::datatable()
  
  return(list("up"=fgseaRes.psd.up, "down"=fgseaRes.psd.down))
  
}