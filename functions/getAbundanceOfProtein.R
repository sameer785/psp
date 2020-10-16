getAbundanceOfProtein <- function(abTibble, pId, colId, sampleSheet){
  pAb <- abTibble %>% dplyr::filter(id.query==pId) %>% dplyr::select(starts_with(colId))
  pAb.m <- melt(pAb)
  pAb.m$genotype <- sampleSheet$genotype
  return(pAb.m)
}