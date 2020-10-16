# Make sample sheet based on column names of individual abundances

makeSampleSheet <- function(abTibble){
  cNames <- colnames(abTibble)
  conditionSplitList <- strsplit(cNames, ".", fixed=TRUE)
  sampleId <- unlist(lapply(conditionSplitList, '[[', 16))
  genotypes <- factor(removeNumbers(sampleId), levels=c("WT", "Het", "KO")) ####Hard coded factor levels
  sampleSheet <- data.frame(id= as.factor(cNames), 
                                 cleanId= sampleId, 
                                 genotype= genotypes,
                                  color=as.numeric(genotypes)
                                 )
  return (sampleSheet)
}
