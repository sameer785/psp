# Make sample sheet based on column names of individual abundances

makeSampleSheet <- function(abTibble){
  cNames <- colnames(abTibble)
  sampleId <- vector("list", length(cNames))
  exptId <- vector("list", length(cNames))
  i <- 1
  for (item in cNames){
    conditionSplitList <- strsplit(item, ".", fixed=TRUE)
    unlisted <- unlist(conditionSplitList)
    sampleId[[i]] <- tail(unlisted, n=1)
    exptId[[i]] <- unlist(strsplit(unlisted[1], "_", fixed=TRUE))[1]
    i <- i + 1
  }
  
  sampleId <- unlist(sampleId)
  exptId <- unlist(exptId)
  genotypes <- factor(removeNumbers(sampleId), levels=c("WT", "Het", "KO")) ####Hard coded factor levels
  sampleSheet <- data.frame(id= as.factor(cNames), 
                                exptId=exptId,
                                 cleanId= sampleId, 
                                 genotype= genotypes,
                                  color=as.numeric(genotypes)
                                 )
  return (sampleSheet)
}
