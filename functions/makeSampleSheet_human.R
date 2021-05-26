# Make sample sheet based on column names of individual abundances

makeSampleSheetHuman <- function(abTibble){
  cNames <- colnames(abTibble)
  sampleId <- vector("list", length(cNames))
  exptId <- vector("list", length(cNames))
  i <- 1
  for (item in cNames){
    conditionSplitList <- strsplit(item, ".", fixed=TRUE)
    unlisted <- unlist(conditionSplitList)
    sampleId[[i]] <- unlisted[6]
    exptId[[i]] <- unlist(strsplit(unlisted[1], "_", fixed=TRUE))[1]
    i <- i + 1
  }
  
  sampleId <- unlist(sampleId)
  exptId <- unlist(exptId)
  condition <- factor(sub("_", "", removeNumbers(sampleId)), levels=c("B", "A", "C")) ####Hard coded factor levels
  genotype <- plyr::revalue(condition, c("A"="BP", "B"="CTRL", "C"="SCZ"))
  sampleSheet <- data.frame(id= as.factor(cNames), 
                            exptId=exptId,
                            cleanId= sampleId, 
                            genotype= genotype,
                            color=as.numeric(genotype),
                            condition = condition
  )
  return (sampleSheet)
}
