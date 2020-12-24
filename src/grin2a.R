---
  title: "Trio PSD proteome analysis"
output:
  html_document: default
html_notebook: default
pdf_document: default
---
  
  Sameer Aryal 

Postdoctoral Associate

Sheng Laboratory

Broad Institute of MIT and Harvard

Oct. 9, 2020

How does genetically removing Trio alter the postsynaptic density (PSD) proteome?
  Examining TMT labeled PSD proteome from WT, Trio/+, and Trio/Trio mice. 
Experiment carried out by Borislav Dejanovic. 

```{r, message=FALSE}
rm(list=ls())
require(readxl)
require(tidyverse)
require(tm)
require(ggfortify)
require(reshape2)
require(ggbeeswarm)
require(EnhancedVolcano)
require(RColorBrewer)
require(fgsea)
require(ComplexHeatmap)
```
Loading data and helper functions. 

```{r}
files.sources = list.files("../functions" , full.names = TRUE)
invisible(lapply(files.sources, source))
trio.xls <- read_xlsx("../data/results_PSD-Trio-proteome_Two-sample_mod_T_2020-07-24.xlsx")
trio.xls <- trio.xls %>% drop_na(logFC.WT.over.KO)
```
Does the data have any underlying structure?
  
  ```{r}
trio.abundances <- trio.xls %>% dplyr::select(starts_with("Trio_PSD_TMT11_Proteome.."))

#Renaming columns for clarity
trio.sampleSheet <- makeSampleSheet(trio.abundances)
colnames(trio.abundances) <- trio.sampleSheet$cleanId


# Remove any rows with NA
trio.abundances.clean <- na.omit(trio.abundances)
trio.abundances.pca.plot <- makePcaPlot(trio.abundances.clean, trio.sampleSheet$color)
trio.abundances.pca.plot

```



Does log transformation reveal any underlying structure?
  
  ```{r}
###
trio.abundances.clean.log <- log(trio.abundances.clean+abs(min(trio.abundances.clean))+1)
trio.abundances.clean.log.pca.plot <- makePcaPlot(trio.abundances.clean.log, trio.sampleSheet$color)
trio.abundances.clean.log.pca.plot

# No inherent structure in the data
# Samples do not vary primarily by genotype
```



How does expresssion of Trio vary with genotype?
  ```{r}
# Check abundance of Trio protein
trio.trio.abundance.m <- getAbundanceOfProtein(trio.xls, 
                                               'Q0KL02',
                                               "Trio_PSD_TMT11_Proteome..", 
                                               trio.sampleSheet)
trio.trio.abundance.plot <- makeAbundancePlot(trio.trio.abundance.m)
trio.trio.abundance.plot
```

The Broad modeling (moderated t-test followed by FDR adjustment) finds that no genes, including Trio, are significantly altered after FDR adjustment of p-values. The following plot shows the distribution of FDR adjusted p-values. 

```{r}
plot(density(trio.xls$adj.P.Val.WT.over.KO), xlim=c(0,1))
```
I am therefore using nominal p-values for further analysis of the data. Here is how the nominal p-value distribution looks like.

```{r}
plot(density(trio.xls$P.Value.WT.over.KO), xlim=c(0,1))
```

Since we are using nominal p-values, the chances of getting false positive hits is quite high. The number of proteins with p-values below 0.05 is:
  ```{r}
sum(trio.xls$P.Value.WT.over.KO < 0.05)
```



Which proteins are the most consistently altered across replicates? Plotting LFC(KO/WT) against nominal p-value.

```{r}
#Plotting FDR adjusted p value against LFC
trio.xls <- trio.xls %>% dplyr::mutate(logFC.KO.over.WT = logFC.WT.over.KO*(-1))
vPlot <- makeVolcanoPlot(trio.xls)
vPlot
ggsave(filename = "~/Google Drive/psp/analysis/plots/trio/volcano_2fc_3p.pdf", width = 8, height = 6, units = "in")
```

```{r}
large <- trio.xls %>% filter(logFC.KO.over.WT > 2 & P.Value.WT.over.KO > 10e-3) %>% select(geneSymbol)
small <- trio.xls %>% filter(logFC.KO.over.WT < -2 & P.Value.WT.over.KO > 10e-3) %>% select(geneSymbol)
allGenes <- rbind(large,small)
vPlot <- makeVolcanoPlot(trio.xls, allGenes$geneSymbol)
vPlot
ggsave(filename = "~/Google Drive/psp/analysis/plots/trio/volcano_2fc.pdf", width = 8, height = 6, units = "in")
```



Labeling the genes that Borislav has picked out for further confirmation and has validated antibodies for. 
```{r}
borislavLabels <- c("Trio", "Grin1", "Nrcam", "Dlgap1", "Clu", "Prrt1", "Zdhhc5", "Nono", "Cers6", "Uba52", "Rpl29")
vPlot2 <- makeVolcanoPlot(trio.xls, borislavLabels)
vPlot2
```


Does the MW of the protein dictate its LFC? To examine this, I divided all proteins into 6 color coded bins. The bins are unequal in size, so that I can see any representative effects at extremes. Bin 1 harbors the proteins with the smallest MW, bin 6 the largest MWs. 
```{r}
trio.xls <- trio.xls %>% dplyr::mutate(log_protein_mw = log(protein_mw))
trio.xls <- setupBins(trio.xls, "log_protein_mw", c(0,9.5,10.25,11.25,12,12.5,16))
keyvals <- colorizeBins(trio.xls)
vPlot3 <- makeVolcanoPlot(trio.xls, NULL, 0.8, keyvals, c(0.1,0.9))
vPlot3
```


It does not look like the MW is associated with LFC. To examine this more systematically, I am using the same bins and plotting the cumulative frequency vs. LFC in each bin. 
```{r}
#Stratify cumulative frequency by protein MW
lfcCumFreqPlot <- makeStratifiedCumLfcPlot(trio.xls)
lfcCumFreqPlot
```


In each bin, proteins are as likely to upregulated as they are to be downregulated. Therefore, the MW of a protein does not dictate its LFC. Neither large nor small proteins aren't especially overabundant in Trio KO mice. 


MW is related but not wholly representative of the number of amino acids in a protein. Are the alterations in protein expression at the PSD with deletion of Trio associated with the number of amino acids in the protein (the protein length)?
```{r}
#Stratify by CDS length
geneLengths <- read.csv("~/Google Drive/geneSets/geneLengths/S3.genes.results", sep="\t", header=T)
geneLengths <- geneLengths[,c(1:3)]
resLen <- inner_join(trio.xls, geneLengths, by=c("geneSymbol"="gene_id"))
resLen$aaSize <- resLen$length/3
resLen$logAAsize <- log2(resLen$aaSize)
##### CDS length distribution is shifted left in the PSD compared to tissue RF. Why??
resLen <- setupBins(resLen, "logAAsize", c(0,6.5,7.5,9,10.5,12,16))
keyvals <- colorizeBins(resLen)
vPlot4 <- makeVolcanoPlot(resLen, NULL, 0.8, keyvals, c(0.1,0.9))
vPlot4
```


It does not look like LFC is dependent on protein length either. Analysing this more systematically now. 
```{r}
# Lfcbins has changed now. Stratifying LFC cum freq w/ CDS length bins. 
lfcCumFreqPlot <- makeStratifiedCumLfcPlot(resLen)
lfcCumFreqPlot
```
We can conclude that protein length is not associated with alteration in protein expression. 

Carrying out Gene Set Enrichment Analysis now. 
```{r}
# Gene ontology analysis
trio.xls <- trio.xls %>% mutate(t.KO.over.WT = t.WT.over.KO*(-1))
goPathways <- "~/Google Drive/geneSets/mSigDb/v7_2/ontology/c5.all.v7.2.symbols.gmt"
trio.go <- doGoAnalysis(trio.xls, goPathways)
trio.go$up
trio.go$down
```
```{r}
goPathways2 <- "~/Google Drive/geneSets/mSigDb/v7_2/curated/c2.cp.kegg.v7.2.symbols.gmt"
trio.go.kegg <- doGoAnalysis(trio.xls, goPathways2)
trio.go.kegg$up
trio.go.kegg$down
```

Plotting genes associated with RNA splicing GO
```{r}
rnaProcessing <- read.csv("~/Google Drive/psp/analysis/goLists/trio/rnaSplicing.csv", header=1, row.names = 1)
rnaProcessing <- rnaProcessing$x
rnaProcessing.sample <- sample(rnaProcessing, size = min(length(rnaProcessing), 30))
vPlot5 <- makeVolcanoPlot(trio.xls, rnaProcessing.sample)
vPlot5
```


Plotting genes associated with mitochondrial inner membrane GO

```{r}
mito <- str_to_title(trio.go$down$x$data$leadingEdge[[6]])
mito.sample <- sample(mito, size = min(length(mito), 30))
vPlot6 <- makeVolcanoPlot(trio.xls, mito.sample)
vPlot6
```


Plotting genes associated with synapse assembly GO
```{r}
syn <- read.csv("~/Google Drive/psp/analysis/goLists/trio/synapseAssembly.csv", header = 1, row.names = 1)
syn <- syn$x
syn.sample <- sample(syn, size =  min(length(syn), 30))
vPlot7 <- makeVolcanoPlot(trio.xls, syn.sample)
vPlot7
```

Plotting genes associated with chromatin binding GO
```{r}
chromProcessing <- read.csv("~/Google Drive/psp/analysis/goLists/trio/chromatinBinding.csv", header=1, row.names = 1)
chromProcessing <- chromProcessing$x
chromProcessing.sample <- sample(chromProcessing, size =  min(length(chromProcessing), 30))
vPlot8 <- makeVolcanoPlot(trio.xls, chromProcessing.sample)
vPlot8
```
How do the alterations in Trio/+ PSDs compare with the Trio/Trio mice? Looking first at alterations in Het PSD. 
```{r}
plot(density(trio.xls$adj.P.Val.WT.over.Het), xlim=c(0,1.2))
plot(density(trio.xls$P.Value.WT.over.Het), xlim=c(0,1.2))
dim(trio.xls %>% filter(P.Value.WT.over.Het < 0.05))[1]
```

Like with KO Trio/Trio PSD, no proteins have FDR adjusted p-value of 0.99.Proceeding with nominal p-values, which look similarly distributed to those in KO. 109 proteins less than 0.05. 

Now looking at LFC against nominal p-value. 

```{r}
trio.xls <- trio.xls %>% mutate(logFC.Het.over.WT = logFC.WT.over.Het*(-1))
makeVolcanoHet(trio.xls, , , , , 2)
```

Labeling proteins that Borislav has antibodies for. 

```{r}
makeVolcanoHet(trio.xls, borislavLabels, , , ,2)
```
While the changes mostly appear to be in the same directions as those in the KO, they are not always as robust (as indicated by LFC and p-value pairing).  

Carrying out GO analysis now on genes ranked by t statistic of (Het/WT). 

```{r}
# Gene ontology analysis
trio.xls <- trio.xls %>% mutate(t.Het.over.WT = t.WT.over.Het*(-1))
trio.het2.go <- doGoAnalysisHet(trio.xls, goPathways)
trio.het2.go$up
trio.het2.go$down
```
There is Keratin contamination in the Het samples. 

GOs associated with splicing, rna processing, and ribosome are elevated in the Het as well, compared to the WT (same as with the KO).

Respirasome is elevated in the Het compared to the WT. This is in opposition to what is observed in the KO, where the abundance of mitochondrial and electron transport chain proteins are reduced. 


```{r}
trio.xls %>% filter(logFC.Het.over.WT > 5) %>% select(geneSymbol, logFC.Het.over.WT, P.Value.WT.over.Het)
```
Pretty much all the extremely large effect size, but low nominal p-value proteins are Keratin proteins. Suggests one of the samples is hairy. 

```{r}
kerPlot <- makeAbundancePlot(getAbundanceOfProtein(trio.xls,'Q6NXH9',"Trio_PSD_TMT11_Proteome..", trio.sampleSheet))
kerPlot
```

Abundnace of Krt8. Looks like two of the Hets and one of the KO samples is contaminated with Keratin. Possibly one of the WTs. Ignoring GOs related to Keratin now. Have to figure out how to remove Keratin from datasets without compromising it. 

Now looking at correlation between LFCs of Het/WT and KO/WT.

```{r}
cor(trio.xls$logFC.Het.over.WT , trio.xls$logFC.KO.over.WT)
makeCorrPlot(trio.xls, "logFC.Het.over.WT", "logFC.KO.over.WT")
```

Correlation of 0.3840326; excluding the Keratins, the correlation looks quite good. Alterations appear to be related to gene dosage. 

Now looking at KO/Het

```{r}
cor(trio.xls$logFC.KO.over.Het , trio.xls$logFC.KO.over.WT)
makeCorrPlot(trio.xls, "logFC.KO.over.Het", "logFC.KO.over.WT")
```

```{r}
# Gene ontology analysis
trio.het3.go <- doGoAnalysisHet3(trio.xls, goPathways)
trio.het3.go$up
trio.het3.go$down
```

GOs associated with RNA processing and splicing are further elevated in the KO compaored to the Het. In contrast,  mitochondrial proteins are reduced in the KO compared to the Het (Therefore, expected abundances for mito proteins, if Wt=0 -> Het =2, KO=-2)

Now, looking at raw abundances of genes associated with specific GOs to get an intuitive understanding of the changes. 

Looking first at 'RNA processsing,' the GO term most upregulated in KO/WT. Taking genes from that set, and looking at normalized abundances. 

```{r}
rnaProcessing <- str_to_title(trio.go$up$x$data$leadingEdge[[1]]) 
rp.ab <- trio.xls %>%
  filter(geneSymbol %in% rnaProcessing) %>%
  select(starts_with("Trio_PSD_TMT11_Proteome.."))
colnames(rp.ab) <- trio.sampleSheet$cleanId
Heatmap(as.matrix(rp.ab))

rp.ab <- rp.ab %>% mutate(WT=rowMeans(select(rp.ab, starts_with("WT"))))
rp.ab <- rp.ab %>% mutate(KO=rowMeans(select(rp.ab, starts_with("KO"))))
rp.ab <- rp.ab %>% mutate(Het=rowMeans(select(rp.ab, starts_with("Het"))))
Heatmap(as.matrix(rp.ab%>% select(WT, Het, KO)))



```

Arguably there is a gene dosage dependent alteration in RNA processing proteins. Data is very noisy though.

Now looking at the genes associated with the top GO term that's downregulated in KO/WT, which is Oxidoreductase complex. 

```{r}
rnaProcessing <- str_to_title(trio.go$down$x$data$leadingEdge[[1]]) 
rp.ab <- trio.xls %>%
  filter(geneSymbol %in% rnaProcessing) %>%
  select(starts_with("Trio_PSD_TMT11_Proteome.."))
colnames(rp.ab) <- trio.sampleSheet$cleanId
Heatmap(as.matrix(rp.ab))

rp.ab <- rp.ab %>% mutate(WT=rowMeans(select(rp.ab, starts_with("WT"))))
rp.ab <- rp.ab %>% mutate(KO=rowMeans(select(rp.ab, starts_with("KO"))))
rp.ab <- rp.ab %>% mutate(Het=rowMeans(select(rp.ab, starts_with("Het"))))
Heatmap(as.matrix(rp.ab%>% select(WT, Het, KO)))

```

As hinted earlier, halving the dosage of Trio elevates mitochondrial proteins, while knocking out Trio reduces mitochondrial proteins. Again, data is noisy.  

```{r}
# Check abundance of Trio protein
trio.trio.abundance.m <- getAbundanceOfProtein(trio.xls, 
                                               'A2AI20',
                                               "Trio_PSD_TMT11_Proteome..", 
                                               trio.sampleSheet)
trio.trio.abundance.plot <- makeAbundancePlot(trio.trio.abundance.m)
trio.trio.abundance.plot
```


When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Cmd+Shift+K* to preview the HTML file).
