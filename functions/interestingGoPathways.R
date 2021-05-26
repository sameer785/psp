require(fgsea)
goPathways <- "~/Google Drive/geneSets/mSigDb/v7_2/ontology/c5.all.v7.2.symbols.gmt"
pathways.go <- gmtPathways(goPathways)
RNA_SPLICING <- pathways.go$GO_RNA_SPLICING
CYTOSOLIC_RIBOSOME <- pathways.go$GO_CYTOSOLIC_RIBOSOME
GLUTAMATERGIC_SYNAPSE <- pathways.go$GO_GLUTAMATERGIC_SYNAPSE
VOLTAGE_GATED_ION_CHANNEL_ACTIVITY <- pathways.go$GO_VOLTAGE_GATED_ION_CHANNEL_ACTIVITY
MITOCHONDRIAL_PROTEIN_COMPLEX <- pathways.go$GO_MITOCHONDRIAL_PROTEIN_COMPLEX
SMALL_GTPASE_BINDING <- pathways.go$GO_SMALL_GTPASE_BINDING
INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE <- pathways.go$GO_INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE
PROTEIN_LOCALIZATION_TO_SYNAPSE <- pathways.go$GO_PROTEIN_LOCALIZATION_TO_SYNAPSE
NEURON_PROJECTION_EXTENSION <- pathways.go$GO_NEURON_PROJECTION_EXTENSION
MITOCHONDRIAL_MATRIX <- pathways.go$GO_MITOCHONDRIAL_MATRIX
OXIDATIVE_PHOSPHORYLATION <- pathways.go$GO_OXIDATIVE_PHOSPHORYLATION
MGLUR <- paste("GRM", c(1:8), sep="")
GABA_RECEPTOR <- c(paste("GABRA", c(1:5), sep=""), paste("GABRB", c(1:3), sep=""), paste("GABRG", c(1:3), sep=""), paste("GABRB", c("D","E","P","Q"), sep=""), paste("GABRR", c(1:3), sep=""))
ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I <- pathways.go$GO_ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I
FC_RECEPTOR_SIGNALING_PATHWAY <- pathways.go$GO_FC_RECEPTOR_SIGNALING_PATHWAY
GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY <- pathways.go$GO_GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY
MACROAUTOPHAGY <- pathways.go$GO_MACROAUTOPHAGY

interestingPathways <- list(RNA_SPLICING=RNA_SPLICING, 
                            CYTOSOLIC_RIBOSOME=CYTOSOLIC_RIBOSOME,
                            GLUTAMATERGIC_SYNAPSE=GLUTAMATERGIC_SYNAPSE,
                            VOLTAGE_GATED_ION_CHANNEL_ACTIVITY=VOLTAGE_GATED_ION_CHANNEL_ACTIVITY,
                            MITOCHONDRIAL_PROTEIN_COMPLEX=MITOCHONDRIAL_PROTEIN_COMPLEX,
                            SMALL_GTPASE_BINDING=SMALL_GTPASE_BINDING,
                            INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE=INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE,
                            PROTEIN_LOCALIZATION_TO_SYNAPSE=PROTEIN_LOCALIZATION_TO_SYNAPSE,
                            NEURON_PROJECTION_EXTENSION=NEURON_PROJECTION_EXTENSION,
                            MITOCHONDRIAL_MATRIX=MITOCHONDRIAL_MATRIX,
                            OXIDATIVE_PHOSPHORYLATION=OXIDATIVE_PHOSPHORYLATION,
                            MGLUR=MGLUR,
                            GABA_RECEPTOR=GABA_RECEPTOR,
                            ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I=ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I,
                            FC_RECEPTOR_SIGNALING_PATHWAY=FC_RECEPTOR_SIGNALING_PATHWAY,
                            GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY=GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY,
                            MACROAUTOPHAGY=MACROAUTOPHAGY
                            )

setGoVars <- function(dat.xls) {
  dat.xls <- dat.xls %>% mutate(RNA_SPLICING = ifelse(str_to_upper(geneSymbol) %in% RNA_SPLICING, 1, 0))
  dat.xls <- dat.xls %>% mutate(CYTOSOLIC_RIBOSOME = ifelse(str_to_upper(geneSymbol) %in% CYTOSOLIC_RIBOSOME, 2, 0))
  dat.xls <- dat.xls %>% mutate(spl.ribo = RNA_SPLICING + CYTOSOLIC_RIBOSOME)
  #dat.xls <- dat.xls %>% mutate(STRESS_GRANULE_ASSEMBLY = ifelse(geneSymbol %in% STRESS_GRANULE_ASSEMBLY, 1, 0))
  dat.xls <- dat.xls %>% mutate(GLUTAMATERGIC_SYNAPSE = ifelse(str_to_upper(geneSymbol) %in% GLUTAMATERGIC_SYNAPSE, 1, 0))
  dat.xls <- dat.xls %>% mutate(VOLTAGE_GATED_ION_CHANNEL_ACTIVITY = 
                                  ifelse(str_to_upper(geneSymbol) %in% VOLTAGE_GATED_ION_CHANNEL_ACTIVITY, 1, 0))
  dat.xls <- dat.xls %>% mutate(MITOCHONDRIAL_PROTEIN_COMPLEX = 
                                  ifelse(str_to_upper(geneSymbol) %in% MITOCHONDRIAL_PROTEIN_COMPLEX, 1, 0))
  dat.xls <- dat.xls %>% mutate(SMALL_GTPASE_BINDING = 
                                  ifelse(str_to_upper(geneSymbol) %in% SMALL_GTPASE_BINDING, 1, 0))
  dat.xls <- dat.xls %>% mutate(INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE = 
                                  ifelse(str_to_upper(geneSymbol) %in% INTRINSIC_COMPONENT_OF_POSTSYNAPTIC_MEMBRANE, 1, 0))
  dat.xls <- dat.xls %>% mutate(PROTEIN_LOCALIZATION_TO_SYNAPSE = 
                                  ifelse(str_to_upper(geneSymbol) %in% PROTEIN_LOCALIZATION_TO_SYNAPSE, 1, 0))
  dat.xls <- dat.xls %>% mutate(NEURON_PROJECTION_EXTENSION = 
                                  ifelse(str_to_upper(geneSymbol) %in% NEURON_PROJECTION_EXTENSION, 1, 0))
  dat.xls <- dat.xls %>% mutate(MITOCHONDRIAL_MATRIX = 
                                  ifelse(str_to_upper(geneSymbol) %in% MITOCHONDRIAL_MATRIX, 1, 0))
  dat.xls <- dat.xls %>% mutate(OXIDATIVE_PHOSPHORYLATION = 
                                  ifelse(str_to_upper(geneSymbol) %in% OXIDATIVE_PHOSPHORYLATION, 1, 0))
  dat.xls <- dat.xls %>% mutate(MGLUR = 
                                  ifelse(str_to_upper(geneSymbol) %in% MGLUR, 1, 0))
  dat.xls <- dat.xls %>% mutate(GABA_RECEPTOR = 
                                  ifelse(str_to_upper(geneSymbol) %in% GABA_RECEPTOR, 1, 0))
  dat.xls <- dat.xls %>% mutate(ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I =
                                  ifelse(str_to_upper(geneSymbol) %in% ANTIGEN_PROCESSING_AND_PRESENTATION_OF_EXOGENOUS_PEPTIDE_ANTIGEN_VIA_MHC_CLASS_I, 1, 0))
  dat.xls <- dat.xls %>% mutate(FC_RECEPTOR_SIGNALING_PATHWAY = 
                                  ifelse(str_to_upper(geneSymbol) %in% FC_RECEPTOR_SIGNALING_PATHWAY, 1, 0))
  dat.xls <- dat.xls %>% mutate(GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY = 
                                  ifelse(str_to_upper(geneSymbol) %in% GLUTAMATE_RECEPTOR_SIGNALING_PATHWAY, 1, 0))
  dat.xls <- dat.xls %>% mutate(MACROAUTOPHAGY = 
                                  ifelse(str_to_upper(geneSymbol) %in% MACROAUTOPHAGY, 1, 0))
  return (dat.xls)
  
}