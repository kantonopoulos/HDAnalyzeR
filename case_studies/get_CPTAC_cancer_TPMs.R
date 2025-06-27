library(TCGAbiolinks)
library(SummarizedExperiment)


#Sample sheet for all cancer types in CPTAC
query <- GDCquery(
  project = "CPTAC-3",
  data.category = "Sequencing Reads",
  experimental.strategy = "RNA-Seq"
)

sample_sheet <- getResults(query)

#Clinical data for all cancer types in CPTAC
clinical <- GDCquery_clinic(project = "CPTAC-3", type = "clinical")
clinical <- subset(clinical, select=which(!duplicated(colnames(clinical)))) 

#Example for Kidney, this would need to be done for each cancer
KIAD_submitter_id <- clinical |> filter(primary_site == 'Kidney') |> pull(submitter_id)
KIAD_cases <- sample_sheet |> filter(cases.submitter_id %in% KIAD_submitter_id) |> pull(cases) |> unique()
KIAD_sample_sheet <- sample_sheet |> filter(cases.submitter_id %in% KIAD_submitter_id)

# Gene expression aligned against hg38, download the files 
query <- GDCquery(
  project = "CPTAC-3",
  data.category = "Transcriptome Profiling",
  data.type = "Gene Expression Quantification",
  workflow.type = "STAR - Counts",
  barcode = KIAD_cases #put here which cases you want
)

#here chang paths
GDCdownload(query = query, directory = ".../CPTAC3_mRNA_counts/")
all_CPTAC_KIAD <- GDCprepare(query = query, directory =  ".../CPTAC3_mRNA_counts/")

saveRDS(object = all_CPTAC_KIAD, "all_CPTAC_KIAD.rds")

#Extract the unstranded TPM, from summarized experiment
df_CPTAC_KIAD <- assay(all_CPTAC_KIAD, "tpm_unstrand") |> 
  as.data.frame()

#write_delim(x = clinical, file = "clinical.tsv", delim = '\t')
write_delim(x = KIAD_sample_sheet, file = "sample_sheet_KIAD.tsv", delim = '\t')
saveRDS(object = df_CPTAC_KIAD, "df_CPTAC_KIAD.rds")