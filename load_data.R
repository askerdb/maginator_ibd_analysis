library(readxl)
#Load the translation table from ENA
ena_table = read.table("~/Downloads/Metagenome_analysis/data/filereport_read_run_PRJNA400072_tsv.txt", row.names = NULL)

#Load the data/metadata from supplementary data from Franzosa et al.
frantab = read_xlsx("~/Downloads/Metagenome_analysis/data/FranzosaMetagenome.xlsx")
#Remove the four samples that failed assembly
frantab = frantab[,-which(colnames(frantab) %in% missing)]
missing = c('PRISM|7238', 'PRISM|7445', 'PRISM|7947', 'PRISM|8550')
#Extract metadata
franmeta = t(data.frame(frantab[1:8,]))
colnames(franmeta) = franmeta[1,]
franmeta = data.frame(franmeta[-1,])
#Add Variables
franmeta$Validation = grepl("Validation" ,row.names(franmeta))
franmeta$Control = franmeta$Diagnosis == "Control"

#Merge the samplesheet from ENA to get the identifiers
franmeta_ena = merge(franmeta, ena_table, by.x = "SRA_metagenome_name", by.y = "fastq_ftp", sort = F)

fran_tax_table = data.frame(frantab[,1,])[-c(1:8),,drop = F]
colnames(fran_tax_table) = c("Taxa")

#Create phyloseq object from Fransoza et al.
fran_otu =  data.frame(frantab[-c(1:8), -1])
fran_otu_nr = apply(fran_otu, 2, as.numeric)
row.names(fran_otu_nr)=row.names(fran_otu)
#match names in otu table
row.names(fran_tax_table) = row.names(fran_otu)

#Build the object
ps_fran = phyloseq(otu_table(fran_otu_nr, taxa_are_rows = T),tax_table(as.matrix(fran_tax_table)), sample_data(data.frame(franmeta)))

#Load MAGinator results
load("~/Downloads/abundance_phyloseq.RData")
otu = otu_table(final.physeq)
#Order by metadata and replace the otu table in phyloseq object
otu_ordered = otu[match(franmeta_ena$experiment_accession, row.names(otu)),]
row.names(otu_ordered) = row.names(franmeta)
otu_table(final.physeq) = otu_ordered
ps_maginator = phyloseq(otu_table(final.physeq), tax_table(final.physeq), sample_data(data.frame(franmeta)))

