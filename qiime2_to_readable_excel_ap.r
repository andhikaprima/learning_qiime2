###### Modifying qiime2 output to be readable in the EXCEL file ##########
# Andhika Prasetyo
# July 4th, 2020
# Manchester
##########################################################################

library(dplyr)
library(tidyr)
# library(xlsx)

# set working directory
# Adjusted based on you own directory
setwd("~/Documents/0. Training/28. INBIO - Metagenomic/Output_QIIME2/ampvis2")

# ############# EXTRACT FROM RAW FILE TO GIVE HEADER
# extract data
beaverGUT <- read.delim("table.from_biom_w_taxonomy_1400.tsv", header=FALSE, comment.char="#")

# get heading
header <- read.csv("table.from_biom_w_taxonomy_1400.tsv", header=FALSE)

# Take just header row 2
header <- data.frame(header[2, ])

# Create initial column for labeling in respect to number of field in QIIME2's output
prefix <- "V"
suffix <- seq(1, ncol(beaverGUT))

# separate the file by "tab" or "\t"
header1 <- header %>% separate(colnames(header), c(paste(prefix, suffix, sep="")), sep="\t")
header1

# Change value of #OTU ID into OTU.ID
header1$V1 <- c("OTU")

# Assign the name
colnames(beaverGUT) <- header1[1, ]

# Checking
# View(beaverGUT)
# colnames(beaverGUT)

# Take only taxonomy field
tax <- data.frame(beaverGUT$taxonomy)
tax

# divide taxonomy into level of taxonomy by identifying ";__"
tax1 <- tax %>% separate(beaverGUT.taxonomy, c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep=";")
tax1

# corrected the value, remove the initial field code
tax1$Kingdom <- gsub("k__", "", as.character(tax1$Kingdom))
tax1$Phylum <- gsub("p__", "", as.character(tax1$Phylum))
tax1$Class <- gsub("c__", "", as.character(tax1$Class))
tax1$Order <- gsub("o__", "", as.character(tax1$Order))
tax1$Family <- gsub("f__", "", as.character(tax1$Family))
tax1$Genus <- gsub("g__", "", as.character(tax1$Genus))
tax1$Species <- gsub("s__", "", as.character(tax1$Species))
tax1


# replace <NA> to "unknown"
tax1 <- tax1 %>% replace_na(list(Kingdom = "unknown", Phylum = "unknown", Class = "unknown", Order = "unknown", Family = "unknown", Genus = "unknown", Species = "unknown"))
tax1

# filled blank value to "unknown"
tax1[tax1==" "] <- "unknown"
tax1

#merge two data frame
beaverGUT_new <- cbind(beaverGUT, tax1)
# View(beaverGUT_new)

# remove column taxonomy
beaverGUT_new <- beaverGUT_new %>% select (-taxonomy) 

# Export to csv
write.csv(beaverGUT_new, file="beaverGUT1400.csv", row.names = FALSE)

# # Export to Excel file
# write.xlsx2(beaverGUT_new, file="beaverGUT1400.xlsx", sheetName = "output", col.names = TRUE, row.names = FALSE, append = FALSE)

# Save data file
save.image("beaverGUT1400.RData")
