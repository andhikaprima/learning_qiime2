###### Modifying qiime2 output to be readable in the EXCEL file ##########
# Andhika Prasetyo
# July 4th, 2020
# Manchester
##########################################################################

library(dplyr)
library(tidyr)
library(xlsx)

# set working directory
# Adjusted based on you own directory
setwd("C:/Users/EZP715/Desktop/INBIO/INBIO27 - Metagenomik Bakteri (NGS)/4. Meeting_4")

# IMPORTANT
# Open the tsv in the notepad DELETE first row "# Constructed from biom file" then SAVE it

# Import dataset
# Adjusted based on you own directory where the file located
beaverGUT <- read.delim("C:/Users/EZP715/Desktop/INBIO/INBIO27 - Metagenomik Bakteri (NGS)/4. Meeting_4/table.from_biom_w_taxonomy01.tsv", stringsAsFactors=FALSE)

# View(beaverGUT)
# colnames(beaverGUT)

# Take only taxonomy field
tax <- data.frame(beaverGUT$taxonomy)
tax

# divide taxonomy into level of taxonomy by identifying ";__"
tax1 <- tax %>% separate(beaverGUT.taxonomy, c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep=";")
tax1

# replace <NA> to "unknown"
tax1 <- tax1 %>% replace_na(list(kingdom = "unknown", phylum = "unknown", class = "unknown", order = "unknown", family = "unknown", genus = "unknown", species = "unknown"))
tax1


# corrected the value, remove the initial field code
tax1$kingdom <- gsub("k__", "", as.character(tax1$kingdom))
tax1$phylum <- gsub("p__", "", as.character(tax1$phylum))
tax1$class <- gsub("c__", "", as.character(tax1$class))
tax1$order <- gsub("o__", "", as.character(tax1$order))
tax1$family <- gsub("f__", "", as.character(tax1$family))
tax1$genus <- gsub("g__", "", as.character(tax1$genus))
tax1$species <- gsub("s__", "", as.character(tax1$species))
tax1

# filled blank value to "unknown"
tax1[tax1==" "] <- "unknown"
tax1

#merge two data frame
beaverGUT_new <- cbind(beaverGUT, tax1)
# View(beaverGUT_new)

# remove column taxonomy
beaverGUT_new <- beaverGUT_new %>% select (-taxonomy) 

# Export to Excel file
write.xlsx2(beaverGUT_new, file="beaverGUT_new.xlsx", sheetName = "output", col.names = TRUE, row.names = FALSE, append = FALSE)

