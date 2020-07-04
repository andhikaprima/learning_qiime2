# install.packages("dplyr")     # To manipulate dataframes
# install.packages("readxl")    # To read Excel files into R
# install.packages("ggplot2")   # for high quality graphics

library("dplyr")     # To manipulate dataframes
library("readxl")    # To read Excel files into R
library("ggplot2")   # graphics
# install.packages("BiocManager")
# BiocManager::install("phyloseq")
library("phyloseq") 
library("tidyverse")




# Reload EWT if data from xlsx
beaver<- read_excel("beaverGUT_new.xlsx", sheet="output")

# Reload EWT if data from sample information
samples_df <- read.delim("metadata.txt")

# get corret dataset in respect of OTU and TAX
otu_mat <- beaver %>% select(1:9)
tax_mat <- beaver %>% select(1, 10:16)

# define the row names from the otu and tax column
row.names(otu_mat) <- otu_mat$X.OTU.ID
row.names(tax_mat) <- tax_mat$X.OTU.ID
row.names(samples_df) <- samples_df$X.SampleID

# remove the column otu since it is now used as a row name
otu_mat <- otu_mat %>% select (-X.OTU.ID) 
tax_mat <- tax_mat %>% select (-X.OTU.ID)
samples_df <- samples_df %>% select (-X.SampleID) 

# Transform into matrixes otu and tax tables (sample table can be left as data frame)
otu_mat <- as.matrix(otu_mat)
tax_mat <- as.matrix(tax_mat)


# Transform to phyloseq objects no sample info
OTU = otu_table(otu_mat, taxa_are_rows = TRUE)
TAX = tax_table(tax_mat)
samples = sample_data(samples_df)

# phyloseq objects without sample info
beaver <- phyloseq(OTU, TAX, samples)
beaver


# # Visualize data
sample_names(beaver)
rank_names(beaver)
sample_variables(beaver)

# # Keep only samples to be analyzed
# beaver <- subset_samples(beaver, Location =="Cecum")
# beaver

# Keep only class: Bacteroidia & Clostridia
beaver <- subset_taxa(beaver, class %in% c("Bacteroidia", "Clostridia"))
beaver

# Exclude "None" in family
beaver <- subset_taxa(beaver, !(family %in% c("unknown")))
beaver

# Normalize number of reads in each sample using median sequencing depth.
total = median(sample_sums(beaver))
standf = function(x, t=total) round(t * (x / sum(x)))
beaver = transform_sample_counts(beaver, standf)

# Bar graphs
# Basic bar graph based on family
plot_bar(beaver, fill = "family")

# Make the bargraph nicer by removing OTUs boundaries. This is done by adding ggplot2 modifier.
plot_bar(beaver, fill = "family") + 
  geom_bar(aes(color=family, fill=family), stat="identity", position="stack")

# Regroup together samples
beaver_fraction <- merge_samples(beaver, "Sex")
beaver_fraction
plot_bar(beaver_fraction, fill = "family") +
  geom_bar(aes(color=family, fill=family), stat="identity", position="stack")

# Keep only family Bacteroidia and use color according to genus. Do separate panels BARCODENAME and SEX samples.
beaver_Bacteroidia <- subset_taxa(beaver, class %in% c("Bacteroidia"))
beaver_Bacteroidia
plot_bar(beaver_Bacteroidia, x="genus", fill = "genus", facet_grid = Sex~Location) + geom_bar(aes(color=genus, fill=genus), stat="identity", position="stack")


# Heatmaps
# A basic heatmap using the default parameters.
plot_heatmap(beaver, method = "NMDS", distance = "bray")

# It is very very cluttered. It is better to only consider the most abundant OTUs for heatmaps. For example one can only take OTUs that represent at least 5% of reads in at least one sample. Remember we normalized all the sampples to median number of reads (total). We are left with only 33 OTUS which makes the reading much more easy.
beaver_abund <- filter_taxa(beaver, function(x) sum(x > total*0.05) > 0, TRUE)
beaver_abund

otu_table(beaver_abund)[1:8, 1:5]

plot_heatmap(beaver_abund, method = "NMDS", distance = "bray")

# It is possible to use different distances and different multivaraite methods. For example Jaccard distance and MDS and label OTUs with family_name, order by family_name We can also change the Palette (the default palette is a bit ugly.).

plot_heatmap(beaver_abund, method = "MDS", distance = "(A+B-2*J)/(A+B-J)", 
             taxa.label = "family", taxa.order = "family", 
             trans=NULL, low="beige", high="red", na.value="beige")

# Many different built-in distances can be used
dist_methods <- unlist(distanceMethodList)
print(dist_methods)

# Another strategy is to do a heatmap for a specific taxonomy group.
# For example we can target the bacteria and then label the OTUs using the order

plot_heatmap(beaver_abund, method = "NMDS", distance = "bray", 
             taxa.label = "order", taxa.order = "order", 
             low="beige", high="green", na.value="beige")


# Alpha diversity
# Plot Chao1 richness estimator and Shannon diversity estimator.
plot_richness(beaver, measures=c("Chao1", "Shannon"))

# Regroup together samples from the same Sex
plot_richness(beaver, measures=c("Chao1", "Shannon"), x="Location", color="Sex")


# Ordination
# Do multivariate analysis based on Bray-Curtis distance and NMDS ordination.
beaver.ord <- ordinate(beaver, "NMDS", "bray")

# Plot OTUs
plot_ordination(beaver, beaver.ord, type="taxa", color="family", shape= "order", title="OTUs")

# A bit confusing, so make it more easy to visualize by breaking according to taxonomic order.
plot_ordination(beaver, beaver.ord, type="taxa", color="family", 
                title="OTUs", label="family") + 
  facet_wrap(~order, 3)

# Now display samples and enlarge the points to make it more easy to read.
plot_ordination(beaver, beaver.ord, type="X.sampleID", color="Location",
                shape="Sex", title="Samples") + geom_point(size=3)

# Display both samples and OTUs but in 2 different panels.
plot_ordination(beaver, beaver.ord, type="X.sampleID", color="Location",
                shape="Sex", title="biplot", label = "BarcodeName") + 
                geom_point (size=3)


# Network analysis
# Simple network analysis
plot_net(beaver, distance = "(A+B-2*J)/(A+B)", type = "taxa", 
         maxdist = 0.7, color="class", point_label="genus")

# This is quite confusing. Let us make it more simple by using only major OTUs
plot_net(beaver_abund, distance = "(A+B-2*J)/(A+B)", type = "taxa", 
         maxdist = 0.8, color="family", point_label="genus") 
