library("ape")
library("phangorn")
library("phytools")
library("ggplot2")
library("ggtree")
library("phylogram")
library("apTreeshape")
library("BiocManager")
library("RColorBrewer")
library("tibble")
library("MASS")
library("MPSEM")
library(phylobase)
library(evobiR)
library(tidytree)
library(dplyr)
library(viridis)
library(cowplot)

# Setting up dataframe for annotate phylogenetic tree
setwd("C:/Users/Anais/OneDrive/Documents/UAF/Research2")

#-----------Formatting Data------------------------------------------------------------------------------
colfeat = read.table("colwellia.wo.NS.FeatureData.txt", fill = T,header = T)
#resetting each genome ID so that it also includes the lagging 0s and therefore can be used as rownames and can be merged with the genome ID from 
#the phylogenetic tree

colbasic <- read.delim("Colwellia.wo.NS.BasicData.txt")
for (i in 1:nrow(colbasic)) {
  colbasic$newID[i]<- format(round(colbasic[i,1],digits = 2),nsmall = 2)
}
colbasic$All.Colwellia.Strains.2.genome_id <- NULL
colnames(colbasic) <- c("genome.genome_name","genome.genome_length","All.Colwellia.Strains.2.genome_id")

colgene <- read.table("Colwellia.wo.NS.GeneData.txt",fill = T,header = T)
for (i in 1:nrow(colgene)) {
  colgene$newID[i]<- format(round(colgene[i,1],digits = 2),nsmall = 2)
}
colgene$Genome <- NULL
colnames(colgene)<-c("Gene.Count","SingleCopy.Count","FilteredSingleCopy.Count","All.Colwellia.Strains.2.genome_id")

OGT_Data <- read.csv("Colwellia OGT Data.csv",fill = T, head=T)
OGT_Data$X<- NULL
OGT_Data <- subset(OGT_Data,Strain %in% colbasic$genome.genome_name)
colnames(OGT_Data) <- c("genome.genome_name","OGT","In.Out.Bounds","max.rate","std.error.min","std.error.max")
strains <- split.data.frame(colfeat,as.factor(colfeat$All.Colwellia.Strains.2.genome_id))
FeatureData = as.data.frame(matrix(nrow = length(strains),ncol= 4))
for (i in 1:length(strains)) {
  FeatureData[i,1] <- format(round(strains[[i]][1,1],digits = 2),nsmall = 2)
  FeatureData[i,2] <- length(unique(unlist(strains[[i]][c("feature.patric_id")])))
  FeatureData[i,3] <- length(unique(unlist(strains[[i]][c("feature.pgfam_id")])))
  FeatureData[i,4] <- length(unique(unlist(strains[[i]][c("feature.plfam_id")])))
}
colnames(FeatureData)<- c("All.Colwellia.Strains.2.genome_id","Feature.Patric_id.Count","Feature.pgfam_id.Count","Feature.plfam_id.Count")

genomeData <- merge.data.frame(OGT_Data,colbasic, by = "genome.genome_name")
genomeData <- merge.data.frame(FeatureData,genomeData,by = "All.Colwellia.Strains.2.genome_id")
genomeData <- merge.data.frame(genomeData,colgene,by = "All.Colwellia.Strains.2.genome_id")
rownames(genomeData) <- genomeData[,1]
genomeData$All.Colwellia.Strains.2.genome_id <- NULL

# Creating the plain phylogenetic tree with no datafrmae
Col_tree_nwk <- read.tree("All.Colwellia.Strains.2.tree.nwk")
for ( i in 1:length(Col_tree_nwk$tip.label)){
  p <- as.numeric(Col_tree_nwk$tip.label[i])
  Col_tree_nwk$tip.label[i] <- format(round(p,digits = 2),nsmall = 2)
}
Col_tree_nwk$tip.label <- as.character(Col_tree_nwk$tip.label)
genomeData2 <- ReorderData(Col_tree_nwk,genomeData,taxa.names = "row names") #dataframe with same order as tips
genomeData2$Feature.Patric_id.Count <- NULL
label <- rownames(genomeData2)
rownames(genomeData2) <- NULL
genomeData3 <- cbind(label,genomeData2)
genomeData3$genome.genome_name <- as.character(genomeData3$genome.genome_name)
genomeData3$genome.genome_length <- as.numeric(genomeData3$genome.genome_length)

#-------------------------------------------Setting up the multi-panel figure-----------------------------------------------------------------------------------
d1 <- data.frame(label = c(Col_tree_nwk$tip.label))
p <- ggtree(Col_tree_nwk,branch.length = "none") + geom_tiplab(size = 2) + geom_text(aes(label = node)) # to view the basic tree with nodes  

#creating a dataframe with the tip identification number(y)so that the data matches each tip label
d <- filter(p,isTip) %>% select(c(label,y))
genomeData3 <- left_join(genomeData3,d, by = "label")
tips <- data.frame(old = Col_tree_nwk$tip.label, new = genomeData3$genome.genome_name)
Col_tree_nwk <- rename_taxa(Col_tree_nwk,tips) # renaming the tips with the taxa instead of the genome ID

#creating the 4 'facets' of the overall plot : tree, length(pb),gene count, and OGT
plot1 <- ggtree(Col_tree_nwk,branch.length = "none",size = 1.3,alpha = 0.6) + 
  geom_tiplab(size = 2.7) + 
  geom_hilight(node = 111,fill = "lightblue",extend = 6) +
  ggplot2::xlim(0,25)

p1 <- ggplot(genomeData3,aes(x = 'Length(bp)',y = genomeData3$y)) + 
  geom_tile(aes(fill=genomeData3$genome.genome_length),show.legend = F) + 
  scale_fill_gradient(low = "green",high = "red") +
  #scale_fill_viridis_c(option = "B",direction = -1) +
  theme_tree2() +
  theme(legend.position = "top",legend.title = element_blank())
p2 <- ggplot(genomeData3,aes(x = 'Genes', y = genomeData3$y))+
  geom_tile(aes(fill = genomeData3$Gene.Count),show.legend = F) + 
  scale_fill_gradient(low = "green",high = "red") +
  #scale_fill_viridis_c() +
  theme_tree2() + 
  theme(legend.position = "top",legend.title = element_blank())
p3 <- ggplot(genomeData3,aes(x = "OGT",y = genomeData3$y))+
  geom_tile(aes(fill = genomeData3$OGT),show.legend = F) + 
  scale_fill_gradient(low = "green",high = "red") +
  #scale_fill_viridis_c() + 
  theme_tree2() + 
  theme(legend.position = "top",legend.title = element_blank())

plot_grid(plot1,p1,p2,p3,ncol = 4,align = 'h',axis = 'bt' ,rel_widths = c(2,0.2,0.2,0.2))



