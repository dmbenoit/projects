#GLFC Object 2: Cluster Analysis
#Author: David Benoit

setwd("C:/Users/David/OneDrive/Desktop/GLFC Postdoc")
library(NbClust)
library(factoextra)
library(ggplot2)
library(tidyverse)

#Load principal coordinate data
coords <- read.csv("Principal_coordinates_all.csv", header=TRUE, sep =",", stringsAsFactors = TRUE)

#Make column of species names rownames
coords2 <- coords[,-1]
rownames(coords2) <- coords[,1]

#Scale data
coords2 <- scale(coords2)

#Run NbClust on cluster using Euclidean distances and Ward's
res <- NbClust(data = coords2, distance = "euclidean", min.nc=2, max.nc = 10, method = "ward.D2", index = "all", alphaBeale = 0.1)

#########################################################
#Suggests that five clusters are optimal (majority rule)#
#########################################################

coords <- read.csv("Principal_coordinates_all.csv", header=TRUE, sep =",", stringsAsFactors = TRUE)

#Make column of species names rownames
coords2 <- coords[,-1]
rownames(coords2) <- coords[,1]

#Add markings for potential invaders on dendrogram
library(dendextend)

wards <- hclust(dist(scale(coords2), method ="euclidean"), method = "ward.D2")

dend <- as.dendrogram(wards)

#Change branch colours
dend %>% set("branches_k_color", value = c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>%
  plot()

#Change color and size of labels
dend %>% set("labels_col", c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>% set("labels_cex", 0.5) %>% plot()

#Add leaf dots that represent observed vs. potnetial invaders
dend %>% set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 0.5) %>%  # node point size
  set("leaves_col", "black") %>% # node point color
  plot()

#Combine the above
dend %>% set("branches_k_color", value = c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>%
  set("labels_col", c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>% set("labels_cex", 0.5) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 0.4) %>%  # node point size
  set("leaves_col", "black") %>% # node point color
  plot()

#get labels to add red dots for potential invaders
labels <- dend %>% labels
labels <- as.data.frame(labels)

status <- read.csv("labels.csv", header = TRUE, sep = ",")

colours <- as.vector(status$Colour)

dend %>% set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 0.5) %>%  # node point size
  set("leaves_col", colours) %>% # node point color
  plot()

#Combine the above
dend %>% set("branches_k_color", value = c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>%
  set("labels_col", c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>% set("labels_cex", 0.5) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 0.5) %>%  # node point size
  set("leaves_col", colours) %>% # node point color
  plot()

#Creating a high resolution plot
png("sample_graphic.png", res =300, width =5000, height = 2000)

dend %>% set("branches_k_color", value = c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>%
  set("labels_col", c("#49997c", "#027ab0", "#d19c2f","#1ebecd","#ae3918"), k = 5) %>% set("labels_cex", 0.5) %>%
  set("leaves_pch", 19) %>%  # node point type
  set("leaves_cex", 0.5) %>%  # node point size
  set("leaves_col", colours) %>% # node point color
  plot()

dev.off()