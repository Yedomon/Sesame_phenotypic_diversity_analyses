##--Script for Principal component analysis and clustering
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeOmics
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | Department of Genomics |RDA | Republic of South Korea



#### Libraries

library(FactoMineR)
library(factoextra)
library(patchwork)


#### Data

data_acp= read.csv("ACP.csv", h=T, sep = ",", row.names = 1)

#### See the table

View(data_acp)


##acp

res.pca = PCA(data_acp[1:16], scale = TRUE, graph=F)

## Get the contributions of the variables

res.pca$var$contrib

## Get the correlation with each dimension

res.pca$var$cor


## get a full description of variables

dimdesc(res.pca, axes = 1:4)


# Visualize eigenvalues/variances

fviz_screeplot(res.pca, addlabels = TRUE, title = "")


# Contributions of variables to PC1
contrib1 = fviz_contrib(res.pca, choice = "var", axes = 1, title = "(A) Principal component 1", xtickslab.rt = 0)


# Contributions of variables to PC2
contrib2 = fviz_contrib(res.pca, choice = "var", axes = 2, title = "(B) Principal component 2", xtickslab.rt = 0)


# Contributions of variables to PC3
contrib3 = fviz_contrib(res.pca, choice = "var", axes = 3, title = "(C) Principal component 3", xtickslab.rt = 0)


# Contributions of variables to PC4
contrib4 = fviz_contrib(res.pca, choice = "var", axes = 4, title = "(D) Principal component 4", xtickslab.rt = 0)



### Merge the four graphs

(contrib1 | contrib2) /
  (contrib3 | contrib4)


# Control variable colors using their contributions

dim12 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE, 
                     title = "(A)",
                     axes = c(1, 2)
)



dim13 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE, 
                     title = "(B)",
                     axes = c(1, 3)
)



dim14 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE, 
                     title = "(C)",
                     axes = c(1, 4)
)

### Representing the plot of thwe most contributing variables following axes


dim12 + dim13 + dim14




### Biplot habillage infloresence type

grp_inflo = as.factor(data_acp[, "Inflorescence_type"])

inflo = fviz_pca_biplot(res.pca, 
                        repel = FALSE,
                        col.var = "black",
                        geom.var = "text",
                        label = "var",
                        title = "A: Growth habit",
                        habillage = grp_inflo, # color by groups)
                        palette = c("#00AFBB", "#E7B800", "#FC4E07")
)+
  theme(legend.position="top")


### Biplot habillage Branching_grouping

grp_branch = as.factor(data_acp[, "Branching_grouping"])

branching  = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "B: Branching type",
                             habillage = grp_branch, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
)+
  theme(legend.position="top")




### Biplot habillage Flower_color

grp_flocol = as.factor(data_acp[, "Flower_color"])

flowercol  = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "C: Flower color",
                             habillage = grp_flocol, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top")



### Biplot habillage seed color

grp_seedcol = as.factor(data_acp[, "Seed_color_grouping"])


seed_color = fviz_pca_biplot(res.pca, 
                             repel = FALSE,
                             col.var = "black",
                             geom.var = "text",
                             label = "var",
                             title = "D: Seed color",
                             habillage = grp_seedcol, # color by groups)
                             palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top")


### pLOT THE FOUR GRAPHS TOGETHER


inflo + branching + flowercol + seed_color


### Biplot habillage region of origin

grp_region = as.factor(data_acp[, "Region"])


region = fviz_pca_biplot(res.pca, 
                         repel = F,
                         col.var = "black",
                         geom.var = "text",
                         #label = "var",
                         title = "",
                         habillage = grp_region #, # color by groups)
                         #palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") 


region


### HCPC on ACP


res.hcpc = HCPC(res.pca, graph = FALSE)


### description of the clusters by the variables

res.hcpc$desc.var

res.hcpc$desc.var$quanti



### get the clusters

clust = res.hcpc$data.clust


### Save in csv file


write.csv(clust, "clusters.csv")


### Biplot habillage cluster

grp_cluster = as.factor(data_acp[, "Cluster"])


cluster = fviz_pca_biplot(res.pca, 
                           repel = FALSE,
                           col.var = "black",
                           geom.var = "text",
                           label = "var",
                           title = "",
                           habillage = grp_cluster,  # color by groups)
                           palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()




cluster

### Visualize cluster

fviz_cluster(res.hcpc)


###Visualize the relative contribution of sesame provenance to each cluster


## Data importation

data_cluster_provenance = read.csv("data_cluster_region_plot_whole_collection_percentage.csv", h=T, sep = ",")

### Names oh the header
names(data_cluster_provenance)

### View the table
View(data_cluster_provenance)


### Load library

library(ggplot2)

### Plot

ggplot(data_cluster_provenance, aes(fill=Region, y=Percentage, x=Cluster)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#ff3030","#ff4444","#ff5959",
                               "#003366","#194775","#325b84",
                               "#368d33","#4a9847","#5ea35b",
                               "#b366b3","#bb77bb","#c488c4"))+
  xlab("Cluster") +
  ylab("Proportion of accessions (%)")+
  theme_bw()+
  coord_flip()




##########################################PCA Core Oil data included######################################

### Packages

library(FactoMineR)
library(factoextra)
library(patchwork)

### Data importation

data_pca = read.csv("acp_oil_agro_core.csv", h=T, row.names = 1)

### View of the data

View(data_pca)


### Scaled the data

df = scale(data_pca[1:25])


### PCA

res.pca = PCA(df, graph=F)
res.pca = PCA(data_pca[1:25], graph=F)

# Visualize eigenvalues/variances

fviz_screeplot(res.pca, addlabels = TRUE, title = "")


# Contributions of variables to PC1
contrib1 = fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, title = "(A) Principal component 1", xtickslab.rt = 0)


# Contributions of variables to PC2
contrib2 = fviz_contrib(res.pca, choice = "var", axes = 2, top = 10, title = "(B) Principal component 2", xtickslab.rt = 0)


# Contributions of variables to PC3
contrib3 = fviz_contrib(res.pca, choice = "var", axes = 3, top = 10, title = "(C) Principal component 3", xtickslab.rt = 0)


# Contributions of variables to PC4
contrib4 = fviz_contrib(res.pca, choice = "var", axes = 4, top = 10, title = "(D) Principal component 4", xtickslab.rt = 0)


# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10, title = "")



### Merge the four graphs

(contrib1 | contrib2) /
  (contrib3 | contrib4)




# Control variable colors using their contributions

dim12 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(A)",
                     axes = c(1, 2)
)



dim13 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(B)",
                     axes = c(1, 3)
)



dim14 = fviz_pca_var(res.pca, col.var="contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     # gradient.cols = c("#3B9AB2", "#EEEEEE", "#F21A00"),
                     #gradient.cols = c("#3B9AB2", "#e5cf5b", "#F21A00"),
                     repel = TRUE, # Avoid text overlapping
                     title = "(C)",
                     axes = c(1, 4)
)

### Representing the plot of thwe most contributing variables following axes


(dim12) /
  (dim13) /
  (dim14)



### HCPC on ACP


res.hcpc = HCPC(res.pca, graph = FALSE)


### get the clusters

clust_oil = res.hcpc$data.clust


### Save in csv file


write.csv(clust_oil, "clusters_oil.csv")


### Biplot habillage cluster

grp_cluster = as.factor(data_pca[, "Cluster"])


biplot_core = fviz_pca_biplot(res.pca, 
                              repel = TRUE,
                              col.var = "black",
                              geom.var = "text",
                              #abel = "var",
                              title = "(A) Core collection",
                              show.legend = FALSE,
                              #addEllipses = TRUE,
                              habillage = grp_cluster, #, # color by groups)
                              palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()


biplot_core


# Top 30 most contributing individuals


biplot_top_30_contrib = fviz_pca_biplot(res.pca, 
                                        repel = TRUE,
                                        select.ind = list(contrib = 30),
                                        col.var = "black",
                                        geom.var = "text",
                                        #label = "var",
                                        title = "(B) Top 30 most contributing accessions",
                                        show.legend = FALSE,
                                        #addEllipses = TRUE,
                                        habillage = grp_cluster, #, # color by groups)
                                        palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
  theme(legend.position="top") +
  theme_bw()

biplot_top_30_contrib



### Plot the two graphs together



(biplot_core) / (biplot_top_30_contrib)



# Description of clusters


res.hcpc$desc.var # by variables

res.hcpc$desc.ind # by accesions


### DENDROGRAM VIEW

fviz_cluster(res.hcpc, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             show.legend = FALSE, title = "" ) +
  theme_bw()


