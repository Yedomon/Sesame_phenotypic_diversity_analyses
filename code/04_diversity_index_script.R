##--Script for diversity index calculation
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeOmics
##--15.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics |RDA | Republic of South Korea

### Install vegan package

install.packages("vegan", dependencies = TRUE)

### Load vegan package

library(vegan)


### Load the datasets

capsule_hairiness = read.csv("capsule_hairiness.csv", sep = ",", h=T)
braching_type = read.csv("braching_type.csv", sep = ",", h=T)
flower_color = read.csv("flower_color.csv", sep = ",", h=T)
inflorescence_type = read.csv("inflorescence_type.csv", sep = ",", h=T)
seed_color = read.csv("seed_color.csv", sep = ",", h=T)


### Shanon index calculation
diversity(inflorescence_type)
diversity(flower_color)
diversity(seed_color)
diversity(braching_type)
diversity(capsule_hairiness)


### Simpson index calculation
diversity(inflorescence_type, index = "simpson")
diversity(flower_color, index = "simpson")
diversity(seed_color, index = "simpson")
diversity(braching_type, index = "simpson")
diversity(capsule_hairiness, index = "simpson")







