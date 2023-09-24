##--Script for correlation
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeOmics
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | RDA | Republic of South Korea


### Library

library(ggstatsplot)


### Data

data_correlation= read.csv("ACP.csv", h=T, sep = ",")

## Plot

ggcorrmat(data = data_correlation[2:17], type = "nonparametric", matrix.type = "lower")
