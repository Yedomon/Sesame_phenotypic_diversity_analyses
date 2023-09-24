##--Script for path coefficient analysis
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeOmics
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | Department of Genomics |RDA | Republic of South Korea

### Libraries

library(agricolae)

### Data

data_correlation= read.csv("ACP.csv", h=T, sep = ",")

# Dependant variables definition

x = data_correlation[, c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17)]


#  Independent variable definition

y = data_correlation[,8]


# Correlation y and x


cor.y = correlation(y,x)$correlation

cor.x = correlation(x)$correlation


# Path analysis


path.table = path.analysis(cor.x,cor.y)


# Save the results


write.csv(path.table, "pathcoefanalysisresults.csv")
