##--Script for data diagnosis and descriptive statistics
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeOmics
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics |RDA | Republic of South Korea


### Libraries

library(dlookr) # Diagnosis
library(pastecs) # descriptive statistics

### Data importation

data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
data_nqr1 = read.csv("data_nqr1.csv", h=T)
data_capsule_length_width = read.csv("capsule_length_and_width.csv", h=T)


### Check the presence of outliers, data distribution, and perform the Shapiro-Wilk analysis for the normality test


find_outliers(data_nqr1)
find_outliers(data_qr)
find_outliers(data_capsule_length_width)

plot_outlier(data_nqr1)
plot_outlier(data_qr)
plot_outlier(data_capsule_length_width)

normality(data_nqr1)
normality(data_qr)
normality(data_capsule_length_width)

plot_normality(data_nqr1)
plot_normality(data_qr)
plot_normality(data_capsule_length_width)

desc.stats.data_nqr1 = describe(data_nqr1)
desc.stats.data_qr = describe(data_qr)
desc.stats.data_capsule_length_width = describe(data_capsule_length_width)


write.csv(desc.stats.data_nqr1, "desc.nqr1.csv")
write.csv(desc.stats.data_qr, "desc.qr.csv")
write.csv(desc.stats.data_capsule_length_width, "desc.capsule.length.width.csv")



# Capsule number per leaf axil and number of locules per capsule

data_cap= read.csv("ACP.csv", h=T, sep = ",")

describe(data_cap[16:17])



### Descriptive stats and normality test with the package pastecs
desc1= stat.desc(na.omit(data_qr[4:8]), norm=TRUE)
desc2 = stat.desc(na.omit(data_nqr1[4:10]), norm=TRUE)
desc3 = stat.desc(na.omit(data_capsule_length_width[4:5]), norm=TRUE)
desc4 = stat.desc(na.omit(data_cap[16:17]), norm=TRUE)

### Save the results

write.csv(desc1, "desc.1.csv")
write.csv(desc2, "desc.2.csv")
write.csv(desc3, "desc.3.csv")
write.csv(desc4, "desc.4.csv")



### Qualitative


data_quali= read.csv("ACP.csv", h=T, sep = ",")


diagnose_category(data_quali[18:24])
