##--Script for pie charts
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | Department of Genomics |RDA | Republic of South Korea


### Libraries
library(ggplot2)
library(ggstatsplot)
library(patchwork)



### Data 
data_nqr_a = read.csv("branching_patterns.csv", h=T)
data_nqr_b = read.csv("capsule_hairiness.csv", h=T)
data_nqr_c = read.csv("flower color.csv", h=T)
data_nqr_d = read.csv("seed_color.csv", h=T)
data_nqr_e = read.csv("number_locules_per_capsule.csv", h=T)
data_nqr_f = read.csv("number_capsule_per_leaf_axil.csv", h=T)

### Check the variables names

names(data_nqr_a)
names(data_nqr_b)
names(data_nqr_c)
names(data_nqr_d)
names(data_nqr_e)



### Branching paterns

A = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_a,
     Continent %in% c("Africa", "Asia")
  ),
  x = Branching_Patterns,
  y = Continent
) +
  guides(fill = guide_legend(title = "Branching patterns"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(A)")

A

### Capsule_Hairiness

B = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_b,
    Continent %in% c("Africa", "Asia")
  ),
  x = Capsule_Hairiness,
  y = Continent
) +
  guides(fill = guide_legend(title = "Capsule hairiness"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(B)")

B

### Flower_Color


C = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_c,
      Continent %in% c("Africa", "Asia")
  ),
  x = Flower_Color,
  y = Continent
) +
  guides(fill = guide_legend(title = "Flower color"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(C)")


C



### Seed_Color


D = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_d,
    Continent %in% c("Africa", "Asia")
  ),
  x = Seed_Color,
  y = Continent,
  package = "RColorBrewer", palette = "Paired", direction = 1) +
  guides(fill = guide_legend(title = "Seed color"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(D)")

D

## Number_locules_Capsule

E = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_e,
     Continent %in% c("Africa", "Asia")
  ),
  x = Number_locules_per_capsule,
  y = Continent
) +
  guides(fill = guide_legend(title = "Number of locules per capsule"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(E)")

E


### Number_Capsule_per_leaf_axil

F = ggpiestats(
  data = dplyr::filter(
    .data = data_nqr_f,
      Continent %in% c("Africa", "Asia")
  ),
  x = Capsule_type,
  y = Continent
) +
  guides(fill = guide_legend(title = "Number of capsules per leaf axil"))+
  theme_bw()+
  theme(legend.position="top")+
  ggtitle("(F)")

F

### Render 6 graphs together

(A | B | C) /
  (D | E | F )





