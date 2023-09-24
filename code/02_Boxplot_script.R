##--Script for boxplot
##--By Yedomon Ange Bovys Zoclanclounon | Twitter: @AngeBovys27
##--12.03.2021
##--National Institute of Agricultural Sciences | Department of Genomics | Department of Genomics |RDA | Republic of South Korea


### Libraries

library(ggstatsplot)
library(patchwork)



### Data importation
data_qr = read.csv("Data_all_quantitative_repeated_traits.csv", h=T)
data_nqr1 = read.csv("data_nqr1.csv", h=T)
data_capsule_length_width = read.csv("capsule_length_and_width.csv", h=T)



### Plant height
a = data_qr %>% 
    select(Plant_height, Continent) %>%
    ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Plant_height,
    type = "robust",
    messages = TRUE,
    title = "(A)") +
    xlab("Continent") + ylab("Plant height (cm)")+
    theme(panel.background = element_rect(colour = "grey20"))+
    theme(axis.text.x = element_text(face="bold", color="black", size=10),
    axis.text.y = element_text(face="bold", color="black", size=10))+
    ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))


### Productive_axis_length

b = data_qr %>% 
  select(Productive_axis_length, Continent) %>% 
  ggstatsplot::ggbetweenstats(
  x = Continent,
  y = Productive_axis_length,
  messages = TRUE,
  type = "robust",
  title = "(B)") +
  xlab("Continent") + ylab("Productive axis length (cm)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Branch number 

c = data_qr %>% 
    select(Branch_number, Continent)%>% 
    ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Branch_number,
    messages = TRUE,
    type = "robust",
    title = "(C)") +
    xlab("Continent") + ylab("Branch number")+
    theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Diameter

d = data_qr %>% 
    select(Diameter, Continent)%>% 
    ggstatsplot::ggbetweenstats(
    data = data_d,
    x = Continent,
    y = Diameter,
    messages = TRUE,
    type = "robust",
    title = "(D)") +
    xlab("Continent") + ylab("Stem diameter (mm)")+
    theme(panel.background = element_rect(colour = "grey20"))+
    theme(axis.text.x = element_text(face="bold", color="black", size=10),
    axis.text.y = element_text(face="bold", color="black", size=10))+
    ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))


### Render 4 graphs together

(a | b ) / (c | d)



### Capsule number

data_cn = data_qr %>% 
  select(Accession, Capsule_Number, Continent) 

e = ggstatsplot::ggbetweenstats(
  data = data_cn,
  x = Continent,
  y = Capsule_Number,
  messages = TRUE,
  type = "robust",
  #outlier.tagging = TRUE, 
  #outlier.coef = 1.5,
  #outlier.label = Accession,
  #pairwise.comparisons = TRUE,
  #p.adjust.method = "bonferroni",
  title = "(E)"
) +
  xlab("Continent") + ylab("Capsule number")+
  scale_y_continuous(breaks=c(0,250,500,750,1000,1250))+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Dried biomass

f = data_nqr1 %>% 
  select(Dried_Biomass, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Dried_Biomass,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(F)"
  ) +
  xlab("Continent") + ylab("Dried biomass (g)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))


### Dried seed weight



g = data_nqr1 %>% 
  select(Dried_Seed_Weight, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Dried_Seed_Weight,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(G)"
  ) +
  xlab("Continent") + ylab("Dried seed weight (g)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Harvest index



h = data_nqr1 %>% 
  select(Harvest_Index, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Harvest_Index,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(H)"
  ) +
  xlab("Continent") + ylab("Harvest index")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Render 4 graphs together

(e | f ) / (g | h)


### Thousand_Seed_Weight

i = data_nqr1 %>% 
  select(Thousand_Seed_Weight, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Thousand_Seed_Weight,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(I)"
  ) +
  xlab("Continent") + ylab("Thousand Seed Weight (g)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Flowering

j = data_nqr1 %>% 
  select(Flowering, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Flowering,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(J)"
  ) +
  xlab("Continent") + ylab("Days to 50% flowering")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Maturity


k = data_nqr1 %>% 
  select(Maturity, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Maturity,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(K)"
  ) +
  xlab("Continent") + ylab("Days to 50% maturity") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Flowering_to_Maturity


l = data_nqr1 %>% 
  select(Flowering_to_Maturity, Continent)%>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Flowering_to_Maturity,
    messages = TRUE,
    type = "robust",
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(L)"
  ) +
  xlab("Continent") + ylab("Days from 50% flowering to 50% maturity") +
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))



### Render 4 graphs together

(i | j ) / (k | l)



### Capsule width

capwidth = data_capsule_length_width %>% 
  select(Capsule_width, Continent) %>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Capsule_width,
    type = "robust",
    messages = TRUE,
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(M)"
  ) +
  xlab("Continent") + ylab("Capsule width (mm)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))




### Capsule length

caplength = data_capsule_length_width %>% 
  select(Capsule_length, Continent) %>% 
  ggstatsplot::ggbetweenstats(
    x = Continent,
    y = Capsule_length,
    type = "robust",
    messages = TRUE,
    #pairwise.comparisons = TRUE,
    #p.adjust.method = "bonferroni",
    title = "(N)"
  ) +
  xlab("Continent") + ylab("Capsule length (mm)")+
  theme(panel.background = element_rect(colour = "grey20"))+
  theme(axis.text.x = element_text(face="bold", color="black", size=10),
        axis.text.y = element_text(face="bold", color="black", size=10))+
  ggplot2::scale_color_manual(values = c("#0E6251", "#FF5733", "#581845"))




### Plot together

capwidth + caplength 


