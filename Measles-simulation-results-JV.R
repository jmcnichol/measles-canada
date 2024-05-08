library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel)
library(ggridges)
# library(ggpubr)
library(xtable)

#load ("smallpops4.Rdata")
load("largepops.Rdata")
#load ("smallpops4QC.Rdata")
#load("largepopsQC.Rdata")

#simdata <- rbind(outsmall95,outsmall90,outsmall85,outsmall80,outsmall75,outsmall70,outsmall65,outsmall60,outsmall55,outsmall50)
#simdata <- rbind(outsmall90weak,outsmall85weak,outsmall80weak,outsmall75weak,outsmall70weak,outsmall65weak,outsmall60weak)
simdata <- rbind(outlarge95,outlarge90,outlarge85,outlarge80,outlarge75,outlarge70)
#simdata <- rbind(outlarge90weak,outlarge80weak,outlarge70weak)

cols <- c(  "#E41A1C","#1B9E77" ,"#D95F02", "#7570B3", "#E7298A",
            "#66A61E", "#E6AB02", "#A6761D" ,"#666666")

ridge.large <- simdata %>% group_by(simnum,vax) %>% summarise(size = max(day)) %>%
  ggplot(aes(x = size, y=vax, color=vax,fill=vax)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "Vaccination coverage") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)
# scale_fill_manual(values = cols[c(2,3,4,5,6,7,8)])+
#scale_color_manual(values=cols[c(2,3,4,5,6,7,8)])

ggsave(filename="QC-large-strong-duration-ridge.png",path="Plots/Outbreak duration/",
       device = "png", dpi=300, bg="white",width = 4, height=6)



scatterplot <- simdata %>%
  group_by(simnum, vax) %>%
  summarise(size = sum(inci),  # Sum of incidents as size
            duration = max(day))  # Max day as duration

ggplot(scatterplot, aes(x = size, y = duration)) +
  geom_point(aes(color = factor(vax)), size = 0.5, alpha = 0.5) + 
  facet_wrap(~ vax, scales = "free") + # use scales = "free" for each facet having separate scaling
  labs(x = "Size of Outbreak",
       y = "Duration of Outbreak (Days)") +
  theme_minimal() +
  scale_color_manual(values = cols) +  
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename="large-strong-duration-scatter-scalefree.png",path="Plots/Outbreak duration/",
       device = "png", dpi=300, bg="white",width = 6, height=6)



