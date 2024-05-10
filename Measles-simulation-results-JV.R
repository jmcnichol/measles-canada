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
#load("largepops.Rdata")
#load ("smallpops4QC.Rdata")
#load("largepopsQC.Rdata")

#simdata <- rbind(outsmall95,outsmall90,outsmall85,outsmall80,outsmall75,outsmall70,outsmall65,outsmall60,outsmall55,outsmall50)
#simdata <- rbind(outsmall90weak,outsmall85weak,outsmall80weak,outsmall75weak,outsmall70weak,outsmall65weak,outsmall60weak)
#simdata <- rbind(outlarge95,outlarge90,outlarge85,outlarge80,outlarge75,outlarge70)
#simdata <- rbind(outlarge90weak,outlarge80weak,outlarge70weak)

cols <- c(  "#E41A1C","#1B9E77" ,"#D95F02", "#7570B3", "#E7298A",
            "#66A61E", "#E6AB02", "#A6761D" ,"#666666")

ridge.large <- simdata %>% group_by(simnum,vax) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=vax, color=vax,fill=vax)) +
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


# Plotting for changing one intervention parameter

# load ("smallpops_qs_change.Rdata")
# load ("smallpops_qspep_change.Rdata")
# load ("smallpops_qi_change.Rdata")

# load ("largepops_qs_change.Rdata")
# load ("largepops_qspep_change.Rdata")
# load ("largepops_qi_change.Rdata")


# qs changing

# simdata_qs <- rbind(small_qs2,small_qs4,small_qs6,small_qs8,small_qs10,small_qs12)
simdata_qs <- rbind(large_qs2,large_qs4,large_qs6,large_qs8,large_qs10,large_qs12)


ridge.qs_duration <- simdata_qs %>% group_by(simnum,par_qs) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qs, color=par_qs,fill=par_qs)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qs") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qs_changing_duration.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)


ridge.qs_size <- simdata_qs %>% group_by(simnum,par_qs) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qs, color=par_qs,fill=par_qs)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qs") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qs_changing_size.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)


scatterplot <- simdata_qs %>%
  group_by(simnum, par_qs) %>%
  summarise(size = sum(inci),  # Sum of incidents as size
            duration = max(day))  # Max day as duration

ggplot(scatterplot, aes(x = size, y = duration)) +
  geom_point(aes(color = factor(par_qs)), size = 0.5, alpha = 0.5) + 
  facet_wrap(~ par_qs, scales = "free") + # use scales = "free" for each facet having separate scaling
  labs(x = "Size of Outbreak",
       y = "Duration of Outbreak (Days)") +
  theme_minimal() +
  scale_color_manual(values = cols) +  
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(filename="small_qs_changing-scatter-scalefree.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 6, height=6)


#qspep changing

# simdata_qspep <- rbind(small_qspep7,small_qspep12, small_qspep1875, small_qspep23, small_qspep28, small_qspep33)
simdata_qspep <- rbind(large_qspep7,large_qspep12, large_qspep1875, large_qspep23, large_qspep28, large_qspep33)

ridge.qspep_duration <- simdata_qspep %>% group_by(simnum,par_qspep) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qspep, color=par_qspep,fill=par_qspep)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qspep") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qspep_changing_duration.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)


ridge.qspep_size <- simdata_qspep %>% group_by(simnum,par_qspep) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qspep, color=par_qspep,fill=par_qspep)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qspep") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qspep_changing_size.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)

# qi changing

# simdata_qi <- rbind(small_qi2, small_qi3, small_qi4, small_qi5, small_qi6, small_qi7)
simdata_qi <- rbind(large_qi2, large_qi3, large_qi4, large_qi5, large_qi6, large_qi7)

ridge.qi_duration <- simdata_qi %>% group_by(simnum,par_qi) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qi, color=par_qi,fill=par_qi)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qi") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qi_changing_duration.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)


ridge.qi_duration <- simdata_qi %>% group_by(simnum,par_qi) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qi, color=par_qi,fill=par_qi)) +
  geom_density_ridges(stat = "binline",
                      bins = 25, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qi") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qi_changing_size.png",path="Plots/Changing one parameter/",
       device = "png", dpi=300, bg="white",width = 4, height=6)
