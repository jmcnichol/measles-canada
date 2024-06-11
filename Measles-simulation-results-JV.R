library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel)
library(ggridges)
library(xtable)

cols <- c(  "#E41A1C","#1B9E77" ,"#D95F02", "#7570B3", "#E7298A",
            "#66A61E", "#E6AB02", "#A6761D" ,"#666666")

# load ("smallpops_strongparams.Rdata")
# load ("smallpops_weakparams.Rdata")
# load ("largepops_strongparams.Rdata")
# load ("largepops_weakparams.Rdata")

# simdata <- rbind(small_strong_95,small_strong_90,small_strong_85,small_strong_80,small_strong_75,small_strong_70,small_strong_65,small_strong_60,small_strong_55)
# simdata <- rbind(small_weak_95,small_weak_90,small_weak_85,small_weak_80,small_weak_75,small_weak_70,small_weak_65,small_weak_60,small_weak_55)
# simdata <- rbind(large_strong_95,large_strong_90,large_strong_85,large_strong_80,large_strong_75,large_strong_70,large_strong_65,large_strong_60)
# simdata <- rbind(large_weak_95,large_weak_90,large_weak_85,large_weak_80,large_weak_75,large_weak_70,large_weak_65,large_weak_60)



ridge.duration <- simdata %>% group_by(simnum,vax) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=vax, color=vax,fill=vax)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "Vaccination coverage") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large-weak-duration-ridge.png",path="Plots/k=1:12/",
       device = "png", dpi=300, bg="white",width = 5, height=8)


faceted.duration <- simdata %>%
  group_by(simnum, vax) %>%
  summarise(duration = max(day), .groups = 'drop') %>%
  ggplot(aes(x = duration, fill = vax)) +
  geom_histogram(bins = 30, alpha = 0.6) +  # Set the number of bins
  facet_wrap(~ vax, scales = "free_y") +
  theme_minimal() +
  labs(x = "Outbreak duration", y = "Frequency") +
  theme(strip.text.x = element_text(size = 10))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(
  filename = "large-weak-duration-faceted.png",
  path = "Plots/k=1:12/",
  device = "png",
  dpi = 300,
  bg = "white",
  width = 12,
  height = 8
)

ridge.size <- simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=vax, color=vax,fill=vax)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak size", y = "Vaccination coverage") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large-weak-size-ridge.png",path="Plots/k=1:12/",
       device = "png", dpi=300, bg="white",width = 5, height=8)


faceted.size <- simdata %>%
  group_by(simnum, vax) %>%
  summarise(size = sum(inci), .groups = 'drop') %>%
  ggplot(aes(x = size, fill = vax)) +
  geom_histogram(bins = 30, alpha = 0.6) +  # Set the number of bins
  facet_wrap(~ vax, scales = "free_y") +
  theme_minimal() +
  labs(x = "Outbreak size", y = "Frequency") +
  theme(strip.text.x = element_text(size = 10))+
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(
  filename = "large-weak-size-faceted.png",
  path = "Plots/k=1:12/",
  device = "png",
  dpi = 300,
  bg = "white",
  width = 12,
  height = 8
)



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

ggsave(filename="large-weak-duration-scatter-scalefree.png",path="Plots/k=1:12/",
       device = "png", dpi=300, bg="white",width = 6, height=6)


scatterplot <- simdata %>%
  group_by(simnum, vax) %>%
  summarise(size = sum(inci),  # Sum of incidents as size
            duration = max(day))  # Max day as duration

ggplot(scatterplot, aes(x = size, y = duration)) +
  geom_point(aes(color = factor(vax)), size = 0.5, alpha = 0.5) + 
  facet_wrap(~ vax) + # use scales = "free" for each facet having separate scaling
  labs(x = "Size of Outbreak",
       y = "Duration of Outbreak (Days)") +
  theme_minimal() +
  scale_color_manual(values = cols) +  
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename="large-weak-duration-scatter.png",path="Plots/k=1:12/",
       device = "png", dpi=300, bg="white",width = 6, height=6)




### An example (Fig 2) ###



simdata %>% group_by(day,vax) %>% filter(simnum==2) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x=day, y=size, fill=vax)) + 
  geom_bar(stat="identity")+
  facet_wrap(~vax) +
  labs(x="Time (days)", y = "Incident infections") +
  theme_minimal() +
  guides(fill="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(,filename="small-weak-incidence.png",path="Plots/k=1:12/Figure 2/",
       device = "png", dpi=300, bg="white",width = 6, height=6)




### Plotting for changing one intervention parameter ###

# load ("smallpops_qs_change.Rdata")
# load ("smallpops_qspep_change.Rdata")
# load ("smallpops_qi_change.Rdata")
load ("smallpops_v_change.Rdata")

# load ("largepops_qs_change.Rdata")
# load ("largepops_qspep_change.Rdata")
# load ("largepops_qi_change.Rdata")
# load ("largepops_v_change.Rdata")


# qs changing

# simdata_qs <- rbind(small_qs2,small_qs4,small_qs6,small_qs8,small_qs10,small_qs12)
# simdata_qs <- rbind(large_qs2,large_qs4,large_qs6,large_qs8,large_qs10,large_qs12)





ridge.qs_size <- simdata_qs %>% group_by(simnum,par_qs) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qs, color=par_qs,fill=par_qs)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qs") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qs_changing_size.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)



ridge.qs_duration <- simdata_qs %>% group_by(simnum,par_qs) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qs, color=par_qs,fill=par_qs)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qspep") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qs_changing_duration.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)


#qspep changing

# simdata_qspep <- rbind(small_qspep7,small_qspep12, small_qspep1875, small_qspep23, small_qspep28, small_qspep33)
simdata_qspep <- rbind(large_qspep7,large_qspep12, large_qspep1875, large_qspep23, large_qspep28, large_qspep33)

ridge.qspep_duration <- simdata_qspep %>% group_by(simnum,par_qspep) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qspep, color=par_qspep,fill=par_qspep)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qspep") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qspep_changing_duration.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)


ridge.qspep_size <- simdata_qspep %>% group_by(simnum,par_qspep) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qspep, color=par_qspep,fill=par_qspep)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qspep") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_qspep_changing_size.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)

# qi changing

simdata_qi <- rbind(small_qi2, small_qi3, small_qi4, small_qi5, small_qi6, small_qi7)
# simdata_qi <- rbind(large_qi2, large_qi3, large_qi4, large_qi5, large_qi6, large_qi7)

ridge.qi_duration <- simdata_qi %>% group_by(simnum,par_qi) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_qi, color=par_qi,fill=par_qi)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "qi") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="small_qi_changing_duration.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)


ridge.qi_size <- simdata_qi %>% group_by(simnum,par_qi) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_qi, color=par_qi,fill=par_qi)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak size", y = "qi") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="small_qi_changing_size.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)



# v changing

# simdata_v <- rbind(small_v1, small_v3, small_v5, small_v7, small_v9)
simdata_v <- rbind(large_v1, large_v3, large_v5, large_v7, large_v9)

ridge.v_duration <- simdata_v %>% group_by(simnum,par_v) %>% summarise(duration = max(day)) %>%
  ggplot(aes(x = duration, y=par_v, color=par_v,fill=par_v)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak duration", y = "v") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_v_changing_duration.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)


ridge.v_size <- simdata_v %>% group_by(simnum,par_v) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=par_v, color=par_v,fill=par_v)) +
  geom_density_ridges(stat = "binline",
                      bins = 30, draw_baseline = FALSE, alpha = 0.2, scale = 0.9) +
  theme_minimal() +
  labs(x="Outbreak size", y = "v") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)

ggsave(filename="large_v_changing_size.png",path="Plots/k=1:12/Sensitivity/",
       device = "png", dpi=300, bg="white",width = 5, height=7)
