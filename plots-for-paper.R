library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel)
library(ggridges)
library(ggpubr)
library(xtable)

# load simulation results data
load("Rdata/smallpops_weakparams.Rdata")
load("Rdata/smallpops_strongparams.Rdata")

### -- df manipulation -- ####
#combine results dfs -- run one of these for the one you want
simdata <- rbind(small_strong_55,small_strong_60,small_strong_65,
                 small_strong_70,small_strong_75,small_strong_80,small_strong_85,
                 small_strong_90,small_strong_95,
                 small_weak_55,small_weak_60,small_weak_65,
                 small_weak_70,small_weak_75,small_weak_80,small_weak_85,
                 small_weak_90, small_weak_95) 
                   
cols <- c("#666666","#A6761D","#E6AB02", 
                   "#66A61E", "#E7298A", 
                   "#7570B3","#D95F02",
                   "#1B9E77","#E41A1C")

### -- outbreak histograms -- ####
simdata$par_qs <- factor(simdata$par_qs,labels = c("Strong interventions","Weak interventions"))

simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>%
  ggplot(aes(x = size,fill=vax, group=vax)) +
  geom_histogram(binwidth=10, alpha = 0.6) + 
  labs(x = "Outbreak size",
       y = "Count",
       tag = "Vaccination coverage") +
  theme_minimal(base_size=12) +
  facet_grid(vax~par_qs, scales = "free")+ 
  theme(legend.position = "none",
        plot.margin=unit(c(0.1,0.8,0.1,0.1), 'cm'),
        plot.tag=element_text(angle=-90,vjust = -0.5, size=11),
        plot.tag.position=c(1, 0.5))+
  scale_fill_manual(values=cols)

ggsave(filename = "outbreak-hist-strong-weak.png",dpi=300,height=7,width = 7,bg="white")

### -- duration scatter plots -- ####
str.scat <- simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>% filter(par_qs=="Strong interventions") %>%
  ggplot(aes(x = size,y=duration,color=vax)) +
  geom_hline(yintercept = 60, color="black",linewidth=0.5,alpha=0.5,linetype="dashed")+
  geom_point(size = 0.5, alpha = 0.3) + 
  labs(x = "Outbreak size",
       y = "Duration",
       tag = "Strong interventions") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  facet_wrap(vax~., scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(0.7,0.1,0.1,0.1), 'cm'),
        plot.tag=element_text(vjust = -0.5, size=10),
        plot.tag.position=c(0.525, 1)) 

wea.scat <- simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>% filter(par_qs=="Weak interventions") %>%
  ggplot(aes(x = size,y=duration,color=vax)) +
  geom_hline(yintercept = 60, color="black",linewidth=0.5,alpha=0.5,linetype="dashed")+
  geom_point(size = 0.5, alpha = 0.3) + 
  labs(x = "Outbreak size",
       y = "Duration",
       tag = "Weak interventions") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  facet_wrap(vax~., scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(0.7,0.1,0.1,0.1), 'cm'),
        plot.tag=element_text(vjust = -0.5, size=10),
        plot.tag.position=c(0.525, 1)) 

ggarrange(str.scat,wea.scat+rremove(object = "ylab"))

ggsave(filename = "duration-strong-weak.png",dpi=300,height=8,width = 8.5,bg="white")

### -- same but for large pop size -- ####

load("Rdata/largepops_weakparams.Rdata")
load("Rdata/largepops_strongparams.Rdata")

#combine results dfs -- run one of these for the one you want
simdata <- rbind(large_strong_60,large_strong_65,
                 large_strong_70,large_strong_75,large_strong_80,large_strong_85,
                 large_strong_90,large_strong_95,
                 large_weak_60,large_weak_65,
                 large_weak_70,large_weak_75,large_weak_80,large_weak_85,
                 large_weak_90, large_weak_95) 

cols <- c("#A6761D","#E6AB02", 
                   "#66A61E", "#E7298A", 
                   "#7570B3","#D95F02",
                   "#1B9E77","#E41A1C")
                   
simdata$par_qs <- factor(simdata$par_qs,labels = c("Strong interventions","Weak interventions"))

simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>%
  ggplot(aes(x = size,fill=vax, group=vax)) +
  geom_histogram(binwidth=10, alpha = 0.6) + 
  labs(x = "Outbreak size",
       y = "Count",
       tag = "Vaccination coverage") +
  theme_minimal(base_size=12) +
  facet_grid(vax~par_qs, scales = "free")+ 
  theme(legend.position = "none",
        plot.margin=unit(c(0.1,0.8,0.1,0.1), 'cm'),
        plot.tag=element_text(angle=-90,vjust = -0.5, size=11),
        plot.tag.position=c(1, 0.5))+
  scale_fill_manual(values=cols)

ggsave(filename = "outbreak-hist-strong-weak-large.png",dpi=300,height=7,width = 7,bg="white")

### -- duration scatter plots -- ####
str.scat <- simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>% filter(par_qs=="Strong interventions") %>%
  ggplot(aes(x = size,y=duration,color=vax)) +
  geom_hline(yintercept = 60, color="black",linewidth=0.5,alpha=0.5,linetype="dashed")+
  geom_point(size = 0.5, alpha = 0.3) + 
  labs(x = "Outbreak size",
       y = "Duration",
       tag = "Strong interventions") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  facet_wrap(vax~., scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(0.7,0.1,0.1,0.1), 'cm'),
        plot.tag=element_text(vjust = -0.5, size=10),
        plot.tag.position=c(0.525, 1)) 

wea.scat <- simdata %>%
  group_by(simnum, vax, par_qs) %>%
  summarise(size = sum(inci),
            duration = max(day)) %>% filter(par_qs=="Weak interventions") %>%
  ggplot(aes(x = size,y=duration,color=vax)) +
  geom_hline(yintercept = 60, color="black",linewidth=0.5,alpha=0.5,linetype="dashed")+
  geom_point(size = 0.5, alpha = 0.3) + 
  labs(x = "Outbreak size",
       y = "Duration",
       tag = "Weak interventions") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  facet_wrap(vax~., scales="free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin=unit(c(0.7,0.1,0.1,0.1), 'cm'),
        plot.tag=element_text(vjust = -0.5, size=10),
        plot.tag.position=c(0.525, 1)) 

ggarrange(str.scat,wea.scat+rremove(object = "ylab"))

ggsave(filename = "duration-strong-weak-large.png",dpi=300,height=8,width = 8.5,bg="white")

