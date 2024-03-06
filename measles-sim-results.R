######## RESULTS #
#' this script takes the outputs from 'measles-simulations.R' and synthesizes 
#' the results into plots and tables

library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel)
library(ggridges)

# load simulation data
load("smallpops4.Rdata")

### -- df manipulation -- ####
#combine results dfs
simdata <- rbind(outsmall95,outsmall90,outsmall85,outsmall80,outsmall75,outsmall70,outsmall65,outsmall60,outsmall55,outsmall50)
simdata <- rbind(outsmall90weak,outsmall85weak,outsmall80weak,outsmall75weak,outsmall70weak,outsmall65weak,outsmall60weak)



### -- outbreaks sizes -- ####
# outbreak size histograms with density overlay 
simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x=size,fill=vax)) +
  geom_histogram(aes(y=..density..), alpha=0.4, position = "identity", binwidth = 1) +
  geom_density(alpha=0,aes(color=vax)) +
  labs(x="Outbreak size", y = "Density") +
  theme_minimal() +
  guides(fill=guide_legend(title="Vaccination coverage"), color="none")
# outbreak ridge plots
simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=vax, color=vax,fill=vax)) +
  geom_density_ridges(stat = "binline",
                      bins = 20, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "Vaccination coverage") +
  guides(fill="none", color="none")

#ggsave(,filename="outsmall-weak-ridge.png",path="Plots/",
#       device = "png", dpi=300, bg="white",width = 12.3, height=6.67)


### -- symptomatic per day -- ####
simdata %>% group_by(day,vax) %>% summarize(Symptomatic=median(I), 
                                            low5 = quantile(I, 0.05),
                                            high5=quantile(I, 0.95)) %>%
  ggplot(aes(x=day, y=Symptomatic,color=vax))+
  geom_ribbon(inherit.aes = F,aes(x=day,ymin=low5, ymax=high5,fill=vax),alpha=0.25) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Time") +
  facet_grid(~vax)

#ggsave(,filename="outsmall-weak-symptom.png",path="Plots/",
#       device = "png", dpi=300, bg="white",width = 12.3, height=6.67)

#### TABLE. #####
outbreak.size <- simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) 
outbreak.size %>% group_by(vax) %>% reframe(med.size = median(size),
                                              range.size = max(range(size)))



