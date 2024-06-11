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
library(ggpubr)
library(xtable)

# load simulation data
load("largepops.Rdata")

### -- df manipulation -- ####
#combine results dfs
simdata <- rbind(outsmall95,outsmall90,outsmall85,outsmall80,outsmall75,outsmall70,outsmall65,outsmall60,outsmall55,outsmall50)
simdata <- rbind(outsmall90weak,outsmall85weak,outsmall80weak,outsmall75weak,outsmall70weak,outsmall65weak,outsmall60weak)
simdata <- rbind(outlarge95,outlarge90,outlarge85,outlarge80,outlarge75,outlarge70)
simdata <- rbind(outlarge90weak,outlarge80weak,outlarge70weak)

### -- outbreaks sizes -- ####
cols <- c(  "#E41A1C","#1B9E77" ,"#D95F02", "#7570B3", "#E7298A",
           "#66A61E", "#E6AB02", "#A6761D" ,"#666666")
# outbreak size histograms with density overlay 
simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x=size,fill=vax)) +
  geom_histogram(aes(y=..density..), alpha=0.4, position = "identity", binwidth = 1) +
  geom_density(alpha=0,aes(color=vax)) +
  labs(x="Outbreak size", y = "Density") +
  theme_minimal() +
  guides(fill=guide_legend(title="Vaccination coverage"), color="none") 
# outbreak ridge plots
ridge.large <- simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) %>%
  ggplot(aes(x = size, y=vax, color=vax,fill=vax)) +
  geom_density_ridges(stat = "binline",
                      bins = 20, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "Vaccination coverage") +
  guides(fill="none", color="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)
 # scale_fill_manual(values = cols[c(2,3,4,5,6,7,8)])+
  #scale_color_manual(values=cols[c(2,3,4,5,6,7,8)])

ggsave(,filename="outlarge-strong-ridge.png",path="Plots/",
       device = "png", dpi=300, bg="white",width = 4, height=4)


### -- symptomatic per day -- ####
sym.large <- simdata %>% group_by(day,vax) %>% #filter(vax %in% c("0.75","0.7","0.65","0.6")) %>% 
  summarize(Symptomatic=median(I), 
                                            low5 = quantile(I, 0.05),
                                            high5=quantile(I, 0.95)) %>%
  ggplot(aes(x=day, y=Symptomatic,color=vax))+
  geom_ribbon(inherit.aes = F,aes(x=day,ymin=low5, ymax=high5,fill=vax),alpha=0.25) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Time") +
  facet_wrap(.~vax) +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)
 # scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vaccination coverage", breaks = NULL, labels = NULL)) +
 # scale_fill_manual(values = cols[c(5,6,7,8)])+
#  scale_color_manual(values=cols[c(5,6,7,8)])

ggsave(,filename="outlarge-strong-symptom.png",path="Plots/",
       device = "png", dpi=300, bg="white",width = 4, height=4)


#### large pop --- outbreaks ####

newbars <- simdata %>% group_by(day,vax) %>% filter(simnum==4) %>% summarise(size = sum(inci)) 

newbars[c(7,8),3] <-1

ggplot(newbars,aes(x=day, y=size, fill=vax)) + 
  geom_bar(stat="identity")+
  facet_wrap(~vax) +
  labs(x="Time", y = "Incident infections") +
  theme_minimal() +
  guides(fill="none") +
  scale_fill_manual(values = cols[c(1:6)])+
  scale_color_manual(values=cols[c(1:6)])

newbars2 <- simdata %>% group_by(day,vax) %>% filter(simnum==4) %>% summarise(size = sum(inci)) 
newbars2[1,3] <-1
  ggplot(newbars2, aes(x=day, y=size, fill=vax)) + 
  geom_bar(stat="identity")+
  facet_wrap(~vax, ncol=2) +
  labs(x="Time", y = "Incident infections") +
  theme_minimal() +
  guides(fill="none") +
  scale_fill_manual(values = cols[c(2,4,6)])+
  scale_color_manual(values=cols[c(2,4,6)])

ggsave(,filename="incidence-weak-large.png",path="Plots/",
       device = "png", dpi=300, bg="white",width = 4, height=4)

sum(filter(newbars2,vax=="0.8")$size)
sum(filter(newbars2,vax=="0.7")$size)

simdata %>% group_by(vax) %>% filter(simnum==4) %>% 
ggplot(aes(x=ctime, y=I, color=vax)) + 
  geom_line() +
  facet_wrap(~vax)+
  labs(x="Time", y = "I") +
  theme_minimal() +
  guides(color="none") +
  scale_fill_manual(values = cols[c(1:6)])+
  scale_color_manual(values=cols[c(1:6)])



simdata %>% group_by(day,vax) %>% filter(simnum==8) %>% summarise(size = sum(inci)) %>%
ggplot(aes(x=day, y=size, fill=vax)) + 
  geom_bar(stat="identity")+
  facet_wrap(~vax, ncol=2) +
  labs(x="Time (days)", y = "Incident infections") +
  theme_minimal() +
  guides(fill="none") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values=cols)


ggplot(filter(simdata,simnum %in% c(1:10) & vax =="0.7"), aes(x=ctime, y=I, color=simnum))+geom_line() +
  facet_wrap(~simnum) 


calc.meddqy <- (simdata %>% group_by(simnum) %>% summarise(daytotal=max(day)))

median(as.vector(calc.meddqy$daytotal))



## panels ####

ggarrange(ridge.small,sym.small,ridge.small.weak,sym.small.weak)

#### TABLE. #####
outbreak.size <- simdata %>% group_by(simnum,vax) %>% summarise(size = sum(inci)) 
report.tab.weak.small <- outbreak.size %>% group_by(vax) %>% reframe(med.size = median(size),
                                                          low5 = quantile(size, 0.05),
                                                          high5=quantile(size, 0.95))


write.csv(rbind(report.tab.strong.large,report.tab.strong.small,report.tab.weak.small),"measles-table.csv")







