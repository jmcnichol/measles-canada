# plots 
library(ggplot2)
library(ggpubr)

#### Symptomatic v time ########
# c = 0.3, qi = 0.15
sumdat.int.1000.5.plot <- ggplot(sumdat.int.1000.5, aes(x=time, y=Symptomatic, color = vax, fill = vax))+
  geom_ribbon(inherit.aes = F,aes(x=time,ymin=low5, ymax=high5),alpha=0.3) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Time") +
  ylim(0,25)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vaccination rate", breaks = NULL, labels = NULL),limits = c(0,75)) +
  facet_grid(~vax)
ggsave(sumdat.int.1000.5.plot,filename="symptomatic-int.png",
       path="Plots-c0.3/",device = "png", dpi=300, bg="white",
       width = 12.3, height=6.67)

sumdat.noint.1000.5.plot <- ggplot(sumdat.noint.1000.5, aes(x=time, y=Symptomatic, color = vax, fill = vax))+
  geom_ribbon(inherit.aes = F,aes(x=time,ymin=low5, ymax=high5),alpha=0.3) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x="Time") +
  ylim(0,25)+
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Vaccination rate", breaks = NULL, labels = NULL),limits = c(0,75)) +
  facet_grid(~vax)
ggsave(sumdat.noint.1000.5.plot,filename="symptomatic-noint.png",
       path="Plots-c0.3/",device = "png", dpi=300, bg="white",
       width = 12.3, height=6.67)

#combine them 
ggarrange(sumdat.noint.1000.5.plot,sumdat.int.1000.5.plot,
          labels = c("No interventions","Interventions: qi=0.15"), 
          font.label = list(size = 10), common.legend = T, legend = "none", nrow=2)

ggsave(filename="symptomatic-compare.png",
       path="Plots-c0.3/",device = "png", dpi=300, bg="white",
       width = 12.3, height=12.67)


#### outbreak sizes #######
outsizes.1000.noint.5.plot <- ggplot(outsizes.1000.noint.5, aes(x=size, fill=vax)) +
  geom_histogram(aes(y=..density..), alpha=0.4, position = "identity", binwidth = 1) +
  geom_density(alpha=0, aes(color=vax)) +
  labs(x="Outbreak size", y = "Density") +
  theme_minimal() +
  guides(fill=guide_legend(title="Vaccination rate"), color="none")

ggsave(outsizes.1000.noint.5.plot ,filename="outsize-noint.png",path="Figs-c0.3/",
       device = "png", dpi=300, bg="white",width = 12.3, height=6.67)

outsizes.1000.int.5.plot <- ggplot(outsizes.1000.int.5, aes(x=size, fill=vax)) +
  geom_histogram(aes(y=..density..), alpha=0.4, position = "identity", binwidth = 1) +
  geom_density(alpha=0, aes(color=vax)) +
  labs(x="Outbreak size", y = "Density") +
  theme_minimal() +
  guides(fill=guide_legend(title="Vaccination rate"), color="none")

ggsave(outsizes.1000.int.5.plot ,filename="outsize-int.png",path="Figs-c0.3/",
       device = "png", dpi=300, bg="white",width = 12.3, height=6.67)


