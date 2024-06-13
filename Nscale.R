#Diff approach

library(dplyr)
library(ggplot2)
library(ggridges)

# Parameters for the simulation
nsims <- 100
nstep <- 50000 ## this needs to be higher (!!!) we talked about this in the last meeting -- i suggest 50,000
results <- list()
population_sizes <- c(100,500,1000,1500,2000, 2500,3000,3500, 4000, 4500, 5000, 5500, 6000, 6500,7000,7500,8000,8500,9000,9500,10000)
population_sizes2 <- c(12500,15000,20000)


# --- QC adjustment ----

# reasoning: 
# qs: lower. This is because they don't contact everyone in the whole susceptible population 
# and achieve the rates of isolation; efforts focus on those who've been exposed. but some Ss are included of course. 
# probably, the real term fro mS to E should have the number of incident rash cases in it. 
# but i decided not to change the whole model before we write the paper 

# qspep: pep is probably higher than i thought, based on the comments 
# then add: some of those who aren't eligible for pep know they were exposed 
# and they isolate 

#  qpep strong
# ( 1/time to contact (2- 3 days)) (frac eligible for PEP) (acceptance) (50 % effective) 
# ( 1/2)* (1/2)* (0.9)* (0.5) ~ 0.1125  (i used 1/2 for 2 days )
# qs (E -> Q) strong 
# (1/time to contact) (frac not PEP eligible) (want to isolate) (effectiveness of isolation)
# (0.5 )* (1/2)* (0.6)* (1/2) = 0.075 


# qpep weak 
# (1/4 )* (1/3)* (0.9)*(0.5) = 0.0375
# then add: some of those who aren't eligible for pep know they were exposed 
# and they isolate 
# qs (E -> Q) weak 
# (1/time to contact) (frac not PEP eligible) (want to isolate) (effectiveness of isolatn)
# ( 1/4) *(2/3)* (0.6) *(1/2) = 0.05


# for S to E qs strong: 0.03; weak 0.02 
# --- set parameters 
params.strong.QC <- list(
  c=0.3,
  v=0.005,
  qs = 0.06, # highly uncertain; this is one we should change in the one-by-one analyis
  qspep = 0.1875, # qpep + qs above
  qi=0.4,
  k=1/14)
# WEAK INTERVENTIONS: int pars set to 0.6* strong ones 
params.weak.QC <- list(
  c=0.3,
  v=0.003,
  qs = 0.04, # here also highly uncertain
  qspep = 0.0875,
  qi=0.4*0.7, # the 0.7 is kind of made up , i don't know how this would change from strong to weak
  k=1/14)
params.none <- list(  # this is the same, just leaving it here for completion 
  c=0.3,
  v=0,
  qs = 0,
  qspep = 0,
  qi=0)

# Run simulations for each population size
for (pop_size in population_sizes2) {
  set.seed(1234)
  results[[as.character(pop_size)]] <- measles.seir.sim(
    vax.rate = 0.8,
    pop.size = pop_size,
    I0 = 3,  
    nsims = nsims,
    nstep = nstep,
    c = 0.3,
    v = 0.003,
    R0 = 15,
    gamma = 1/4,
    qs = 0.04,  
    qspep = 0.0875,
    qi = 0.4*0.7,
    l = 1/15,
    k = 1/12
  )
  print("done")
}

# Combine all simulation data
all_data <- do.call(rbind, lapply(names(results), function(size) {
  data <- results[[size]]
  data$pop_size <- as.numeric(size)
  data
}))

# Calculate total outbreak size per simulation
all_data_summarised <- all_data %>%
  group_by(simnum, pop_size) %>%
  summarise(total_outbreak_size = sum(inci), .groups = 'drop')

# Calculate quantiles for each population size
quantile_data <- all_data_summarised %>%
  group_by(pop_size) %>%
  summarise(
    median_size = median(total_outbreak_size),
    quantile95_size = quantile(total_outbreak_size, 0.95)
  ) %>%
  pivot_longer(
    cols = c(median_size, quantile95_size),
    names_to = "quantile_type",
    values_to = "outbreak_size"
  )


# ridgeline plot
ggplot(all_data_summarised2, aes(x = total_outbreak_size, y = as.factor(pop_size))) +
  geom_density_ridges(
    gradient_lwd = 0.1,
    scale = 3,   # Adjust the overlap of the ridges
    rel_min_height = 0.01  # Adjust to remove very small ridges
  ) +
  scale_fill_manual(values = c("median_size" = "#FF6347", "quantile95_size" = "#4682B4")) +
  labs(
    title = "Distribution of Outbreak Sizes Across Population Sizes",
    x = "Total Outbreak Size",
    y = "Population Size",
    fill = "Quantile Type"
  ) +
  theme_ridges(grid = TRUE) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

  ggplot(quantile_data,aes(y=outbreak_size,x=pop_size, color=quantile_type)) +
  geom_point() +
  theme_minimal()+
  labs(
    y = "Quantile of outbreaksize over 20 sims",
    x="N"
  )

quantile_data %>% filter(quantile_type=="quantile95_size") %>%
  ggplot(aes(y=outbreak_size,x=pop_size)) +
  labs(
    y = "95% qunatile",
    x="N"
  )



ggplot() +
  geom_dotplot(data=filter(all_data_summarised,pop_size!=100),aes(y=total_outbreak_size,x=pop_size,group=pop_size),
               binaxis = "y",stackdir = "center",binwidth = 7,alpha=0.2,color="grey") +
  geom_point(data=filter(quantile_data,pop_size!=100), aes(y=outbreak_size, x=pop_size, color=quantile_type),size=0.75) +
  geom_line(data=filter(quantile_data,pop_size!=100), aes(y=outbreak_size, x=pop_size, color=quantile_type),lwd=0.5,alpha=0.5)+
  theme_minimal() +
  theme(legend.position="bottom")+
  labs(
    y = "Outbreak size",
    x="Population size"
  )+
  scale_x_continuous(limits=c(500,20000),breaks = c(500,5000,10000,15000,20000))

ggplot(all_data_summarised, aes(x=total_outbreak_size,y=pop_size, fill=pop_size, group=pop_size)) +
  geom_density_ridges(stat = "binline",
                      bins = 20, draw_baseline = FALSE, alpha = 0.2) +
  theme_minimal() +
  labs(x="Outbreak size", y = "N") +
  guides(fill="none", color="none") 

ggsave(filename = "Nscale.png",device = "png",dpi=300,width = 6,height = 4,bg="white")

