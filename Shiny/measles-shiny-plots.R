# ploty plots for measles shiny app

library(plotly)
library(ggplot)
library(lubridate)
# i make ggplots then convert them to plotly

load("Rdata/smallpops_strongparams.Rdata")

### -- df manipulation -- ####
#combine results dfs -- run one of these for the one you want
simdata <- small_strong_55

##-- ggplot of outbreaks --####

outbreak.plot <- simdata %>%
  mutate(week = day%/%7) %>% 
  group_by(week,vax,simnum) %>% 
  summarise(size=sum(inci)) %>% 
  group_by(week,vax) %>%
  summarise(medsize=median(size)) %>%
  ggplot(aes(y = medsize,x=week,color=vax)) +
 # geom_point(size = 0.5, alpha = 0.3) + 
  geom_point() +
  geom_line() +
  labs(x = "Weeks since first case",
       y = "Number of cases") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") 

mainplot <- ggplotly(outbreak.plot)
