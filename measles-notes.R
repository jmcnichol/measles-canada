library(ggplot2)
library(dplyr)
library(readr) 
setwd("~/measles-canada/") # set to the location of the repo on your computer
source("measles-model.R")  # read the model functions

# ---- baseline: simulations with the model without interventions --- 
# script: create a few simulations 

nsims <- 5 #number of simulations
pop.size <- 500 #total population size
I0 <- 3 #initial number infected
VacFraction = 0.9 # -- VAX DATA WILL GO HERE -
S0 <- round((1-VacFraction)*pop.size) # initial number susceptible 
nstep <- 500 #number of events to simulate
xstart <- c(time=0, S=S0, E=0, I = I0, R = pop.size-S0-I0, Qs=0, Qr=0) #initial conditions
# R0 should be 12-18 in the absence of any qs etc, let's use that to set beta 
#  R0=15; and in my model, R0 = N beta (1/(gamma+qi) ( k/(k+qs)), or if q=0, simply R0=beta/gamma, 
b= 15*(1/8)/500 # beta = R0*gamma/N i think
params <- list(beta = b,
               c=0.2, # the Es are a little infectious -- pre-symptom 
               v=0, # rate of vaccination of S 
               qs = 0, # rate we find and quarantine susceptible people 
               qspep = 0, # quarantine and/or PEP for exposed people
               qi=0, # quarantine for infectious people (send home/isolate)
               l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
               k=1/6, # mean E duration of 6 days before infectiousness
               gamma=1/8) # 8 day infectiousness wo the qi  ) # parameters

data <- vector(mode='list',length=nsims) #initialize list to store the output
set.seed(1) #set seed
for (k in 1:nsims) { #simulate nsims times
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
}

# ---- plot the outbreak simulations ---- 

# merge them 
bigdf= bind_rows(data, .id="simnum")
# if you have done relatively few simulations (say < 20) 
ggplot(bigdf, aes(x=ctime, y=I, color=simnum))+geom_point() +
    facet_wrap(~simnum) # +  ylim(c(0,max(bigdf$S)*1.2))

# outbreak plots as they would likely be reported by public health,
# indicating n  new cases on day t with a bar of height n 
tdata = lapply(data,convtime) # time now in 0, 1, 2, 3, .. max
inctdata = lapply(tdata, addincidence) 
ggplot(bind_rows(inctdata, .id="simnum"), aes(x=time, y=incid, fill=simnum))+geom_bar(stat="identity")+
    facet_wrap(~simnum) 


# ---- do more simulatoins and plot median, quantiles; distribution of outbreak sizes ---- 

# if you have done a lot of simulations and you want the median and summary stats
# then you must account for the fact that the times won't align. The function convtime 
# handles this

# NOTE the next part over-writes the earlier part ... 
# we can run more simulations, group them by the harmonized time, and plot stats (median, quantiles) 
# for the outbreaks over time: 
for (k in 1:100) { #simulate 100 times
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
}
tdata = lapply(data,convtime) # time now in 0, 1, 2, 3, .. max

tbigdf = bind_rows(tdata, .id="simnum") 
sumdata = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                  low5 = quantile(I, 0.05),
                                                  high5=quantile(I, 0.95))
ggplot(sumdata, aes(x=time, y=Symptomatic))+geom_line() +
    geom_ribbon(inherit.aes = F,aes(x=time,ymin=low5, ymax=high5),alpha=0.3,fill="blue")

# Here we make a histogram of the outbreak sizes
inctdata=bind_rows(lapply(tdata, addincidence), .id="simnum") 
outsizes = inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
# with 1 infective, exposures mostly don't go anywhere, but get large relative to the 
# suscpetible population size if they do take off. Note that this is with 8 days 
# of exposure and no intervention. 
ggplot(outsizes, aes(x=size))+geom_histogram()

# ---- data on vaccination --- 
abschoolvax = read_csv("Data/Vaccination/Alberta school coverage by geography.csv") %>% 
    filter(Sex=="Both")
ggplot(abschoolvax, aes(x=Geography, y=`Immunization Percent`, fill=`Immunization Type`))+
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(breaks=seq(0,100, by=10), limits=c(0,100))

abother= read_csv("Data/Vaccination/Alberta childhood coverage by geography.csv")
# here there are much more highly-resolved geographies (subzones) 

ggplot(abother, aes(x=`Immunization Percent`, fill=`Immunization Type`))+
    geom_histogram(position = "dodge", alpha=0.9)+facet_wrap(~`Immunization Type`,nrow = 2)
# hm. kind of hard to see 

ggplot(abother, aes(x=`Immunization Type`,y=`Immunization Percent`))+
    geom_violin(fill="blue",alpha=0.5)+geom_jitter(alpha=0.5)+
    scale_y_continuous(breaks=seq(0,100, by=10), limits=c(0,100))
# yikes they go down pretty low ! probably not reliable for the very low ones
# they'd have had lots of measles already ... wouldn't they? 

vchschools = read_csv("Data/Vaccination/VCH school coverage.csv")
glimpse(vchschools)
ggplot(vchschools, aes(x=`Coverage (%)`))+geom_histogram(fill="blue",color="grey",alpha=0.5)

# we should add a few more from the table J added in the google doc
# but already these figures give a sense of what the variability is 



# ---- next steps for simulations ---- 

# (1) sanity and reality checks : continue. 
# Time course, compared to reported outbreaks; sizes
# CC added outbreak reports to the google doc. These all had several interventions 
# in place and probably effective pop sizes in the 1000s, with 
# outbreaks of 20-125 cases taking about 60 days , larger outbreaks of course take longer
# (Lyon 400 in 18 months) ; Messina 59 cases in Feb-August 

# (2) what interventions are we modelling? put these in. relates to (1) 
# because the outbreaks we compare to in the reality checks probably had 
# some interventions in place (yes. ) 

# notes for interventions: 
# we need v, qs, qspep, qi 
# v: 

# (3) decide what to model: schools, and communities larger than schools, and what interventions

# (4) model the things in (3) and create some plots and summaries 
# this should use the data javad has input 

# (5) sensitivity analysis 






# alternative models -- these are limited; just leaving notes here anyway
# EpiModel
# see this https://epimodel.github.io/epimodel-training/epidemics_and_networks/basic_icms.html
# the link has an SI model. I've made an SIR model. 

library(EpiModel)

param <- param.icm(inf.prob = 0.5, act.rate = 0.25,rec.rate=0.02)
init <- init.icm(s.num = 500, i.num = 1,r.num=0)
control <- control.icm(type = "SIR", nsims = 10, nsteps = 300)
mod <- icm(param, init, control)


plot(mod)
plot(mod, y = "i.num", sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE)
# the simulation can be accessed directly eg with mod$epi$i.num[,1] for the fist 

plot(mod$epi$i.num[,1])

# however, the model types -- see ?control -- don't include SEIR. 
# could use Christopher's thing? but this is nice because it collects the simulation so nicely 
# and has the intervention built in, which C's doesn't 

# how to extend this to be SEIR instead? Or: use our current one (Christopher's). Or: use another package
# like SimInf which has a built-in SEIR model simulation https://rdrr.io/cran/SimInf/man/SEIR.html
# however, it would have to be glued together in 2 time periods, before and after intervention,
# to cope with PH detecting an outbreak ? Or it has stuff but it's harder to access 

# ----- SimInf ----- # 

library(SimInf)
model <- SEIR(u0 = data.frame(S = 99, E = 0, I = 1, R = 0),
              tspan = 1:100,
              beta = 0.16,
              epsilon = 0.25,
              gamma = 0.077)

## Run the SEIR model and plot the result.
set.seed(3)
result <- run(model)
plot(result)
# seems to work , not obvious how to access the results directly, would have to learn about the package
# you can put in scheduled events and whatnot but this seems hard, much harder than either 
# learning Christopher's one or making our own 

# ---- planning ---- # 

# what do I even want, anyway? 
# probably need 2 groups, vaccinated and unvaccinated, with very different probs of getting infected
# in children the mixing is probably equal 
# 




