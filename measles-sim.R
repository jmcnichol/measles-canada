library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)

source("measles-model.R")

### Sim function ####

#' simulation parameters:
#' vax rates:  c(95,90,85,80,75,70,65)
#' pop sizes: c(1000,8000)
#' interventions: T/F (for now; perhaps we do partial int. later)
#' 
#' output: list of lists of SEIRQ pars and cum time for each sim

measles.sim <- function(vax.rate,pop.size,intervention=T){
  #pop.size #total population size
  
  nsims <- 1000 #number of simulations
  I0 <- 3 #initial number infected
  VacFraction = vax.rate # -- VAX DATA WILL GO HERE -
  S0 <- round((1-VacFraction)*pop.size) # initial number susceptible 
  nstep <- 500 #number of events to simulate
  xstart <- c(time=0, S=S0, E=0, I = I0, R = pop.size-S0-I0, Qs=0, Qr=0) #initial conditions
  # R0 should be 12-18 in the absence of any qs etc, let's use that to set beta 
  #  R0=15; and in my model, R0 = N beta (1/(gamma+qi) ( k/(k+qs)), or if q=0, simply R0=beta/gamma, 
  b= 15*(1/8)/pop.size # beta = R0*gamma/N i think
  
  if (intervention == F){
  params <- list(beta = b,
                 c=0.3, # the Es are a little infectious -- pre-symptom 
                 v=0, # if on: 0.05 ( 0.01-0.1) rate of vaccination of S . makes a diff! 
                 qs = 0, # does not make much diff if on: 0.06 (qs = 0.014 - 0.125 ) rate we find and quarantine susceptible people 
                 qspep = 0, # if on: 2/3 qs quarantine and/or PEP for exposed people
                 qi=0, # if on: 0.45 ( 0.2-0.72)  quarantine for infectious people (send home/isolate)
                 l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
                 k=1/6, # mean E duration of 6 days before infectiousness
                 gamma=1/8) # 8 day infectiousness wo the qi  ) # parameters
  }else if (intervention==T){
    # for now, all interventions on 
    params <- list(beta = b,
                   c=0.3, # the Es are a little infectious -- pre-symptom 
                   v=0.02, # if on: 0.05 ( 0.01-0.1) rate of vaccination of S 
                   qs = 0, # does not make much diff if on: 0.06 (qs = 0.014 - 0.125 ) rate we find and quarantine susceptible people 
                   qspep = 0.04, # if on: 2/3 qs quarantine and/or PEP for exposed people
                   qi=0.15, 
                   l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
                   k=1/6, # mean E duration of 6 days before infectiousness
                   gamma=1/8) # 8 day infectiousness wo the qi  ) # parameters
  }
  
  
  data <- vector(mode='list',length=nsims) #initialize list to store the output
  set.seed(12345)
  for (k in 1:nsims) { 
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
  }
  
  return(data)
}

### results ####
# results. naming convention: noint = no interventions, N = 1000, vax =95% for ex.
noint.1000.95.5 <- measles.sim(0.95,1000,F)
noint.1000.90.5 <- measles.sim(0.90,1000,F)
noint.1000.85.5 <- measles.sim(0.85,1000,F)
noint.1000.80.5 <- measles.sim(0.80,1000,F)
noint.1000.75.5 <- measles.sim(0.75,1000,F)
noint.1000.70.5 <- measles.sim(0.70,1000,F)
noint.1000.65.5 <- measles.sim(0.65,1000,F)

int.1000.95 <- measles.sim(0.95,1000,T)
int.1000.90 <- measles.sim(0.90,1000,T)
int.1000.85 <- measles.sim(0.85,1000,T)
int.1000.80 <- measles.sim(0.80,1000,T)
int.1000.75 <- measles.sim(0.75,1000,T)
int.1000.70 <- measles.sim(0.70,1000,T)
int.1000.65 <- measles.sim(0.65,1000,T)

#news pars 
#'  v=0.02, # if on: 0.05 ( 0.01-0.1) rate of vaccination of S 
#'  qs = 0, # does not make much diff if on: 0.06 (qs = 0.014 - 0.125 ) rate we find and quarantine susceptible people 
#'  qspep = 0.04, # if on: 2/3 qs quarantine and/or PEP for exposed people
#'  qi=0.08, # if

int.1000.95.5 <- measles.sim(0.95,1000,T)
int.1000.90.5 <- measles.sim(0.90,1000,T)
int.1000.85.5 <- measles.sim(0.85,1000,T)
int.1000.80.5 <- measles.sim(0.80,1000,T)
int.1000.75.5 <- measles.sim(0.75,1000,T)
int.1000.70.5 <- measles.sim(0.70,1000,T)
int.1000.65.5 <- measles.sim(0.65,1000,T)


noint.8000.95 <- measles.sim(0.95,1000,F)
noint.8000.90 <- measles.sim(0.90,1000,F)
noint.8000.85 <- measles.sim(0.85,1000,F)
noint.8000.80 <- measles.sim(0.80,1000,F)
noint.8000.75 <- measles.sim(0.75,1000,F)
noint.8000.70 <- measles.sim(0.70,1000,F)
noint.8000.65 <- measles.sim(0.65,1000,F)

int.8000.95.5 <- measles.sim(0.95,8000,T)
int.8000.90.5 <- measles.sim(0.90,8000,T)
int.8000.85.5 <- measles.sim(0.85,8000,T)
int.8000.80.5 <- measles.sim(0.80,8000,T)
int.8000.75.5 <- measles.sim(0.75,8000,T)
int.8000.70.5 <- measles.sim(0.70,8000,T)
int.8000.65.5 <- measles.sim(0.65,8000,T)

# manipulate the sim output to get outbreak size and symptomatic -- yes, i change the file name everytime, quick and dirty ;) 

library(parallel)

tdata = mclapply(int.8000.95.5,convtime,mc.cores=4)
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata1 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata1 <- data.frame(sumdata1, vax = as.factor(rep(0.95)))
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes1 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes1 <- data.frame(outsizes1, vax = as.factor(rep(0.95)))

tdata = mclapply(int.8000.90.5,convtime,mc.cores=4) 
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata2 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata2 <- data.frame(sumdata2, vax = as.factor(rep(0.90)))
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes2 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes2 <- data.frame(outsizes2, vax = as.factor(rep(0.90)))

tdata = mclapply(int.8000.85.5,convtime,mc.cores=4) 
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata3 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata3 <- data.frame(sumdata3, vax = as.factor(rep(0.85)))
inctdata <- bind_rows(mclapply(tdata, addincidence, mc.cores=4), .id="simnum") 
outsizes3 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes3 <- data.frame(outsizes3, vax = as.factor(rep(0.85)))

tdata = mclapply(int.8000.80.5,convtime,mc.cores=4) 
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata4 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata4 <- data.frame(sumdata4, vax = as.factor(rep(0.80)))
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes4 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes4 <- data.frame(outsizes4, vax = as.factor(rep(0.80)))

tdata = mclapply(int.8000.75.5,convtime,mc.cores=4) 
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata5 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata5 <- data.frame(sumdata5, vax = as.factor(rep(0.75)))
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes5 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes5 <- data.frame(outsizes5, vax = as.factor(rep(0.75)))

tdata = mclapply(int.8000.70.5,convtime,mc.cores=4) 
tbigdf = bind_rows(tdata, .id="simnum") 
sumdata6 = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                   low5 = quantile(I, 0.05),
                                                   high5=quantile(I, 0.95))
sumdata6 <- data.frame(sumdata6, vax = as.factor(rep(0.70)))
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes6 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes6 <- data.frame(outsizes6, vax = as.factor(rep(0.70)))

tdata = mclapply(int.8000.65.5,convtime,mc.cores=4) 
inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
outsizes7 <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
outsizes7 <- data.frame(outsizes7, vax = as.factor(rep(0.65)))

outsizes.8000.int.5 <- rbind(outsizes1,outsizes2,outsizes3, outsizes4, outsizes5, outsizes6, outsizes7)
sumdat.int.8000.5 <- rbind(sumdata1,sumdata2,sumdata3,sumdata4,sumdata5,sumdata6)







