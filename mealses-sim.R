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
#' output: df of SEIRQ pars and cum time for each sim

measles.sim <- function(vax.rate,pop.size,intervention=F){
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
                 c=0.2, # the Es are a little infectious -- pre-symptom 
                 v=0, # if on: 0.05 ( 0.01-0.1) rate of vaccination of S . makes a diff! 
                 qs = 0, # does not make much diff if on: 0.06 (qs = 0.014 - 0.125 ) rate we find and quarantine susceptible people 
                 qspep = 0, # if on: 2/3 qs quarantine and/or PEP for exposed people
                 qi=0, # if on: 0.45 ( 0.2-0.72)  quarantine for infectious people (send home/isolate)
                 l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
                 k=1/6, # mean E duration of 6 days before infectiousness
                 gamma=1/8) # 8 day infectiousness wo the qi  ) # parameters
  }else if (intervention==T){
    # need to update 
  }
  
  
  data <- vector(mode='list',length=nsims) #initialize list to store the output
  set.seed(12345)
  for (k in 1:nsims) { 
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
  }
  
  return(bind_rows(data, .id="simnum"))
}

### results ####
# results. naming convention: noint = no interventions, N = 1000, vax =95% for ex.
noint.1000.95 <- measles.sim(0.95,1000,F)
noint.1000.90 <- measles.sim(0.90,1000,F)
noint.1000.85 <- measles.sim(0.85,1000,F)
noint.1000.80 <- measles.sim(0.80,1000,F)
noint.1000.75 <- measles.sim(0.75,1000,F)
noint.1000.70 <- measles.sim(0.70,1000,F)
noint.1000.65 <- measles.sim(0.65,1000,F)

#save the results 
