library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel) #for the mclapply function

source("measles-model.R")

### Sim function ####

#' simulation parameters:
#' vax rates:  c(95,90,85,80,75,70,65)
#' pop sizes: c(1000,8000)
#' interventions: T/F (for now; perhaps we do partial int. later)
#' 
#' output: list of lists of SEIRQ pars and cum time for each sim

measles.sim <- function(vax.rate,nstep,pop.size,I0,pars){
  #pop.size #total population size
  
  nsims <- 1000 #number of simulations
  VacFraction = vax.rate # -- VAX DATA WILL GO HERE -
  S0 <- round((1-VacFraction)*pop.size) # initial number susceptible 
  xstart <- c(time=0, S=S0, E=0, I = I0, R = pop.size-S0-I0, Qs=0, Qr=0) #initial conditions
  # R0 should be 12-18 in the absence of any qs etc, let's use that to set beta 
  #  R0=15; and in my model, R0 = N beta (1/(gamma+qi) ( k/(k+qs)), or if q=0, simply R0=beta/gamma, 
  b= 15*(1/8)/pop.size # beta = R0*gamma/N i think
  
  params <- list(beta = b,
                 c=pars$c,
                 v=pars$v, # if on: 0.05 ( 0.01-0.1) rate of vaccination of S . makes a diff! 
                 qs = pars$qs, # does not make much diff if on: 0.06 (qs = 0.014 - 0.125 ) rate we find and quarantine susceptible people 
                 qspep = pars$qspep, # if on: 2/3 qs quarantine and/or PEP for exposed people
                 qi=pars$qi, # if on: 0.45 ( 0.2-0.72)  quarantine for infectious people (send home/isolate)
                 l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
                 k=1/6, # mean E duration of 6 days before infectiousness
                 gamma=1/8) # 8 day infectiousness wo the qi  ) # parameters

  data <- vector(mode='list',length=nsims) #initialize list to store the output
  set.seed(12345)
  for (k in 1:nsims) { 
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
  }
  
  return(data)
}

# this fun does some post processing and outputs two dfs of all simulations. theyre contained in a list. -- use these for plot inputs
useful.stuff <- function(d){
  e = d 
  for (i in 1:length(d)){
    tdata = mclapply(d[[i]],convtime,mc.cores=4)
    tbigdf = bind_rows(tdata, .id="simnum") 
    sumdata = tbigdf %>% group_by(time) %>% summarize(Symptomatic=median(I), 
                                                      low5 = quantile(I, 0.05),
                                                      high5=quantile(I, 0.95))
    sumdata <- data.frame(sumdata, vax = as.factor(rep(names(d)[i])))
    d[[i]] <- sumdata
    inctdata <- bind_rows(mclapply(tdata, addincidence,mc.cores=4), .id="simnum") 
    outsizes <- inctdata %>% group_by(simnum) %>% summarise(size = sum(incid)) 
    outsizes <- data.frame(outsizes, vax = as.factor(rep(names(d)[i])))
    e[[i]] <- outsizes
  }
  d <- bind_rows(d)
  e <- bind_rows(e)
  return(list("convtime"=d,"outsize"=e))
}


##### parameters #####

# STRONG INTERVETIONS 
params.strong <- list(
               c=0.3,
               v=0.005, 
               qs = 0.03,
               qspep = 0.05,
               qi=0.15) 

# WEAK INTERVENTIONS 
params.weak <- list(
               c=0.3, 
               v=0,  
               qs = 0.02, 
               qspep = 0.04,
               qi=0.1)

##### results ######
#'2 population sizes (pop.size = 1000, 8000), range of vax coverage -- as we have now : 
#'number of cases (use I0 = 2 for the small population and I0=3 for the larger one), use these parameters:
#'Strong interventions:  c=0.3,  v = 0.005, qs = 0.03, qspep = 0.05, qi = 0.15
#'Medium/weaker interventions: c=0.3, v=0, qs =0.02, qspep = 0.04, qi = 0.1

#weak interventions
weak.1000.95 <- measles.sim(0.95,500,1000,2,params.weak)
weak.1000.90 <- measles.sim(0.90,500,1000,2,params.weak)
weak.1000.85 <- measles.sim(0.85,500,1000,2,params.weak)
weak.1000.80 <- measles.sim(0.80,500,1000,2,params.weak)
weak.1000.75 <- measles.sim(0.75,500,1000,2,params.weak)
weak.1000.70 <- measles.sim(0.70,500,1000,2,params.weak)
weak.1000.65 <- measles.sim(0.65,500,1000,2,params.weak)

strong.1000.95 <- measles.sim(0.95,500,1000,2,params.strong)
strong.1000.90 <- measles.sim(0.90,500,1000,2,params.strong)
strong.1000.85 <- measles.sim(0.85,500,1000,2,params.strong)
strong.1000.80 <- measles.sim(0.80,500,1000,2,params.strong)
strong.1000.75 <- measles.sim(0.75,500,1000,2,params.strong)
strong.1000.70 <- measles.sim(0.70,500,1000,2,params.strong)
strong.1000.65 <- measles.sim(0.65,500,1000,2,params.strong)


#### combine results into dfs ####
weak.1000.df <- useful.stuff(list("0.95" = weak.1000.95, "0.90" = weak.1000.90, 
                  "0.85" = weak.1000.85, "0.80" = weak.1000.80,
                  "0.75" = weak.1000.75, "0.70" = weak.1000.70,
                  "0.65" = weak.1000.65))

