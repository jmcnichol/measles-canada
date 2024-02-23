
# script: create a few simulations 

nsims <- 5 #number of simulations
pop.size <- 500 #total population size
I0 <- 3 #initial number infected
VacFraction = 0.9 # -- VAX DATA WILL GO HERE -
S0 <- round((1-VacFraction)*pop.size) # initial number susceptible 
nstep <- 500 #number of events to simulate
xstart <- c(time=0, S=S0, E=0, I = I0, R = pop.size-S0-I0, Q=0) #initial conditions
# R0 should be 12-18 in the absence of any qs etc, let's use that to set beta 
#  R0=15; and in my model, R0 = N beta (1/(gamma+qi) ( k/(k+qs)), or if q=0, simply R0=beta/gamma, 
b= 15*(1/8)/500 # beta = R0*gamma/N i think
params <- list(beta = b,
               c=0.5,
               v=0, # for now don't do a supplementary vaccination program 
               qs = 0,
               qi=0,
               l=1/15, # mean duration of quarantine is 21 days but people do it imperfectly but some are infectious, gah! 
               k=1/6, # mean E of 6 days 
               gamma=1/8) # 8 day infectiousness ) # parameters

data <- vector(mode='list',length=nsims) #initialize list to store the output
set.seed(1) #set seed
for (k in 1:nsims) { #simulate nsims times
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
    data[[k]]$ctime <- cumsum(data[[k]]$time) # cumulative
}

# ---- plot the outbreak simulations

# merge them 
bigdf= bind_rows(data, .id="simnum")
# if you have done relatively few simulations (say < 20) 
ggplot(bigdf, aes(x=ctime, y=I, color=simnum))+geom_point() +
    facet_wrap(~simnum) # +  ylim(c(0,max(bigdf$S)*1.2))

# if you have done a lot of simulations and you want the median and summary stats
# then you must account for the fact that the times won't align. The function convtime 
# handles this

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


# what about the distribution of outbreak sizes? 


# get the incidence from the outbreak sims. Incidence: new cases. 
# this should work on the data[[k]] output, and on the data frames after we convert the time
addincidence <- function(outk) {
    outk$incid = 0
    newCases = diff(outk$E)
    whereIncid= 1+which(newCases >= 1) # indices where there are incident cases
    outk$incid[whereIncid] = newCases
}






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




