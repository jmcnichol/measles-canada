#' @description Stochastic SEIR measles simulator
#' 
#' @name measles.seir.sim
#' 
#' @note The original sims (for report march 2024) were run with the function in 
#' `measles-model.R` and a lot of the params were hard-coded. I've changed this 
#' for the next step (ie, further sims for the paper) for ease of use for others. 
#' 
#' @details This function carries out simulations of the measles stochastic 
#' seir model.
#' 
#' @param vax.rate the vaccination coverage rate in the population as a decimal. 
#' @param pop.size number of people in the population. 
#' @param I0 the number of initial infections in the population. 
#' @param nsims number of simulations to perform.
#' @param nstep the total number of events that can happen, ie. total number of steps or transitions. 
#' @param R0 the reproductive number. R0=15 by default.
#' @param c infectiousness in E.
#' @param v rate of vaccination of S.
#' @param gamma 8 day infectiousness without qi. default is 1/4.
#' @param qs the rate that we find and quarantine susceptible people. qs=0 if no interventions.
#' @param qspep the rate of quarantine and/or PEP for exposed people. qspep=0 if no interventions.
#' @param qi rate of quarantine for infectious people (send home/isolate). qi=0 if no interventions.
#' @param l mean duration of quarantine (people do it imperfectly but some are infectious). default value is 1/15.
#' @param k mean E duration of 6 days before infectiousness. default value is 1/10.
#' 
#' @returns A datafame with 12 columns (number of rows varies depending on how long outbreaks last): 
#' * `simnum` entries from 1 to nsims
#' * `time` time between events
#' * `S` number of susceptible individuals at time 
#' * `E` number of exposed individuals at time 
#' * `I` number of infected individuals at time 
#' * `R` number of recovered individuals at time 
#' * `Qr` number of quarantined-recovered individuals at time 
#' * `Qs` number of quarantined-susceptible individuals at time 
#' * `ctime` the cumulative time that has passed
#' * `day` time converted to number of days passed
#' * `inci` incident: an indicator for movement into E; 0 if no new exposure, 1 if exposure 
#' * `vax` the level of vaccination coverage that was used for the simulations.
#' 
#' @example 
#' # a simple run with no interventions on 
#' no.int <- measles.seir.sim(0.95,100,2,1,500,c=0.3,v=0.005,seed=123)
#' 
#' 
require(dplyr)
measles.seir.sim <- function(vax.rate,
         pop.size,
         I0,
         nsims=25,
         nstep=50000,
         R0 = 15,
         c=0.3,
         v=0.005,
         gamma = 1/4,
         qs = 0,
         qspep = 0,
         qi = 0,
         l = 1/15,
         k = 1/10){
  
  SEIR.onestep <- function (x, params) { #function to calculate one step of stochastic SEIR
    S <- x[2] #local variable for susceptible
    E <- x[3] #local variable for exposed
    I <- x[4] #local variable for infected
    R <- x[5] #local variable for recovered
    ## does something need to happen in the initial Q to account for qi>0 --A: no. 
    Qs <- x[6] # quarantined people who won't end up in R (for now, completely) 
    Qr <- x[7] # quarantined people who WILL end up in R (infected) 
    #      N <- X+Y+Z+R #total population size (subject to demographic change)
    with( #use with to simplify code
      as.list(params),
      {
        total.rate <-  beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E+ gamma*I+qi*I+l*Qs + l*Qr
        if (total.rate > 0) { # events : new infection (S to S-1, E to E+1), vax an S (S to S-1, R to R+1), quar an S (S-1, Q+1)
          # progress an E (E-1, I+1) , quar an E (E-1, Q+1), rec an I (I-1, R+1), quar an I (I-1, Q+1), 
          # release a Qs (Qs-1, S+1), release a Qr (Qr-1, S+1) 
          tau <- rexp(n=1,rate=total.rate) #inter-event time
          new.xyz <- c(S,E,I,R,Qs,Qr) #initialize a local variable at previous state variable values (not strictly necessary)
          U <- runif(1) # uniform random deviate
          #             new.xyz<-c(X,Y,Z-1) 
          ## the thing below is called the same as the thing above... which one is it?? -A: holdover from the code i startd with; redundant
          new.xyz <- c(S, E, I, R+1, Qs, Qr-1) # last event is release a Qr (to R) (I removed waning an R, immunity lasts too long) 
          # for each event, if U < (sum up to that one) we say we are going to do that one. If none of the other ifs are true
          # that's the one that happens. this results in each event having the correct probability. 
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E+ gamma*I+qi*I +l*Qs)/total.rate) new.xyz <- c(S+1, E, I, R, Qs-1, Qr) # release a Qs
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E+ gamma*I+qi*I )/total.rate) new.xyz <- c(S, E, I-1, R, Qs, Qr+1) # quar an I
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E+ gamma*I)/total.rate) new.xyz <- c(S, E, I-1, R+1, Qs, Qr) # rec an I
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E)/total.rate) new.xyz <- c(S, E-1, I, R, Qs, Qr+1) # quar or PEP an E 
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E)/total.rate) new.xyz <- c(S, E-1, I+1, R,  Qs, Qr) # progress an E
          if (U<=(beta*(I+ c*E)*S + v*S+ qs*S )/total.rate) new.xyz <- c(S-1, E, I, R, Qs+1, Qr) # quar an S
          if (U<=(beta*(I+ c*E)*S + v*S)/total.rate) new.xyz <- c(S-1, E, I, R+1,  Qs, Qr) # vax an S
          if (U<=(beta*(I+ c*E)*S )/total.rate) new.xyz <- c(S-1, E+1, I, R,  Qs, Qr) # new infection
          c(tau,new.xyz) #store result
          
        } else { 
          return(NA) } 
      }
    )
  }
  
  # iterate the onestep function to simulate an outbreak (once)  
  SEIR.model <- function (x, params, nstep) { #function to simulate stochastic SIR
    output <- data.frame("time" = 0,"S"=x[2], "E"=x[3], "I"=x[4], "R"=x[5], "Qs"=x[6], "Qr"=x[7],"ctime"=0,"day"=0,"inci"=0)
    ctime = inci = day = vector(mode = "numeric","length"=nstep) #vectors to save our calculated things...maybe add more later like for the converted time
    for (k in 1:nstep) { #iterate for nstep steps -- k is inside SEIR.onestep.......
      if (x[3] == 0 & x[4] == 0){break} #if E and I are 0 then we done 
      x <- SEIR.onestep(x,params)
      ctime[k] <- x[1] + sum(output[1:k,1])  # compute cumulative time 
      day[k] <- floor(ctime[k]) #time in days
      inci[k] <- as.numeric(output$E[k] < x[3]) # x[3] = E; this flags if there was an incident we will add it later
      ## when Q is turned on the else gives an error on the first iter
      if (any(is.na(x)) ) {
        break 
      } else {output[k+1,] <- c(x,ctime[k],day[k],inci[k])} 
    }
    output = output[ which(rowSums(output)>0), ] # only keep rows where some state was nonzero
  }
  
  VacFraction = vax.rate 
  S0 <- round((1-VacFraction)*pop.size) # initial number susceptible 
  xstart <- c(time=0, S=S0, E=0, I = I0, R = pop.size-S0-I0, Qs=0, Qr=0) #initial conditions
  
  params <- list(beta = R0*gamma/pop.size, 
                 c=c,
                 v=v, 
                 qs = qs,
                 qspep = qspep, 
                 qi=qi, 
                 l=l,
                 k=k, 
                 gamma=gamma) 
  
  data <- vector(mode='list',length=nsims) #initialize list to store the output
  set.seed(1234)
  for (k in 1:nsims) { 
    data[[k]] <- as.data.frame(SEIR.model(xstart,params,nstep))
  }
  
  data <- bind_rows(data, .id="simnum")
  data <- data.frame(data,vax = as.factor(rep(as.character(vax.rate))),
                     par_qi = as.factor(rep(as.character(qi))),
                     par_qs = as.factor(rep(as.character(qs))),
                     par_qspep = as.factor(rep(as.character(qspep))))
  
  return(data)
}
