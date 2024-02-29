# i am following the sismid slides from Drake and Rohani at https://daphnia.ecology.uga.edu/drakelab/wp-content/uploads/2016/07/sismid-stochastic-lecture.pdf 
# and modifying to create an SEIR model 

# stochastic SEIR  model 
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
                if (U<=(beta*(I+ c*E)*S + v*S+ qs*S + k*E+ qspep*E+ gamma*I+qi*I )/total.rate) new.xyz <- c(S, E, I-1, R, Qr+1) # quar an I
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
    output <- array(dim=c(nstep+1,7)) # set up array to store results (time, S, E, I, R, Qs, Qr) 
    colnames(output) <- c("time","S", "E", "I", "R", "Qs", "Qr") #name variables
    output[1,] <- x # first record of output is initial condition
    for (k in 1:nstep) { #iterate for nstep steps
        x <- SEIR.onestep(x,params)
        ## when Q is turned on the else gives an error on the first iter
        if (any(is.na(x))) {
            break 
            } else {output[k+1,] <- x} 
    }
    output = output[ which(rowSums(output)>0), ] # only keep rows where some state was nonzero
}



# here's a function that harmonizes the time to 0, 1, 2, .. n days
# this function works on one entry of 'data' , whose cols are "time"  "S"     "E"     "I"     "R"     "Qs" "Qr"  "ctime" 
# cols 2:6 of the input have to be S E I R Q anad there has to be a col called ctime
convtime <- function(outk) { 
    t=0:ceiling(max(outk$ctime)) 
    res=data.frame(time=t, S=0, E=0, I=0, R=0, Qs=0, Qr=0)
    for (k in 2:length(t)) { 
        ind = max(which(outk$ctime <= t[k]))
        res[k, 2:(ncol(res))] = outk[ind, 2:7] # NOTE hard-coded variable cols , if 2:7 isn't SEIRQs Qr there is a problem
    }
    return(res)
}


# get the incidence from the outbreak sims. Incidence: new cases. 
# this works on the data[[k]] output, and on the analogous data frames 
# after we convert the time to be 0,1,2, 3, instead of event times 
addincidence <- function(outk) {
    outk$incid = 0
    newCases = diff(outk$E)
    whereIncid= 1+ which(newCases >= 1) # indices where there are incident cases
    outk$incid[whereIncid] = newCases[whereIncid-1] # put 'em in 
    return(outk)
}
