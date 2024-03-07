# this script runs the simulations Caroline wants 

# start with a workspace that's not cluttered with stuff from a previous session 
# needs to source the model first 
source("measles-model.R")  # read the model functions

# --- set parameters 
params.strong <- list(
    c=0.3,
    v=0.005,
    qs = 0.09,
    qspep = 0.15,
    qi=0.4)
# WEAK INTERVENTIONS: int pars set to 0.6* strong ones 
params.weak <- list(
    c=0.3,
    v=0.003,
    qs = 0.09*0.6,
    qspep = 0.15*0.6,
    qi=0.4*0.6)
params.none <- list(
    c=0.3,
    v=0,
    qs = 0,
    qspep = 0,
    qi=0)

# QX 2011 700+ cases mostly from one ss event plus sustained transmission, other
# intros dying out, but they didn't do qi and qspep and qs, jsut mainly v and a bit of qspep. 
# good example for v weak interventions

# BC 500+ cases in 2014 in FH outbreak in NL but LOW vax rates 
# Lyon i'm not sure what was happening, same w Brooklyn 

# ---- small pop: run all the strong interventions 
N=300
outsmall95 = measles.sim(0.95, 1000, 2, params.strong, N) 
outsmall90 = measles.sim(0.9, 1000, 2, params.strong, N) 
outsmall85 = measles.sim(0.85, 1000, 2, params.strong, N) 
outsmall80 = measles.sim(0.8, 1000, 2, params.strong, N) 
outsmall75 = measles.sim(0.75, 1000, 2, params.strong, N) 
outsmall70 = measles.sim(0.7, 1000, 2, params.strong, N) 
outsmall65 = measles.sim(0.65, 1000, 2, params.strong, N) 
outsmall60 = measles.sim(0.6, 1000, 2, params.strong, N) 
outsmall55 = measles.sim(0.55, 1000, 2, params.strong, N) 
outsmall50 = measles.sim(0.55, 1000, 2, params.strong, N) 




# ---- small pop: run some of the weaker interventions 

# start with 3 cases. with weaker interventions they are less vigilant and the measures 
# begin later 
outsmall90weak = measles.sim(0.9, 1000, 3, params.weak, N) 
outsmall80weak = measles.sim(0.8, 1000, 3, params.weak, N) 
outsmall70weak = measles.sim(0.7, 1000, 3, params.weak, N) 
outsmall60weak = measles.sim(0.6, 1000, 3, params.weak, N) 

outsmall85weak = measles.sim(0.85, 1000, 3, params.weak, N) 
outsmall75weak = measles.sim(0.75, 1000, 3, params.weak, N) 
outsmall65weak = measles.sim(0.65, 1000, 3, params.weak, N) 

save.image(file = "smallpops4.Rdata") 


# ---- large pop: run all the strong interventions

outlarge95 = measles.sim(0.95, 8000, 3, params.strong, 300) 
outlarge90 = measles.sim(0.90, 8000, 3, params.strong, 300) 
outlarge85 = measles.sim(0.85, 8000, 3, params.strong, 300) 
outlarge80 = measles.sim(0.8,  8000, 3, params.strong, 300) 
outlarge75 = measles.sim(0.75, 8000, 3, params.strong, 300) 
outlarge70 = measles.sim(0.70, 8000, 3, params.strong, 300) 

# ---- large pop: run many fewer, with the weak interventions 

outlarge90weak = measles.sim(0.90, 8000, 3, params.weak, 10) 
outlarge80weak = measles.sim(0.8,  8000, 3, params.weak, 10)
outlarge70weak = measles.sim(0.7,  8000, 3, params.weak, 10)



save.image(file = "largepops.Rdata") 


# ---- run all the w