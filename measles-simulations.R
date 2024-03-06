# this script runs the simulations Caroline wants 

# needs to source the model first 
source("measles-model.R")  # read the model functions

# --- set parameters 
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
params.none <- list(
    c=0.3,
    v=0,
    qs = 0,
    qspep = 0,
    qi=0)

# ---- small pop: run all the strong interventions 

outsmall95 = measles.sim(0.95, 1000, 2, params.strong, 300) 
outsmall90 = measles.sim(0.9, 1000, 2, params.strong, 300) 
outsmall85 = measles.sim(0.85, 1000, 2, params.strong, 300) 
outsmall80 = measles.sim(0.8, 1000, 2, params.strong, 300) 
outsmall75 = measles.sim(0.75, 1000, 2, params.strong, 300) 
outsmall70 = measles.sim(0.7, 1000, 2, params.strong, 300) 
outsmall65 = measles.sim(0.65, 1000, 2, params.strong, 300) 
outsmall60 = measles.sim(0.6, 1000, 2, params.strong, 300) 

# ---- small pop: run some of the weaker interventions 

# start with 3 cases. with weaker interventions they are less vigilant and the measures 
# begin later 
outsmall90weak = measles.sim(0.9, 1000, 3, params.weak, 300) 
outsmall80weak = measles.sim(0.8, 1000, 3, params.weak, 300) 
outsmall70weak = measles.sim(0.7, 1000, 3, params.weak, 300) 
outsmall60weak = measles.sim(0.6, 1000, 3, params.weak, 300) 


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




# ---- run all the w