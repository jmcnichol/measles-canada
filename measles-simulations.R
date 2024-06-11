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


# ---- run all the weak interventions 

outlarge90weak = measles.sim(0.90, 8000, 3, params.weak, 10) 
outlarge80weak = measles.sim(0.8,  8000, 3, params.weak, 10)
outlarge70weak = measles.sim(0.7,  8000, 3, params.weak, 10)



save.image(file = "largepops.Rdata") 


# --- QC adjustment ----

# reasoning: 
# qs: lower. This is because they don't contact everyone in the whole susceptible population 
# and achieve the rates of isolation; efforts focus on those who've been exposed. but some Ss are included of course. 
# probably, the real term fro mS to E should have the number of incident rash cases in it. 
# but i decided not to change the whole model before we write the paper 

# qspep: pep is probably higher than i thought, based on the comments 
# then add: some of those who aren't eligible for pep know they were exposed 
# and they isolate 

#  qpep strong
# ( 1/time to contact (2- 3 days)) (frac eligible for PEP) (acceptance) (50 % effective) 
# ( 1/2)* (1/2)* (0.9)* (0.5) ~ 0.1125  (i used 1/2 for 2 days )
# qs (E -> Q) strong 
# (1/time to contact) (frac not PEP eligible) (want to isolate) (effectiveness of isolation)
# (0.5 )* (1/2)* (0.6)* (1/2) = 0.075 


# qpep weak 
# (1/4 )* (1/3)* (0.9)*(0.5) = 0.0375
# then add: some of those who aren't eligible for pep know they were exposed 
# and they isolate 
# qs (E -> Q) weak 
# (1/time to contact) (frac not PEP eligible) (want to isolate) (effectiveness of isolatn)
# ( 1/4) *(2/3)* (0.6) *(1/2) = 0.05


# for S to E qs strong: 0.03; weak 0.02 


# --- set parameters 
params.strong.QC <- list(
    c=0.3,
    v=0.005,
    qs = 0.06, # highly uncertain; this is one we should change in the one-by-one analyis
    qspep = 0.1875, # qpep + qs above
    qi=0.4,
    k=1/14)
# WEAK INTERVENTIONS: int pars set to 0.6* strong ones 
params.weak.QC <- list(
    c=0.3,
    v=0.003,
    qs = 0.04, # here also highly uncertain
    qspep = 0.0875,
    qi=0.4*0.7, # the 0.7 is kind of made up , i don't know how this would change from strong to weak
    k=1/14)
params.none <- list(  # this is the same, just leaving it here for completion 
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
outsmall95 = measles.sim(0.95, 1000, 2, params.strong.QC, N) 
outsmall90 = measles.sim(0.9, 1000, 2, params.strong.QC, N) 
outsmall85 = measles.sim(0.85, 1000, 2, params.strong.QC, N) 
outsmall80 = measles.sim(0.8, 1000, 2, params.strong.QC, N) 
outsmall75 = measles.sim(0.75, 1000, 2, params.strong.QC, N) 
outsmall70 = measles.sim(0.7, 1000, 2, params.strong.QC, N) 
outsmall65 = measles.sim(0.65, 1000, 2, params.strong.QC, N) 
outsmall60 = measles.sim(0.6, 1000, 2, params.strong.QC, N) 
outsmall55 = measles.sim(0.55, 1000, 2, params.strong.QC, N) 
outsmall50 = measles.sim(0.55, 1000, 2, params.strong.QC, N) 




# ---- small pop: run some of the weaker interventions 

# start with 3 cases. with weaker interventions they are less vigilant and the measures 
# begin later 
outsmall90weak = measles.sim(0.9, 1000, 3, params.weak.QC, N) 
outsmall80weak = measles.sim(0.8, 1000, 3, params.weak.QC, N) 
outsmall70weak = measles.sim(0.7, 1000, 3, params.weak.QC, N) 
outsmall60weak = measles.sim(0.6, 1000, 3, params.weak.QC, N) 

outsmall85weak = measles.sim(0.85, 1000, 3, params.weak.QC, N) 
outsmall75weak = measles.sim(0.75, 1000, 3, params.weak.QC, N) 
outsmall65weak = measles.sim(0.65, 1000, 3, params.weak.QC, N) 

save.image(file = "smallpops4QC.Rdata") 


# ---- large pop: run all the strong interventions

outlarge95 = measles.sim(0.95, 8000, 3, params.strong.QC, 300) 
outlarge90 = measles.sim(0.90, 8000, 3, params.strong.QC, 300) 
outlarge85 = measles.sim(0.85, 8000, 3, params.strong.QC, 300) 
outlarge80 = measles.sim(0.8,  8000, 3, params.strong.QC, 300) 
outlarge75 = measles.sim(0.75, 8000, 3, params.strong.QC, 300) 
outlarge70 = measles.sim(0.70, 8000, 3, params.strong.QC, 300) 

# ---- large pop: run many fewer, with the weak interventions 

outlarge90weak = measles.sim(0.90, 8000, 3, params.weak.QC, 10) 
outlarge80weak = measles.sim(0.8,  8000, 3, params.weak.QC, 10)
outlarge70weak = measles.sim(0.7,  8000, 3, params.weak.QC, 10)



save.image(file = "largepopsQC.Rdata") 

