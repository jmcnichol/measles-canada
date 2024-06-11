# this script runs the simulations Caroline wants for changing one parameter at time
#! Warning: Make sure to change the function in the simulator based on what you want to change here!!

# start with a workspace that's not cluttered with stuff from a previous session 
# needs to source the model first 

source("Simulator-JV.R")  # read the model functions

# Baseline parameters are: 
# vax.rate = 0.75
# pop.size = 1000
# I0 = 2
# v = 0.005
# c = 0.3
# qs = 0.06
# qspep = 0.1875
# qi = 0.4



# Here I change qs as [0.02,0.04,0.06,0.08,0.1,0.12]


small_qs2 = measles.seir.sim.JV(nsims = 1000 , qs = 0.02)
small_qs4 = measles.seir.sim.JV(nsims = 1000 , qs = 0.04)
small_qs6 = measles.seir.sim.JV(nsims = 1000 , qs = 0.06)
small_qs8 = measles.seir.sim.JV(nsims = 1000 , qs = 0.08)
small_qs10 = measles.seir.sim.JV(nsims = 1000 , qs = 0.1)
small_qs12 = measles.seir.sim.JV(nsims = 1000 , qs = 0.12)

# save.image(file = "smallpops_qs_change.Rdata") 

large_qs2 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.02)
large_qs4 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.04)
large_qs6 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.06)
large_qs8 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.08)
large_qs10 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.1)
large_qs12 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qs = 0.12)

# save.image(file = "largepops_qs_change.Rdata") 

# Here I change qspep as [0.07,0.12,0.1875,0.23,0.28,0.33]



small_qspep7 = measles.seir.sim.JV(nsims = 1000, qspep = 0.07)
small_qspep12 = measles.seir.sim.JV(nsims = 1000, qspep = 0.12)
small_qspep1875 = measles.seir.sim.JV(nsims = 1000, qspep = 0.1875)
small_qspep23 = measles.seir.sim.JV(nsims = 1000, qspep = 0.23)
small_qspep28 = measles.seir.sim.JV(nsims = 1000, qspep = 0.28)
small_qspep33 = measles.seir.sim.JV(nsims = 1000, qspep = 0.33)

# save.image(file = "smallpops_qspep_change.Rdata") 

large_qspep7 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.07)
large_qspep12 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.12)
large_qspep1875 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.1875)
large_qspep23 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.23)
large_qspep28 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.28)
large_qspep33 = measles.seir.sim.JV(pop.size = 8000, nsims = 200 , qspep = 0.33)


# save.image(file = "largepops_qspep_change.Rdata")


# Here I change qi as [0.2,0.3,0.4,0.5,0.6,0.7]

small_qi2 = measles.seir.sim.JV(nsims = 1000, qi = 0.2)
small_qi3 = measles.seir.sim.JV(nsims = 1000, qi = 0.3)
small_qi4 = measles.seir.sim.JV(nsims = 1000, qi = 0.4)
small_qi5 = measles.seir.sim.JV(nsims = 1000, qi = 0.5)
small_qi6 = measles.seir.sim.JV(nsims = 1000, qi = 0.6)
small_qi7 = measles.seir.sim.JV(nsims = 1000, qi = 0.7)

# save.image(file = "smallpops_qi_change.Rdata")

large_qi2 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.2)
large_qi3 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.3)
large_qi4 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.4)
large_qi5 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.5)
large_qi6 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.6)
large_qi7 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, qi = 0.7)


 
# save.image(file = "largepops_qi_change.Rdata") 



# Here I change v as [0.001,0.003,0.005,0.007,0.009]


small_v1 = measles.seir.sim.JV(nsims = 1000, v=0.001)
small_v3 = measles.seir.sim.JV(nsims = 1000, v=0.003)
small_v5 = measles.seir.sim.JV(nsims = 1000, v=0.005)
small_v7 = measles.seir.sim.JV(nsims = 1000, v=0.007)
small_v9 = measles.seir.sim.JV(nsims = 1000, v=0.009)

# save.image(file = "smallpops_v_change.Rdata") 

large_v1 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, v=0.001)
large_v3 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, v=0.003)
large_v5 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, v=0.005)
large_v7 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, v=0.007)
large_v9 = measles.seir.sim.JV(pop.size = 8000, nsims = 200, v=0.009)



# save.image(file = "largepops_v_change.Rdata") 


# Different vaccination coverage for strong and weak parameters and small and large population

small_strong_95 = measles.seir.sim.JV(vax.rate = 0.95)
small_strong_90 = measles.seir.sim.JV(vax.rate = 0.90)
small_strong_85 = measles.seir.sim.JV(vax.rate = 0.85)
small_strong_80 = measles.seir.sim.JV(vax.rate = 0.80)
small_strong_75 = measles.seir.sim.JV(vax.rate = 0.75)
small_strong_70 = measles.seir.sim.JV(vax.rate = 0.70)
small_strong_65 = measles.seir.sim.JV(vax.rate = 0.65)
small_strong_60 = measles.seir.sim.JV(vax.rate = 0.60)
small_strong_55 = measles.seir.sim.JV(vax.rate = 0.55)
small_strong_50 = measles.seir.sim.JV(vax.rate = 0.50)


# save.image(file = "smallpops_strongparams.Rdata") 



small_weak_95 = measles.seir.sim.JV(vax.rate = 0.95, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_90 = measles.seir.sim.JV(vax.rate = 0.90, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_85 = measles.seir.sim.JV(vax.rate = 0.85, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_80 = measles.seir.sim.JV(vax.rate = 0.80, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_75 = measles.seir.sim.JV(vax.rate = 0.75, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_70 = measles.seir.sim.JV(vax.rate = 0.70, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_65 = measles.seir.sim.JV(vax.rate = 0.65, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_60 = measles.seir.sim.JV(vax.rate = 0.60, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_55 = measles.seir.sim.JV(vax.rate = 0.55, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
small_weak_50 = measles.seir.sim.JV(vax.rate = 0.50, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)


# save.image(file = "smallpops_weakparams.Rdata") 



large_strong_95 = measles.seir.sim.JV(vax.rate = 0.95, pop.size = 8000, nsims = 200)
large_strong_90 = measles.seir.sim.JV(vax.rate = 0.90, pop.size = 8000, nsims = 200)
large_strong_85 = measles.seir.sim.JV(vax.rate = 0.85, pop.size = 8000, nsims = 200)
large_strong_80 = measles.seir.sim.JV(vax.rate = 0.80, pop.size = 8000, nsims = 200)
large_strong_75 = measles.seir.sim.JV(vax.rate = 0.75, pop.size = 8000, nsims = 200)
large_strong_70 = measles.seir.sim.JV(vax.rate = 0.70, pop.size = 8000, nsims = 200)
large_strong_65 = measles.seir.sim.JV(vax.rate = 0.65, pop.size = 8000, nsims = 200)
large_strong_60 = measles.seir.sim.JV(vax.rate = 0.60, pop.size = 8000, nsims = 200)

# save.image(file = "largepops_strongparams.Rdata") 

large_weak_95 = measles.seir.sim.JV(vax.rate = 0.95, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
large_weak_90 = measles.seir.sim.JV(vax.rate = 0.90, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
large_weak_85 = measles.seir.sim.JV(vax.rate = 0.85, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
large_weak_80 = measles.seir.sim.JV(vax.rate = 0.80, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
large_weak_75 = measles.seir.sim.JV(vax.rate = 0.75, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)

large_weak_70 = measles.seir.sim.JV(vax.rate = 0.70, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)

large_weak_65 = measles.seir.sim.JV(vax.rate = 0.65, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)
large_weak_60 = measles.seir.sim.JV(vax.rate = 0.60, pop.size = 8000, nsims = 200, v = 0.003, qs = 0.04, qspep = 0.0875, qi = 0.28)

save.image(file = "largepops_weakparams.Rdata") 



