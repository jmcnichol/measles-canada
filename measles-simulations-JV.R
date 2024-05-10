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
#! Each time I want to run the code, for each changing parameter I change the added column in Simulator-JV.R.
# I know this is not clean but I am not that easy with R now.

small_qs2 = measles.seir.sim.JV(nsims = 1000 , qs = 0.02)
small_qs4 = measles.seir.sim.JV(nsims = 1000 , qs = 0.04)
small_qs6 = measles.seir.sim.JV(nsims = 1000 , qs = 0.06)
small_qs8 = measles.seir.sim.JV(nsims = 1000 , qs = 0.08)
small_qs10 = measles.seir.sim.JV(nsims = 1000 , qs = 0.1)
small_qs12 = measles.seir.sim.JV(nsims = 1000 , qs = 0.12)

# save.image(file = "smallpops_qs_change.Rdata") 

large_qs2 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.02)
large_qs4 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.04)
large_qs6 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.06)
large_qs8 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.08)
large_qs10 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.1)
large_qs12 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qs = 0.12)

save.image(file = "largepops_qs_change.Rdata") 

# Here I change qspep as [0.07,0.12,0.1875,0.23,0.28,0.33]
#! Each time I want to run the code, for each changing parameter I change the added column in Simulator-JV.R.



small_qspep7 = measles.seir.sim.JV(nsims = 1000, qspep = 0.07)
small_qspep12 = measles.seir.sim.JV(nsims = 1000, qspep = 0.12)
small_qspep1875 = measles.seir.sim.JV(nsims = 1000, qspep = 0.1875)
small_qspep23 = measles.seir.sim.JV(nsims = 1000, qspep = 0.23)
small_qspep28 = measles.seir.sim.JV(nsims = 1000, qspep = 0.28)
small_qspep33 = measles.seir.sim.JV(nsims = 1000, qspep = 0.33)

large_qspep7 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.07)
large_qspep12 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.12)
large_qspep1875 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.1875)
large_qspep23 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.23)
large_qspep28 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.28)
large_qspep33 = measles.seir.sim.JV(pop.size = 8000, nsims = 100 , qspep = 0.33)


# save.image(file = "smallpops_qspep_change.Rdata") 
save.image(file = "largepops_qspep_change.Rdata")


# Here I change qi as [0.2,0.3,0.4,0.5,0.6,0.7]
#! Each time I want to run the code, for each changing parameter I change the added column in Simulator-JV.R.

small_qi2 = measles.seir.sim.JV(nsims = 1000, qi = 0.2)
small_qi3 = measles.seir.sim.JV(nsims = 1000, qi = 0.3)
small_qi4 = measles.seir.sim.JV(nsims = 1000, qi = 0.4)
small_qi5 = measles.seir.sim.JV(nsims = 1000, qi = 0.5)
small_qi6 = measles.seir.sim.JV(nsims = 1000, qi = 0.6)
small_qi7 = measles.seir.sim.JV(nsims = 1000, qi = 0.7)

large_qi2 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.2)
large_qi3 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.3)
large_qi4 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.4)
large_qi5 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.5)
large_qi6 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.6)
large_qi7 = measles.seir.sim.JV(pop.size = 8000, nsims = 100, qi = 0.7)




# save.image(file = "smallpops_qi_change.Rdata") 
save.image(file = "largepops_qi_change.Rdata") 
