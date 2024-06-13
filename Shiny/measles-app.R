### Measles Shiny App back end 
source("simulator.R")

#' need to have some calculations to go from things in words to the 
#' parameters we have in the model, i.e., user doesn't input qs directly

ui.adjust.pars <- c(0.95,2,100,0,0,0)

no.int <- measles.seir.sim(vax.rate = ui.adjust.pars[1],
                           I0 = ui.adjust.pars[2],
                           pop.size = ui.adjust.pars[3],
                           qi=ui.adjust.pars[4],
                           qs=ui.adjust.pars[5],
                           qspep=ui.adjust.pars[6])
