# Incorrect Rounding: Simulation

#### Conditions ####
roundinglevel <- c(0.051, 0.055, 0.1)

cond.roundHack <- expand.grid(roundinglevel)

#### Simulation ####

simmultiple.roundHack <- function(par, strategy){
  data.frame(sim.roundhack(roundinglevel = par[1],
                         iter = 10000,
                         alpha = 0.05))
  
  
}

simresults.roundHack <- apply(cond.roundHack, 1, function(x) {
  simmultiple.roundHack(x)
})

sum(simresults.roundHack[[1]]$ps.hack <= 0.05)/10000

#### Plots ####
hist(simresults.roundHack[[3]]$ps.hack)