# Exploiting different Cut-Off Values: Simulation

#### Conditions ####

nobs <- c(30, 50, 100, 300)
strategy <- c("firstsig", "smallest", "smallest.sig")

cond.cutoffHack <- expand.grid(nobs)

#### Simulation ####

simresults.cutoffHack <- list()

simmultiple.cutoffHack <- function(par, strategy){
  data.frame(sim.cutoffHack(nobs = par[1],
                            strategy = strategy,
                            iter = 10000,
                            alpha = 0.05))
                         
}

simresults.cutoffHack$firstsig <- apply(cond.cutoffHack, 1, function(x) {
  simmultiple.cutoffHack(x, strategy = "firstsig")
})

simresults.cutoffHack$smallest <- apply(cond.cutoffHack, 1, function(x) {
  simmultiple.cutoffHack(x, strategy = "smallest")
})

simresults.cutoffHack$smallestsig <- apply(cond.cutoffHack, 1, function(x) {
  simmultiple.cutoffHack(x, strategy = "smallest.sig")
})

save(simresults.cutoffHack, file = "simulations/SIM_cutoffHack.RData")

