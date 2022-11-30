# Outlier Exclusion: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
howmany <- c(3, 5, 12)

cond.outHack <- expand.grid(nobs, howmany)

#### Simulation ####
simresults.outHack <- list()

simmultiple.outHack <- function(par, strategy){
  data.frame(sim.outHack(nobs = par[1],
                         which = sample(1:12, size = par[2]),
                         strategy = strategy,
                         iter = 10000,
                         alpha = 0.05))
}

simresults.outHack$firstsig <- apply(cond.outHack, 1, function(x) {
  simmultiple.outHack(x, strategy = "firstsig")
})

simresults.outHack$smallest <- apply(cond.outHack, 1, function(x) {
  simmultiple.outHack(x, strategy = "smallest")
})

simresults.outHack$smallestsig <- apply(cond.outHack, 1, function(x) {
  simmultiple.outHack(x, strategy = "smallest.sig")
})

save(simresults.outHack, file = "simulations/SIM_outHack.RData")

