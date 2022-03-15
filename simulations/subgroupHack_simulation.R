# Subgroup Analysis: Simulation

#### Conditions ####
nobs.group <-  c(30, 50, 100, 300)
nsubvars <- c(1, 3, 5)

cond.subgroupHack <- expand.grid(nobs.group, nsubvars)

#### Simulation ####
simresults.subgroupHack <- list()

simmultiple.subgroupHack <- function(par, strategy){
  data.frame(sim.subgroupHack(nobs.group = par[1],
                              nsubvars = par[2],
                                  strategy = strategy,
                                  iter = 1000,
                                  alternative = "two.sided",
                                  alpha = 0.05
  ))
}

simresults.subgroupHack$firstsig <- apply(cond.subgroupHack, 1, function(x) {
  simmultiple.subgroupHack(x, strategy = "firstsig")
})

simresults.subgroupHack$smallest <- apply(cond.subgroupHack, 1, function(x) {
  simmultiple.subgroupHack(x, strategy = "smallest")
})

simresults.subgroupHack$smallestsig <- apply(cond.subgroupHack, 1, function(x) {
  simmultiple.subgroupHack(x, strategy = "smallest.sig")
})

save(simresults.subgroupHack, file = "simulations/SIM_subgroupHack.RData")

