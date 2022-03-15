# Selective Reporting of the Independent Variable: Simulation

#### Conditions ####

nobs.group <- c(30, 50, 100, 300)
nvar <- c(3, 5, 10)
r <- c(0, 0.3, 0.8)

cond.multIVhack <- expand.grid(nobs.group, nvar, r)

#### Simulation ####
simresults.multIVhack <- list()

simmultiple.multIVhack <- function(par, strategy){
  data.frame(sim.multIVhack(nobs.group = par[1],
                            nvar = par[2],
                            r = par[3],
                            strategy = strategy,
                            iter = 1000,
                            alternative = "two.sided",
                            alpha = 0.05))
}

simresults.multIVhack$firstsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack(x, strategy = "firstsig")
})

simresults.multIVhack$smallest <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack(x, strategy = "smallest")
})

simresults.multIVhack$smallestsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack(x, strategy = "smallest.sig")
})

save(simresults.multIVhack, file = "simulations/SIM_multIVhack.RData")

