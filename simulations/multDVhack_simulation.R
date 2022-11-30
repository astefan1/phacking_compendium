# Selective Reporting of the Dependent Variable: Simulation

#### Conditions ####

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

#### Simulation ####

simresults.multDVhack <- list()

simmultiple.multDVhack <- function(par, strategy){
  data.frame(sim.multDVhack(nobs.group = par[1],
                 nvar = par[2],
                 r = par[3],
                 strategy = strategy,
                 iter = 10000,
                 alternative = "two.sided",
                 alpha = 0.05))
}

simresults.multDVhack$firstsig <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "firstsig")
                                        })

simresults.multDVhack$smallest <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "smallest")
})

simresults.multDVhack$smallestsig <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "smallest.sig")
})

save(simresults.multDVhack, file = "simulations/SIM_multDVhack.RData")

