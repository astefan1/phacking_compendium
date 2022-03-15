# Variable Transformation: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
transvar <- c(1:3)

cond.varTransHack <- expand.grid(nobs, transvar)

#### Simulation ####
simresults.varTransHack <- list()

simmultiple.varTransHack <- function(par, strategy){
  data.frame(sim.varTransHack(nobs = par[1],
                              transvar = switch(par[2],
                                                "x" = 1,
                                                "y" = 2,
                                                "xy" = 3),
                              strategy = strategy,
                              iter = 1000,
                              alpha = 0.05
  ))
}

simresults.varTransHack$firstsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "firstsig")
})

simresults.varTransHack$smallest <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest")
})

simresults.varTransHack$smallestsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest.sig")
})

save(simresults.varTransHack, file = "simulations/SIM_varTransHack.RData")
