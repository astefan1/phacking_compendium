# Variable Transformation: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
transvar <- c(1:3)

cond.varTransHack <- expand.grid(nobs, transvar)

#### Simulation without tests of normality of residuals ####
simresults.varTransHack_nonormtest <- list()

simmultiple.varTransHack <- function(par, strategy){
  data.frame(sim.varTransHack(nobs = par[1],
                              transvar = switch(par[2],
                                                "x" = 1,
                                                "y" = 2,
                                                "xy" = 3),
                              testnorm = FALSE,
                              strategy = strategy,
                              iter = 10000,
                              alpha = 0.05
  ))
}

simresults.varTransHack_nonormtest$firstsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "firstsig")
})

simresults.varTransHack_nonormtest$smallest <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest")
})

simresults.varTransHack_nonormtest$smallestsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest.sig")
})

save(simresults.varTransHack_nonormtest, file = "simulations/SIM_varTransHack_nonormtest.RData")

#### Simulation with tests of normality of residuals ####

simresults.varTransHack_normtest <- list()

simmultiple.varTransHack <- function(par, strategy){
  data.frame(sim.varTransHack(nobs = par[1],
                              transvar = switch(par[2],
                                                "x" = 1,
                                                "y" = 2,
                                                "xy" = 3),
                              testnorm = TRUE,
                              strategy = strategy,
                              iter = 10000,
                              alpha = 0.05
  ))
}

simresults.varTransHack_normtest$firstsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "firstsig")
})

simresults.varTransHack_normtest$smallest <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest")
})

simresults.varTransHack_normtest$smallestsig <- apply(cond.varTransHack, 1, function(x) {
  simmultiple.varTransHack(x, strategy = "smallest.sig")
})

save(simresults.varTransHack_normtest, file = "simulations/SIM_varTransHack_normtest.RData")

