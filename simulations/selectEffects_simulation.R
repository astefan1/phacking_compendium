# Selecting Effects: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
niv <- c(3, 5, 10)
riv <- c(0, 0.3, 0.7)

cond.selectEffects <- expand.grid(nobs, niv, riv)

#### Simulation ####
simresults.selectEffects <- list()

simmultiple.selectEffects <- function(par, strategy){
  data.frame(sim.selectEffects(nobs = par[1],
                               niv = par[2],
                               riv = par[3],
                               interactions = FALSE,
                               strategy = strategy,
                               iter = 1000,
                               alpha = 0.05
                         ))
}

simresults.selectEffects$firstsig <- apply(cond.selectEffects, 1, function(x) {
  simmultiple.selectEffects(x, strategy = "firstsig")
})

simresults.selectEffects$smallest <- apply(cond.selectEffects, 1, function(x) {
  simmultiple.selectEffects(x, strategy = "smallest")
})

simresults.selectEffects$smallestsig <- apply(cond.selectEffects, 1, function(x) {
  simmultiple.selectEffects(x, strategy = "smallest.sig")
})

