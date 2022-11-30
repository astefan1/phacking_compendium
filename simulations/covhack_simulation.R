# Exploiting Covariates: Simulation

#### Conditions ####
nobs.group <- c(30, 50, 100, 300)
ncov <- c(3, 5, 10)
rcov <- c(0, 0.3, 0.8)
rcovdv <- c(0, 0.3)
strategy <- c("firstsig", "smallest", "smallest.sig")

cond.covhack <- expand.grid(nobs.group, ncov, rcov, rcovdv)

#### Simulation ####

simresults.covhack <- list()

simmultiple.covhack <- function(par, strategy){
  data.frame(sim.covhack(nobs.group = par[1],
                         ncov = par[2],
                         rcov = par[3],
                         rcovdv = par[4],
                         strategy = strategy,
                         interactions = FALSE,
                         iter = 10000,
                         alpha = 0.05))
}
  
simresults.covhack$firstsig <- apply(cond.covhack, 1, function(x) {
  simmultiple.covhack(x, strategy = "firstsig")
})

simresults.covhack$smallest <- apply(cond.covhack, 1, function(x) {
  simmultiple.covhack(x, strategy = "smallest")
})

simresults.covhack$smallestsig <- apply(cond.covhack, 1, function(x) {
  simmultiple.covhack(x, strategy = "smallest.sig")
})

save(simresults.covhack, file = "simulations/SIM_covhack.RData")


