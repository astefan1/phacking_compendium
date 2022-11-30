# Scale Redefinition / Composite Scores: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
ncompv <- c(5, 10) 
rcomp <- c(0.3, 0.7)
ndelete <- c(1, 3, 7)
strategy <- c("firstsig", "smallest", "smallest.sig")

cond.compscoreHack <- expand.grid(nobs, ncompv, rcomp, ndelete)
cond.compscoreHack <- cond.compscoreHack[cond.compscoreHack$Var4 < cond.compscoreHack$Var2, ]

#### Simulation ####

simresults.compscoreHack <- list()

simmultiple.compscoreHack <- function(par, strategy){
  data.frame(sim.compscoreHack(nobs = par[1],
                            ncompv = par[2],
                            rcomp = par[3],
                            ndelete = par[4],
                            strategy = strategy,
                            iter = 10000,
                            alpha = 0.05))
}

simresults.compscoreHack$firstsig <- apply(cond.compscoreHack, 1, function(x) {
  simmultiple.compscoreHack(x, strategy = "firstsig")
})

simresults.compscoreHack$smallest <- apply(cond.compscoreHack, 1, function(x) {
  simmultiple.compscoreHack(x, strategy = "smallest")
})

simresults.compscoreHack$smallestsig <- apply(cond.compscoreHack, 1, function(x) {
  simmultiple.compscoreHack(x, strategy = "smallest.sig")
})

save(simresults.compscoreHack, file = "simulations/SIM_compscoreHack.RData")
