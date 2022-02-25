# Exploit Statistical Analysis Options: Simulation

#### Conditions ####
nobs.group <- c(30, 50, 100, 300)

cond.statAnalysisHack <- expand.grid(nobs.group)

#### Simulation ####
simresults.statAnalysisHack <- list()

simmultiple.statAnalysisHack <- function(par, strategy){
  data.frame(sim.statAnalysisHack(nobs.group = par[1],
                                  strategy = strategy,
                                  iter = 1000,
                                  alternative = "two.sided",
                                  alpha = 0.05
                            ))
}

simresults.statAnalysisHack$firstsig <- apply(cond.statAnalysisHack, 1, function(x) {
  simmultiple.statAnalysisHack(x, strategy = "firstsig")
})

simresults.statAnalysisHack$smallest <- apply(cond.statAnalysisHack, 1, function(x) {
  simmultiple.statAnalysisHack(x, strategy = "smallest")
})

simresults.statAnalysisHack$smallestsig <- apply(cond.statAnalysisHack, 1, function(x) {
  simmultiple.statAnalysisHack(x, strategy = "smallest.sig")
})
