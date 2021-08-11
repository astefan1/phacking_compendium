# Favorable Imputation: Simulation

#### Conditions ####
nobs <- c(30, 50, 100, 300)
missing <- c(0.05, 0.2)
howmany = c(3, 5, 10)

cond.impHack <- expand.grid(nobs, missing, howmany)

#### Simulation ####
simresults.impHack <- list()

simmultiple.impHack <- function(par, strategy){
  data.frame(sim.impHack(nobs = par[1],
                         missing = par[2],
                         which = sample(1:10, size = par[3]),
                         strategy = strategy,
                         iter = 1000,
                         alpha = 0.05))
                            
  
}

simresults.impHack$firstsig <- apply(cond.impHack, 1, function(x) {
  simmultiple.impHack(x, strategy = "firstsig")
})

simresults.impHack$smallest <- apply(cond.impHack, 1, function(x) {
  simmultiple.impHack(x, strategy = "smallest")
})

simresults.impHack$smallestsig <- apply(cond.impHack, 1, function(x) {
  simmultiple.impHack(x, strategy = "smallest.sig")
})
