# Selective Reporting of the Independent Variable: Simulation

#### Conditions ####

nobs.group <- c(30, 50, 100, 300)
nvar <- c(3, 5, 10)
r <- c(0, 0.3, 0.8)

cond.multIVhack <- expand.grid(nobs.group, nvar, r)

#### Simulation t-Test ####
simresults.multIVhack_ttest <- list()

simmultiple.multIVhack_ttest <- function(par, strategy){
  data.frame(sim.multIVhack(nobs.group = par[1],
                            nvar = par[2],
                            r = par[3],
                            regression = FALSE,
                            strategy = strategy,
                            iter = 10000,
                            alternative = "two.sided",
                            alpha = 0.05))
}

simresults.multIVhack_ttest$firstsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_ttest(x, strategy = "firstsig")
})

simresults.multIVhack_ttest$smallest <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_ttest(x, strategy = "smallest")
})

simresults.multIVhack_ttest$smallestsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_ttest(x, strategy = "smallest.sig")
})

save(simresults.multIVhack_ttest, file = "simulations/SIM_multIVhack_ttest.RData")

### Simulation regression ####

simresults.multIVhack_reg <- list()

simmultiple.multIVhack_reg <- function(par, strategy){
  data.frame(sim.multIVhack(nobs.group = par[1],
                            nvar = par[2],
                            r = par[3],
                            regression = TRUE,
                            strategy = strategy,
                            iter = 10000,
                            alternative = "two.sided",
                            alpha = 0.05))
}

simresults.multIVhack_reg$firstsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_reg(x, strategy = "firstsig")
})

simresults.multIVhack_reg$smallest <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_reg(x, strategy = "smallest")
})

simresults.multIVhack_reg$smallestsig <- apply(cond.multIVhack, 1, function(x) {
  simmultiple.multIVhack_reg(x, strategy = "smallest.sig")
})

save(simresults.multIVhack_reg, file = "simulations/SIM_multIVhack_reg.RData")



