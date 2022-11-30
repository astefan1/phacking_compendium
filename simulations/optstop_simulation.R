# Optional Stopping: Simulation

#### Change n.max ####

# Conditions
n.min <- 5
n.max <- c(30, 50, 100, 300)
step <- c(1, 5, 10, 50)

cond.optstop_nmax <- expand.grid(n.max, step)

simresults.optstop_nmax <- list()

simmultiple.optstop_nmax <- function(par){
  data.frame(sim.optstop(n.min = 5,
                         n.max = par[1],
                         step = par[2],
                         alternative = "two.sided",
                         iter = 10000,
                         alpha = 0.05))
  
}

simresults.optstop_nmax <- apply(cond.optstop_nmax, 1, function(x) {
  simmultiple.optstop_nmax(x)
})

save(simresults.optstop_nmax, file = "simulations/SIM_optstop_nmax.RData")

#### Change n.min ####
n.min <- c(5, 30, 50, 100)
n.max <- 300
step <- c(1, 5, 10, 50)

cond.optstop_nmin <- expand.grid(n.min, step)

#### Simulation ####

simresults.optstop_nmin <- list()

simmultiple.optstop_nmin <- function(par){
  data.frame(sim.optstop(n.min = par[1],
                         n.max = 300,
                         step = par[2],
                         alternative = "two.sided",
                         iter = 10000,
                         alpha = 0.05))
  
}

simresults.optstop_nmin <- apply(cond.optstop_nmin, 1, function(x) {
  simmultiple.optstop_nmin(x)
})

save(simresults.optstop_nmin, file = "simulations/SIM_optstop_nmin.RData")
