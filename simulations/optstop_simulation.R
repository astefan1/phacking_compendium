# Optional Stopping: Simulation

#### Conditions ####
n.min <- 5
n.max <- c(30, 50, 100, 300)
step = c(1, 5, 10, 50)

cond.optstop <- expand.grid(n.max, step)

#### Simulation ####
simresults.optstop <- list()

simmultiple.optstop <- function(par){
  data.frame(sim.optstop(n.min = 5,
                         n.max = par[1],
                         step = par[2],
                         alternative = "two.sided",
                         iter = 1000,
                         alpha = 0.05))
  
}

simresults.optstop <- apply(cond.optstop, 1, function(x) {
  simmultiple.optstop(x)
})

