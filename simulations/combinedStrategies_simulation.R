# Combined p-Hacking Strategies: Simulations

SIM_combinedHack_t <- sim.combined.t(nobs.group = 100,
                                     nDV = 5,
                                     rDV = 0.6,
                                     nCOV = 3,
                                     rCOV = 0.3,
                                     rcovdv = 0.3,
                                     nSUB = 3,
                                     roundinglevel = 0.051,
                                     alternative = "two.sided",
                                     strategy = "firstsig",
                                     alpha = 0.05,
                                     iter = 10000)

save(SIM_combinedHack_t, file = "simulations/SIM_combinedHack_t.RData")
               
SIM_combinedHack_reg <- sim.combined.reg(nobs = 100, 
                                         missing = 0.1, 
                                         ncompv = 5, 
                                         rcomp = 0.75, 
                                         roundinglevel = 0.051, 
                                         nImpMethods = 5, 
                                         transvar = "xy", 
                                         ndelete = 3, 
                                         nOutMethods = 3, 
                                         strategy = "firstsig", 
                                         alpha = 0.05, 
                                         iter = 10000)

save(SIM_combinedHack_reg, file = "simulations/SIM_combinedHack_reg.RData")


# Combined p-Hacking Strategies: Simulations with alpha = 0.005

SIM_combinedHack_t_005 <- sim.combined.t(nobs.group = 100,
                                     nDV = 5,
                                     rDV = 0.6,
                                     nCOV = 3,
                                     rCOV = 0.3,
                                     rcovdv = 0.3,
                                     nSUB = 3,
                                     roundinglevel = 0.0051,
                                     alternative = "two.sided",
                                     strategy = "firstsig",
                                     alpha = 0.005,
                                     iter = 10000)

save(SIM_combinedHack_t_005, file = "simulations/SIM_combinedHack_t_005.RData")

SIM_combinedHack_reg_005 <- sim.combined.reg(nobs = 100, 
                                         missing = 0.1, 
                                         ncompv = 5, 
                                         rcomp = 0.75, 
                                         roundinglevel = 0.0051, 
                                         nImpMethods = 5, 
                                         transvar = "xy", 
                                         ndelete = 3, 
                                         nOutMethods = 3, 
                                         strategy = "firstsig", 
                                         alpha = 0.005, 
                                         iter = 10000)

save(SIM_combinedHack_reg_005, file = "simulations/SIM_combinedHack_reg_005.RData")
