# ==============================================================================
# Shiny App: Simulation for Plots at Start
# ==============================================================================

startplots <- list()

# 1: Composite Scores

res1 <- sim.compscoreHack(nobs=30, ncompv=5, rcomp=0.8, ndelete=2, strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$compscorePlot <- pplots(simdat=res1, alpha=0.05)
startplots$compscorePlotES <- esplots(simdat=res1, EScolumn.hack=3, EScolumn.orig=4)
startplots$compscore.fprate.p <- paste0(round(sum(res1[,"ps.hack"] < 0.05)/1000*100, 2), " %")
startplots$compscore.fprate.o <- paste0(round(sum(res1[,"ps.orig"] < 0.05)/1000*100, 2), " %")

# 2: Exploit Covariates

res2 <- sim.covhack(nobs.group = 30, ncov = 3, rcov = 0.3, rcovdv = 0.5, interactions = FALSE, strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$expCovPlot <- pplots(simdat=res2, alpha=0.05)
startplots$expCovES <- esplots(simdat=res2, EScolumn.hack=3, EScolumn.orig=4, titles = c(expression("Distribution of p-hacked effect sizes "*eta^2),
                                                                              expression("Distribution of original effect sizes "*eta^2)))
startplots$expcov.fprate.p <- paste0(round(sum(res2[,"ps.hack"] < 0.05)/1000*100, 2), " %")
startplots$expcov.fprate.o <- paste0(round(sum(res2[,"ps.orig"] < 0.05)/1000*100, 2), " %")

# 3: Exploit Cutoffs

res3 <- sim.cutoffHack(nobs = 30, strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$expCutPlot <- pplots(simdat=res3, alpha=0.05)
startplots$expCutES <- esplots(simdat=res3, EScolumn.hack=3, EScolumn.orig=4)
startplots$expcut.fprate.p <- paste0(round(sum(res3[,"ps.hack"] < 0.05)/1000*100, 2), " %")
startplots$expcut.fprate.o <- paste0(round(sum(res3[,"ps.orig"] < 0.05)/1000*100, 2), " %")

# 4: Favorable Imputation
res4 <- sim.impHack(nobs = 30, missing = 0.1, which = c(1:3), strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$favImpPlot <- pplots(simdat=res4, alpha=0.05)
startplots$favImpES <- esplots(simdat=res4, EScolumn.hack=3, EScolumn.orig=4)
startplots$favimp.fprate.p <- paste0(round(sum(res4[,"ps.hack"] < 0.05)/1000*100, 2), " %")
startplots$favimp.fprate.o <- paste0(round(sum(res4[,"ps.orig"] < 0.05)/1000*100, 2), " %")

# 5: Incorrect Rounding
res5 <- sim.roundhack(roundinglevel = 0.051, iter = 1000, alternative = "two.sided", alpha = 0.05)
startplots$roundingPlot <- pplots(simdat=res5, alpha=0.05)
startplots$roundingES <- esplots(simdat=res5, EScolumn.hack=3, EScolumn.orig=4)
startplots$rounding.fprate.p <- paste0(sum(round(res5[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$rounding.fprate.o <- paste0(sum(round(res5[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 6: Optional Stopping
res6 <- sim.optstop(n.min = 10, n.max = 100, step = 1, alternative = "two.sided", iter = 1000, alpha = 0.05)
startplots$optstopPlot <- pplots(simdat = res6, alpha = 0.05)
startplots$optstopESr2 <- esplots(simdat=res6, EScolumn.hack=3, EScolumn.orig=4)
startplots$optstopESd <- esplots(simdat=res6, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                expression("Distribution of original effect sizes "*delta)))
startplots$optstop.fprate.p <- paste0(round(sum(res6[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$optstop.fprate.o <- paste0(round(sum(res6[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 7: Outlier Exclusion
res7 <- sim.outHack(nobs = 30, which = c(1:2), strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$outExclPlot <- pplots(simdat = res7, alpha = 0.05)
startplots$outExclES <- esplots(simdat = res7, EScolumn.hack = 3, EScolumn.orig = 4)
startplots$outExcl.fprate.p <- paste0(round(sum(res7[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$outExcl.fprate.o <- paste0(round(sum(res7[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 8: Selective Reporting of Effects
res8 <- sim.selectEffects(nobs = 30, niv = 3, interactions = FALSE, riv = 0.5, strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$selectEffPlot <- pplots(simdat = res8, alpha = 0.05)
startplots$selectEffES <- esplots(simdat = res8, EScolumn.hack = 3, EScolumn.orig = 4)
startplots$selectEff.fprate.p <- paste0(round(sum(res8[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$selectEff.fprate.o <- paste0(round(sum(res8[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 9: Selective Reporting DV
res9 <- sim.multDVhack(nobs.group = 30, nvar = 5, r = 0.5, strategy = "firstsig", iter = 1000, alternative = "two.sided", alpha = 0.05)
startplots$SRDVPlot <- pplots(simdat = res9, alpha = 0.05)
startplots$SRDVESr2 <- esplots(simdat=res9, EScolumn.hack=3, EScolumn.orig=4)
startplots$SRDVESd <- esplots(simdat=res9, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                             expression("Distribution of original effect sizes "*delta)))
startplots$SRDV.fprate.p <- paste0(round(sum(res9[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$SRDV.fprate.o <- paste0(round(sum(res9[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 10: Selective Reporting IV
res10 <- sim.multDVhack(nobs.group = 30, nvar = 5, r = 0.5, strategy = "firstsig", iter = 1000, alternative = "two.sided", alpha = 0.05)
startplots$SRIVPlot <- pplots(simdat = res10, alpha = 0.05)
startplots$SRIVESr2 <- esplots(simdat=res10, EScolumn.hack=3, EScolumn.orig=4)
startplots$SRIVESd <- esplots(simdat=res10, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                              expression("Distribution of original effect sizes "*delta)))
startplots$SRIV.fprate.p <- paste0(round(sum(res10[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$SRIV.fprate.o <- paste0(round(sum(res10[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 11: Statistical Analyses
res11 <- sim.statAnalysisHack(nobs.group = 30, strategy = "firstsig", alternative = "two.sided", alpha = 0.05, iter = 1000)
startplots$statAnalysisPlot <- pplots(simdat = res11, alpha = 0.05)
startplots$statAnalysis.fprate.p <- paste0(round(sum(res11[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$statAnalysis.fprate.o <- paste0(round(sum(res11[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 12: Subgroup Analyses
res12 <- sim.subgroupHack(nobs.group = 30, nsubvars = 5, alternative = "two.sided", strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$subgroupPlot <- pplots(simdat = res12, alpha = 0.05)
startplots$subgroupESr2 <- esplots(simdat=res12, EScolumn.hack=3, EScolumn.orig=4)
startplots$subgroupESd <- esplots(simdat=res12, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                  expression("Distribution of original effect sizes "*delta)))
startplots$subgroup.fprate.p <- paste0(round(sum(res12[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$subgroup.fprate.o <- paste0(round(sum(res12[,"ps.orig"] <= 0.05)/1000*100, 2), " %")

# 13: Variable Transformations
res13 <- sim.varTransHack(nobs = 30, transvar = "x", strategy = "firstsig", alpha = 0.05, iter = 1000)
startplots$varTransPlot <- pplots(simdat = res13, alpha = 0.05)
startplots$varTransES <- esplots(simdat = res13, EScolumn.hack = 3, EScolumn.orig = 4)
startplots$varTrans.fprate.p <- paste0(round(sum(res13[,"ps.hack"] <= 0.05)/1000*100, 2), " %")
startplots$varTrans.fprate.o <- paste0(round(sum(res13[,"ps.orig"] <= 0.05)/1000*100, 2), " %")


saveRDS(startplots, file="./inst/shiny-phack/ShinyPHack/data/startplots.rds")

