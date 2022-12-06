# ==============================================================================
# Plot: p-curves
# ==============================================================================

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)

# ==============================================================================
# Plotting function: p-curve
# ==============================================================================

plot_pCurve <- function(simdat, conddat, nobs, Var3, Var4, strategy, iter=10000, ymax=0.1, valueNoHack=1){
  
  # Select conditions to show in the plot
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == Var3 & conddat$Var4 == Var4)
  
  # p-curve values
  ps <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ps.hack))
  ps <- c(simdat[[strategy]][[cond.plot[1]]]$ps.orig, ps)
  Var2 <- rep(c(valueNoHack, conddat[cond.plot,]$Var2), each=iter)
  
  pvals.intervals <- simplify2array(tapply(ps, Var2, function(x) table(cut(x[x < 0.1], breaks = seq(0, 0.1, by = 0.01)))))/10000
  pvals.intervals <- data.frame(pvals.intervals)
  pvals.intervals$ints <- c(1:10)
  pvals.intervals$title <- switch(strategy,
                                  "firstsig" = "first significant p-value",
                                  "smallestsig" = "smallest significant p-value",
                                  "smallest" = "smallest p-value")
  yvars <- grep("X", colnames(pvals.intervals))
  color <- c("#9AD3EB", "#6AACCC", "#2F7CA6", "#125A84")
  
  # Plot
  plot1 <- ggplot(pvals.intervals, aes(x=ints)) +
    coord_cartesian(ylim = c(0,ymax)) +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25)) +
    scale_x_continuous(breaks = c(1:10), labels = seq(0.01, 0.1, by=0.01)) +
    scale_y_continuous(breaks = seq(0, ymax, length.out=5), 
                       labels = paste0(seq(0, ymax*100, length.out = 5), " %"), 
                       position = "right") +
    facet_grid(. ~ title) +
    labs(y = "Percentage of p-values",
         x = "p-value") +
    geom_hline(yintercept = 0.01, col = "grey", lty="dashed", lwd=1.5) +
    geom_vline(xintercept = 5, col = "grey", lwd=1.5, lty="dashed")
          
  for(i in yvars){
    plot1 <- plot1 + geom_line(aes_string(y=colnames(pvals.intervals)[i]), colour=color[i], lwd=1.5)
  }
  
  return(plot1)
  
}

# ==============================================================================
# Plotting function: p-value distribution
# ==============================================================================

plot_pDist <- function(simdat, conddat, nobs, Var3, Var4, strategy, xlab, iter=10000, valueNoHack=1){
  
  # Select conditions to show in the plot
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == Var3 & conddat$Var4 == Var4)
  
  # p-value distribution
  ps <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ps.hack))
  ps <- c(simdat[[strategy]][[cond.plot[1]]]$ps.orig, ps)
  Var2 <- rep(c(valueNoHack, conddat[cond.plot,]$Var2), each=iter)
  
  plotdat <- data.frame(ps=ps,
                        Var2=as.factor(Var2))
  plotdat$title <- switch(strategy,
                          "firstsig" = "first significant p-value",
                          "smallestsig" = "smallest significant p-value",
                          "smallest" = "smallest p-value")
  
  ggplot(data=plotdat, aes(x = Var2, y = ps)) +
    geom_jitter(aes(colour = Var2), shape = 16, position = position_jitter(0.2), alpha = 0.1) +
    geom_violin(fill = NA, aes(colour = Var2), lwd=1.5) +
    labs(x = xlab,
         y = "p-value") +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25),
          legend.position = "none") +
    facet_grid(. ~ title) +
    scale_color_manual(values = c("#9AD3EB", "#6AACCC", "#2F7CA6", "#125A84"))
  
  
  
}

# ==============================================================================
# Strategy 1: Multiple dependent variables
# ==============================================================================

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)
cond.multDVhack$Var4 <- 0

plot_pCurve(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="firstsig", Var4=0)
plot_pCurve(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="smallestsig", Var4=0)
plot_pCurve(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="smallest", Var4=0)

plot_pDist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="firstsig", Var4=0, xlab="Number of dependent variables")
plot_pDist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="smallestsig", Var4=0, xlab="Number of dependent variables")
plot_pDist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs=100, Var3 = 0.3, strategy="smallest", Var4=0, xlab="Number of dependent variables")

# ==============================================================================
# Strategy 2: Multiple independent variables
# ==============================================================================

nobs.group <- c(30, 50, 100, 300)
nvar <- c(3, 5, 10)
r <- c(0, 0.3, 0.8)

cond.multIVhack <- expand.grid(nobs.group, nvar, r)
cond.multIVhack$Var4 <- 0

plot_pCurve(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="firstsig", Var4=0)
plot_pCurve(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="smallestsig", Var4=0)
plot_pCurve(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="smallest", Var4=0)

plot_pDist(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="firstsig", Var4=0, xlab="Number of independent variables")
plot_pDist(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="smallestsig", Var4=0, xlab="Number of independent variables")
plot_pDist(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs=100, Var3 = 0.3, strategy="smallest", Var4=0, xlab="Number of independent variables")

# ==============================================================================
# Strategy 3: Optional Stopping
# ==============================================================================

n.min <- 5
n.max <- c(30, 50, 100, 300)
step <- c(1, 5, 10, 50)

cond.optstop_nmax <- expand.grid(n.max, step)
cond.optstop_nmax$Var3 <- cond.optstop_nmax$Var4 <- 0

simresults.optstop_nmax1 <- list(firstsig = simresults.optstop_nmax)

plot_pCurve(simdat=simresults.optstop_nmax1, conddat = cond.optstop_nmax, nobs=100, Var3 = 0, strategy="firstsig", Var4=0, ymax=0.2)

plot_pDist(simdat=simresults.optstop_nmax1, conddat = cond.optstop_nmax, nobs=100, Var3 = 0, strategy="firstsig", Var4=0, xlab="Step Size")

# ==============================================================================
# Strategy 4: Outlier exclusion
# ==============================================================================

nobs <- c(30, 50, 100, 300)
howmany <- c(3, 5, 12)

cond.outHack <- expand.grid(nobs, howmany)
cond.outHack$Var3 <- cond.outHack$Var4 <- 0

plot_pCurve(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="firstsig")
plot_pCurve(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallestsig")
plot_pCurve(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallest")

plot_pDist(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="firstsig", xlab="Number of Outlier Exclusion Methods", valueNoHack = 0)
plot_pDist(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallestsig", xlab="Number of Outlier Exclusion Methods", valueNoHack = 0)
plot_pDist(simdat=simresults.outHack, conddat = cond.outHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallest", xlab="Number of Outlier Exclusion Methods", valueNoHack = 0)

# ==============================================================================
# Strategy 5: Controlling Covariates
# ==============================================================================

nobs.group <- c(30, 50, 100, 300)
ncov <- c(3, 5, 10)
rcov <- c(0, 0.3, 0.8)
rcovdv <- c(0, 0.3)

cond.covhack <- expand.grid(nobs.group, ncov, rcov, rcovdv)

plot_pCurve(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="firstsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="smallestsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="smallest", valueNoHack = 0)

plot_pDist(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="firstsig", xlab = "Number of Covariates", valueNoHack = 0)
plot_pDist(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="smallestsig", xlab = "Number of Covariates", valueNoHack = 0)
plot_pDist(simdat=simresults.covhack, conddat = cond.covhack, nobs=100, Var3 = 0, Var4 = 0.3, strategy="smallest", xlab = "Number of Covariates", valueNoHack = 0)

# ==============================================================================
# Strategy 6: Scale redefinition
# ==============================================================================

nobs <- c(30, 50, 100, 300)
ncompv <- c(5, 10) 
rcomp <- c(0.3, 0.7)
ndelete <- c(1, 3, 7)

cond.compscoreHack <- expand.grid(nobs, ncompv, rcomp, ndelete)
cond.compscoreHack <- cond.compscoreHack[cond.compscoreHack$Var4 < cond.compscoreHack$Var2, ]
colnames(cond.compscoreHack) <- c("Var1", "Var4", "Var3", "Var2")

plot_pCurve(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="firstsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="smallestsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="smallest", valueNoHack = 0)

plot_pDist(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="firstsig", xlab = "Number of Deleted Items", valueNoHack = 0)
plot_pDist(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="smallestsig", xlab = "Number of Deleted Items", valueNoHack = 0)
plot_pDist(simdat=simresults.compscoreHack, conddat = cond.compscoreHack, nobs=100, Var3 = 0.7, Var4 = 10, strategy="smallest", xlab = "Number of Deleted Items", valueNoHack = 0)

# ==============================================================================
# Strategy 7: Variable transformation
# ==============================================================================

nobs <- c(30, 50, 100, 300)
transvar <- c(1:3)

cond.varTransHack <- expand.grid(nobs, transvar)
cond.varTransHack$Var3 <- cond.varTransHack$Var4 <- 0

plot_pCurve(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="firstsig", ymax=0.2, valueNoHack = 0)
plot_pCurve(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallestsig", ymax=0.2, valueNoHack = 0)
plot_pCurve(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallest", ymax=0.2, valueNoHack = 0)

plot_pDist(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="firstsig", xlab="Transformed Variables", valueNoHack = 0)
plot_pDist(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallestsig", xlab="Transformed Variables", valueNoHack = 0)
plot_pDist(simdat=simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, Var3 = 0, Var4 = 0, strategy="smallest", xlab="Transformed Variables", valueNoHack = 0)

# ==============================================================================
# Strategy 8: Discretizing variables
# ==============================================================================

nobs <- c(30, 50, 100, 300)

cond.cutoffHack <- expand.grid(nobs)
cond.cutoffHack$Var2 <- cond.cutoffHack$Var3 <- cond.cutoffHack$Var4 <- 1 

plot_pCurve(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="firstsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallestsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallest", valueNoHack = 0)

plot_pDist(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="firstsig", xlab="Discretized Variable", valueNoHack = 0)
plot_pDist(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallestsig", xlab="Discretized Variable", valueNoHack = 0)
plot_pDist(simdat=simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallest", xlab="Discretized Variable", valueNoHack = 0)


# ==============================================================================
# Strategy 9: Exploiting alternative hypothesis tests
# ==============================================================================

nobs.group <- c(30, 50, 100, 300)

cond.statAnalysisHack <- expand.grid(nobs.group)
cond.statAnalysisHack$Var2 <- cond.statAnalysisHack$Var3 <- cond.statAnalysisHack$Var4 <- 1

plot_pCurve(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="firstsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallestsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallest", valueNoHack = 0)

plot_pDist(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="firstsig", valueNoHack = 0, xlab="Alternative Hypothesis Tests")
plot_pDist(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallestsig", valueNoHack = 0, xlab="Alternative Hypothesis Tests")
plot_pDist(simdat=simresults.statAnalysisHack, conddat = cond.statAnalysisHack, nobs=100, Var3 = 1, Var4 = 1, strategy="smallest", valueNoHack = 0, xlab="Alternative Hypothesis Tests")

# ==============================================================================
# Strategy 10: Favorable imputation
# ==============================================================================

nobs <- c(30, 50, 100, 300)
missing <- c(0.05, 0.2)
howmany = c(3, 5, 10)

cond.impHack <- expand.grid(nobs, missing, howmany)
colnames(cond.impHack) <- c("Var1", "Var3", "Var2")
cond.impHack$Var4 <- 4

plot_pCurve(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="firstsig", ymax=0.2, valueNoHack = 0)
plot_pCurve(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="smallestsig", ymax=0.2, valueNoHack = 0)
plot_pCurve(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="smallest", ymax=0.2, valueNoHack = 0)

plot_pDist(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="firstsig", xlab="Number of Imputation Methods", valueNoHack = 0)
plot_pDist(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="smallestsig", xlab="Number of Imputation Methods", valueNoHack = 0)
plot_pDist(simdat=simresults.impHack, conddat = cond.impHack, nobs=100, Var3 = 0.2, Var4 = 4, strategy="smallest", xlab="Number of Imputation Methods", valueNoHack = 0)

# ==============================================================================
# Strategy 11: Subgroup analyses
# ==============================================================================

nobs.group <-  c(30, 50, 100, 300)
nsubvars <- c(1, 3, 5)

cond.subgroupHack <- expand.grid(nobs.group, nsubvars)
cond.subgroupHack$Var3 <- cond.subgroupHack$Var4 <- 4

plot_pCurve(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="firstsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="smallestsig", valueNoHack = 0)
plot_pCurve(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="smallest",valueNoHack = 0)

plot_pDist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="firstsig",valueNoHack = 0, xlab="Number of Subgroup Variables")
plot_pDist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="smallestsig",valueNoHack = 0, xlab="Number of Subgroup Variables")
plot_pDist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs=100, Var3 = 4, Var4 = 4, strategy="smallest",valueNoHack = 0, xlab="Number of Subgroup Variables")

# ==============================================================================
# Strategy 12: Incorrect rounding
# ==============================================================================

nobs <- 100
roundinglevel <- c(0.051, 0.055, 0.1)

cond.roundHack <- expand.grid(nobs, roundinglevel)
cond.roundHack$Var3 <- cond.roundHack$Var4 <- 2

simresults.roundHack1 <- list(firstsig = simresults.roundHack)

plot_pCurve(simdat=simresults.roundHack1, conddat = cond.roundHack, nobs=100, Var3 = 2, Var4 = 2, strategy="firstsig")
plot_pDist(simdat=simresults.roundHack1, conddat = cond.roundHack, nobs=100, Var3 = 2, Var4 = 2, strategy="firstsig", xlab="Implied Significance Level", valueNoHack = 0.05)


