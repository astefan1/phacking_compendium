# Plot: Effect size distribution for different reporting strategies

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Plot the legend ####
# ------------------------------------------------------------------------------

plot.new()
legend(x = "topleft", legend = c("p-hacked", "original"), fill = c("#FFAE4A", "#5AB4BD"))

# ------------------------------------------------------------------------------
# Plotting functions ####
# ------------------------------------------------------------------------------

# Plotting function for Cohen's d

plot.Ddist <- function(simdat, conddat, nobs, r, strategy="firstsig", ylab, yval){
  
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == r)
  
  ds.hack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ds.hack))
  ds.nohack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ds.orig))
  
  iter <- nrow(simdat[[strategy]][[1]])
  nDV <- rep(conddat[cond.plot,]$Var2, each=iter)
  
  plotdat <- data.frame(ds=ds.hack, 
                        ds.nohack=ds.nohack,
                        nDV=as.factor(nDV))
  
  newplotdat <- plotdat %>% # compute densities from ES
    group_by(nDV) %>%
    do(data.frame(loc.ds = density(.$ds)$x,
                  dens.ds = density(.$ds)$y / (2.2*max(density(.$ds)$y)),
                  loc.ds.nohack = density(.$ds.nohack)$x,
                  dens.ds.nohack = -1*density(.$ds.nohack)$y/(2*max(density(.$ds.nohack)$y))))
  
  newplotdat$dens.ds <- newplotdat$dens.ds + as.numeric(newplotdat$nDV) # y-offset for different number of DVs
  newplotdat$dens.ds.nohack <- newplotdat$dens.ds.nohack + as.numeric(newplotdat$nDV)
  newplotdat$title <- paste0("N = ", nobs)
  
  ggplot(data=newplotdat, aes(group = nDV)) +
    geom_polygon(aes(y=dens.ds, x=loc.ds), fill = "#FFAE4A") +
    geom_polygon(aes(y=dens.ds.nohack, x=loc.ds.nohack), fill = "#5AB4BD") +
    labs(x = "Cohen's d",
         y = ylab) +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25)) +
    scale_y_continuous(breaks = c(1,2,3), labels = as.character(yval)) +
    scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), limits =c(-1, 1)) +
    facet_grid(. ~ title)
}

# Plotting function R-squared

plot.R2dist <- function(simdat, conddat, nobs, r, strategy, ylab, yval){
  
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == r)
  
  rs.hack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$r2s.hack))
  rs.nohack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$r2s.orig))
  
  iter <- nrow(simdat[[strategy]][[1]])
  nDV <- rep(conddat[cond.plot,]$Var2, each=iter)
  
  plotdat <- data.frame(rs=rs.hack, 
                        rs.nohack=rs.nohack,
                        nDV=as.factor(nDV))
  
  newplotdat <- plotdat %>% # compute densities from ES
    group_by(nDV) %>%
    do(data.frame(loc.rs = density(.$rs)$x,
                  dens.rs = density(.$rs)$y / (2.2*max(density(.$rs)$y)),
                  loc.rs.nohack = density(.$rs.nohack)$x,
                  dens.rs.nohack = -1*density(.$rs.nohack)$y/(2*max(density(.$rs.nohack)$y))))
  
  newplotdat$dens.rs <- newplotdat$dens.rs + as.numeric(newplotdat$nDV) # y-offset for different number of DVs
  newplotdat$dens.rs.nohack <- newplotdat$dens.rs.nohack + as.numeric(newplotdat$nDV)
  newplotdat$title <- paste0("N = ", nobs)
  
  ggplot(data=newplotdat, aes(group = nDV)) +
    geom_polygon(aes(y=dens.rs, x=loc.rs), fill = "#FFAE4A") +
    geom_polygon(aes(y=dens.rs.nohack, x=loc.rs.nohack), fill = "#5AB4BD") +
    labs(x = "R squared",
         y = ylab) +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25)) +
    scale_y_continuous(breaks = seq_along(yval), labels = as.character(yval)) +
    coord_cartesian(xlim = c(-0.01, 0.15)) +
    facet_grid(. ~ title)
}

# Plotting function for eta2

plot.eta2dist <- function(simdat, conddat, nobs, r, strategy, ylab, yval){
  
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == r)
  
  etas.hack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$eta2s.hack))
  etas.nohack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$eta2s.orig))
  
  iter <- nrow(simdat[[strategy]][[1]])
  nDV <- rep(conddat[cond.plot,]$Var2, each=iter)
  
  plotdat <- data.frame(etas=etas.hack, 
                        etas.nohack=etas.nohack,
                        nDV=as.factor(nDV))
  
  newplotdat <- plotdat %>% # compute densities from ES
    group_by(nDV) %>%
    do(data.frame(loc.etas = density(.$etas)$x,
                  dens.etas = density(.$etas)$y / (2.2*max(density(.$etas)$y)),
                  loc.etas.nohack = density(.$etas.nohack)$x,
                  dens.etas.nohack = -1*density(.$etas.nohack)$y/(2*max(density(.$etas.nohack)$y))))
  
  newplotdat$dens.etas <- newplotdat$dens.etas + as.numeric(newplotdat$nDV) # y-offset for different number of DVs
  newplotdat$dens.etas.nohack <- newplotdat$dens.etas.nohack + as.numeric(newplotdat$nDV)
  newplotdat$title <- paste0("N = ", nobs)
  
  ggplot(data=newplotdat, aes(group = nDV)) +
    geom_polygon(aes(y=dens.etas, x=loc.etas), fill = "#FFAE4A") +
    geom_polygon(aes(y=dens.etas.nohack, x=loc.etas.nohack), fill = "#5AB4BD") +
    labs(x = "Eta squared",
         y = ylab) +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25)) +
    scale_y_continuous(breaks = c(1,2,3), labels = as.character(yval)) +
    coord_cartesian(xlim = c(-0.01, 0.15)) +
    facet_grid(. ~ title)
}

# ------------------------------------------------------------------------------
# Multiple dependent variables ####
# ------------------------------------------------------------------------------

# Conditions for multiple dependent variables

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

# Plots first significant
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="firstsig", ylab="Number of dependent variables", yval=nvar)
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="firstsig", ylab="Number of dependent variables", yval=nvar)
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="firstsig", ylab="Number of dependent variables", yval=nvar)
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="firstsig", ylab="Number of dependent variables", yval=nvar)

# Plots smallest significant
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of dependent variables", yval=nvar)
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of dependent variables", yval=nvar)
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of dependent variables", yval=nvar)
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of dependent variables", yval=nvar)

# Plots smallest
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallest", ylab="Number of dependent variables", yval=nvar)
plot.R2dist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallest", ylab="Number of dependent variables", yval=nvar)
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallest", ylab="Number of dependent variables", yval=nvar)
plot.Ddist(simdat=simresults.multDVhack, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallest", ylab="Number of dependent variables", yval=nvar)

# ------------------------------------------------------------------------------
# Multiple independent variables ####
# ------------------------------------------------------------------------------

# Conditions for multiple independent variables

nobs.group <- c(30, 50, 100, 300)
nvar <- c(3, 5, 10)
r <- c(0, 0.3, 0.8)

cond.multIVhack <- expand.grid(nobs.group, nvar, r)

# T-TEST 

# Plots first significant
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multIVhack, nobs = 50, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 50, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)

# Plots smallest significant
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)

# Plots smallest
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)
plot.Ddist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_ttest, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)

# REGRESSION

# first significant
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 50, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 100, r=0, strategy="firstsig", ylab="Number of independent variables", yval=nvar)

# smallest significant
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of independent variables", yval=nvar)

# smallest
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 50, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)
plot.R2dist(simdat=simresults.multIVhack_reg, conddat = cond.multDVhack, nobs = 100, r=0, strategy="smallest", ylab="Number of independent variables", yval=nvar)

# ------------------------------------------------------------------------------
# Optional Stopping ####
# ------------------------------------------------------------------------------

# For optional stopping, there is no distinction between "hacked" and "original” 
# effect sizes because it is unclear what the “original” p-value would be.

# ------------------------------------------------------------------------------
# Outlier Exclusion ####
# ------------------------------------------------------------------------------

nobs <- c(30, 50, 100, 300)
howmany <- c(3, 5, 12)
r <- 0

cond.outHack <- expand.grid(nobs, howmany, r)

# first significant
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=50, r=0, strategy="firstsig", ylab="Number of outlier detection methods", yval=howmany)
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=100, r=0, strategy="firstsig", ylab="Number of outlier detection methods", yval=howmany)

# smallest significant
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=50, r=0, strategy="smallestsig", ylab="Number of outlier detection methods", yval=howmany)
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=100, r=0, strategy="smallestsig", ylab="Number of outlier detection methods", yval=howmany)

# smallest
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=50, r=0, strategy="smallest", ylab="Number of outlier detection methods", yval=howmany)
plot.R2dist(simdat = simresults.outHack, conddat = cond.outHack, nobs=100, r=0, strategy="smallest", ylab="Number of outlier detection methods", yval=howmany)

# ------------------------------------------------------------------------------
# Controlling for Covariates ####
# ------------------------------------------------------------------------------

nobs.group <- c(30, 50, 100, 300)
ncov <- c(3, 5, 10)
rcov <- c(0, 0.3, 0.8)
rcovdv <- c(0, 0.3)

cond.covhack <- expand.grid(nobs.group, ncov, rcov, rcovdv)

# Reduce simulation results to 3 dimensions (drop rcovdv variation)

cond.plot <- which(cond.covhack$Var4 == 0)
simresults.covhackRed <- list(firstsig = simresults.covhack$firstsig[cond.plot],
                              smallestsig = simresults.covhack$smallestsig[cond.plot],
                              smallest = simresults.covhack$smallestsig[cond.plot])
cond.covhack <- cond.covhack[cond.covhack$Var4 == 0, ]

# first significant
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=50, r=0, strategy="firstsig", ylab="Number of Covariates", yval=ncov)
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=100, r=0, strategy="firstsig", ylab="Number of Covariates", yval=ncov)

# smallest significant
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=50, r=0, strategy="smallestsig", ylab="Number of Covariates", yval=ncov)
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=100, r=0, strategy="smallestsig", ylab="Number of Covariates", yval=ncov)

# smallest
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=50, r=0, strategy="smallest", ylab="Number of Covariates", yval=ncov)
plot.eta2dist(simdat=simresults.covhackRed, conddat=cond.covhack, nobs=100, r=0, strategy="smallest", ylab="Number of Covariates", yval=ncov)

# ------------------------------------------------------------------------------
# Scale Redefinition ####
# ------------------------------------------------------------------------------

nobs <- c(30, 50, 100, 300)
ncompv <- c(5, 10) 
rcomp <- c(0.3, 0.7)
ndelete <- c(1, 3, 7)

cond.compscoreHack <- expand.grid(nobs, ncompv, rcomp, ndelete)
cond.compscoreHack <- cond.compscoreHack[cond.compscoreHack$Var4 < cond.compscoreHack$Var2, ]

# Reduce simulation results to 3 dimensions (drop rcomp variation)

cond.plot <- which(cond.compscoreHack$Var3 == 0.7)
simresults.compscoreHackRed <- list(firstsig = simresults.compscoreHack$firstsig[cond.plot],
                              smallestsig = simresults.compscoreHack$smallestsig[cond.plot],
                              smallest = simresults.compscoreHack$smallestsig[cond.plot])
cond.compscoreHack <- cond.compscoreHack[cond.compscoreHack$Var3 == 0.7, ]
colnames(cond.compscoreHack) <- paste0("Var", c(1,2,4,3))

# first significant
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=50, r=3, strategy="firstsig", ylab="Number of variables in the score", yval=ncompv)
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=100, r=3, strategy="firstsig", ylab="Number of variables in the score", yval=ncompv)

# smallest significant
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=50, r=3, strategy="smallestsig", ylab="Number of variables in the score", yval=ncompv)
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=100, r=3, strategy="smallestsig", ylab="Number of variables in the score", yval=ncompv)

# smallest
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=50, r=3, strategy="smallest", ylab="Number of variables in the score", yval=ncompv)
plot.R2dist(simdat = simresults.compscoreHackRed, conddat = cond.compscoreHack, nobs=100, r=3, strategy="smallest", ylab="Number of variables in the score", yval=ncompv)

# ------------------------------------------------------------------------------
# Variable transformation ####
# ------------------------------------------------------------------------------

nobs <- c(30, 50, 100, 300)
transvar <- c(1:3)

cond.varTransHack <- expand.grid(nobs, transvar)
cond.varTransHack$Var3 <- 0

# first significant
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=50, r=0, strategy="firstsig", ylab="Transformation applied to", yval=c("x", "y", "xy"))
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, r=0, strategy="firstsig", ylab="Transformation applied to", yval=c("x", "y", "xy"))

# smallest significant
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=50, r=0, strategy="smallestsig", ylab="Transformation applied to", yval=c("x", "y", "xy"))
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, r=0, strategy="smallestsig", ylab="Transformation applied to", yval=c("x", "y", "xy"))

# smallest
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=50, r=0, strategy="smallest", ylab="Transformation applied to", yval=c("x", "y", "xy"))
plot.R2dist(simdat = simresults.varTransHack_nonormtest, conddat = cond.varTransHack, nobs=100, r=0, strategy="smallest", ylab="Transformation applied to", yval=c("x", "y", "xy"))

# ------------------------------------------------------------------------------
# Discretizing variables ####
# ------------------------------------------------------------------------------

nobs <- c(30, 50, 100, 300)

cond.cutoffHack <- expand.grid(nobs,1,0)

# first significant
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=50, r=0, strategy="firstsig", ylab="", yval=c(""))
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, r=0, strategy="firstsig", ylab="", yval=c(""))

# smallest significant
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=50, r=0, strategy="smallestsig", ylab="", yval=c(""))
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, r=0, strategy="smallestsig", ylab="", yval=c(""))

# smallest
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=50, r=0, strategy="smallest", ylab="", yval=c(""))
plot.R2dist(simdat = simresults.cutoffHack, conddat = cond.cutoffHack, nobs=100, r=0, strategy="smallest", ylab="", yval=c(""))

# ------------------------------------------------------------------------------
# Alternative hypothesis tests ####
# ------------------------------------------------------------------------------

# For alternative hypothesis tests, we do not report effect sizes because the 
# data does not change, i.e., effect sizes do not change for different p-hacked 
# analyses.

# ------------------------------------------------------------------------------
# Favorable imputation ####
# ------------------------------------------------------------------------------

nobs <- c(30, 50, 100, 300)
missing <- c(0.05, 0.2)
howmany = c(3, 5, 10)

cond.impHack <- expand.grid(nobs, missing, howmany)
colnames(cond.impHack) <- paste0("Var", c(1,3,2))

# first significant
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=50, r=0.05, strategy="firstsig", ylab="Number of imputation methods", yval=howmany)
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=100, r=0.05, strategy="firstsig", ylab="Number of imputation methods", yval=howmany)

# smallest significant
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=50, r=0.05, strategy="smallestsig", ylab="Number of imputation methods", yval=howmany)
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=100, r=0.05, strategy="smallestsig", ylab="Number of imputation methods", yval=howmany)

# smallest
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=50, r=0.05, strategy="smallest", ylab="Number of imputation methods", yval=howmany)
plot.R2dist(simdat = simresults.impHack, conddat = cond.impHack, nobs=100, r=0.05, strategy="smallest", ylab="Number of imputation methods", yval=howmany)

# ------------------------------------------------------------------------------
# Subgroup analyses ####
# ------------------------------------------------------------------------------

nobs.group <-  c(30, 50, 100, 300)
nsubvars <- c(1, 3, 5)

cond.subgroupHack <- expand.grid(nobs.group, nsubvars, 0)

# first significant
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="firstsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="firstsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="firstsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="firstsig", ylab="Number of subgroup variables", yval=nsubvars)

# smallest significant
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="smallestsig", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="smallestsig", ylab="Number of subgroup variables", yval=nsubvars)

# smallest 
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="smallest", ylab="Number of subgroup variables", yval=nsubvars)
plot.Ddist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="smallest", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 50, r=0, strategy="smallest", ylab="Number of subgroup variables", yval=nsubvars)
plot.R2dist(simdat=simresults.subgroupHack, conddat = cond.subgroupHack, nobs = 100, r=0, strategy="smallest", ylab="Number of subgroup variables", yval=nsubvars)

# ------------------------------------------------------------------------------
# Incorrect Rounding ####
# ------------------------------------------------------------------------------

# We do not report effect sizes for incorrect rounding because incorrect 
# rounding does not change the reported effect size.


