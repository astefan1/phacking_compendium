# Compute Bayes factors from p-values

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)
library(dplyr)
library(BayesFactor)

# Conditions for multiple dependent variables

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

# ------------------------------------------------------------------------------
# Extract p-values and t-values and compute BFs ####
# ------------------------------------------------------------------------------

getBFs <- function(simdat, conddat, nobs, r, strategy){
  
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == r)
  
  # extract p-values and t-values
  ps.hack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ps.hack))
  ts.hack <- qt(ps.hack/2, df=nobs-2)
  ps.nohack <- unlist(lapply(simdat[[strategy]][cond.plot], function(x) x$ps.orig))
  ts.nohack <- qt(ps.nohack/2, df=nobs-2)
  
  # compute BFs
  BFs.hack <- lapply(ts.hack, function(x) ttest.tstat(x, n1=nobs, n2=nobs, simple=TRUE))
  BFs.hack <- unname(simplify2array(BFs.hack))
  BFs.nohack <- lapply(ts.nohack, function(x) ttest.tstat(x, n1=nobs, n2=nobs, simple=TRUE))
  BFs.nohack <- unname(simplify2array(BFs.nohack))
  
  return(cbind(BFs.hack, BFs.nohack))
}

# Compute all BFs

BF50_firstsig <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=50, r=0, strategy="firstsig")
BF50_smallestsig <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=50, r=0, strategy="smallestsig")
BF50_smallest <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=50, r=0, strategy="smallest")

BF300_firstsig <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=300, r=0, strategy="firstsig")
BF300_smallestsig <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=300, r=0, strategy="smallestsig")
BF300_smallest <- getBFs(simdat=simresults.multDVhack, conddat=cond.multDVhack, nobs=300, r=0, strategy="smallest")


# ------------------------------------------------------------------------------
# Plot the legend ####
# ------------------------------------------------------------------------------


# Data for plot

plotBFDist <- function(BFobj, conddat, nobs, r){
  
  cond.plot <- which(conddat$Var1 == nobs & conddat$Var3 == r)
  iter <- nrow(BFobj)
  nDV <- rep(conddat[cond.plot,]$Var2, each=iter)
  BFs.hack <- BFobj[,1]
  BFs.nohack <- BFobj[,2]
  
  plotdat <- data.frame(lbfs.hack=log(BFs.hack),
                        lbfs.nohack=log(BFs.nohack),
                        nDV=as.factor(nDV))
  
  newplotdat <- plotdat %>% # compute densities from ES
    group_by(nDV) %>%
    do(data.frame(loc.bfs = density(.$lbfs.hack)$x,
                  dens.bfs = density(.$lbfs.hack)$y / (2.2*max(density(.$lbfs.hack)$y)),
                  loc.bfs.nohack = density(.$lbfs.nohack)$x,
                  dens.bfs.nohack = -1*density(.$lbfs.nohack)$y/(2*max(density(.$lbfs.nohack)$y))))
  newplotdat$dens.bfs <- newplotdat$dens.bfs + as.numeric(newplotdat$nDV) # y-offset for different number of DVs
  newplotdat$dens.bfs.nohack <- newplotdat$dens.bfs.nohack + as.numeric(newplotdat$nDV)
  
  newplotdat$title <- paste0("N = ", nobs)
  
  ggplot(data=newplotdat, aes(group = nDV)) +
    geom_polygon(aes(y=dens.bfs, x=loc.bfs), fill = "#FFAE4A") +
    geom_polygon(aes(y=dens.bfs.nohack, x=loc.bfs.nohack), fill = "#5AB4BD") +
    labs(x = "Bayes factor",
         y = "Number of dependent variables") +
    theme_bw() +
    theme(text = element_text(size=35),
          axis.title = element_text(size=25),
          axis.text = element_text(size=25)) +
    scale_y_continuous(breaks = c(1,2,3), labels = c("3", "5", "10")) +
    scale_x_continuous(breaks = log(c(1/10, 1/3, 1, 3, 10)), labels = c("1/10", "1/3", "1", "3", "10")) +
    coord_cartesian(xlim = log(c(1/20, 100))) +
    facet_grid(. ~ title) +
    geom_vline(xintercept = 0, linetype = "dashed", col = "grey", lwd = 1.5) 
  
  
}

plotBFDist(BFobj=BF50_firstsig, conddat=cond.multDVhack, nobs=50, r=0)
plotBFDist(BFobj=BF50_smallestsig, conddat=cond.multDVhack, nobs=50, r=0)
plotBFDist(BFobj=BF50_smallest, conddat=cond.multDVhack, nobs=50, r=0)

plotBFDist(BFobj=BF300_firstsig, conddat=cond.multDVhack, nobs=300, r=0)
plotBFDist(BFobj=BF300_smallestsig, conddat=cond.multDVhack, nobs=300, r=0)
plotBFDist(BFobj=BF300_smallest, conddat=cond.multDVhack, nobs=300, r=0)