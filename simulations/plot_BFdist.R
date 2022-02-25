# Compute Bayes factors from p-values

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)
library(dplyr)

# Conditions for multiple dependent variables

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

# Select conditions to show in the plot

nobs.extracted <- 50
cond.plot <- which(cond.multDVhack$Var1 == nobs.extracted & cond.multDVhack$Var3 == 0)

# Extract t-values and compute BFs

ps.hack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ps.hack))
ts.hack <- qt(ps.hack/2, df=nobs.extracted-2)
ps.nohack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ps.orig))
ts.nohack <- qt(ps.nohack/2, df=nobs.extracted-2)
library(BayesFactor)
BFs.hack <- lapply(ts.hack, function(x) ttest.tstat(x, n1=nobs.extracted, n2=nobs.extracted, simple=TRUE))
BFs.hack <- unname(simplify2array(BFs.hack))
BFs.nohack <- lapply(ts.nohack, function(x) ttest.tstat(x, n1=nobs.extracted, n2=nobs.extracted, simple=TRUE))
BFs.nohack <- unname(simplify2array(BFs.nohack))

# Data for plot

iter <- nrow(simresults.multDVhack$firstsig[[1]])
nDV <- rep(cond.multDVhack[cond.plot,]$Var2, each=iter)

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

newplotdat$title <- paste0("N = ", nobs.extracted)

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
  scale_x_continuous(breaks = log(c(1/10, 1/3, 1, 3, 10)), labels = c("1/10", "1/3", "1", "3", "10"), limits = log(c(1/11, 100))) +
  facet_grid(. ~ title)
                     

plot.new()
legend(x = "topleft", legend = c("p-hacked", "original"), fill = c("#FFAE4A", "#5AB4BD"))
