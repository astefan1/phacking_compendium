# Plot: Effect size distribution for different reporting strategies

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)

# Conditions for multiple dependent variables

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

# Select conditions to show in the plot
nobs.extracted <- 50
cond.plot <- which(cond.multDVhack$Var1 == nobs.extracted & cond.multDVhack$Var3 == 0)

# ES distributions for the firstsig strategy

ds.hack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ds.hack))
ds.nohack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ds.orig))
iter <- nrow(simresults.multDVhack$firstsig[[1]])
nDV <- rep(cond.multDVhack[cond.plot,]$Var2, each=iter)

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
newplotdat$title <- paste0("N = ", nobs.extracted)

ggplot(data=newplotdat, aes(group = nDV)) +
  geom_polygon(aes(y=dens.ds, x=loc.ds), fill = "#FFAE4A") +
  geom_polygon(aes(y=dens.ds.nohack, x=loc.ds.nohack), fill = "#5AB4BD") +
  labs(x = "Cohen's d",
       y = "Number of dependent variables") +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25)) +
  scale_y_continuous(breaks = c(1,2,3), labels = c("3", "5", "10")) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), limits =c(-1, 1)) +
  facet_grid(. ~ title)

# R-squared distributions for the firstsig strategy

rs.hack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$r2s.hack))
rs.nohack <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$r2s.orig))

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
newplotdat$title <- paste0("N = ", nobs.extracted)

ggplot(data=newplotdat, aes(group = nDV)) +
  geom_polygon(aes(y=dens.rs, x=loc.rs), fill = "#FFAE4A") +
  geom_polygon(aes(y=dens.rs.nohack, x=loc.rs.nohack), fill = "#5AB4BD") +
  labs(x = "R squared",
       y = "Number of dependent variables") +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25)) +
  scale_y_continuous(breaks = c(1,2,3), labels = c("3", "5", "10")) +
  coord_cartesian(xlim = c(-0.01, 0.15)) +
  facet_grid(. ~ title)

plot.new()
legend(x = "topleft", legend = c("p-hacked", "original"), fill = c("#FFAE4A", "#5AB4BD"))

