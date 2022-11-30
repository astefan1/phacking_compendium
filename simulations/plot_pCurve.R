# Plot: p-curves

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
cond.plot <- which(cond.multDVhack$Var1 == 50 & cond.multDVhack$Var3 == 0.3)

# p-curve first significant

ps <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ps.hack))
ps <- c(simresults.multDVhack$firstsig[[cond.plot[1]]]$ps.orig, ps)
iter <- nrow(simresults.multDVhack$firstsig[[1]])
nDV <- rep(c(1, cond.multDVhack[cond.plot,]$Var2), each=iter)

pvals.intervals <- simplify2array(tapply(ps, nDV, function(x) table(cut(x[x < 0.1], breaks = seq(0, 0.1, by = 0.01)))))/10000
pvals.intervals <- data.frame(pvals.intervals)
pvals.intervals$ints <- c(1:10)
pvals.intervals$title <- "first significant p-value"

ggplot(pvals.intervals, aes(x=ints)) +
  geom_line(aes(y=X1), colour = "#9AD3EB", lwd=1.5) +
  geom_line(aes(y=X3), colour = "#6AACCC", lwd=1.5) +
  geom_line(aes(y=X5), colour = "#2F7CA6", lwd=1.5) +
  geom_line(aes(y=X10), colour = "#125A84", lwd=1.5) +
  coord_cartesian(ylim = c(0,0.1)) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25)) +
  scale_x_continuous(breaks = c(1:10), labels = seq(0.01, 0.1, by=0.01)) +
  scale_y_continuous(breaks = seq(0, 0.1, length.out=5), labels = paste0(seq(0, 10, length.out = 5), " %"), position = "right") +
  facet_grid(. ~ title) +
  labs(y = "Percentage of p-values",
       x = "p-value") +
  geom_hline(yintercept = 0.01, col = "grey", lty="dashed", lwd=1.5) +
  geom_vline(xintercept = 5, col = "grey", lwd=1.5, lty="dashed")
  
# p-curve smallest significant

ps <- unlist(lapply(simresults.multDVhack$smallestsig[cond.plot], function(x) x$ps.hack))
ps <- c(simresults.multDVhack$smallestsig[[cond.plot[1]]]$ps.orig, ps)
iter <- nrow(simresults.multDVhack$smallestsig[[1]])
nDV <- rep(c(1, cond.multDVhack[cond.plot,]$Var2), each=iter)

pvals.intervals <- simplify2array(tapply(ps, nDV, function(x) table(cut(x[x < 0.1], breaks = seq(0, 0.1, by = 0.01)))))/10000
pvals.intervals <- data.frame(pvals.intervals)
pvals.intervals$ints <- c(1:10)
pvals.intervals$title <- "smallest significant p-value"

ggplot(pvals.intervals, aes(x=ints)) +
  geom_line(aes(y=X1), colour = "#9AD3EB", lwd=1.5) +
  geom_line(aes(y=X3), colour = "#6AACCC", lwd=1.5) +
  geom_line(aes(y=X5), colour = "#2F7CA6", lwd=1.5) +
  geom_line(aes(y=X10), colour = "#125A84", lwd=1.5) +
  coord_cartesian(ylim = c(0,0.1)) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25)) +
  scale_x_continuous(breaks = c(1:10), labels = seq(0.01, 0.1, by=0.01)) +
  scale_y_continuous(breaks = seq(0, 0.1, length.out=5), labels = paste0(seq(0, 10, length.out = 5), " %"), position = "right") +
  facet_grid(. ~ title) +
  labs(y = "Percentage of p-values",
       x = "p-value") +
  geom_hline(yintercept = 0.01, col = "grey", lty="dashed", lwd=1.5) +
  geom_vline(xintercept = 5, col = "grey", lwd=1.5, lty="dashed")

# p-curve smallest 

ps <- unlist(lapply(simresults.multDVhack$smallest[cond.plot], function(x) x$ps.hack))
ps <- c(simresults.multDVhack$smallest[[cond.plot[1]]]$ps.orig, ps)
iter <- nrow(simresults.multDVhack$smallest[[1]])
nDV <- rep(c(1, cond.multDVhack[cond.plot,]$Var2), each=iter)

pvals.intervals <- simplify2array(tapply(ps, nDV, function(x) table(cut(x[x < 0.1], breaks = seq(0, 0.1, by = 0.01)))))/10000
pvals.intervals <- data.frame(pvals.intervals)
pvals.intervals$ints <- c(1:10)
pvals.intervals$title <- "smallest p-value"

ggplot(pvals.intervals, aes(x=ints)) +
  geom_line(aes(y=X1), colour = "#9AD3EB", lwd=1.5) +
  geom_line(aes(y=X3), colour = "#6AACCC", lwd=1.5) +
  geom_line(aes(y=X5), colour = "#2F7CA6", lwd=1.5) +
  geom_line(aes(y=X10), colour = "#125A84", lwd=1.5) +
  coord_cartesian(ylim = c(0,0.1)) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25)) +
  scale_x_continuous(breaks = c(1:10), labels = seq(0.01, 0.1, by=0.01)) +
  scale_y_continuous(breaks = seq(0, 0.1, length.out=5), labels = paste0(seq(0, 10, length.out = 5), " %"), position = "right") +
  facet_grid(. ~ title) +
  labs(y = "Percentage of p-values",
       x = "p-value") +
  geom_hline(yintercept = 0.01, col = "grey", lty="dashed", lwd=1.5) +
  geom_vline(xintercept = 5, col = "grey", lwd=1.5, lty="dashed")
