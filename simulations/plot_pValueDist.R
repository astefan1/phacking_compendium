# Plot: p-value distribution for different reporting strategies

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

# p-value distributions for the firstsig strategy

ps <- unlist(lapply(simresults.multDVhack$firstsig[cond.plot], function(x) x$ps.hack))
ps <- c(simresults.multDVhack$firstsig[[cond.plot[1]]]$ps.orig, ps)
iter <- nrow(simresults.multDVhack$firstsig[[1]])
nDV <- rep(c(1, cond.multDVhack[cond.plot,]$Var2), each=iter)

plotdat <- data.frame(ps=ps,
                      nDV=as.factor(nDV))
plotdat$title <- "first significant p-value"

ggplot(data=plotdat, aes(x = nDV, y = ps)) +
  geom_jitter(aes(colour = nDV), shape = 16, position = position_jitter(0.2), alpha = 0.6) +
  geom_violin(fill = NA, aes(colour = nDV), lwd=1.5) +
  labs(x = "Number of dependent variables",
       y = "p-value") +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25),
        legend.position = "none") +
  facet_grid(. ~ title) +
  scale_color_manual(values = c("#9AD3EB", "#6AACCC", "#2F7CA6", "#125A84"))

# p-value distributions for the smallest significant strategy

ps2 <- unlist(lapply(simresults.multDVhack$smallestsig[cond.plot], function(x) x$ps.hack))
ps2 <- c(simresults.multDVhack$smallestsig[[cond.plot[1]]]$ps.orig, ps2)
iter <- nrow(simresults.multDVhack$smallestsig[[1]])

plotdat2 <- data.frame(ps=ps2,
                      nDV=as.factor(nDV))
plotdat2$title <- "smallest significant p-value"

ggplot(data=plotdat2, aes(x = nDV, y = ps2)) +
  geom_jitter(aes(colour = nDV), shape = 16, position = position_jitter(0.2), alpha = 0.6) +
  geom_violin(fill = NA, aes(colour = nDV), lwd=1.5) +
  labs(x = "Number of dependent variables",
       y = "p-value") +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25),
        legend.position = "none") +
  facet_grid(. ~ title) +
  scale_color_manual(values = c("#9AD3EB", "#6AACCC", "#2F7CA6", "#125A84"))

# p-value distribution for the smallest strategy

ps3 <- unlist(lapply(simresults.multDVhack$smallest[cond.plot], function(x) x$ps.hack))
ps3 <- c(simresults.multDVhack$smallest[[cond.plot[1]]]$ps.orig, ps3)
iter <- nrow(simresults.multDVhack$smallest[[1]])

plotdat3 <- data.frame(ps=ps3,
                       nDV=as.factor(nDV))
plotdat3$title <- "smallest p-value"

ggplot(data=plotdat3, aes(x = nDV, y = ps3)) +
  geom_jitter(aes(colour = nDV), shape = 16, position = position_jitter(0.2), alpha = 0.6) +
  geom_violin(fill = NA, aes(colour = nDV), lwd=1.5) +
  labs(x = "Number of dependent variables",
       y = "p-value") +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.title = element_text(size=25),
        axis.text = element_text(size=25),
        legend.position = "none") +
  facet_grid(. ~ title) +
  scale_color_manual(values = c("#9AD3EB", "#6AACCC", "#2F7CA6", "#125A84"))

