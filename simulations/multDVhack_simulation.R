# Selective Reporting of the Dependent Variable

nobs.group <- c(30, 50, 100, 300)   # number of observations per group
nvar <- c(3, 5, 10)                 # number of dependent variables
r <- c(0, 0.3, 0.8)                 # correlation between dependent variables
strategy <- c("firstsig", "smallest", "smallest.sig")

cond.multDVhack <- expand.grid(nobs.group, nvar, r)

simresults.multDVhack <- list()

simmultiple.multDVhack <- function(par, strategy){
  data.frame(sim.multDVhack(nobs.group = par[1],
                 nvar = par[2],
                 r = par[3],
                 strategy = strategy,
                 iter = 1000,
                 alternative = "two.sided",
                 alpha = 0.05))
}

simresults.multDVhack$firstsig <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "firstsig")
                                        })

simresults.multDVhack$smallest <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "smallest")
})

simresults.multDVhack$smallestsig <- apply(cond.multDVhack, 1, function(x) {
  simmultiple.multDVhack(x, strategy = "smallest.sig")
})

cond.multDVhack$FP.rates <- findFPrate(simresults.multDVhack)
# false positive rates are the same no matter which strategy is used (once 
# hacking was successful, this contributes to FP rate, no matter what the actual
# value of the p-value is). Therefore, I average over the different strategies
# to obtain a more reliable value.

library(ggplot2)

ggplot(cond.multDVhack, aes(Var2, FP.rates, colour = as.factor(Var1), shape=as.factor(Var3))) +
  geom_point(size = 2) +
  geom_line(aes(linetype = as.factor(Var3))) +
  theme_classic() +
  labs(title = "False Positive Rates",
       x = "Number of Dependent Variables",
       y = "False Positive Rate",
       color = "Sample Size per Group",
       shape = "Correlation",
       linetype = "Correlation" ) +
  geom_hline(yintercept = 0.05, col = "grey")

# p-value distributions for firstsig strategy

ps <- unlist(lapply(simresults.multDVhack$firstsig, function(x) x$ps.hack))
iter <- nrow(simresults.multDVhack$firstsig[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ps=ps, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ps)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "blue", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "p-Value Distributions (Strategy: first significant)",
       x = "Number of Dependent Variables",
       y = "p-Value")

# p-value distributions for smallest strategy

ps <- unlist(lapply(simresults.multDVhack$smallest, function(x) x$ps.hack))
iter <- nrow(simresults.multDVhack$smallest[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ps=ps, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ps)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "blue", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "p-Value Distributions (Strategy: smallest)",
       x = "Number of Dependent Variables",
       y = "p-Value")  

# p-value distributions for smallest.sig strategy

ps <- unlist(lapply(simresults.multDVhack$smallestsig, function(x) x$ps.hack))
iter <- nrow(simresults.multDVhack$smallestsig[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ps=ps, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ps)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "blue", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "p-Value Distributions (Strategy: smallest significant)",
       x = "Number of Dependent Variables",
       y = "p-Value")  

# Effect size distribution for firstsig strategy

ds <- unlist(lapply(simresults.multDVhack$firstsig, function(x) x$ds.hack))
iter <- nrow(simresults.multDVhack$firstsig[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ds=ds, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ds)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "orange", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "Cohen's d Distributions (Strategy: first significant)",
       x = "Number of Dependent Variables",
       y = "Cohen's d")
  
# Effect size distribution for smallest strategy

ds <- unlist(lapply(simresults.multDVhack$smallest, function(x) x$ds.hack))
iter <- nrow(simresults.multDVhack$smallest[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ds=ds, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ds)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "orange", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "Cohen's d Distributions (Strategy: smallest)",
       x = "Number of Dependent Variables",
       y = "Cohen's d")

# Effect size distribution for smallest.sig strategy

ds <- unlist(lapply(simresults.multDVhack$smallestsig, function(x) x$ds.hack))
iter <- nrow(simresults.multDVhack$smallestsig[[1]])
samplesize <- rep(cond.multDVhack$Var1, each=iter)
nDV <- rep(cond.multDVhack$Var2, each=iter)
rs <- rep(cond.multDVhack$Var3, each=iter)

plotdat <- data.frame(ds=ds, 
                      samplesize=as.factor(samplesize), 
                      nDV=as.factor(nDV), 
                      rs=as.factor(rs))
new.labels.ss <- c("30" = "N = 30", "50" = "N = 50", "100" = "N = 100", "300" = "N = 300")
new.labels.r <- c("0" = "r = 0", "0.3" = "r = 0.3", "0.8" = "r = 0.8")
ggplot(data=plotdat, aes(x = nDV, y = ds)) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "orange", alpha = 0.05) +
  geom_violin(fill = NA) +
  facet_grid(rs ~ samplesize, labeller = labeller(samplesize = new.labels.ss,
                                                  rs = new.labels.r)) +
  labs(title = "Cohen's d Distributions (Strategy: smallest significant)",
       x = "Number of Dependent Variables",
       y = "Cohen's d")
