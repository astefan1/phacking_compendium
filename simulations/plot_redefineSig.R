# Impact of redefining statistical significance

# Get simulation results

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)

library(ggplot2)

# Function to calculate FP-rate only from smallest strategy 

findFPrateR <- function(simresult, alpha = 0.005){
  sapply(simresult$smallestsig,
         function(x) {sum(x$ps.hack < alpha) / nrow(x)})
}

# Find FP-rates

FP.multDV <- max(unname(findFPrate(simresults.multDVhack)))
FPR.multDV <- max(unname(findFPrateR(simresults.multDVhack)))

FP.multIV <- max(unname(findFPrate(simresults.multIVhack_reg)))
FPR.multIV <- max(unname(findFPrateR(simresults.multIVhack_reg)))

FP.optstop <- max(sapply(simresults.optstop_nmin,
       function(x) {sum(x$ps.hack < 0.05) / nrow(x)}))

# optional stopping requires re-simulating with alpha=0.005 because stopping
# was determined on reaching 0.05
# ----
n.min <- 5
n.max <- c(300) #Var1
step = c(1) #Var2

cond.optstop <- expand.grid(n.max, step)

simresults.optstopR <- list()

simmultiple.optstop <- function(par){
  data.frame(sim.optstop(n.min = 5,
                         n.max = par[1],
                         step = par[2],
                         alternative = "two.sided",
                         iter = 10000,
                         alpha = 0.005))
  
}

simresults.optstopR <- apply(cond.optstop, 1, function(x) {
  simmultiple.optstop(x)
})
save(simresults.optstopR, file = "simulations/SIM_optstop_Redefine.RData")

# ------

FPR.optstop <- max(sapply(simresults.optstopR,
                      function(x) {sum(x$ps.hack < 0.005) / nrow(x)}))

FP.outHack <- max(unname(findFPrate(simresults.outHack)))
FPR.outHack <- max(unname(findFPrateR(simresults.outHack)))

FP.covHack <- max(unname(findFPrate(simresults.covhack)))
FPR.covHack <- max(unname(findFPrateR(simresults.covhack)))

FP.compscoreHack <- max(unname(findFPrate(simresults.compscoreHack)))
FPR.compscoreHack <- max(unname(findFPrateR(simresults.compscoreHack)))

FP.varTransHack <- max(unname(findFPrate(simresults.varTransHack_nonormtest)))
FPR.varTransHack <- max(unname(findFPrateR(simresults.varTransHack_nonormtest)))

FP.cutoffHack <- max(unname(findFPrate(simresults.cutoffHack)))
FPR.cutoffHack <- max(unname(findFPrateR(simresults.cutoffHack)))

FP.statAnalysisHack <- max(unname(findFPrate(simresults.statAnalysisHack)))
FPR.statAnalysisHack <- max(unname(findFPrateR(simresults.statAnalysisHack)))

FP.impHack <- max(unname(findFPrate(simresults.impHack)))
FPR.impHack <- max(unname(findFPrateR(simresults.impHack)))

FP.subgroupHack <- max(unname(findFPrate(simresults.subgroupHack)))
FPR.subgroupHack <- max(unname(findFPrateR(simresults.subgroupHack)))

FPregular <- c(FP.multDV, FP.multIV, FP.optstop, FP.outHack, FP.covHack,
               FP.compscoreHack, FP.varTransHack, FP.cutoffHack, 
               FP.statAnalysisHack, FP.impHack, FP.subgroupHack)

FPredefined <- c(FPR.multDV, FPR.multIV, FPR.optstop, FPR.outHack, FPR.covHack,
                 FPR.compscoreHack, FPR.varTransHack, FPR.cutoffHack, 
                 FPR.statAnalysisHack, FPR.impHack, FPR.subgroupHack)

FPregularByTen <- FPregular/10

plotdat <- data.frame(FP.rate = c(FPregular, FPredefined, FPregularByTen),
                      whichFP = rep(c("0.05", "0.005", "byTen"), each=length(FPregular)),
                      strategy = rep(1:length(FPregular), 3),
                      linetype = rep(c(1,1,2), each=length(FPregular)),
                      linecolor = rep(c(1,2,1), each=length(FPregular)))

ggplot(plotdat, aes(x = strategy, 
                    y=FP.rate, 
                    group=as.factor(whichFP), 
                    linetype = as.factor(linetype),
                    colour = as.factor(whichFP))) +
  geom_hline(yintercept = 0.05, col = "grey") +
  geom_hline(yintercept = 0.005, col = "grey") +
  geom_line(size=1) +
  scale_x_continuous(breaks=c(1:11),
                     labels=c("Selective reporting DV", "Selective reporting IV",
                              "Optional Stopping", "Outlier exclusion", 
                              "Controlling covariates", "Scale redefinition",
                              "Variable transformation", "Discretizing variables",
                              "Alt. hypothesis tests", "Favorable imputation",
                              "Inclusion criteria")) +
  labs(x = "",
       y = "Highest false positive rate") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 20),
        axis.title = element_text(size=20, colour = "grey30"),
        legend.position = "none",
        plot.margin = unit(c(10,0,0,40), unit="pt")) +
  scale_color_manual(values= c("#009975", "black", "grey"))

plot.new()
legend(x = "topleft", legend = c("p-hacked: p < 0.05", "p-hacked: p < 0.005"), col = c("black", "#009975"), lty = "solid", lwd = 3)
