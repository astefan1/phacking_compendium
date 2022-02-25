# Plotting the False Positive Rates for All Simulations

source("simulations/00_simulation_helpers.R")
lapply(paste0("simulations/", 
              dir("simulations")[grepl(dir("simulations"), pattern="SIM*")]),
       load,
       .GlobalEnv)
library(ggplot2)
library(wesanderson)

#### Scale Redefinition / Composite Scores ####

nobs <- c(30, 50, 100, 300) #Var1
ncompv <- c(5, 10) #Var2
rcomp <- c(0.3, 0.7) #Var3
ndelete <- c(1, 3, 7) #Var4

cond.compscoreHack <- expand.grid(nobs, ncompv, rcomp, ndelete)
cond.compscoreHack <- cond.compscoreHack[cond.compscoreHack$Var4 < cond.compscoreHack$Var2, ]

cond.compscoreHack$FP.rates <- findFPrate(simresults.compscoreHack)
new.labels.ncompv <- c("5" = "5-Item Scale", "10" = "10-Item Scale")

ggplot(cond.compscoreHack, aes(Var4, FP.rates, colour = as.factor(Var1), shape=as.factor(Var3))) +
  geom_point(size = 3) +
  geom_line(aes(linetype = as.factor(Var3))) +
  theme_classic() +
  labs(title = "",
       x = "Maximum Number of Deleted Items",
       y = "False Positive Rate",
       color = "Sample Size",
       shape = "Correlation",
       linetype = "Correlation" ) +
  geom_hline(yintercept = 0.05, col = "grey") +
  facet_wrap(~ Var2, labeller = labeller(Var2 = new.labels.ncompv)) +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4), limits = c(0,0.4)) +
  scale_x_continuous(breaks = c(1,3,7))

#### Exploit Covariates ####

nobs.group <- c(30, 50, 100, 300) #Var1
ncov <- c(3, 5, 10) #Var2
rcov <- c(0, 0.3, 0.8) #Var3
rcovdv <- c(0, 0.3) #Var4

cond.covhack <- expand.grid(nobs.group, ncov, rcov, rcovdv)
cond.covhack$FP.rates <- findFPrate(simresults.covhack)

cond.covhack$Var4 <- as.factor(cond.covhack$Var4)
new.labels.rcovdv <- c("'r'['ZY']*'= 0'", "'r'['ZY']*'= 0.3'")
levels(cond.covhack$Var4) <- new.labels.rcovdv

ggplot(cond.covhack, aes(Var2, FP.rates, colour = as.factor(Var1), shape=as.factor(Var3))) +
  geom_point(size = 3) +
  geom_line(aes(linetype = as.factor(Var3))) +
  theme_classic() +
  labs(title = "",
       x = "Number of Covariates",
       y = "False Positive Rate",
       color = "Sample Size",
       shape = bquote('r'[' Z']),
       linetype = bquote('r'[' Z']) 
       ) +
  geom_hline(yintercept = 0.05, col = "grey") +
  facet_wrap(~ Var4, labeller = label_parsed) +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), limits = c(0,0.3)) +
  scale_x_continuous(breaks = c(3,5,10))


#### Exploit Cutoffs ####

nobs <- c(30, 50, 100, 300)
cond.cutoffHack <- expand.grid(nobs)

cond.cutoffHack$FP.rates <- findFPrate(simresults.cutoffHack)

ggplot(cond.cutoffHack, aes(Var1, FP.rates)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.05, col = "grey") +
  theme_classic() +
  labs(title = "",
       x = "Sample Size",
       y = "False Positive Rate") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2), limits = c(0,0.2)) +
  scale_x_continuous(breaks = c(20, 50, 100, 300))

#### Favorable Imputation ####

nobs <- c(30, 50, 100, 300) #Var1
missing <- c(0.05, 0.2) #Var2
howmany = c(3, 5, 10) #Var3

cond.impHack <- expand.grid(nobs, missing, howmany)
cond.impHack$FP.rates <- findFPrate(simresults.impHack)

ggplot(cond.impHack, aes(Var3, FP.rates, colour = as.factor(Var1), shape=as.factor(Var2))) +
  geom_point(size = 3) +
  geom_line(aes(linetype = as.factor(Var2)), size = 1) +
  theme_classic() +
  labs(title = "",
       x = "Number of Imputation Methods",
       y = "False Positive Rate",
       color = "Sample Size",
       shape = bquote(rho["missing"]),
       linetype = bquote(rho["missing"]) ) +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.15, 0.25, 0.35, 0.45), limits = c(0,0.5)) +
  scale_x_continuous(breaks = c(3,5,10))

#### Incorrect Rounding ####

roundinglevel <- c(0.051, 0.055, 0.1)
cond.roundHack <- expand.grid(roundinglevel)

FP.rates <- sapply(simresults.roundHack,
       function(x) {sum(x$ps.hack < 0.05) / nrow(x)})
cond.roundHack$FP.rates <- FP.rates

ggplot(cond.roundHack, aes(Var1, FP.rates)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(yintercept = 0.05, col = "grey") +
  theme_classic() +
  labs(title = "False Positive Rates",
       x = "Rounding Level",
       y = "False Positive Rate")

# This plot is actually misleading because the FP rates are dependent on the 
# sampling variability, i.e., if we want to be able to get a proper estimate
# of the small differences in FP rates, we would have to conduct many more
# simulations.

#### Optional Stopping ####

n.min <- 5
n.max <- c(30, 50, 100, 300) #Var1
step = c(1, 5, 10, 50) #Var2

cond.optstop <- expand.grid(n.max, step)

FP.rates <- sapply(simresults.optstop,
                   function(x) {sum(x$ps.hack < 0.05) / nrow(x)})
cond.optstop$FP.rates <- FP.rates

ggplot(cond.optstop, aes(Var2, FP.rates, colour = as.factor(Var1))) +
  geom_point(size = 3) +
  geom_line(aes(colour = as.factor(Var1))) +
  theme_classic() +
  labs(title = "",
       x = "Step Size",
       y = "False Positive Rate",
       color = bquote("N"["max"])) +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.1,0.2, 0.3, 0.4), limits = c(0,0.45)) +
  scale_x_continuous(breaks = c(1,3,5,10,50))

#### Outlier Exclusion ####

nobs <- c(30, 50, 100, 300)
howmany <- c(3, 5, 12)

cond.outHack <- expand.grid(nobs, howmany)
cond.outHack$FP.rates <- findFPrate(simresults.outHack)

ggplot(cond.outHack, aes(Var2, FP.rates, colour = as.factor(Var1))) +
  geom_point(size = 3) +
  geom_line(aes(colour = as.factor(Var1))) +
  theme_classic() +
  labs(title = "",
       x = "Number of Outlier Detection Methods",
       y = "False Positive Rate",
       color = "Sample Size") +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.1,0.2, 0.3), limits = c(0,0.3)) +
  scale_x_continuous(breaks = c(3,5,12))

#### Selective Reporting DV ####

nobs.group <- c(30, 50, 100, 300) # Var1
nvar <- c(3, 5, 10) #Var2            
r <- c(0, 0.3, 0.8) #Var3              

cond.multDVhack <- expand.grid(nobs.group, nvar, r)
cond.multDVhack$FP.rates <- findFPrate(simresults.multDVhack)

ggplot(cond.multDVhack, aes(Var2, FP.rates, colour = as.factor(Var1), shape=as.factor(Var3))) +
  geom_point(size = 3) +
  geom_line(aes(linetype = as.factor(Var3)), size = 1) +
  theme_classic() +
  labs(title = "",
       x = "Number of Dependent Variables",
       y = "False Positive Rate",
       color = "Sample Size",
       shape = "Correlation",
       linetype = "Correlation" ) +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.1,0.2, 0.3, 0.4), limits = c(0,0.45)) +
  scale_x_continuous(breaks = c(3,5,10))

#### Selective Reporting of IV ####

nobs.group <- c(30, 50, 100, 300)
nvar <- c(3, 5, 10)
r <- c(0, 0.3, 0.8)

cond.multIVhack <- expand.grid(nobs.group, nvar, r)
cond.multIVhack$FP.rates <- findFPrate(simresults.multIVhack)

ggplot(cond.multIVhack, aes(Var2, FP.rates, colour = as.factor(Var1), shape=as.factor(Var3))) +
  geom_point(size = 3) +
  geom_line(aes(linetype = as.factor(Var3)), size = 1) +
  theme_classic() +
  labs(title = "",
       x = "Number of Independent Variables",
       y = "False Positive Rate",
       color = "Sample Size",
       shape = "Correlation",
       linetype = "Correlation" ) +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), limits = c(0,0.3)) +
  scale_x_continuous(breaks = c(3,5,10))

#### Exploit Statistical Analyses ####

nobs.group <- c(30, 50, 100, 300) #Var1

cond.statAnalysisHack <- expand.grid(nobs.group)
cond.statAnalysisHack$FP.rates <- findFPrate(simresults.statAnalysisHack)

ggplot(cond.statAnalysisHack, aes(Var1, FP.rates)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.05, col = "grey") +
  theme_classic() +
  labs(title = "",
       x = "Sample Size",
       y = "False Positive Rate") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15), limits = c(0,0.15)) +
  scale_x_continuous(breaks = c(30,50,100, 300))

#### Subgroup Analyses ####

nobs.group <-  c(30, 50, 100, 300) #Var1
nsubvars <- c(1, 3, 5) #Var2

cond.subgroupHack <- expand.grid(nobs.group, nsubvars)
cond.subgroupHack$FP.rates <- findFPrate(simresults.subgroupHack)

ggplot(cond.subgroupHack, aes(Var2, FP.rates, colour = as.factor(Var1))) +
  geom_point(size = 3) +
  geom_line(aes(colour = as.factor(Var1)), size = 1) +
  theme_classic() +
  labs(title = "",
       x = "Number of Grouping Variables",
       y = "False Positive Rate",
       color = "Sample Size") +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3), limits = c(0,0.3))+
  scale_x_continuous(breaks = c(1,3,5))

#### Variable Transformation ####

nobs <- c(30, 50, 100, 300) #Var1
transvar <- c(1:3) #Var2

cond.varTransHack <- expand.grid(nobs, transvar)
cond.varTransHack$FP.rates <- findFPrate(simresults.varTransHack)

ggplot(cond.varTransHack, aes(Var2, FP.rates, colour = as.factor(Var1))) +
  geom_point(size = 3) +
  geom_line(aes(colour = as.factor(Var1)), size = 1) +
  theme_classic() +
  labs(title = "",
       x = "Transformed Variables",
       y = "False Positive Rate",
       color = "Sample Size") +
  geom_hline(yintercept = 0.05, col = "grey") +
  scale_color_manual(values=rev(wes_palette("Rushmore"))) +
  theme(text = element_text(size=20)) +
  scale_y_continuous(breaks = c(0, 0.05, 0.15, 0.25, 0.35), limits = c(0,0.4)) +
  scale_x_continuous(breaks = c(1,2,3), labels = c("X", "Y", "X and Y"))
  


#### FP Rate Overview Plot ####

FPCombined <- matrix(NA, nrow=0, ncol=2)
FPCombined <- rbind(FPCombined,
                    cbind(cond.multDVhack$FP.rates, 11))
FPCombined <- rbind(FPCombined,
                    cbind(cond.multIVhack$FP.rates, 10))
FPCombined <- rbind(FPCombined,
                    cbind(cond.optstop$FP.rates, 9))
FPCombined <- rbind(FPCombined,
                    cbind(cond.outHack$FP.rates, 8))
FPCombined <- rbind(FPCombined,
                    cbind(cond.covhack$FP.rates, 7))
FPCombined <- rbind(FPCombined,
                    cbind(cond.compscoreHack$FP.rates, 6))
FPCombined <- rbind(FPCombined,
                    cbind(cond.varTransHack$FP.rates, 5))
FPCombined <- rbind(FPCombined,
                    cbind(cond.cutoffHack$FP.rates, 4))
FPCombined <- rbind(FPCombined,
                    cbind(cond.statAnalysisHack$FP.rates, 3))
FPCombined <- rbind(FPCombined,
                    cbind(cond.impHack$FP.rates, 2))
FPCombined <- rbind(FPCombined,
                    cbind(cond.subgroupHack$FP.rates, 1))
FPCombined <- rbind(FPCombined,
                    cbind(c(0.051, 0.06), 0))

FPCombined <- as.data.frame(FPCombined)
colnames(FPCombined) <- c("FP.rate", "method")
FPCombined$method <- as.factor(FPCombined$method)
levels(FPCombined$method) <- c("Incorrect rounding",
                               "Inclusion criteria",
                               "Favorable imputation",
                               "Alt. hypothesis tests",
                               "Discretizing variables",
                               "Variable transformation",
                               "Scale redefinition",
                               "Controlling covariates",
                               "Outlier exclusion",
                               "Optional stopping",
                               "Selective reporting IV",
                               "Selective reporting DV")

ggplot(FPCombined, aes(x = FP.rate, y = method)) +
  geom_point(aes(color = method)) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, vjust = -2),
        axis.text.x = element_text(size = 14),
        plot.margin = unit(c(3,3,20,3), "pt")) +
  labs(x = "False Positive Rate") +
  geom_vline(xintercept = 0.05, linetype = "dashed") 
