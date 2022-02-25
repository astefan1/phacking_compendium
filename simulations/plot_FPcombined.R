library(ggplot2)
library(ggforce)
load("simulations/SIM_combinedHack_t.RData")
load("simulations/SIM_combinedHack_reg.RData")

# ------------------------------------------------------------------------------
# t-test stacked barplot
# ------------------------------------------------------------------------------

# Data handling for plot

df.SIM_combinedHack_t <- as.data.frame(SIM_combinedHack_t)
df.SIM_combinedHack_t$stage <- as.factor(df.SIM_combinedHack_t$stage)
df.SIM_combinedHack_t$stage <- factor(df.SIM_combinedHack_t$stage,
                                      levels = c(1, 1.5, 2, 2.5, 3, 4, 5, 6))

# How often did incorrect rounding succeed?

RoundSucceed <- sum(df.SIM_combinedHack_t$stage == 1.5) + 
  sum(df.SIM_combinedHack_t$stage == 2.5) + 
  nrow(df.SIM_combinedHack_t[df.SIM_combinedHack_t$ps.hack > 0.05 & df.SIM_combinedHack_t$stage != 6, ])

OverallSucceed <- sum(df.SIM_combinedHack_t$stage != 6)

RoundPercent <- RoundSucceed / OverallSucceed

# Incorporate initial incorrect rounding into stage 2
df.SIM_combinedHack_t$stage <- forcats::fct_collapse(df.SIM_combinedHack_t$stage, 
                                                     "2" = c("1.5", "2", "2.5"))

# False positive results per stage
nHackedAtStage <- table(df.SIM_combinedHack_t$stage)
iter <- sum(nHackedAtStage)

hackingFlow <- matrix(NA, nrow = (length(nHackedAtStage)-1) * iter, ncol = 4)
colnames(hackingFlow) <- c("stage", "result", "test", "freq")

for(i in 1:(length(nHackedAtStage)-1)){
  hackingFlow[((i-1)*iter+1):(i*iter), ] <- matrix(c(
    rep(i, iter),
    c(rep(1, sum(nHackedAtStage[1:i])), rep(0, iter-sum(nHackedAtStage[1:i]))),
    c(1:iter),
    rep(1, iter)
  ), byrow = FALSE, ncol = 4)
}

hackingFlow <- as.data.frame(hackingFlow)
hackingFlow <- transform(hackingFlow,
                         result = factor(result, 
                                         levels = c(0,1),
                                         labels = c("not significant",
                                                    "significant")),
                         stage = factor(stage, 
                                        levels = c(1:5), 
                                        labels = c("Original",
                                                   "Stat. Analysis",
                                                   "Select DV",
                                                   "Covariates",
                                                   "Inclusion Crit."))
                         )

# Plot

ggplot(hackingFlow,
       aes(fill = result, y = freq/iter, x = stage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("grey20", "#E84A5F")) +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                      ends = "last"))) +
  labs(title = "",
       x = "",
       y = "False Positive Rate",
       fill = "") +
  geom_hline(yintercept = 0.05, col = "grey")
  
  

# ------------------------------------------------------------------------------
# regression stacked barplot
# ------------------------------------------------------------------------------

# Data handling for plot

df.SIM_combinedHack_reg <- as.data.frame(SIM_combinedHack_reg)
df.SIM_combinedHack_reg$stage <- as.factor(df.SIM_combinedHack_reg$stage)
df.SIM_combinedHack_reg$stage <- factor(df.SIM_combinedHack_reg$stage,
                                      levels = c(1, 1.5, 2, 3, 4, 5, 6))

# How often did incorrect rounding succeed?

RoundSucceed <- sum(df.SIM_combinedHack_reg$stage == 1.5) + 
  sum(df.SIM_combinedHack_reg$stage == 2.5) + 
  nrow(df.SIM_combinedHack_reg[df.SIM_combinedHack_reg$ps.hack > 0.05 & df.SIM_combinedHack_reg$stage != 6, ])

OverallSucceed <- sum(df.SIM_combinedHack_reg$stage != 6)

RoundPercent <- RoundSucceed / OverallSucceed


# Incorporate initial incorrect rounding into stage 2
df.SIM_combinedHack_reg$stage <- forcats::fct_collapse(df.SIM_combinedHack_reg$stage, 
                                                     "2" = c("1.5", "2"))

nHackedAtStage <- table(df.SIM_combinedHack_reg$stage)
iter <- sum(nHackedAtStage)

hackingFlow <- matrix(NA, nrow = (length(nHackedAtStage)-1) * iter, ncol = 4)
colnames(hackingFlow) <- c("stage", "result", "test", "freq")

for(i in 1:(length(nHackedAtStage)-1)){
  hackingFlow[((i-1)*iter+1):(i*iter), ] <- matrix(c(
    rep(i, iter),
    c(rep(1, sum(nHackedAtStage[1:i])), rep(0, iter-sum(nHackedAtStage[1:i]))),
    c(1:iter),
    rep(1, iter)
  ), byrow = FALSE, ncol = 4)
}

hackingFlow <- as.data.frame(hackingFlow)
hackingFlow <- transform(hackingFlow,
                         result = factor(result, 
                                         levels = c(0,1),
                                         labels = c("not significant",
                                                    "significant")),
                         stage = factor(stage, 
                                        levels = c(1:5), 
                                        labels = c("Original",
                                                   "Imputation",
                                                   "Transform.",
                                                   "Scale Redef.",
                                                   "Outlier"))
)

# Plot

ggplot(hackingFlow,
       aes(fill = result, y = freq/iter, x = stage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("grey20", "#E84A5F")) +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"),
                                                       ends = "last"))) +
  labs(title = "",
       x = "",
       y = "False Positive Rate",
       fill = "") +
  geom_hline(yintercept = 0.05, col = "grey")

# How much does outlier exclusion raise FP rate?

sum(df.SIM_combinedHack_reg$stage %in% c(1:5))/5000-sum(df.SIM_combinedHack_reg$stage %in% c(1:4))/5000
