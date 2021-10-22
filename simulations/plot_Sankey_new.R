library(ggplot2)
library(ggforce)
load("simulations/SIM_combinedHack_t.RData")
load("simulations/SIM_combinedHack_reg.RData")

# ------------------------------------------------------------------------------
# Sankey Plot for t-test
# ------------------------------------------------------------------------------

# Data handling for plot

df.SIM_combinedHack_t <- as.data.frame(SIM_combinedHack_t)
df.SIM_combinedHack_t$stage <- as.factor(df.SIM_combinedHack_t$stage)
df.SIM_combinedHack_t$stage <- factor(df.SIM_combinedHack_t$stage,
                                      levels = c(1, 1.5, 2, 2.5, 3, 4, 5, 6))
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
                                        levels = c(1:7), 
                                        labels = c("Original",
                                                   "Rounding 1",
                                                   "Stat. Analysis",
                                                   "Rounding 2",
                                                   "Select DV",
                                                   "Covariates",
                                                   "Subgroups"))
                         )

# Plot

g <- ggplot(hackingFlow) +
  geom_parallel_sets(aes(x = stage, id = test, split = result, value = freq), fill = "grey", alpha = 0.5) +
  geom_parallel_sets_axes(aes(x = stage, id = test, split = result, value = freq), fill = "#99B798", axis.width = 0.4) +
  geom_bar(aes(x = stage, fill = result), width = 0.4) +
  theme_classic() +
  scale_fill_manual(values = c("transparent", "#E84A5F")) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank()
  ) +
  annotate("text", x = 1, y = 150, label = as.character(round(nHackedAtStage[1]/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 2, y = 150, label = as.character(round(sum(nHackedAtStage[1:2])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 3, y = 150, label = as.character(round(sum(nHackedAtStage[1:3])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 4, y = 150, label = as.character(round(sum(nHackedAtStage[1:4])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 5, y = 150, label = as.character(round(sum(nHackedAtStage[1:5])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 6, y = 150, label = as.character(round(sum(nHackedAtStage[1:6])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 7, y = 150, label = as.character(round(sum(nHackedAtStage[1:7])/iter, digits = 2)), size = 8, color = "white") +
  
  annotate("text", x = 1, y = 5000, label = as.character(round(1-nHackedAtStage[1]/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 2, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:2])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 3, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:3])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 4, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:4])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 5, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:5])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 6, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:6])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 7, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:7])/iter, digits = 2)), size = 8, color = "white")
  
g

# ------------------------------------------------------------------------------
# Sankey Plot for regression
# ------------------------------------------------------------------------------

# Data handling for plot

df.SIM_combinedHack_reg <- as.data.frame(SIM_combinedHack_reg)
df.SIM_combinedHack_reg$stage <- as.factor(df.SIM_combinedHack_reg$stage)
df.SIM_combinedHack_reg$stage <- factor(df.SIM_combinedHack_reg$stage,
                                      levels = c(1, 1.5, 2, 2.5, 3, 4, 5, 6))
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
                                        levels = c(1:7), 
                                        labels = c("Original",
                                                   "Rounding 1",
                                                   "Imputation",
                                                   "Rounding 2",
                                                   "Transform.",
                                                   "Scale Redef.",
                                                   "Outlier"))
)

# Plot

g <- ggplot(hackingFlow) +
  geom_parallel_sets(aes(x = stage, id = test, split = result, value = freq), fill = "grey", alpha = 0.5) +
  geom_parallel_sets_axes(aes(x = stage, id = test, split = result, value = freq), fill = "#99B798", axis.width = 0.4) +
  geom_bar(aes(x = stage, fill = result), width = 0.4) +
  theme_classic() +
  scale_fill_manual(values = c("transparent", "#E84A5F")) +
  scale_x_discrete(position = "top") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank()
  ) +
  annotate("text", x = 1, y = 150, label = as.character(round(nHackedAtStage[1]/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 2, y = 150, label = as.character(round(sum(nHackedAtStage[1:2])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 3, y = 150, label = as.character(round(sum(nHackedAtStage[1:3])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 4, y = 150, label = as.character(round(sum(nHackedAtStage[1:4])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 5, y = 150, label = as.character(round(sum(nHackedAtStage[1:5])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 6, y = 150, label = as.character(round(sum(nHackedAtStage[1:6])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 7, y = 150, label = as.character(round(sum(nHackedAtStage[1:7])/iter, digits = 2)), size = 8, color = "white") +
  
  annotate("text", x = 1, y = 5000, label = as.character(round(1-nHackedAtStage[1]/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 2, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:2])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 3, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:3])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 4, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:4])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 5, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:5])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 6, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:6])/iter, digits = 2)), size = 8, color = "white") +
  annotate("text", x = 7, y = 5000, label = as.character(round(1-sum(nHackedAtStage[1:7])/iter, digits = 2)), size = 8, color = "white")

g
