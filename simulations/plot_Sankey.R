library(networkD3)
load("simulations/SIM_combinedHack_t.RData")

# Nodes: At every stage of the analysis process, the result can either be
# significant or non-significant
nodes <- data.frame("name" = c("start",
                               rep(c("not significant", "significant"), 7)))
nodes$ID <- c(0:(nrow(nodes)-1))

# Links: Determine how many (more) results got significant at the stage
df.SIM_combinedHack_t <- as.data.frame(SIM_combinedHack_t)
df.SIM_combinedHack_t$stage <- as.factor(df.SIM_combinedHack_t$stage)
df.SIM_combinedHack_t$stage <- factor(df.SIM_combinedHack_t$stage,
                                      levels = c(1, 1.5, 2, 2.5, 3, 4, 5, 6))
nHackedAtStage <- table(df.SIM_combinedHack_t$stage)
iter <- sum(nHackedAtStage)

links <- as.data.frame(matrix(c(
  0, 1, (iter-nHackedAtStage[1]), # not sig in stage 1
  0, 2, (nHackedAtStage[1]), # sig in stage 1
  
  1, 3, (iter-sum(nHackedAtStage[1:2])), # ns in stage 1 -> ns in stage 1.5
  1, 4, (nHackedAtStage[2]), # ns in stage 1 -> sig in stage 1.5
  2, 4, (nHackedAtStage[1]), # sig in stage 1 -> sig in stage 1.5
  
  3, 5, (iter-sum(nHackedAtStage[1:3])), # stage 2
  3, 6, (nHackedAtStage[3]), 
  4, 6, (sum(nHackedAtStage[1:2])), 
  
  5, 7, (iter-sum(nHackedAtStage[1:4])), # stage 2.5
  5, 8, (nHackedAtStage[4]), 
  6, 8, (sum(nHackedAtStage[1:3])), 
  
  7, 9, (iter-sum(nHackedAtStage[1:5])), # stage 3
  7, 10, (nHackedAtStage[5]), 
  8, 10, (sum(nHackedAtStage[1:4])),
  
  9, 11, (iter-sum(nHackedAtStage[1:6])), # stage 4
  9, 12, (nHackedAtStage[6]), 
  10, 12, (sum(nHackedAtStage[1:5])), 
  
  11, 13, (iter-sum(nHackedAtStage[1:7])), # stage 5
  11, 14, (nHackedAtStage[7]), 
  12, 14, (sum(nHackedAtStage[1:6]))
  
  
  
), byrow = TRUE, ncol = 3))

names(links) <- c("source", "target", "value")

SankeyPlot_tTest <- sankeyNetwork(Links = links, Nodes = nodes,
                                  Source = "source", Target = "target",
                                  Value = "value", NodeID = "name",
                                  fontSize= 0, nodeWidth = 30, sinksRight = FALSE)

require(htmlwidgets)
saveWidget(SankeyPlot_tTest, file="SankeyPlot_tTest.html")

require(webshot)
webshot("SankeyPlot_tTest.html", 
        "SankeyPlot_tTest.pdf")
