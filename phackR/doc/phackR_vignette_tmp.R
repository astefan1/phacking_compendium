## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(phackR)

## ----selectiveReportingDV-----------------------------------------------------
set.seed(1234)
sim.multDVhack(nobs.group = 30, nvar = 5, r = 0.3, strategy = "smallest", 
               iter = 10, alternative = "two.sided", alpha = 0.05)

## ----selectiveReportingIV-----------------------------------------------------
set.seed(1234)
sim.multIVhack(nobs.group = 30, nvar = 5, r = 0.3, strategy = "smallest", 
               iter = 10, alternative = "two.sided", alpha = 0.05)

## ----incorrectRounding--------------------------------------------------------
set.seed(1234)
sim.roundhack(roundinglevel = 0.06, iter = 10, alternative = "two.sided", 
              alpha = 0.05)

## ----optionalStopping---------------------------------------------------------
set.seed(1234)
sim.optstop(n.min = 10, n.max = 20, step = 2, alternative = "two.sided", 
            iter = 10, alpha = 0.05)

## ----outlierExclusion---------------------------------------------------------
set.seed(1234)
sim.outHack(nobs = 30, which = "random", strategy = "smallest", alpha = 0.05, 
            iter = 10)

## ----exploitCovariates--------------------------------------------------------
set.seed(1234)
sim.covhack(nobs.group = 30, ncov = 4, rcov = 0.3, rcovdv = 0.5, 
            interactions = FALSE, strategy = "smallest", 
            alpha = 0.05, iter = 10)

## ----subgroupAnalysis---------------------------------------------------------
set.seed(1234)
sim.subgroupHack(nobs.group = 30, nsubvars = 3, alternative = "two.sided", 
                 strategy = "smallest", alpha = 0.05, iter = 10)

## ----compositeScores----------------------------------------------------------
set.seed(1234)
sim.compscoreHack(nobs = 30, ncompv = 5, rcomp = 0.7, ndelete = 3, 
                  strategy = "smallest", alpha = 0.05, iter = 10)

## ----selectEffects------------------------------------------------------------
set.seed(1234)
sim.selectEffects(nobs = 30, niv = 4, riv = 0.1, interactions = FALSE, 
                  strategy = "smallest", alpha = 0.05, iter = 10)

## ----variableTransformation---------------------------------------------------
set.seed(1234)
sim.varTransHack(nobs = 30, transvar = "xy", strategy = "smallest", 
                 alpha = 0.05, iter = 10)

## ----exploitCutoffs-----------------------------------------------------------
set.seed(1234)
sim.cutoffHack(nobs = 30, strategy = "smallest", alpha = 0.05, iter = 10)

## ----statAnalysis-------------------------------------------------------------
set.seed(1234)
sim.statAnalysisHack(nobs.group = 30, strategy = "smallest", 
                     alternative = "two.sided", alpha = 0.05, iter = 10)

## ----favorableImputation------------------------------------------------------
set.seed(1234)
sim.impHack(nobs = 30, missing = 0.2, which = c(1:10), strategy = "smallest", 
            alpha = 0.05, iter = 10)

