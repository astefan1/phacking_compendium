library(shinydashboard)

function(input, output) {

  # ------------------- Composite Scores ---------------------------------------

  output$uindeleteCompScores <- renderUI({
    sliderInput("ndeleteCompScores", label = "Maximum number of variables to be deleted from composite score",
                value = 2, min = 1, max = input$ncompvCompScores - 3, step = 1)
  })

  observeEvent(input$calcCompScores > 0, {
      ifelse(length(input$uindeleteCompScores)==0, ndelete<-2, ndelete<-input$uindeleteCompScores)
      res1 <- sim.compscoreHack(nobs=input$nobsCompScores, ncompv=input$ncompvCompScores, rcomp=input$rcompCompScores, ndelete=ndelete, strategy = input$strategyCompScores, alpha = input$alphaCompScores, iter = input$iterCompScores, shinyEnv=TRUE)
      compscorePlot <- pplots(simdat=res1, alpha=input$alphaCompScores)
      compscorePlotES <- esplots(simdat=res1, EScolumn.hack=3, EScolumn.orig=4)
      compscore.fprate.p <- paste0(sum(res1[,"ps.hack"] < input$alphaCompScores)/input$iterCompScores*100, " %")
      compscore.fprate.o <- paste0(sum(res1[,"ps.orig"] < input$alphaCompScores)/input$iterCompScores*100, " %")
      output$compScoresPlot1 = renderPlot(compscorePlot$phack)
      output$compScoresPlot2 = renderPlot(compscorePlot$pnohack)
      output$compScoresPlot3 = renderPlot(compscorePlot$pcomp)
      output$compScoresFPHack = renderText(compscore.fprate.p)
      output$compScoresFPOrig = renderText(compscore.fprate.o)
      output$compScoresPlot4 = renderPlot(compscorePlotES$eshack)
      output$compScoresPlot5 = renderPlot(compscorePlotES$esnohack)
  })

  # ------------------- Exploit Covariates -------------------------------------

  observeEvent(input$calcExpCov > 0, {
    if(input$interactExpCov == "Yes") interactions <- TRUE
    else if(input$interactExpCov == "No") interactions <- FALSE
    res2 <- sim.covhack(nobs.group = input$nobsExpCov, ncov = input$ncovExpCov, rcov = input$rcovExpCov, rcovdv = input$rcovdvExpCov, interactions = interactions, strategy = input$strategyExpCov, alpha = input$alphaExpCov, iter = input$iterExpCov, shinyEnv=TRUE)
    expCovPlot <- pplots(simdat=res2, alpha=input$alphaExpCov)
    expCovES <- esplots(simdat=res2, EScolumn.hack=3, EScolumn.orig=4, titles = c(expression("Distribution of p-hacked effect sizes "*eta^2),
                                                                                 expression("Distribution of original effect sizes "*eta^2)))
    expcov.fprate.p <- paste0(sum(res2[,"ps.hack"] < input$alphaExpCov)/input$iterExpCov*100, " %")
    expcov.fprate.o <- paste0(sum(res2[,"ps.orig"] < input$alphaExpCov)/input$iterExpCov*100, " %")
    output$expCovPlot1 = renderPlot(expCovPlot$phack)
    output$expCovPlot2 = renderPlot(expCovPlot$pnohack)
    output$expCovPlot3 = renderPlot(expCovPlot$pcomp)
    output$expCovFPHack = renderText(expcov.fprate.p)
    output$expCovFPOrig = renderText(expcov.fprate.o)
    output$expCovPlot4 = renderPlot(expCovES$eshack)
    output$expCovPlot5 = renderPlot(expCovES$esnohack)
  })

  # ------------------- Exploit Cutoffs ----------------------------------------

  observeEvent(input$calcExpCut > 0, {
    res3 <- sim.cutoffHack(nobs = input$nobsExpCut, strategy = input$strategyExpCut, alpha = input$alphaExpCut, iter = input$iterExpCut, shinyEnv=TRUE)
    expCutPlot <- pplots(simdat=res3, alpha=input$alphaExpCut)
    expCutES <- esplots(simdat=res3, EScolumn.hack=3, EScolumn.orig=4)
    expcut.fprate.p <- paste0(sum(res3[,"ps.hack"] < input$alphaExpCut)/input$iterExpCut*100, " %")
    expcut.fprate.o <- paste0(sum(res3[,"ps.orig"] < input$alphaExpCut)/input$iterExpCut*100, " %")
    output$expCutPlot1 = renderPlot(expCutPlot$phack)
    output$expCutPlot2 = renderPlot(expCutPlot$pnohack)
    output$expCutPlot3 = renderPlot(expCutPlot$pcomp)
    output$expCutFPHack = renderText(expcut.fprate.p)
    output$expCutFPOrig = renderText(expcut.fprate.o)
    output$expCutPlot4 = renderPlot(expCutES$eshack)
    output$expCutPlot5 = renderPlot(expCutES$esnohack)
  })

  # ------------------- Favorable Imputation -----------------------------------

  observeEvent(input$calcfavImp > 0, {
    res4 <- sim.impHack(nobs = input$nobsfavImp, missing = input$missingfavImp, which = as.numeric(input$whichImpfavImp), strategy = input$strategyfavImp, alpha = input$alphafavImp, iter = input$iterfavImp, shinyEnv = TRUE)
    favImpPlot <- pplots(simdat=res4, alpha=input$alphafavImp)
    favImpES <- esplots(simdat=res4, EScolumn.hack=3, EScolumn.orig=4)
    favimp.fprate.p <- paste0(sum(res4[,"ps.hack"] < input$alphafavImp)/input$iterfavImp*100, " %")
    favimp.fprate.o <- paste0(sum(res4[,"ps.orig"] < input$alphafavImp)/input$iterfavImp*100, " %")
    output$favImpPlot1 = renderPlot(favImpPlot$phack)
    output$favImpPlot2 = renderPlot(favImpPlot$pnohack)
    output$favImpPlot3 = renderPlot(favImpPlot$pcomp)
    output$favImpFPHack = renderText(favimp.fprate.p)
    output$favImpFPOrig = renderText(favimp.fprate.o)
    output$favImpPlot4 = renderPlot(favImpES$eshack)
    output$favImpPlot5 = renderPlot(favImpES$esnohack)
  })

  # ------------------- Incorrect Rounding -------------------------------------

  observeEvent(input$calcRounding > 0, {
    res5 <- sim.roundhack(roundinglevel = input$levelRounding+input$alphaRounding, iter = input$iterRounding, alternative = input$altRounding, alpha = input$alphaRounding, shinyEnv = TRUE)
    roundingPlot <- pplots(simdat=res5, alpha=input$alphaRounding)
    roundingES <- esplots(simdat=res5, EScolumn.hack=3, EScolumn.orig=4)
    rounding.fprate.p <- paste0(sum(res5[,"ps.hack"] <= input$alphaRounding)/input$iterRounding*100, " %")
    rounding.fprate.o <- paste0(sum(res5[,"ps.orig"] <= input$alphaRounding)/input$iterRounding*100, " %")
    output$roundingPlot1 = renderPlot(roundingPlot$phack)
    output$roundingPlot2 = renderPlot(roundingPlot$pnohack)
    output$roundingPlot3 = renderPlot(roundingPlot$pcomp)
    output$roundingFPHack = renderText(rounding.fprate.p)
    output$roundingFPOrig = renderText(rounding.fprate.o)
    output$roundingPlot4 = renderPlot(roundingES$eshack)
    output$roundingPlot5 = renderPlot(roundingES$esnohack)
  })

  # ------------------- Optional Stopping --------------------------------------

  observeEvent(input$calcOptStop > 0, {
    res6 <- sim.optstop(n.min = input$nminOptStop, n.max = input$nmaxOptStop, step = input$stepOptStop, alternative = input$altOptStop, iter = input$iterOptStop, alpha = input$alphaOptStop, shinyEnv = TRUE)
    optstopPlot <- pplots(simdat = res6, alpha = input$alphaOptStop)
    optstopESr2 <- esplots(simdat=res6, EScolumn.hack=3, EScolumn.orig=4)
    optstopESd <- esplots(simdat=res6, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                    expression("Distribution of original effect sizes "*delta)))
    optstop.fprate.p <- paste0(sum(res6[,"ps.hack"] <= input$alphaOptStop)/input$iterOptStop*100, " %")
    optstop.fprate.o <- paste0(sum(res6[,"ps.orig"] <= input$alphaOptStop)/input$iterOptStop*100, " %")
    output$optStopPlot1 <- renderPlot(optstopPlot$phack)
    output$optStopPlot2 <- renderPlot(optstopPlot$pnohack)
    output$optStopPlot3 <- renderPlot(optstopPlot$pcomp)
    output$optStopFPHack = renderText(optstop.fprate.p)
    output$optStopFPOrig = renderText(optstop.fprate.o)
    output$optStopPlot4 = renderPlot(optstopESr2$eshack)
    output$optStopPlot5 = renderPlot(optstopESr2$esnohack)
    output$optStopPlot6 = renderPlot(optstopESd$eshack)
    output$optStopPlot7 = renderPlot(optstopESd$esnohack)
  })
  
  # ------------------- Outlier Exclusion --------------------------------------
  
  observeEvent(input$calcOutExcl > 0, {
    res7 <- sim.outHack(nobs = input$nobsOutExcl, which = as.numeric(input$whichOutExcl), strategy = input$strategyOutExcl, alpha = input$alphaOutExcl, iter = input$iterOutExcl, shinyEnv = TRUE)
    outExclPlot <- pplots(simdat = res7, alpha = input$alphaOutExcl)
    outExclES <- esplots(simdat = res7, EScolumn.hack = 3, EScolumn.orig = 4)
    outExcl.fprate.p <- paste0(sum(res7[,"ps.hack"] <= input$alphaOutExcl)/input$iterOutExcl*100, " %")
    outExcl.fprate.o <- paste0(sum(res7[,"ps.orig"] <= input$alphaOutExcl)/input$iterOutExcl*100, " %")
    output$outExclPlot1 <- renderPlot(outExclPlot$phack)
    output$outExclPlot2 <- renderPlot(outExclPlot$pnohack)
    output$outExclPlot3 <- renderPlot(outExclPlot$pcomp)
    output$outExclFPHack = renderText(outExcl.fprate.p)
    output$outExclFPOrig = renderText(outExcl.fprate.o)
    output$outExclPlot4 = renderPlot(outExclES$eshack)
    output$outExclPlot5 = renderPlot(outExclES$esnohack)
  })
  
  # ------------------- Selective Reporting of Effects -------------------------
  
  observeEvent(input$calcSelectEff > 0, {
    if(input$interactSelectEff == "Yes") interactions <- TRUE
    else if(input$interactSelectEff == "No") interactions <- FALSE
    res8 <- sim.selectEffects(nobs = input$nobsSelectEff, niv = input$nivSelectEff, interactions = interactions, riv = input$rivSelectEff, strategy = input$strategySelectEff, alpha = input$alphaSelectEff, iter = input$iterSelectEff, shinyEnv = TRUE)
    selectEffPlot <- pplots(simdat = res8, alpha = input$alphaSelectEff)
    selectEffES <- esplots(simdat = res8, EScolumn.hack = 3, EScolumn.orig = 4)
    selectEff.fprate.p <- paste0(sum(res8[,"ps.hack"] <= input$alphaSelectEff)/input$iterSelectEff*100, " %")
    selectEff.fprate.o <- paste0(sum(res8[,"ps.orig"] <= input$alphaSelectEff)/input$iterSelectEff*100, " %")
    output$selectEffPlot1 <- renderPlot(selectEffPlot$phack)
    output$selectEffPlot2 <- renderPlot(selectEffPlot$pnohack)
    output$selectEffPlot3 <- renderPlot(selectEffPlot$pcomp)
    output$selectEffFPHack = renderText(selectEff.fprate.p)
    output$selectEffFPOrig = renderText(selectEff.fprate.o)
    output$selectEffPlot4 = renderPlot(selectEffES$eshack)
    output$selectEffPlot5 = renderPlot(selectEffES$esnohack)
  })
  
  # ------------------- Selective Reporting of DVs -----------------------------
  
  observeEvent(input$calcSRDV > 0, {
    res9 <- sim.multDVhack(nobs.group = input$nobsSRDV, nvar = input$nvarSRDV, r = input$rSRDV, strategy = input$strategySRDV, iter = input$iterSRDV, alternative = input$altSRDV, alpha = input$alphaSRDV, shinyEnv = TRUE)
    SRDVPlot <- pplots(simdat = res9, alpha = input$alphaSRDV)
    SRDVESr2 <- esplots(simdat=res9, EScolumn.hack=3, EScolumn.orig=4)
    SRDVESd <- esplots(simdat=res9, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                    expression("Distribution of original effect sizes "*delta)))
    SRDV.fprate.p <- paste0(sum(res9[,"ps.hack"] <= input$alphaSRDV)/input$iterSRDV*100, " %")
    SRDV.fprate.o <- paste0(sum(res9[,"ps.orig"] <= input$alphaSRDV)/input$iterSRDV*100, " %")
    output$SRDVPlot1 <- renderPlot(SRDVPlot$phack)
    output$SRDVPlot2 <- renderPlot(SRDVPlot$pnohack)
    output$SRDVPlot3 <- renderPlot(SRDVPlot$pcomp)
    output$SRDVFPHack = renderText(SRDV.fprate.p)
    output$SRDVFPOrig = renderText(SRDV.fprate.o)
    output$SRDVPlot4 = renderPlot(SRDVESr2$eshack)
    output$SRDVPlot5 = renderPlot(SRDVESr2$esnohack)
    output$SRDVPlot6 = renderPlot(SRDVESd$eshack)
    output$SRDVPlot7 = renderPlot(SRDVESd$esnohack)
  })
  
  # ------------------- Selective Reporting of IVs -----------------------------
  
  observeEvent(input$calcSRIV > 0, {
    res10 <- sim.multDVhack(nobs.group = input$nobsSRIV, nvar = input$nvarSRIV, r = input$rSRIV, strategy = input$strategySRIV, iter = input$iterSRIV, alternative = input$altSRIV, alpha = input$alphaSRIV, shinyEnv = TRUE)
    SRIVPlot <- pplots(simdat = res10, alpha = input$alphaSRIV)
    SRIVESr2 <- esplots(simdat=res10, EScolumn.hack=3, EScolumn.orig=4)
    SRIVESd <- esplots(simdat=res10, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                 expression("Distribution of original effect sizes "*delta)))
    SRIV.fprate.p <- paste0(sum(res10[,"ps.hack"] <= input$alphaSRIV)/input$iterSRIV*100, " %")
    SRIV.fprate.o <- paste0(sum(res10[,"ps.orig"] <= input$alphaSRIV)/input$iterSRIV*100, " %")
    output$SRIVPlot1 <- renderPlot(SRIVPlot$phack)
    output$SRIVPlot2 <- renderPlot(SRIVPlot$pnohack)
    output$SRIVPlot3 <- renderPlot(SRIVPlot$pcomp)
    output$SRIVFPHack = renderText(SRIV.fprate.p)
    output$SRIVFPOrig = renderText(SRIV.fprate.o)
    output$SRIVPlot4 = renderPlot(SRIVESr2$eshack)
    output$SRIVPlot5 = renderPlot(SRIVESr2$esnohack)
    output$SRIVPlot6 = renderPlot(SRIVESd$eshack)
    output$SRIVPlot7 = renderPlot(SRIVESd$esnohack)
  })
  
  # --------------- Exploiting Statistical Analysis ----------------------------
  
  observeEvent(input$calcStatAnalysis > 0, {
    res11 <- sim.statAnalysisHack(nobs.group = input$nobsStatAnalysis, strategy = input$strategyStatAnalysis, alternative = input$altStatAnalysis, alpha = input$alphaStatAnalysis, iter = input$iterStatAnalysis, shinyEnv = TRUE)
    statAnalysisPlot <- pplots(simdat = res11, alpha = input$alphaStatAnalysis)
    statAnalysis.fprate.p <- paste0(sum(res11[,"ps.hack"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, " %")
    statAnalysis.fprate.o <- paste0(sum(res11[,"ps.orig"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, " %")
    output$statAnalysisPlot1 <- renderPlot(statAnalysisPlot$phack)
    output$statAnalysisPlot2 <- renderPlot(statAnalysisPlot$pnohack)
    output$statAnalysisPlot3 <- renderPlot(statAnalysisPlot$pcomp)
    output$statAnalysisFPHack = renderText(statAnalysis.fprate.p)
    output$statAnalysisFPOrig = renderText(statAnalysis.fprate.o)
  })
  
  # --------------- Subgroup Analyses / Inclusion Criteria ---------------------
  
  observeEvent(input$calcSubgroup > 0, {
    res12 <- sim.subgroupHack(nobs.group = input$nobsSubgroup, nsubvars = input$nsubvarsSubgroup, alternative = input$altSubgroup, strategy = input$strategySubgroup, alpha = input$alphaSubgroup, iter = input$iterSubgroup, shinyEnv = TRUE)
    subgroupPlot <- pplots(simdat = res12, alpha = input$alphaSubgroup)
    subgroupESr2 <- esplots(simdat=res12, EScolumn.hack=3, EScolumn.orig=4)
    subgroupESd <- esplots(simdat=res12, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                 expression("Distribution of original effect sizes "*delta)))
    subgroup.fprate.p <- paste0(sum(res12[,"ps.hack"] <= input$alphaSubgroup)/input$iterSubgroup*100, " %")
    subgroup.fprate.o <- paste0(sum(res12[,"ps.orig"] <= input$alphaSubgroup)/input$iterSubgroup*100, " %")
    output$subgroupPlot1 <- renderPlot(subgroupPlot$phack)
    output$subgroupPlot2 <- renderPlot(subgroupPlot$pnohack)
    output$subgroupPlot3 <- renderPlot(subgroupPlot$pcomp)
    output$subgroupFPHack = renderText(subgroup.fprate.p)
    output$subgroupFPOrig = renderText(subgroup.fprate.o)
    output$subgroupPlot4 = renderPlot(subgroupESr2$eshack)
    output$subgroupPlot5 = renderPlot(subgroupESr2$esnohack)
    output$subgroupPlot6 = renderPlot(subgroupESd$eshack)
    output$subgroupPlot7 = renderPlot(subgroupESd$esnohack)
  })
  
  
}
