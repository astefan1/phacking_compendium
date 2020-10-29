library(shinydashboard)

startplots <- readRDS("data/startplots.rds")

function(input, output) {

  # ------------------- Composite Scores ---------------------------------------

  output$uindeleteCompScores <- renderUI({
    sliderInput("ndeleteCompScores", label = "Maximum number of variables to be deleted from composite score",
                value = 2, min = 1, max = input$ncompvCompScores - 3, step = 1)
  })
  
  output$compScoresPlot1 = renderPlot(startplots$compscorePlot$phack)
  output$compScoresPlot2 = renderPlot(startplots$compscorePlot$pnohack)
  output$compScoresPlot3 = renderPlot(startplots$compscorePlot$pcomp)
  output$compScoresFPHack = renderText(startplots$compscore.fprate.p)
  output$compScoresFPOrig = renderText(startplots$compscore.fprate.o)
  output$compScoresPlot4 = renderPlot(startplots$compscorePlotES$eshack)
  output$compScoresPlot5 = renderPlot(startplots$compscorePlotES$esnohack)

  observeEvent(input$calcCompScores > 0, {
      ifelse(length(input$uindeleteCompScores)==0, ndelete<-2, ndelete<-input$uindeleteCompScores)
      res1 <- sim.compscoreHack(nobs=input$nobsCompScores, ncompv=input$ncompvCompScores, rcomp=input$rcompCompScores, ndelete=ndelete, strategy = input$strategyCompScores, alpha = input$alphaCompScores, iter = input$iterCompScores, shinyEnv=TRUE)
      compscorePlot <- pplots(simdat=res1, alpha=input$alphaCompScores)
      compscorePlotES <- esplots(simdat=res1, EScolumn.hack=3, EScolumn.orig=4)
      compscore.fprate.p <- paste0(round(sum(res1[,"ps.hack"] < input$alphaCompScores)/input$iterCompScores*100, 2), " %")
      compscore.fprate.o <- paste0(round(sum(res1[,"ps.orig"] < input$alphaCompScores)/input$iterCompScores*100, 2), " %")
      output$compScoresPlot1 = renderPlot(compscorePlot$phack)
      output$compScoresPlot2 = renderPlot(compscorePlot$pnohack)
      output$compScoresPlot3 = renderPlot(compscorePlot$pcomp)
      output$compScoresFPHack = renderText(compscore.fprate.p)
      output$compScoresFPOrig = renderText(compscore.fprate.o)
      output$compScoresPlot4 = renderPlot(compscorePlotES$eshack)
      output$compScoresPlot5 = renderPlot(compscorePlotES$esnohack)
  }, ignoreInit = TRUE)

  # ------------------- Exploit Covariates -------------------------------------
  
  output$expCovPlot1 = renderPlot(startplots$expCovPlot$phack)
  output$expCovPlot2 = renderPlot(startplots$expCovPlot$pnohack)
  output$expCovPlot3 = renderPlot(startplots$expCovPlot$pcomp)
  output$expCovFPHack = renderText(startplots$expcov.fprate.p)
  output$expCovFPOrig = renderText(startplots$expcov.fprate.o)
  output$expCovPlot4 = renderPlot(startplots$expCovES$eshack)
  output$expCovPlot5 = renderPlot(startplots$expCovES$esnohack)

  observeEvent(input$calcExpCov > 0, {
    if(input$interactExpCov == "Yes") interactions <- TRUE
    else if(input$interactExpCov == "No") interactions <- FALSE
    res2 <- sim.covhack(nobs.group = input$nobsExpCov, ncov = input$ncovExpCov, rcov = input$rcovExpCov, rcovdv = input$rcovdvExpCov, interactions = interactions, strategy = input$strategyExpCov, alpha = input$alphaExpCov, iter = input$iterExpCov, shinyEnv=TRUE)
    expCovPlot <- pplots(simdat=res2, alpha=input$alphaExpCov)
    expCovES <- esplots(simdat=res2, EScolumn.hack=3, EScolumn.orig=4, titles = c(expression("Distribution of p-hacked effect sizes "*eta^2),
                                                                                 expression("Distribution of original effect sizes "*eta^2)))
    expcov.fprate.p <- paste0(round(sum(res2[,"ps.hack"] < input$alphaExpCov)/input$iterExpCov*100, 2), " %")
    expcov.fprate.o <- paste0(round(sum(res2[,"ps.orig"] < input$alphaExpCov)/input$iterExpCov*100, 2), " %")
    output$expCovPlot1 = renderPlot(expCovPlot$phack)
    output$expCovPlot2 = renderPlot(expCovPlot$pnohack)
    output$expCovPlot3 = renderPlot(expCovPlot$pcomp)
    output$expCovFPHack = renderText(expcov.fprate.p)
    output$expCovFPOrig = renderText(expcov.fprate.o)
    output$expCovPlot4 = renderPlot(expCovES$eshack)
    output$expCovPlot5 = renderPlot(expCovES$esnohack)
  }, ignoreInit = TRUE)

  # ------------------- Exploit Cutoffs ----------------------------------------

  output$expCutPlot1 = renderPlot(startplots$expCutPlot$phack)
  output$expCutPlot2 = renderPlot(startplots$expCutPlot$pnohack)
  output$expCutPlot3 = renderPlot(startplots$expCutPlot$pcomp)
  output$expCutFPHack = renderText(startplots$expcut.fprate.p)
  output$expCutFPOrig = renderText(startplots$expcut.fprate.o)
  output$expCutPlot4 = renderPlot(startplots$expCutES$eshack)
  output$expCutPlot5 = renderPlot(startplots$expCutES$esnohack)
  
  observeEvent(input$calcExpCut > 0, {
    res3 <- sim.cutoffHack(nobs = input$nobsExpCut, strategy = input$strategyExpCut, alpha = input$alphaExpCut, iter = input$iterExpCut, shinyEnv=TRUE)
    expCutPlot <- pplots(simdat=res3, alpha=input$alphaExpCut)
    expCutES <- esplots(simdat=res3, EScolumn.hack=3, EScolumn.orig=4)
    expcut.fprate.p <- paste0(round(sum(res3[,"ps.hack"] < input$alphaExpCut)/input$iterExpCut*100, 2), " %")
    expcut.fprate.o <- paste0(round(sum(res3[,"ps.orig"] < input$alphaExpCut)/input$iterExpCut*100, 2), " %")
    output$expCutPlot1 = renderPlot(expCutPlot$phack)
    output$expCutPlot2 = renderPlot(expCutPlot$pnohack)
    output$expCutPlot3 = renderPlot(expCutPlot$pcomp)
    output$expCutFPHack = renderText(expcut.fprate.p)
    output$expCutFPOrig = renderText(expcut.fprate.o)
    output$expCutPlot4 = renderPlot(expCutES$eshack)
    output$expCutPlot5 = renderPlot(expCutES$esnohack)
  }, ignoreInit = TRUE)

  # ------------------- Favorable Imputation -----------------------------------

  output$favImpPlot1 = renderPlot(startplots$favImpPlot$phack)
  output$favImpPlot2 = renderPlot(startplots$favImpPlot$pnohack)
  output$favImpPlot3 = renderPlot(startplots$favImpPlot$pcomp)
  output$favImpFPHack = renderText(startplots$favimp.fprate.p)
  output$favImpFPOrig = renderText(startplots$favimp.fprate.o)
  output$favImpPlot4 = renderPlot(startplots$favImpES$eshack)
  output$favImpPlot5 = renderPlot(startplots$favImpES$esnohack)
  
  observeEvent(input$calcfavImp > 0, {
    res4 <- sim.impHack(nobs = input$nobsfavImp, missing = input$missingfavImp, which = as.numeric(input$whichImpfavImp), strategy = input$strategyfavImp, alpha = input$alphafavImp, iter = input$iterfavImp, shinyEnv = TRUE)
    favImpPlot <- pplots(simdat=res4, alpha=input$alphafavImp)
    favImpES <- esplots(simdat=res4, EScolumn.hack=3, EScolumn.orig=4)
    favimp.fprate.p <- paste0(round(sum(res4[,"ps.hack"] < input$alphafavImp)/input$iterfavImp*100, 2), " %")
    favimp.fprate.o <- paste0(round(sum(res4[,"ps.orig"] < input$alphafavImp)/input$iterfavImp*100, 2), " %")
    output$favImpPlot1 = renderPlot(favImpPlot$phack)
    output$favImpPlot2 = renderPlot(favImpPlot$pnohack)
    output$favImpPlot3 = renderPlot(favImpPlot$pcomp)
    output$favImpFPHack = renderText(favimp.fprate.p)
    output$favImpFPOrig = renderText(favimp.fprate.o)
    output$favImpPlot4 = renderPlot(favImpES$eshack)
    output$favImpPlot5 = renderPlot(favImpES$esnohack)
  }, ignoreInit = TRUE)

  # ------------------- Incorrect Rounding -------------------------------------

  output$roundingPlot1 = renderPlot(startplots$roundingPlot$phack)
  output$roundingPlot2 = renderPlot(startplots$roundingPlot$pnohack)
  output$roundingPlot3 = renderPlot(startplots$roundingPlot$pcomp)
  output$roundingFPHack = renderText(startplots$rounding.fprate.p)
  output$roundingFPOrig = renderText(startplots$rounding.fprate.o)
  output$roundingPlot4 = renderPlot(startplots$roundingES$eshack)
  output$roundingPlot5 = renderPlot(startplots$roundingES$esnohack)
  
  observeEvent(input$calcRounding > 0, {
    res5 <- sim.roundhack(roundinglevel = input$levelRounding+input$alphaRounding, iter = input$iterRounding, alternative = input$altRounding, alpha = input$alphaRounding, shinyEnv = TRUE)
    roundingPlot <- pplots(simdat=res5, alpha=input$alphaRounding)
    roundingES <- esplots(simdat=res5, EScolumn.hack=3, EScolumn.orig=4)
    rounding.fprate.p <- paste0(sum(round(res5[,"ps.hack"] <= input$alphaRounding)/input$iterRounding*100, 2), " %")
    rounding.fprate.o <- paste0(sum(round(res5[,"ps.orig"] <= input$alphaRounding)/input$iterRounding*100, 2), " %")
    output$roundingPlot1 = renderPlot(roundingPlot$phack)
    output$roundingPlot2 = renderPlot(roundingPlot$pnohack)
    output$roundingPlot3 = renderPlot(roundingPlot$pcomp)
    output$roundingFPHack = renderText(rounding.fprate.p)
    output$roundingFPOrig = renderText(rounding.fprate.o)
    output$roundingPlot4 = renderPlot(roundingES$eshack)
    output$roundingPlot5 = renderPlot(roundingES$esnohack)
  }, ignoreInit = TRUE)

  # ------------------- Optional Stopping --------------------------------------

  output$optStopPlot1 <- renderPlot(startplots$optstopPlot$phack)
  output$optStopPlot2 <- renderPlot(startplots$optstopPlot$pnohack)
  output$optStopPlot3 <- renderPlot(startplots$optstopPlot$pcomp)
  output$optStopFPHack = renderText(startplots$optstop.fprate.p)
  output$optStopFPOrig = renderText(startplots$optstop.fprate.o)
  output$optStopPlot4 = renderPlot(startplots$optstopESr2$eshack)
  output$optStopPlot5 = renderPlot(startplots$optstopESr2$esnohack)
  output$optStopPlot6 = renderPlot(startplots$optstopESd$eshack)
  output$optStopPlot7 = renderPlot(startplots$optstopESd$esnohack)
  
  observeEvent(input$calcOptStop > 0, {
    res6 <- sim.optstop(n.min = input$nminOptStop, n.max = input$nmaxOptStop, step = input$stepOptStop, alternative = input$altOptStop, iter = input$iterOptStop, alpha = input$alphaOptStop, shinyEnv = TRUE)
    optstopPlot <- pplots(simdat = res6, alpha = input$alphaOptStop)
    optstopESr2 <- esplots(simdat=res6, EScolumn.hack=3, EScolumn.orig=4)
    optstopESd <- esplots(simdat=res6, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                    expression("Distribution of original effect sizes "*delta)))
    optstop.fprate.p <- paste0(round(sum(res6[,"ps.hack"] <= input$alphaOptStop)/input$iterOptStop*100, 2), " %")
    optstop.fprate.o <- paste0(round(sum(res6[,"ps.orig"] <= input$alphaOptStop)/input$iterOptStop*100, 2), " %")
    output$optStopPlot1 <- renderPlot(optstopPlot$phack)
    output$optStopPlot2 <- renderPlot(optstopPlot$pnohack)
    output$optStopPlot3 <- renderPlot(optstopPlot$pcomp)
    output$optStopFPHack = renderText(optstop.fprate.p)
    output$optStopFPOrig = renderText(optstop.fprate.o)
    output$optStopPlot4 = renderPlot(optstopESr2$eshack)
    output$optStopPlot5 = renderPlot(optstopESr2$esnohack)
    output$optStopPlot6 = renderPlot(optstopESd$eshack)
    output$optStopPlot7 = renderPlot(optstopESd$esnohack)
  }, ignoreInit = TRUE)
  
  # ------------------- Outlier Exclusion --------------------------------------
  output$outExclPlot1 <- renderPlot(startplots$outExclPlot$phack)
  output$outExclPlot2 <- renderPlot(startplots$outExclPlot$pnohack)
  output$outExclPlot3 <- renderPlot(startplots$outExclPlot$pcomp)
  output$outExclFPHack = renderText(startplots$outExcl.fprate.p)
  output$outExclFPOrig = renderText(startplots$outExcl.fprate.o)
  output$outExclPlot4 = renderPlot(startplots$outExclES$eshack)
  output$outExclPlot5 = renderPlot(startplots$outExclES$esnohack)
  
  observeEvent(input$calcOutExcl > 0, {
    res7 <- sim.outHack(nobs = input$nobsOutExcl, which = as.numeric(input$whichOutExcl), strategy = input$strategyOutExcl, alpha = input$alphaOutExcl, iter = input$iterOutExcl, shinyEnv = TRUE)
    outExclPlot <- pplots(simdat = res7, alpha = input$alphaOutExcl)
    outExclES <- esplots(simdat = res7, EScolumn.hack = 3, EScolumn.orig = 4)
    outExcl.fprate.p <- paste0(round(sum(res7[,"ps.hack"] <= input$alphaOutExcl)/input$iterOutExcl*100, 2), " %")
    outExcl.fprate.o <- paste0(round(sum(res7[,"ps.orig"] <= input$alphaOutExcl)/input$iterOutExcl*100, 2), " %")
    output$outExclPlot1 <- renderPlot(outExclPlot$phack)
    output$outExclPlot2 <- renderPlot(outExclPlot$pnohack)
    output$outExclPlot3 <- renderPlot(outExclPlot$pcomp)
    output$outExclFPHack = renderText(outExcl.fprate.p)
    output$outExclFPOrig = renderText(outExcl.fprate.o)
    output$outExclPlot4 = renderPlot(outExclES$eshack)
    output$outExclPlot5 = renderPlot(outExclES$esnohack)
  }, ignoreInit = TRUE)
  
  # ------------------- Selective Reporting of Effects -------------------------
  
  output$selectEffPlot1 <- renderPlot(startplots$selectEffPlot$phack)
  output$selectEffPlot2 <- renderPlot(startplots$selectEffPlot$pnohack)
  output$selectEffPlot3 <- renderPlot(startplots$selectEffPlot$pcomp)
  output$selectEffFPHack = renderText(startplots$selectEff.fprate.p)
  output$selectEffFPOrig = renderText(startplots$selectEff.fprate.o)
  output$selectEffPlot4 = renderPlot(startplots$selectEffES$eshack)
  output$selectEffPlot5 = renderPlot(startplots$selectEffES$esnohack)
  
  observeEvent(input$calcSelectEff > 0, {
    if(input$interactSelectEff == "Yes") interactions <- TRUE
    else if(input$interactSelectEff == "No") interactions <- FALSE
    res8 <- sim.selectEffects(nobs = input$nobsSelectEff, niv = input$nivSelectEff, interactions = interactions, riv = input$rivSelectEff, strategy = input$strategySelectEff, alpha = input$alphaSelectEff, iter = input$iterSelectEff, shinyEnv = TRUE)
    selectEffPlot <- pplots(simdat = res8, alpha = input$alphaSelectEff)
    selectEffES <- esplots(simdat = res8, EScolumn.hack = 3, EScolumn.orig = 4)
    selectEff.fprate.p <- paste0(round(sum(res8[,"ps.hack"] <= input$alphaSelectEff)/input$iterSelectEff*100, 2), " %")
    selectEff.fprate.o <- paste0(round(sum(res8[,"ps.orig"] <= input$alphaSelectEff)/input$iterSelectEff*100, 2), " %")
    output$selectEffPlot1 <- renderPlot(selectEffPlot$phack)
    output$selectEffPlot2 <- renderPlot(selectEffPlot$pnohack)
    output$selectEffPlot3 <- renderPlot(selectEffPlot$pcomp)
    output$selectEffFPHack = renderText(selectEff.fprate.p)
    output$selectEffFPOrig = renderText(selectEff.fprate.o)
    output$selectEffPlot4 = renderPlot(selectEffES$eshack)
    output$selectEffPlot5 = renderPlot(selectEffES$esnohack)
  }, ignoreInit = TRUE)
  
  # ------------------- Selective Reporting of DVs -----------------------------
  
  output$SRDVPlot1 <- renderPlot(startplots$SRDVPlot$phack)
  output$SRDVPlot2 <- renderPlot(startplots$SRDVPlot$pnohack)
  output$SRDVPlot3 <- renderPlot(startplots$SRDVPlot$pcomp)
  output$SRDVFPHack = renderText(startplots$SRDV.fprate.p)
  output$SRDVFPOrig = renderText(startplots$SRDV.fprate.o)
  output$SRDVPlot4 = renderPlot(startplots$SRDVESr2$eshack)
  output$SRDVPlot5 = renderPlot(startplots$SRDVESr2$esnohack)
  output$SRDVPlot6 = renderPlot(startplots$SRDVESd$eshack)
  output$SRDVPlot7 = renderPlot(startplots$SRDVESd$esnohack)
  
  observeEvent(input$calcSRDV > 0, {
    res9 <- sim.multDVhack(nobs.group = input$nobsSRDV, nvar = input$nvarSRDV, r = input$rSRDV, strategy = input$strategySRDV, iter = input$iterSRDV, alternative = input$altSRDV, alpha = input$alphaSRDV, shinyEnv = TRUE)
    SRDVPlot <- pplots(simdat = res9, alpha = input$alphaSRDV)
    SRDVESr2 <- esplots(simdat=res9, EScolumn.hack=3, EScolumn.orig=4)
    SRDVESd <- esplots(simdat=res9, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                    expression("Distribution of original effect sizes "*delta)))
    SRDV.fprate.p <- paste0(round(sum(res9[,"ps.hack"] <= input$alphaSRDV)/input$iterSRDV*100, 2), " %")
    SRDV.fprate.o <- paste0(round(sum(res9[,"ps.orig"] <= input$alphaSRDV)/input$iterSRDV*100, 2), " %")
    output$SRDVPlot1 <- renderPlot(SRDVPlot$phack)
    output$SRDVPlot2 <- renderPlot(SRDVPlot$pnohack)
    output$SRDVPlot3 <- renderPlot(SRDVPlot$pcomp)
    output$SRDVFPHack = renderText(SRDV.fprate.p)
    output$SRDVFPOrig = renderText(SRDV.fprate.o)
    output$SRDVPlot4 = renderPlot(SRDVESr2$eshack)
    output$SRDVPlot5 = renderPlot(SRDVESr2$esnohack)
    output$SRDVPlot6 = renderPlot(SRDVESd$eshack)
    output$SRDVPlot7 = renderPlot(SRDVESd$esnohack)
  }, ignoreInit = TRUE)
  
  # ------------------- Selective Reporting of IVs -----------------------------
  
  output$SRIVPlot1 <- renderPlot(startplots$SRIVPlot$phack)
  output$SRIVPlot2 <- renderPlot(startplots$SRIVPlot$pnohack)
  output$SRIVPlot3 <- renderPlot(startplots$SRIVPlot$pcomp)
  output$SRIVFPHack = renderText(startplots$SRIV.fprate.p)
  output$SRIVFPOrig = renderText(startplots$SRIV.fprate.o)
  output$SRIVPlot4 = renderPlot(startplots$SRIVESr2$eshack)
  output$SRIVPlot5 = renderPlot(startplots$SRIVESr2$esnohack)
  output$SRIVPlot6 = renderPlot(startplots$SRIVESd$eshack)
  output$SRIVPlot7 = renderPlot(startplots$SRIVESd$esnohack)
  
  observeEvent(input$calcSRIV > 0, {
    res10 <- sim.multDVhack(nobs.group = input$nobsSRIV, nvar = input$nvarSRIV, r = input$rSRIV, strategy = input$strategySRIV, iter = input$iterSRIV, alternative = input$altSRIV, alpha = input$alphaSRIV, shinyEnv = TRUE)
    SRIVPlot <- pplots(simdat = res10, alpha = input$alphaSRIV)
    SRIVESr2 <- esplots(simdat=res10, EScolumn.hack=3, EScolumn.orig=4)
    SRIVESd <- esplots(simdat=res10, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                 expression("Distribution of original effect sizes "*delta)))
    SRIV.fprate.p <- paste0(round(sum(res10[,"ps.hack"] <= input$alphaSRIV)/input$iterSRIV*100, 2), " %")
    SRIV.fprate.o <- paste0(round(sum(res10[,"ps.orig"] <= input$alphaSRIV)/input$iterSRIV*100, 2), " %")
    output$SRIVPlot1 <- renderPlot(SRIVPlot$phack)
    output$SRIVPlot2 <- renderPlot(SRIVPlot$pnohack)
    output$SRIVPlot3 <- renderPlot(SRIVPlot$pcomp)
    output$SRIVFPHack = renderText(SRIV.fprate.p)
    output$SRIVFPOrig = renderText(SRIV.fprate.o)
    output$SRIVPlot4 = renderPlot(SRIVESr2$eshack)
    output$SRIVPlot5 = renderPlot(SRIVESr2$esnohack)
    output$SRIVPlot6 = renderPlot(SRIVESd$eshack)
    output$SRIVPlot7 = renderPlot(SRIVESd$esnohack)
  }, ignoreInit = TRUE)
  
  # --------------- Exploiting Statistical Analysis ----------------------------
  
  output$statAnalysisPlot1 <- renderPlot(startplots$statAnalysisPlot$phack)
  output$statAnalysisPlot2 <- renderPlot(startplots$statAnalysisPlot$pnohack)
  output$statAnalysisPlot3 <- renderPlot(startplots$statAnalysisPlot$pcomp)
  output$statAnalysisFPHack = renderText(startplots$statAnalysis.fprate.p)
  output$statAnalysisFPOrig = renderText(startplots$statAnalysis.fprate.o)
  
  observeEvent(input$calcStatAnalysis > 0, {
    res11 <- sim.statAnalysisHack(nobs.group = input$nobsStatAnalysis, strategy = input$strategyStatAnalysis, alternative = input$altStatAnalysis, alpha = input$alphaStatAnalysis, iter = input$iterStatAnalysis, shinyEnv = TRUE)
    statAnalysisPlot <- pplots(simdat = res11, alpha = input$alphaStatAnalysis)
    statAnalysis.fprate.p <- paste0(round(sum(res11[,"ps.hack"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, 2), " %")
    statAnalysis.fprate.o <- paste0(round(sum(res11[,"ps.orig"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, 2), " %")
    output$statAnalysisPlot1 <- renderPlot(statAnalysisPlot$phack)
    output$statAnalysisPlot2 <- renderPlot(statAnalysisPlot$pnohack)
    output$statAnalysisPlot3 <- renderPlot(statAnalysisPlot$pcomp)
    output$statAnalysisFPHack = renderText(statAnalysis.fprate.p)
    output$statAnalysisFPOrig = renderText(statAnalysis.fprate.o)
  }, ignoreInit = TRUE)
  
  # --------------- Subgroup Analyses / Inclusion Criteria ---------------------
  
  output$subgroupPlot1 <- renderPlot(startplots$subgroupPlot$phack)
  output$subgroupPlot2 <- renderPlot(startplots$subgroupPlot$pnohack)
  output$subgroupPlot3 <- renderPlot(startplots$subgroupPlot$pcomp)
  output$subgroupFPHack = renderText(startplots$subgroup.fprate.p)
  output$subgroupFPOrig = renderText(startplots$subgroup.fprate.o)
  output$subgroupPlot4 = renderPlot(startplots$subgroupESr2$eshack)
  output$subgroupPlot5 = renderPlot(startplots$subgroupESr2$esnohack)
  output$subgroupPlot6 = renderPlot(startplots$subgroupESd$eshack)
  output$subgroupPlot7 = renderPlot(startplots$subgroupESd$esnohack)
  
  observeEvent(input$calcSubgroup > 0, {
    res12 <- sim.subgroupHack(nobs.group = input$nobsSubgroup, nsubvars = input$nsubvarsSubgroup, alternative = input$altSubgroup, strategy = input$strategySubgroup, alpha = input$alphaSubgroup, iter = input$iterSubgroup, shinyEnv = TRUE)
    subgroupPlot <- pplots(simdat = res12, alpha = input$alphaSubgroup)
    subgroupESr2 <- esplots(simdat=res12, EScolumn.hack=3, EScolumn.orig=4)
    subgroupESd <- esplots(simdat=res12, EScolumn.hack=5, EScolumn.orig=6, titles = c(expression("Distribution of p-hacked effect sizes "*delta),
                                                                                 expression("Distribution of original effect sizes "*delta)))
    subgroup.fprate.p <- paste0(round(sum(res12[,"ps.hack"] <= input$alphaSubgroup)/input$iterSubgroup*100, 2), " %")
    subgroup.fprate.o <- paste0(round(sum(res12[,"ps.orig"] <= input$alphaSubgroup)/input$iterSubgroup*100, 2), " %")
    output$subgroupPlot1 <- renderPlot(subgroupPlot$phack)
    output$subgroupPlot2 <- renderPlot(subgroupPlot$pnohack)
    output$subgroupPlot3 <- renderPlot(subgroupPlot$pcomp)
    output$subgroupFPHack = renderText(subgroup.fprate.p)
    output$subgroupFPOrig = renderText(subgroup.fprate.o)
    output$subgroupPlot4 = renderPlot(subgroupESr2$eshack)
    output$subgroupPlot5 = renderPlot(subgroupESr2$esnohack)
    output$subgroupPlot6 = renderPlot(subgroupESd$eshack)
    output$subgroupPlot7 = renderPlot(subgroupESd$esnohack)
  }, ignoreInit = TRUE)
  
  # --------------------- Variable Transformation ------------------------------
  
  output$varTransPlot1 <- renderPlot(startplots$varTransPlot$phack)
  output$varTransPlot2 <- renderPlot(startplots$varTransPlot$pnohack)
  output$varTransPlot3 <- renderPlot(startplots$varTransPlot$pcomp)
  output$varTransFPHack = renderText(startplots$varTrans.fprate.p)
  output$varTransFPOrig = renderText(startplots$varTrans.fprate.o)
  output$varTransPlot4 = renderPlot(startplots$varTransES$eshack)
  output$varTransPlot5 = renderPlot(startplots$varTransES$esnohack)
  
  observeEvent(input$calcVarTrans > 0, {
    res13 <- sim.varTransHack(nobs = input$nobsVarTrans, transvar = input$transvarVarTrans, strategy = input$strategyVarTrans, alpha = input$alphaVarTrans, iter = input$iterVarTrans, shinyEnv = TRUE)
    varTransPlot <- pplots(simdat = res13, alpha = input$alphaVarTrans)
    varTransES <- esplots(simdat = res13, EScolumn.hack = 3, EScolumn.orig = 4)
    varTrans.fprate.p <- paste0(round(sum(res13[,"ps.hack"] <= input$alphaVarTrans)/input$iterVarTrans*100, 2), " %")
    varTrans.fprate.o <- paste0(round(sum(res13[,"ps.orig"] <= input$alphaVarTrans)/input$iterVarTrans*100, 2), " %")
    output$varTransPlot1 <- renderPlot(varTransPlot$phack)
    output$varTransPlot2 <- renderPlot(varTransPlot$pnohack)
    output$varTransPlot3 <- renderPlot(varTransPlot$pcomp)
    output$varTransFPHack = renderText(varTrans.fprate.p)
    output$varTransFPOrig = renderText(varTrans.fprate.o)
    output$varTransPlot4 = renderPlot(varTransES$eshack)
    output$varTransPlot5 = renderPlot(varTransES$esnohack)
  }, ignoreInit = TRUE)
  
}
