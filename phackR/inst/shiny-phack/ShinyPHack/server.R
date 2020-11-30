library(shinydashboard)
library(phackR)

startplots <- readRDS("data/startplots.rds")

function(input, output) {

  # Initiate variable to store simulation results
  sims <- reactiveValues()

  # ------------------- Composite Scores ---------------------------------------

  output$uindeleteCompScores <- renderUI({
    sliderInput("ndeleteCompScores", label = "Maximum number of variables to be deleted from composite score",
                value = 2, min = 1, max = input$ncompvCompScores - 3, step = 1)
  })

  output$compScoresPlot = renderPlot(startplots$compscorePlot$pcomp)
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
      output$compScoresPlot = renderPlot(compscorePlot$pcomp)
      output$compScoresFPHack = renderText(compscore.fprate.p)
      output$compScoresFPOrig = renderText(compscore.fprate.o)
      output$compScoresPlot4 = renderPlot(compscorePlotES$eshack)
      output$compScoresPlot5 = renderPlot(compscorePlotES$esnohack)
      sims$res1 <- res1
  }, ignoreInit = TRUE)

  output$downloadCompScores <- downloadHandler(filename = "compScoresSimdata.csv",
    content = function(file){
      if(!is.null(sims$res1)) write.csv(sims$res1, file, row.names = FALSE)
      else write.csv(startplots$res1, file, row.names = FALSE)
    }
  )

  # ------------------- Exploit Covariates -------------------------------------

  output$expCovPlot = renderPlot(startplots$expCovPlot$pcomp)
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
    output$expCovPlot = renderPlot(expCovPlot$pcomp)
    output$expCovFPHack = renderText(expcov.fprate.p)
    output$expCovFPOrig = renderText(expcov.fprate.o)
    output$expCovPlot4 = renderPlot(expCovES$eshack)
    output$expCovPlot5 = renderPlot(expCovES$esnohack)
    sims$res2 <- res2
  }, ignoreInit = TRUE)

  output$downloadExpCov <- downloadHandler(filename = "expCovSimdata.csv",
                                               content = function(file){
                                                 if(!is.null(sims$res2)) write.csv(sims$res2, file, row.names = FALSE)
                                                 else write.csv(startplots$res2, file, row.names = FALSE)
                                               }
  )

  # ------------------- Exploit Cutoffs ----------------------------------------

  output$expCutPlot = renderPlot(startplots$expCutPlot$pcomp)
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
    output$expCutPlot = renderPlot(expCutPlot$pcomp)
    output$expCutFPHack = renderText(expcut.fprate.p)
    output$expCutFPOrig = renderText(expcut.fprate.o)
    output$expCutPlot4 = renderPlot(expCutES$eshack)
    output$expCutPlot5 = renderPlot(expCutES$esnohack)
    sims$res3 <- res3
  }, ignoreInit = TRUE)

  output$downloadExpCut <- downloadHandler(filename = "expCutSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res3)) write.csv(sims$res3, file, row.names = FALSE)
                                             else write.csv(startplots$res3, file, row.names = FALSE)
                                           }
  )

  # ------------------- Favorable Imputation -----------------------------------

  output$favImpPlot = renderPlot(startplots$favImpPlot$pcomp)
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
    output$favImpPlot = renderPlot(favImpPlot$pcomp)
    output$favImpFPHack = renderText(favimp.fprate.p)
    output$favImpFPOrig = renderText(favimp.fprate.o)
    output$favImpPlot4 = renderPlot(favImpES$eshack)
    output$favImpPlot5 = renderPlot(favImpES$esnohack)
    sims$res4 <- res4
  }, ignoreInit = TRUE)

  output$downloadFavImp <- downloadHandler(filename = "favImpSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res4)) write.csv(sims$res4, file, row.names = FALSE)
                                             else write.csv(startplots$res4, file, row.names = FALSE)
                                           }
  )

  # ------------------- Incorrect Rounding -------------------------------------

  output$roundingPlot = renderPlot(startplots$roundingPlot$pcomp)
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
    output$roundingPlot = renderPlot(roundingPlot$pcomp)
    output$roundingFPHack = renderText(rounding.fprate.p)
    output$roundingFPOrig = renderText(rounding.fprate.o)
    output$roundingPlot4 = renderPlot(roundingES$eshack)
    output$roundingPlot5 = renderPlot(roundingES$esnohack)
    sims$res5 <- res5
  }, ignoreInit = TRUE)

  output$downloadRounding <- downloadHandler(filename = "roundingSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res5)) write.csv(sims$res5, file, row.names = FALSE)
                                             else write.csv(startplots$res5, file, row.names = FALSE)
                                           }
  )

  # ------------------- Optional Stopping --------------------------------------

  output$optStopPlot <- renderPlot(startplots$optstopPlot$pcomp)
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
    output$optStopPlot <- renderPlot(optstopPlot$pcomp)
    output$optStopFPHack = renderText(optstop.fprate.p)
    output$optStopFPOrig = renderText(optstop.fprate.o)
    output$optStopPlot4 = renderPlot(optstopESr2$eshack)
    output$optStopPlot5 = renderPlot(optstopESr2$esnohack)
    output$optStopPlot6 = renderPlot(optstopESd$eshack)
    output$optStopPlot7 = renderPlot(optstopESd$esnohack)
    sims$res6 <- res6
  }, ignoreInit = TRUE)

  output$downloadOptStop <- downloadHandler(filename = "optStopSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res6)) write.csv(sims$res6, file, row.names = FALSE)
                                             else write.csv(startplots$res6, file, row.names = FALSE)
                                           }
  )

  # ------------------- Outlier Exclusion --------------------------------------
  output$outExclPlot <- renderPlot(startplots$outExclPlot$pcomp)
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
    output$outExclPlot <- renderPlot(outExclPlot$pcomp)
    output$outExclFPHack = renderText(outExcl.fprate.p)
    output$outExclFPOrig = renderText(outExcl.fprate.o)
    output$outExclPlot4 = renderPlot(outExclES$eshack)
    output$outExclPlot5 = renderPlot(outExclES$esnohack)
    sims$res7 <- res7
  }, ignoreInit = TRUE)

  output$downloadOutExcl <- downloadHandler(filename = "outExclSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res7)) write.csv(sims$res7, file, row.names = FALSE)
                                             else write.csv(startplots$res7, file, row.names = FALSE)
                                           }
  )

  # ------------------- Selective Reporting of Effects -------------------------

  output$selectEffPlot <- renderPlot(startplots$selectEffPlot$pcomp)
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
    output$selectEffPlot <- renderPlot(selectEffPlot$pcomp)
    output$selectEffFPHack = renderText(selectEff.fprate.p)
    output$selectEffFPOrig = renderText(selectEff.fprate.o)
    output$selectEffPlot4 = renderPlot(selectEffES$eshack)
    output$selectEffPlot5 = renderPlot(selectEffES$esnohack)
    sims$res8 <- res8
  }, ignoreInit = TRUE)

  output$downloadSelectEff <- downloadHandler(filename = "selectEffSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res8)) write.csv(sims$res8, file, row.names = FALSE)
                                             else write.csv(startplots$res8, file, row.names = FALSE)
                                           }
  )

  # ------------------- Selective Reporting of DVs -----------------------------

  output$SRDVPlot <- renderPlot(startplots$SRDVPlot$pcomp)
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
    output$SRDVPlot <- renderPlot(SRDVPlot$pcomp)
    output$SRDVFPHack = renderText(SRDV.fprate.p)
    output$SRDVFPOrig = renderText(SRDV.fprate.o)
    output$SRDVPlot4 = renderPlot(SRDVESr2$eshack)
    output$SRDVPlot5 = renderPlot(SRDVESr2$esnohack)
    output$SRDVPlot6 = renderPlot(SRDVESd$eshack)
    output$SRDVPlot7 = renderPlot(SRDVESd$esnohack)
    sims$res9 <- res9
  }, ignoreInit = TRUE)

  output$downloadSRDV <- downloadHandler(filename = "SRDVSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res9)) write.csv(sims$res9, file, row.names = FALSE)
                                             else write.csv(startplots$res9, file, row.names = FALSE)
                                           }
  )

  # ------------------- Selective Reporting of IVs -----------------------------

  output$SRIVPlot <- renderPlot(startplots$SRIVPlot$pcomp)
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
    output$SRIVPlot <- renderPlot(SRIVPlot$pcomp)
    output$SRIVFPHack = renderText(SRIV.fprate.p)
    output$SRIVFPOrig = renderText(SRIV.fprate.o)
    output$SRIVPlot4 = renderPlot(SRIVESr2$eshack)
    output$SRIVPlot5 = renderPlot(SRIVESr2$esnohack)
    output$SRIVPlot6 = renderPlot(SRIVESd$eshack)
    output$SRIVPlot7 = renderPlot(SRIVESd$esnohack)
    sims$res10 <- res10
  }, ignoreInit = TRUE)

  output$downloadSRIV <- downloadHandler(filename = "SRIVSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res10)) write.csv(sims$res10, file, row.names = FALSE)
                                             else write.csv(startplots$res10, file, row.names = FALSE)
                                           }
  )

  # --------------- Exploiting Statistical Analysis ----------------------------

  output$statAnalysisPlot <- renderPlot(startplots$statAnalysisPlot$pcomp)
  output$statAnalysisFPHack = renderText(startplots$statAnalysis.fprate.p)
  output$statAnalysisFPOrig = renderText(startplots$statAnalysis.fprate.o)

  observeEvent(input$calcStatAnalysis > 0, {
    res11 <- sim.statAnalysisHack(nobs.group = input$nobsStatAnalysis, strategy = input$strategyStatAnalysis, alternative = input$altStatAnalysis, alpha = input$alphaStatAnalysis, iter = input$iterStatAnalysis, shinyEnv = TRUE)
    statAnalysisPlot <- pplots(simdat = res11, alpha = input$alphaStatAnalysis)
    statAnalysis.fprate.p <- paste0(round(sum(res11[,"ps.hack"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, 2), " %")
    statAnalysis.fprate.o <- paste0(round(sum(res11[,"ps.orig"] <= input$alphaStatAnalysis)/input$iterStatAnalysis*100, 2), " %")
    output$statAnalysisPlot <- renderPlot(statAnalysisPlot$pcomp)
    output$statAnalysisFPHack = renderText(statAnalysis.fprate.p)
    output$statAnalysisFPOrig = renderText(statAnalysis.fprate.o)
    sims$res11 <- res11
  }, ignoreInit = TRUE)

  output$downloadStatAnalysis <- downloadHandler(filename = "statAnalysisSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res11)) write.csv(sims$res11, file, row.names = FALSE)
                                             else write.csv(startplots$res11, file, row.names = FALSE)
                                           }
  )

  # --------------- Subgroup Analyses / Inclusion Criteria ---------------------

  output$subgroupPlot <- renderPlot(startplots$subgroupPlot$pcomp)
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
    output$subgroupPlot <- renderPlot(subgroupPlot$pcomp)
    output$subgroupFPHack = renderText(subgroup.fprate.p)
    output$subgroupFPOrig = renderText(subgroup.fprate.o)
    output$subgroupPlot4 = renderPlot(subgroupESr2$eshack)
    output$subgroupPlot5 = renderPlot(subgroupESr2$esnohack)
    output$subgroupPlot6 = renderPlot(subgroupESd$eshack)
    output$subgroupPlot7 = renderPlot(subgroupESd$esnohack)
    sims$res12 <- res12
  }, ignoreInit = TRUE)

  output$downloadSubgroup <- downloadHandler(filename = "subgroupSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res12)) write.csv(sims$res12, file, row.names = FALSE)
                                             else write.csv(startplots$res12, file, row.names = FALSE)
                                           }
  )

  # --------------------- Variable Transformation ------------------------------

  output$varTransPlot <- renderPlot(startplots$varTransPlot$pcomp)
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
    output$varTransPlot <- renderPlot(varTransPlot$pcomp)
    output$varTransFPHack = renderText(varTrans.fprate.p)
    output$varTransFPOrig = renderText(varTrans.fprate.o)
    output$varTransPlot4 = renderPlot(varTransES$eshack)
    output$varTransPlot5 = renderPlot(varTransES$esnohack)
    sims$res13 <- res13
  }, ignoreInit = TRUE)

  output$downloadVarTrans <- downloadHandler(filename = "varTransSimdata.csv",
                                           content = function(file){
                                             if(!is.null(sims$res13)) write.csv(sims$res13, file, row.names = FALSE)
                                             else write.csv(startplots$res13, file, row.names = FALSE)
                                           }
  )

}
