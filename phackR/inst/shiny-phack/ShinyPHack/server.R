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


}
