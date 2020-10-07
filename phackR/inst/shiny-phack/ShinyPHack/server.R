library(shinydashboard)

function(input, output) {

  # ------------------- Composite Scores ---------------------------------------

  output$uindeleteCompScores <- renderUI({
    sliderInput("ndeleteCompScores", label = "Maximum number of variables to be deleted from composite score",
                value = 2, min = 1, max = input$ncompvCompScores - 3, step = 1)
  })

  observeEvent(input$calcCompScores > 0, {
      ifelse(length(input$uindeleteCompScores)==0, ndelete<-2, ndelete<-input$uindeleteCompScores)
      res <- sim.compscoreHack(nobs=input$nobsCompScores, ncompv=input$ncompvCompScores, rcomp=input$rcompCompScores, ndelete=ndelete, strategy = input$strategyCompScores, alpha = input$alphaCompScores, iter = input$iterCompScores, shinyEnv=TRUE)
      compscorePlot <- pplots(simdat=res, alpha=input$alphaCompScores)
      compscorePlotES <- esplots(simdat=res, EScolumn.hack=3, EScolumn.orig=4)
      fprate.p <- paste0(sum(res[,"ps.hack"] < input$alphaCompScores)/input$iterCompScores*100, " %")
      fprate.o <- paste0(sum(res[,"ps.orig"] < input$alphaCompScores)/input$iterCompScores*100, " %")
      output$compScoresPlot1 = renderPlot(compscorePlot$phack)
      output$compScoresPlot2 = renderPlot(compscorePlot$pnohack)
      output$compScoresPlot3 = renderPlot(compscorePlot$pcomp)
      output$compScoresFPHack = renderText(fprate.p)
      output$compScoresFPOrig = renderText(fprate.o)
      output$compScoresPlot4 = renderPlot(compscorePlotES$eshack)
      output$compScoresPlot5 = renderPlot(compscorePlotES$esnohack)
  })
}
