# ==============================================================================
# Exploiting statistical analysis options
# ==============================================================================

# Data simulation can be done with .sim.data

#' P-Hacking function for exploiting different statistical analysis options
#' @param df Data frame with one continuous independent variable and one continuous dependent variable
#' @param group Location of the grouping variable in the data frame
#' @param dv Location of the dependent variabl in the data frame
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test wilcox.test
#' @importFrom WRS2 yuen

.statAnalysisHack <- function(df, group, dv, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  control <- df[df[,group] == unique(df[,group])[1], dv]
  treatment <- df[df[,group] == unique(df[,group])[2], dv]

  analyses <- list(
    .report.twogroup(control = control, treatment = treatment,
                     method = "t.equal", alternative = alternative),
    .report.twogroup(control = control, treatment = treatment,
                     method = "t.welch", alternative = alternative),
    .report.twogroup(control = control, treatment = treatment,
                     method = "wilcox", alternative = alternative)
  )

  trim <- c(0.1, 0.15, 0.2, 0.25)
  for(i in 1:length(trim)){
    analyses[[length(analyses)+1]] <- .report.twogroup(control = control,
                                                       treatment = treatment,
                                                       method = "yuen",
                                                       trim = trim[i],
                                                       alternative = alternative)
  }

  ps <- vapply(analyses, function(x) x[["p"]], numeric(1))
  final.index <- .selectanalysis(ps = ps, strategy = strategy, alpha = alpha)

  return(list(ps.hack = analyses[[final.index]][["p"]],
              ps.orig = analyses[[1]][["p"]],
              report.initial = analyses[[1]],
              report.final = analyses[[final.index]]))

}

#' Simulate p-Hacking for exploiting different statistical analysis options
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs.group Number of observations per group. Either a scalar or a vector with 2 elements.
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.statAnalysisHack <- function(nobs.group, strategy = "firstsig", effect = 0, heterogeneity = 0, alternative = "two.sided", alpha = 0.05, iter = 1000, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = nobs.group, effect = effect, heterogeneity = heterogeneity)
  }

  # Apply p-hacking procedure to each dataset

  .statAnalysisHackList <- function(x){
    .statAnalysisHack(df = x, group = 1, dv = 2, strategy = strategy, alternative = alternative, alpha = alpha)
  }

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .statAnalysisHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .statAnalysisHack(df = x, group = 1, dv = 2, strategy = strategy,
                          alternative = alternative, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig"))


}

