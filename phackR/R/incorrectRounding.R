# ==============================================================================
# Incorrect Rounding
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' P-Hacking function for incorrect rounding
#' @description Outputs a p-hacked p-value and the non-p-hacked-p-value
#' @param df Data frame
#' @param group Scalar defining location of the group vector in the data frame
#' @param dv Scalar defining location of dependent variable in the data frame
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.roundhack <- function(df, group, dv, roundinglevel, alternative = "two.sided", alpha = 0.05){

  control <- df[,dv][df[,group] == unique(df[,group])[1]]
  treatment <- df[,dv][df[,group] == unique(df[,group])[2]]
  report.initial <- .report.twogroup(control = control,
                                     treatment = treatment,
                                     method = "t.equal",
                                     alternative = alternative)
  r2val <- .compR2t(control, treatment)

  # P-hack p-value
  if(report.initial[["p"]] > alpha && report.initial[["p"]] < roundinglevel){
    p.final <- alpha
  } else {
    p.final <- report.initial[["p"]]
  }

  report.final <- report.initial
  report.final[["p"]] <- p.final

  return(list(ps.hack = p.final,
              ps.orig = report.initial[["p"]],
              r2s.hack = r2val,
              r2s.orig = r2val,
              report.initial = report.initial,
              report.final = report.final))

}

#' Simulate p-hacking with incorrect rounding
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param roundinglevel Highest p-value that is rounded down to alpha
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param iter Number of iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export

sim.roundhack <- function(roundinglevel, effect = 0, heterogeneity = 0, iter = 1000, alternative = "two.sided", alpha = 0.05, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = 30, effect = effect,
                          heterogeneity = heterogeneity, empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset
  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .roundhack, group = 1, dv = 2,
                  roundinglevel = roundinglevel, alternative = alternative, alpha = alpha)
  }
  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .roundhack(df=x, group = 1, dv = 2, roundinglevel = roundinglevel,
                   alternative = alternative, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig"))

}
