# ==============================================================================
# Selective Reporting of the Dependent Variable
# ==============================================================================

#' Simulate dataset with multiple dependent variables
#' @description Outputs data frame with a grouping variable and multiple correlated dependent variables
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.

.sim.multDV <- function(nobs.group, nvar, r, effect = 0, heterogeneity = 0, empirical = FALSE){

  # Observations per group
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)

  # Generate group vector
  group <- rep(1:length(nobs.group), nobs.group)

  # Generate dependent variables
  dvs <- .sim.multcor(nobs = sum(nobs.group), nvar = nvar, r = r)
  theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity, empirical = empirical)
  dvs[(nobs.group[1]+1):sum(nobs.group), ] <- dvs[(nobs.group[1]+1):sum(nobs.group), ] + theta
  if(isTRUE(empirical)){
    dvs[(nobs.group[1]+1):sum(nobs.group), 1] <- .set_observed_smd(
      control = dvs[1:nobs.group[1], 1],
      treatment = dvs[(nobs.group[1]+1):sum(nobs.group), 1],
      effect = theta
    )
  }

  # Generate data frame
  res <- cbind(group, dvs)

  return(res)
}

#' P-Hacking function for multiple dependent variables
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame with one group variable and multiple dependent variables
#' @param dvs Vector defining the DV columns (will be checked in given order)
#' @param group Scalar defining grouping column
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test

.multDVhack <- function(df, dvs, group, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  # Prepare data frame
  dvs <- as.matrix(df[, dvs], ncol = length(dvs))
  group <- df[, group]
  analyses <- list()

  # Compute t-tests
  for(i in 1:ncol(dvs)){
    control <- dvs[group == unique(group)[1], i]
    treatment <- dvs[group == unique(group)[2], i]
    report <- .report.twogroup(control = control,
                               treatment = treatment,
                               method = "t.equal",
                               alternative = alternative)
    analyses[[i]] <- list(report = report,
                          r2 = .compR2t(control, treatment),
                          d = .compCohensDStat(statistic = report[["stat"]],
                                               n1 = length(control),
                                               n2 = length(treatment)))
  }

  ps <- vapply(analyses, function(x) x[["report"]][["p"]], numeric(1))
  final.index <- .selectanalysis(ps = ps, strategy = strategy, alpha = alpha)

  return(list(ps.hack = analyses[[final.index]][["report"]][["p"]],
              ps.orig = analyses[[1]][["report"]][["p"]],
              r2s.hack = analyses[[final.index]][["r2"]],
              r2s.orig = analyses[[1]][["r2"]],
              ds.hack = analyses[[final.index]][["d"]],
              ds.orig = analyses[[1]][["d"]],
              report.initial = analyses[[1]][["report"]],
              report.final = analyses[[final.index]][["report"]]))

}

#' Simulate p-Hacking with multiple dependent variables
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export

sim.multDVhack <- function(nobs.group, nvar, r, strategy = "firstsig", effect = 0, heterogeneity = 0, iter = 1000, alternative = "two.sided", alpha = 0.05, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multDV(nobs.group = nobs.group, nvar = nvar, r = r,
                            effect = effect, heterogeneity = heterogeneity,
                            empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .multDVhack, dvs = c(2:(nvar+1)), group = 1,
                  strategy = strategy, alternative = alternative, alpha = alpha)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .multDVhack(df=x, dvs = c(2:(nvar+1)), group = 1,
                    strategy = strategy, alternative = alternative, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig", "ds.hack", "ds.orig"))
}

