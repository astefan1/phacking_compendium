# ==============================================================================
# Subgroup Analyses
# ==============================================================================

#' Simulate data with subgroups
#' @description Outputs data frame with multiple binary variables from which subgroups can be extracted
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.

.sim.subgroup <- function(nobs.group, nsubvars, effect = 0, heterogeneity = 0, empirical = FALSE){

  dat <- .sim.data(nobs.group = nobs.group, effect = effect, heterogeneity = heterogeneity,
                   empirical = empirical)

  # Observations per group and total observations
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  nobs <- sum(nobs.group)

  subvars <- matrix(NA, nrow = nobs, ncol = nsubvars)
  for(i in 1:nsubvars){
    subvars[,i] <- sample(c(0, 1), size = nobs, replace = TRUE)
  }

  res <- cbind(dat, subvars)

  return(res)

}

#' P-Hacking function for multiple subgroups analysis
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df A matrix or data frame containing all relevant data
#' @param iv Integer specifying the location of the binary independent variable in the data frame
#' @param dv Integer specifying the location of the dependent variable in the data frame
#' @param subvars Vector specifying the location of the subgroup variables in the data frame
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test

.subgroupHack <- function(df, iv, dv, subvars, alternative = "two.sided", strategy = "firstsig", alpha = 0.05){

  group.values <- unique(df[,iv])
  control <- df[df[,iv] == group.values[1], dv]
  treatment <- df[df[,iv] == group.values[2], dv]

  analyses <- list(list(report = .report.twogroup(control = control,
                                                  treatment = treatment,
                                                  method = "t.equal",
                                                  alternative = alternative),
                        r2 = .compR2t(control, treatment)))
  analyses[[1]][["d"]] <- .compCohensDStat(statistic = analyses[[1]][["report"]][["stat"]],
                                            n1 = length(control),
                                            n2 = length(treatment))

  for(i in 1:length(subvars)){
    levels.current <- sort(unique(df[,subvars[i]]))
    for(j in 1:length(levels.current)){
      subset.df <- df[df[,subvars[i]] == levels.current[j], , drop = FALSE]
      control.sub <- subset.df[subset.df[,iv] == group.values[1], dv]
      treatment.sub <- subset.df[subset.df[,iv] == group.values[2], dv]
      report <- .report.twogroup(control = control.sub,
                                 treatment = treatment.sub,
                                 method = "t.equal",
                                 alternative = alternative)
      analyses[[length(analyses)+1]] <- list(report = report,
                                             r2 = .compR2t(control.sub, treatment.sub),
                                             d = .compCohensDStat(statistic = report[["stat"]],
                                                                  n1 = length(control.sub),
                                                                  n2 = length(treatment.sub)))
    }
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

#' Simulate p-hacking with multiple subgroups
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export

sim.subgroupHack <- function(nobs.group, nsubvars, effect = 0, heterogeneity = 0, alternative = "two.sided", strategy = "firstsig", alpha = 0.05, iter = 1000, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.subgroup(nobs.group = nobs.group, nsubvars = nsubvars,
                              effect = effect, heterogeneity = heterogeneity,
                              empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset
  .subgroupHackList <- function(x){
    .subgroupHack(df = x, iv = 1, dv = 2, subvars = c(3:(2+nsubvars)),
                  alternative = alternative, strategy = strategy, alpha = alpha)
  }

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .subgroupHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .subgroupHack(df = x, iv = 1, dv = 2, subvars = c(3:(2+nsubvars)),
                      alternative = alternative, strategy = strategy, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig", "ds.hack", "ds.orig"))

}

