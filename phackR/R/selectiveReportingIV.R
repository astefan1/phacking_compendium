# ==============================================================================
# Selective Reporting of the Independent Variable
# ==============================================================================

#' Simulate dataset with multiple independent variables
#' @description Outputs data frame with multiple independent variables
#' @param nobs.group Scalar defining number of observations per group (or number of observations in predictors in regression)
#' @param nvar Number of independent variables in the data frame
#' @param r Desired correlation between the independent variables (scalar)
#' @param regression Should the simulation be conducted for a regression analysis (TRUE) or a t-test? (FALSE)
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.

.sim.multIV <- function(nobs.group, nvar, r, regression = FALSE, effect = 0, heterogeneity = 0, empirical = FALSE){

  if(regression){
    return(.sim.multregression(nobs = nobs.group, nvar = nvar, r = r,
                               effect = effect, heterogeneity = heterogeneity,
                               empirical = empirical))
  }

  # Observations per group
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)

  # Simulate control group
  control <- rnorm(nobs.group[1])

  # Simulate multiple experimental groups / predictor variables
  ivs <- .sim.multcor(nobs = nobs.group[2], nvar = nvar, r = r)
  theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity, empirical = empirical)
  ivs <- ivs + theta
  if(isTRUE(empirical)){
    ivs[,1] <- .set_observed_smd(control = control,
                                  treatment = ivs[,1],
                                  effect = theta)
  }

  # Generate data frame
  nrows <- max(length(control), nrow(ivs))
  control.pad <- c(control, rep(NA, nrows-length(control)))
  ivs.pad <- matrix(NA, nrow = nrows, ncol = ncol(ivs))
  ivs.pad[1:nrow(ivs), ] <- as.matrix(ivs)
  res <- cbind(control.pad, ivs.pad)

  return(res)

}

#' P-Hacking function for multiple independent variables in a t-test
#' @description Returns a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame (wide format) containing a control group variable and multiple treatment group variables
#' @param ivs Location of the independent variables (treatment groups) in the (wide) data frame
#' @param control Location of the control group in the (wide) data frame
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). For the t-test path, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.multIVhack_ttest <- function(df, ivs, control, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  treatm <- df[, ivs]
  control <- df[, control]

  # Prepare dataset
  analyses <- list()

  # Compute t-tests
  for(i in 1:length(ivs)){
    report <- .report.twogroup(control = control,
                               treatment = treatm[,i],
                               method = "t.equal",
                               alternative = alternative)
    analyses[[i]] <- list(report = report,
                          r2 = .compR2t(control, treatm[,i]),
                          d = .compCohensDStat(statistic = report[["stat"]],
                                               n1 = length(control),
                                               n2 = length(treatm[,i])))
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

#' P-Hacking function for multiple predictors in a regression
#' @description Returns a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame containing a criterion variable and multiple predictor variables
#' @param ivs Location of the independent variables (predictors) in the data frame
#' @param control Location of the criterion in the data frame
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). For the regression path, \code{"greater"} tests a positive association.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.multIVhack_reg <- function(df, ivs, control, strategy = "firstsig", alternative="two.sided", alpha = 0.05){
  
  predictors <- df[, ivs]
  criterion <- df[, control]
  
  # Prepare dataset
  analyses <- list()
  
  # Compute regressions
  for(i in 1:length(ivs)){
    report <- .report.association(x = predictors[,i], y = criterion,
                                  method = paste0("lm.predictor.", i),
                                  alternative = alternative)
    analyses[[i]] <- list(report = report,
                          r2 = tanh(report[["effect"]])^2)
  }
  
  ps <- vapply(analyses, function(x) x[["report"]][["p"]], numeric(1))
  final.index <- .selectanalysis(ps = ps, strategy = strategy, alpha = alpha)

  return(list(ps.hack = analyses[[final.index]][["report"]][["p"]],
              ps.orig = analyses[[1]][["report"]][["p"]],
              r2s.hack = analyses[[final.index]][["r2"]],
              r2s.orig = analyses[[1]][["r2"]],
              report.initial = analyses[[1]][["report"]],
              report.final = analyses[[final.index]][["report"]]))
  
}

#' Simulate p-Hacking with multiple independent variables
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of independent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param regression Should the simulation be conducted for a regression analysis (TRUE) or a t-test? (FALSE)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies. For \code{regression = FALSE}, this is on the standardized mean-difference scale. For \code{regression = TRUE}, it is on the Fisher-z scale.
#' @param heterogeneity Between-study heterogeneity. For \code{regression = FALSE}, this is on the standardized mean-difference scale. For \code{regression = TRUE}, it is on the Fisher-z scale.
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). For \code{regression = FALSE}, \code{"greater"} tests whether the treatment or second group exceeds the control or first group. For \code{regression = TRUE}, it tests a positive association.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export

sim.multIVhack <- function(nobs.group, nvar, r, regression=FALSE, strategy = "firstsig", effect = 0, heterogeneity = 0, iter = 1000, alternative = "two.sided", alpha = 0.05, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multIV(nobs.group = nobs.group, nvar = nvar, r = r,
                            regression = regression, effect = effect,
                            heterogeneity = heterogeneity,
                            empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset
  if(regression){
    .multIVhack <- .multIVhack_reg
  } else {
    .multIVhack <- .multIVhack_ttest
  }
  
  .multIVhacklist <- function(x){
    .multIVhack(df = x, ivs = c(2:(nvar+1)), control = 1,
                strategy = strategy, alternative = alternative, alpha = alpha)
  }

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .multIVhacklist)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .multIVhack(df=x, ivs = c(2:(nvar+1)), control = 1,
                    strategy = strategy, alternative = alternative, alpha = alpha)
      })
    })
  }

  if(regression){
    return(.combine.phase1.results(res = res,
                                   legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig")))
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig", "ds.hack", "ds.orig"))

}
