# ==============================================================================
# Optional Stopping Based on Significance
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' Optional Stopping based on existing dataset
#' @description Returns a p-hacked p-value and a non-p-hacked p-value based on the maximum sample size
#' @param df Data frame
#' @param group group Scalar defining grouping column
#' @param dv Scalar defining location of dependent variable in the data frame
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param peek Determines how often one peeks at the data. Overrides step argument if not NULL.
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.optstop <- function(df, group, dv, n.min, n.max, step = 1, peek = NULL, alternative = "two.sided", alpha = 0.05){

  # Extract group variables
  g1 <- df[df[,group] == unique(df[,group])[1], dv]
  g2 <- df[df[,group] == unique(df[,group])[2], dv]

  # Sanity check: Enough data?
  stopifnot(length(g1) >= n.max && length(g2) >= n.max)

  # Determine places of peeks
  if(is.null(peek)){
    peeks <- seq(n.min, n.max, by=step)
    if(step > (n.max-n.min)) peeks <- c(n.min, n.max)
  } else {
    peeks <- round(seq(n.min, n.max, length.out = peek))
  }

  # Compute t-tests
  analyses <- lapply(peeks, function(x){
    control <- g1[1:x]
    treatment <- g2[1:x]
    report <- .report.twogroup(control = control,
                               treatment = treatment,
                               method = "t.equal",
                               alternative = alternative)

    list(report = report,
         r2 = .compR2t(control, treatment),
         d = .compCohensDStat(statistic = report[["stat"]], n1 = length(control), n2 = length(treatment)))
  })

  ps <- vapply(analyses, function(x) x[["report"]][["p"]], numeric(1))

  # Do the p-hacking
  if(any(ps < alpha) == FALSE){
    final.index <- length(peeks)
  } else {
    final.index <- which(ps < alpha)[1]
  }

  initial.index <- length(peeks)

  return(list(ps.hack = analyses[[final.index]][["report"]][["p"]],
              ps.orig = analyses[[initial.index]][["report"]][["p"]],
              r2s.hack = analyses[[final.index]][["r2"]],
              r2s.orig = analyses[[initial.index]][["r2"]],
              ds.hack = analyses[[final.index]][["d"]],
              ds.orig = analyses[[initial.index]][["d"]],
              report.initial = analyses[[initial.index]][["report"]],
              report.final = analyses[[final.index]][["report"]]))
}

#' Simulate p-hacking with optional stopping
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param peek Determines how often one peeks at the data. Overrides step argument if not NULL.
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param alternative Direction of the t-test ("two.sided", "less", "greater"). Here, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param iter Number of iterations
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export
#'

sim.optstop <- function(n.min, n.max, step = 1, peek = NULL, effect = 0, heterogeneity = 0, alternative = "two.sided", iter = 1000, alpha = 0.05, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = n.max, effect = effect,
                          heterogeneity = heterogeneity, empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset
  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .optstop, group = 1, dv = 2,
                  n.min = n.min, n.max = n.max, step = step, peek = peek,
                  alternative = alternative, alpha = alpha)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .optstop(df=x, group = 1, dv = 2,
                 n.min = n.min, n.max = n.max, step = step,
                 alternative = alternative, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig", "ds.hack", "ds.orig"))

}
