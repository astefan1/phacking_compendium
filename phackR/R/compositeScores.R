# ==============================================================================
# Scale Redefinition / Composite Scores
# ==============================================================================

#' Simulate data: Correlated composite score raw variables and one non-correlated dependent variable
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables
#' @param effect Mean effect size across studies on the Fisher-z scale
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.

.sim.compscore <- function(nobs, ncompv, rcomp, effect = 0, heterogeneity = 0, empirical = FALSE){

  iv <- .sim.multcor(nobs = nobs, nvar = ncompv, r = rcomp)
  theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity, empirical = empirical)
  rho <- .fisherz_to_r(theta)
  compscore <- scale(rowMeans(iv))[,1]
  dv <- rho*compscore + sqrt(1-rho^2)*rnorm(nobs, 0, 1)
  if(isTRUE(empirical)) dv <- .set_observed_fisherz(x = compscore, y = dv, effect = theta)

  res <- cbind(dv, iv)

  return(res)

}

#' P-Hacking function for scale redefinition / Composite Scores
#' @param df Data frame containing dependent variable and composite score items as columns
#' @param dv Location of dependent variable in the data frame
#' @param compv Location of composite score variables in the data frame
#' @param ndelete How many items should be deleted from the scale at maximum?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats lm
#' @importFrom performance item_reliability

.compscoreHack <- function(df, dv, compv, ndelete, strategy = "firstsig", alpha = 0.05){

  stopifnot(length(compv)-ndelete >= 2)

  # Compute original p-value and R^2 with full scale
  fullscale <- rowMeans(df[, compv, drop = FALSE])
  report.orig <- .report.association(x = fullscale, y = df[, dv], method = "scale.full")
  analyses <- list(list(report = report.orig,
                        r2 = tanh(report.orig[["effect"]])^2))

  # Prepare and initialize variables for p-hacking
  compscale <- df[, compv]
  changescale <- df[, compv]
  out <- NULL

  # Strategically delete items from the composite scale and re-calculate the p-value
  for(i in 1:ndelete){

    pval <- rep(NA, 2)
    r2val <- rep(NA, 2)

    # Define new item to delete from the scale
    out[i] <- which(colnames(compscale) %in% colnames(changescale)[which.max(performance::item_reliability(changescale)[,2])])

    # Compute p-value for the new composite score
    newscore <- rowMeans(compscale[, -out, drop = FALSE])
    report.new <- .report.association(x = newscore, y = df[, dv],
                                      method = paste0("scale.delete.", paste(out, collapse = "-")))
    analyses[[length(analyses)+1]] <- list(report = report.new,
                                           r2 = tanh(report.new[["effect"]])^2)

    # Compute p-value for the item deleted from the score
    itemscore <- compscale[, out[i]]
    report.item <- .report.association(x = itemscore, y = df[, dv],
                                       method = paste0("item.", out[i]))
    analyses[[length(analyses)+1]] <- list(report = report.item,
                                           r2 = tanh(report.item[["effect"]])^2)

    changescale <- compscale[, -out, drop = FALSE]
  }

  ps <- vapply(analyses, function(x) x[["report"]][["p"]], numeric(1))

  # Select final p-hacked p-value based on strategy
  final.index <- .selectanalysis(ps = ps, strategy = strategy, alpha = alpha)

  return(list(ps.hack = analyses[[final.index]][["report"]][["p"]],
              ps.orig = analyses[[1]][["report"]][["p"]],
              r2s.hack = analyses[[final.index]][["r2"]],
              r2s.orig = analyses[[1]][["r2"]],
              report.initial = analyses[[1]][["report"]],
              report.final = analyses[[final.index]][["report"]]))

}

#' Simulate p-hacking with composite scores
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables
#' @param ndelete How many items should be deleted from the scale at maximum?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies on the Fisher-z scale
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @importFrom pbapply pblapply
#' @importFrom shiny withProgress incProgress
#' @export

sim.compscoreHack <- function(nobs, ncompv, rcomp, ndelete, strategy = "firstsig", effect = 0, heterogeneity = 0, alpha = 0.05, iter = 1000, shinyEnv=FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.compscore(nobs = nobs, ncompv = ncompv, rcomp = rcomp,
                               effect = effect, heterogeneity = heterogeneity,
                               empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset (with progress bar within or outside Shiny)
  if(!shinyEnv){
    .compscoreHackList <- function(x){
      .compscoreHack(df = x, dv = 1, compv = c(2:(ncompv+1)), ndelete = ndelete,
                     strategy = strategy, alpha = alpha)
    }

    res <- pbapply::pblapply(dat, .compscoreHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value=0, {
      res=lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .compscoreHack(df = x, dv = 1, compv = c(2:(ncompv+1)), ndelete = ndelete,
                       strategy = strategy, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig"))

}
