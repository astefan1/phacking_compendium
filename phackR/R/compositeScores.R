# ==============================================================================
# Scale Redefinition / Composite Scores
# ==============================================================================

#' Simulate data: Correlated composite score raw variables and one non-correlated dependent variable
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables

.sim.compscore <- function(nobs, ncompv, rcomp){

  dv <- rnorm(nobs, 0, 1)

  iv <- .sim.multcor(nobs = nobs, nvar = ncompv, r = rcomp)

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

  # Compute original p-value with full scale
  p.orig <- summary(lm(df[, dv] ~ rowMeans(df[, compv])))$coefficients[2, 4]

  # Prepare and initialize variables for p-hacking
  ps <- list()
  compscale <- df[, compv]
  changescale <- df[, compv]
  out <- NULL

  # Strategically delete items from the composite scale and re-calculate the p-value
  for(i in 1:ndelete){

    pval <- rep(NA, 3)

    # Define new item to delete from the scale
    out[i] <- which(colnames(compscale) %in% colnames(changescale)[which.max(performance::item_reliability(changescale)[,2])])

    # Compute p-value for the new composite score
    newscore <- rowMeans(compscale[, -out])
    pval[1] <- summary(lm(df[, dv] ~ newscore))$coefficients[2, 4]

    # Compute p-value for the item deleted from the score
    itemscore <- compscale[, out[i]]
    pval[2] <- summary(lm(df[, dv] ~ itemscore))$coefficients[2, 4]

    # Compute p-value for a scale of all items deleted so far
    nonscore <- rowMeans(cbind(compscale[, out]))
    pval[3] <- summary(lm(df[, dv] ~ nonscore))$coefficients[2, 4]

    changescale <- compscale[, -out]
    ps[[i]] <- pval
  }

  ps <- c(p.orig, unique(unlist(ps)))

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-hacking with composite scores
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables
#' @param ndelete How many items should be deleted from the scale at maximum?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param seed Initial seed for the random process
#' @export

sim.compscoreHack <- function(nobs, ncompv, rcomp, ndelete, strategy = "firstsig", alpha = 0.05, iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.compscore(nobs = nobs, ncompv = ncompv, rcomp = rcomp)
  }

  # Apply p-hacking procedure to each dataset
  .compscoreHackList <- function(x){
    .compscoreHack(df = x, dv = 1, compv = c(2:(ncompv+1)), ndelete = ndelete,
                  strategy = strategy, alpha = alpha)
  }

  res <- lapply(dat, .compscoreHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
