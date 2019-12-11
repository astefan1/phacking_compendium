# ==============================================================================
# Selective Reporting of the Independent Variable
# ==============================================================================

#' Simulate dataset with multiple independent variables
#' @description Outputs data frame with multiple independent variables
#' @param nobs.group Scalar defining number of observations per group
#' @param nvar Number of independent variables in the data frame
#' @param r Desired correlation between the independent variables (scalar)

.sim.multIV <- function(nobs.group, nvar, r){

  # Observations per group
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)

  # Simulate control group
  control <- rnorm(nobs.group[1])

  # Simulate multiple experimental groups
  ivs <- .sim.multcor(nobs = nobs.group[2], nvar = nvar, r = r)

  # Generate data frame
  res <- cbind(control, ivs)

  return(res)

}

#' P-Hacking function for multiple independent variables
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame (wide format) containing a control group variable and multiple treatment group variables
#' @param ivs Location of the independent variables (treatment groups) in the (wide) data frame
#' @param control Location of the control group in the (wide) data frame
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.multIVhack <- function(df, ivs, control, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  treatm <- df[, ivs]
  control <- df[, control]

  # Prepare dataset
  ps <- NULL

  # Compute t-tests
  for(i in 1:length(ivs)){
    ps[i] <- stats::t.test(control, treatm[,i], var.equal = TRUE, alternative = alternative)$p.value
  }

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-Hacking with multiple independent variables
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of independent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @export

sim.multIVhack <- function(nobs.group, nvar, r, strategy = "firstsig", iter = 1000, alternative = "two.sided", alpha = 0.05){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multIV(nobs.group = nobs.group, nvar = nvar, r = r)
  }

  # Apply p-hacking procedure to each dataset

  .multIVhacklist <- function(x){
    .multIVhack(df = x, ivs = c(2:(nvar+1)), control = 1,
                strategy = strategy, alternative = alternative, alpha = alpha)
  }

  res <- lapply(dat, .multIVhacklist)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
