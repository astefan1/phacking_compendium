# ==============================================================================
# Selective Reporting of the Independent Variable
# ==============================================================================

#' Simulate dataset with multiple independent variables
#' @description Outputs data frame
#' @param nobs.group Scalar defining number of observations per group (if length = 2, the first element will be the size of the control group that stays the same)
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
  res <- list(ivs = ivs, control = control)

  return(res)

}

#' P-Hacking function for multiple independent variables
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param ivs Data frame or matrix containing the independent variables as columns
#' @param control Vector containing the control variable
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.multIVhack <- function(ivs, control, ambitious = FALSE, alternative = "two.sided", alpha = 0.05){

  # Prepare dataset
  ps <- NULL

  # Compute t-tests
  for(i in 1:length(ivs)){
    ps[i] <- stats::t.test(control, ivs[,i], var.equal = TRUE, alternative = alternative)$p.value
  }

  # Select p-value "ambitious" p-hacking
  if(ambitious == TRUE){

    if(min(ps) < alpha){
      p.final <- min(ps)
    } else {
      p.final <- ps[1]
    }

  # Select p-value "normal" p-hacking
  } else if (ambitious == FALSE) {

    if(min(ps) < alpha){
      p.final <- ps[which(ps < alpha)[1]]
    } else {
      p.final <- ps[1]
    }
  }

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-Hacking with multiple independent variables
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param seed Initial seed for the random process
#' @export

sim.multIVhack <- function(nobs.group, nvar, r, ambitious = FALSE, iter = 1000, alternative = "two.sided", alpha = 0.05, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.multIV(nobs.group = nobs.group, nvar = nvar, r = r)
  }

  # Apply p-hacking procedure to each dataset

  .multIVhacklist <- function(x){
    .multIVhack(ivs = x[[1]], control = x[[2]], ambitious = ambitious, alternative = alternative, alpha = alpha)
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
