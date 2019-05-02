# ==============================================================================
# Selective Reporting of the Independent Variable
# ==============================================================================

#' Simulate dataset with multiple independent variables
#' @description Outputs data frame
#' @param nobs.group Scalar defining number of observations per group
#' @param nvar Number of independent variables in the data frame
#' @param r Desired correlation between the independent variables (scalar)

.sim.multIV <- function(nobs.group, nvar, r){

  # Simulate control group
  control <- rnorm(nobs.group)

  # Simulate multiple experimental groups
  ivs <- .sim.multcor(nobs = nobs.group, nvar = nvar, r = r)

  # Generate data frame
  res <- cbind(control, ivs)

  return(res)

}

#' P-Hacking function for multiple independent variables
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame with one group variable and multiple dependent variables
#' @param ivs Vector defining the IV columns (will be checked in given order)
#' @param control Scalar defining location of control group variable
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @importFrom stats t.test

.multIVhack <- function(df, ivs, control, ambitious = FALSE, alternative = "two.sided"){

  # Prepare dataset
  colnames(df)[control] <- "control"
  ps <- NULL

  # Compute t-tests
  for(i in 1:length(ivs)){
    ps[i] <- stats::t.test(df[, "control"], df[, ivs[i]], var.equal = TRUE, alternative = alternative)$p.value
  }

  # Select p-value "ambitious" p-hacking
  if(ambitious == TRUE){

    if(min(ps) < 0.05){
      p.final <- min(ps)
    } else {
      p.final <- ps[1]
    }

  # Select p-value "normal" p-hacking
  } else if (ambitious == FALSE) {

    if(!any(ps < 0.05)){
      p.final <- ps[1]
    } else {
      p.final <- ps[which(ps < 0.05)[1]]
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
#' @param seed Initial seed for the random process
#' @export

sim.multIVhack <- function(nobs.group, nvar, r, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.multIV(nobs.group = nobs.group, nvar = nvar, r = r)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .multIVhack, ivs = c(2:(nvar+1)), control = 1,
                ambitious = ambitious, alternative = alternative)
  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
