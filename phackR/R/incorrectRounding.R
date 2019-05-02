# ==============================================================================
# Incorrect Rounding
# ==============================================================================

#' Simulate dataset for independent sample t-test
#' @description Outputs a data frame with two columns
#' @param nobs.group Number of observations per group. Either a scalar or a vector with two elements.
#' @importFrom stats rnorm

.sim.ttest <- function(nobs.group){

  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  V1 <- stats::rnorm(nobs.group[1], 0, 1)
  V2 <- stats::rnorm(nobs.group[2], 0, 1)

  res <- cbind(V1, V2)
  return(res)

}

#' P-Hacking function for incorrect rounding
#' @description Outputs a p-hacked p-value and the non-p-hacked-p-value
#' @param df Data frame
#' @param g1 Scalar defining location of first group in the data frame
#' @param g2 Scalar defining location of second group in the data frame
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @importFrom stats t.test

.roundhack <- function(df, g1, g2, roundinglevel, alternative = "two.sided"){

  # Prepare dataset
  colnames(df)[g1] <- "g1"
  colnames(df)[g2] <- "g2"

  # Compute t-test
  pval <- stats::t.test(df[,"g1"], df[,"g2"], var.equal = TRUE, alternative = alternative)$p.value

  # P-hack p-value
  if(pval > 0.05 && pval < roundinglevel){
    p.final <- 0.05
  } else {
    p.final <- pval
  }

  return(list(p.final = p.final,
              ps = pval))

}

#' Simulate p-hacking with incorrect rounding
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param iter Number of iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param seed Initial seed for the random process
#' @export

sim.roundhack <- function(roundinglevel, iter = 1000, alternative = "two.sided", seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.ttest(nobs.group = 30)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .roundhack, g1 = 1, g2 = 2,
                roundinglevel = roundinglevel, alternative = alternative)
  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
