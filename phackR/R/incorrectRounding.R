# ==============================================================================
# Incorrect Rounding
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' P-Hacking function for incorrect rounding
#' @description Outputs a p-hacked p-value and the non-p-hacked-p-value
#' @param df Data frame
#' @param group Scalar defining location of the group vector in the data frame
#' @param dv Scalar defining location of dependent variable in the data frame
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.roundhack <- function(df, group, dv, roundinglevel, alternative = "two.sided", alpha = 0.05){

  # Compute t-test
  pval <- stats::t.test(df[,dv] ~ df[,group],
                        var.equal = TRUE, alternative = alternative)$p.value
  r2val <- .compR2t(df[,dv][(df[,group] == unique(df[,group])[1])],
                    df[,dv][(df[,group] == unique(df[,group])[2])])

  # P-hack p-value
  if(pval > alpha && pval < roundinglevel){
    p.final <- alpha
  } else {
    p.final <- pval
  }

  ps <- c(pval, p.final)

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2val,
              r2s = rep(r2val, 2)))

}

#' Simulate p-hacking with incorrect rounding
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param iter Number of iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @export

sim.roundhack <- function(roundinglevel, iter = 1000, alternative = "two.sided", alpha = 0.05){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = 30)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .roundhack, group = 1, dv = 2,
                roundinglevel = roundinglevel, alternative = alternative, alpha = alpha)
  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)

}
