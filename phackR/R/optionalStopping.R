# ==============================================================================
# Optional Stopping Based on Significance
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' Optional Stopping based on existing dataset
#' @description Outputs a p-hacked p-value and a non-p-hacked p-value based on the maximum sample size
#' @param df Data frame
#' @param group group Scalar defining grouping column
#' @param y Scalar defining location of dependent variable in the data frame
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test
#' @importFrom utils tail

.optstop <- function(df, group, y, n.min, n.max, step = 1, alternative = "two.sided", alpha = 0.05){

  # Extract group variables
  g1 <- df[df[,group] == unique(df[,group])[1], y]
  g2 <- df[df[,group] == unique(df[,group])[2], y]

  # Sanity check: Enough data?
  stopifnot(length(g1) >= n.max && length(g2) >= n.max)

  # Determine places of peeks
  peeks <- seq(n.min, n.max, by=step)

  # Initialize result vector
  ps <- rep(NA, length(peeks))

  # Compute t-tests
  ps <- sapply(peeks, FUN = function(x) {stats::t.test(g1[1:x], g2[1:x], var.equal = TRUE, alternative = alternative)$p.value})

  # Do the p-hacking
  if(any(ps < alpha) == FALSE){
    p.final <- utils::tail(ps, 1)
  } else if (any(ps < alpha) == TRUE) {
    p.final <- ps[which(ps < alpha)][1]
  }

  return(list(p.final = p.final,
              ps = ps))
}

#' Simulate p-hacking with incorrect rounding
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param iter Number of iterations
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param seed Initial seed for the random process
#' @importFrom utils tail
#' @export
#'

sim.optstop <- function(n.min, n.max, step = 1, alternative = "two.sided", iter = 1000, alpha = 0.05, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = n.max)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .optstop, group = 1, y = 2,
                n.min = n.min, n.max = n.max, step = step,
                alternative = alternative, alpha = alpha)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- utils::tail(res[[i]][["ps"]], 1)
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
