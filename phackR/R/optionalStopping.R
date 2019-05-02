# ==============================================================================
# Optional Stopping Based on Significance
# ==============================================================================

# Sampling function is the same as described in "incorrectRounding.R"

#' Optional Stopping based on existing dataset
#' @description Outputs a p-hacked p-value and a non-p-hacked p-value based on the maximum sample size
#' @param df Data frame
#' @param g1 Scalar defining location of group 1 for the t-test
#' @param g2 Scalar defining location of group 2 for the t-test
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @importFrom stats t.test
#' @importFrom utils tail

.optstop <- function(df, g1, g2, n.min, n.max, step = 1, alternative = "two.sided"){

  # Sanity check: Data frame large enough?
  stopifnot(nrow(df) >= n.max)

  # Determine places of peeks
  peeks <- seq(n.min, n.max, by=step)

  # Prepare dataset
  colnames(df)[g1] <- "g1"
  colnames(df)[g2] <- "g2"

  # Initialize result vector
  ps <- rep(NA, length(peeks))

  # Compute t-tests
  ps <- sapply(peeks, FUN = function(x) {stats::t.test(df[1:x, "g1"], df[1:x, "g2"], var.equal = TRUE, alternative = alternative)$p.value})

  # Do the p-hacking
  if(any(ps < 0.05) == FALSE){
    p.final <- utils::tail(ps, 1)
  } else if (any(ps < 0.05) == TRUE) {
    p.final <- ps[which(ps < 0.05)][1]
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
#' @param seed Initial seed for the random process
#' @importFrom utils tail
#' @export
#'

sim.optstop <- function(n.min, n.max, step = 1, alternative = "two.sided", iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.ttest(nobs.group = n.max)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .optstop, g1 = 1, g2 = 2,
                n.min = n.min, n.max = n.max, step = step, alternative = alternative)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- utils::tail(res[[i]][["ps"]], 1)
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
