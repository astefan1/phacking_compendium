# ==============================================================================
# Incorrect Rounding
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' P-Hacking function for incorrect rounding
#' @description Outputs a p-hacked p-value and the non-p-hacked-p-value
#' @param df Data frame
#' @param group Scalar defining location group vector in the data frame
#' @param y Scalar defining location of dependent variable in the data frame
#' @param roundinglevel Highest p-value that is rounded down to 0.05
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test

.roundhack <- function(df, group, y, roundinglevel, alternative = "two.sided", alpha = 0.05){

  # Prepare dataset
  df <- as.data.frame(df)
  colnames(df)[y] <- "y"
  colnames(df)[group] <- "group"
  g1 <- unique(df$group)[1]
  g2 <- unique(df$group)[2]

  # Compute t-test
  pval <- stats::t.test(df[df$group == g1,"y"], df[df$group == g2,"y"], var.equal = TRUE, alternative = alternative)$p.value

  # P-hack p-value
  if(pval > alpha && pval < roundinglevel){
    p.final <- alpha
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
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param seed Initial seed for the random process
#' @export

sim.roundhack <- function(roundinglevel, iter = 1000, alternative = "two.sided", alpha = 0.05, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = 30)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .roundhack, group = 1, y = 2,
                roundinglevel = roundinglevel, alternative = alternative, alpha = alpha)
  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
