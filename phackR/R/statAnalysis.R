# ==============================================================================
# Exploiting statistical analysis options
# ==============================================================================

# Data simulation can be done with .sim.data

#' P-Hacking function for exploiting different statistical analysis options
#' @param df Data frame with one continuous independent variable and one continuous dependent variable
#' @param group Location of the grouping variable in the data frame
#' @param dv Location of the dependent variabl in the data frame
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test wilcox.test
#' @importFrom WRS2 yuen

.statAnalysisHack <- function(df, group, dv, ambitious = FALSE, alternative = "two.sided", alpha = 0.05){

  dftest <- cbind(df[, group], df[, dv])
  colnames(dftest) <- c("group", "dv")

  # "Normal" t-test
  p.orig <- stats::t.test(dv ~ group, var.equal = TRUE, alternative = alternative,
                          data = dftest)$p.value

  # Welch test
  p.welch <- stats::t.test(dv ~ group, var.equal = FALSE,
                           alternative = alternative, data = dftest)$p.value

  # Mann-Whitney / Wilcoxon test
  p.wilcox <- stats::wilcox.test(dv ~ group, alternative = alternative,
                                 data = dftest)$p.value

  # Yuen test with different levels of trimming
  p.yuen <- rep(NA, 4)
  trim <- c(0.1, 0.15, 0.2, 0.25)
  for(i in 1:4) {
    p.yuen[i] <- WRS2::yuen(dv ~ group, tr = trim[i],
                            data = as.data.frame(dftest))$p.value
  }

  ps <- c(p.orig, p.welch, p.wilcox, p.yuen)

  # select p-value ambitious
  if(ambitious == TRUE){

    if(min(ps) < alpha){
      p.final <- min(ps)
    } else {
      p.final <- ps[1]
    }

    # select p-value "normal" p-hacking
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

#' Simulate p-Hacking for exploiting different statistical analysis options
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Number of observations per group. Either a scalar or a vector with 2 elements.
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test
#' @param iter Number of simulation iterations
#' @param seed Initial seed for the random process
#' @export

sim.statAnalysisHack <- function(nobs.group, ambitious = FALSE, alternative = "two.sided", alpha = 0.05, iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = nobs.group)
  }

  # Apply p-hacking procedure to each dataset

  .statAnalysisHackList <- function(x){
    .statAnalysisHack(df = x, group = 1, dv = 2, ambitious = ambitious, alternative = alternative, alpha = alpha)
  }

  res <- lapply(dat, .statAnalysisHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)


}

