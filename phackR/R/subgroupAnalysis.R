# ==============================================================================
# Subgroup Analyses
# ==============================================================================

#' Simulate data with subgroups
#' @description Outputs data frame with multiple binary variables from which subgroups can be extracted
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups

.sim.subgroup <- function(nobs.group, nsubvars){

  dat <- .sim.data(nobs.group)

  # Observations per group and total observations
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  nobs <- sum(nobs.group)

  subvars <- matrix(NA, nrow = nobs, ncol = nsubvars)
  for(i in 1:nsubvars){
    subvars[,i] <- sample(c(0, 1), size = nobs, replace = TRUE)
  }

  res <- cbind(dat, subvars)

  return(res)

}

#' P-Hacking function for multiple subgroups analysis
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df A matrix or data frame containing all relevant data
#' @param iv Integer specifying the location of the binary independent variable in the data frame
#' @param dv Integer specifying the location of the dependent variable in the data frame
#' @param subvars Vector specifying the location of the subgroup variables in the data frame
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @importFrom dplyr group_by_at do
#' @importFrom stats t.test
#' @importFrom dplyr "%>%"

.subgroupHack <- function(df, iv, dv, subvars, alternative = "two.sided", strategy = "firstsig", alpha = 0.05){

  # Prepare data frame
  ttest.df <- cbind(df[,iv], df[,dv])
  subvars.df <- cbind(df[, subvars])
  dfnew <- as.data.frame(cbind(ttest.df, subvars.df))

  # Compute p-values

  # Not p-hacked
  p.orig <- stats::t.test(ttest.df[,2] ~ ttest.df[,1], var.equal = TRUE, alternative = alternative)$p.value

  # p-hacked
  ps <- list()

  for(i in 1:length(subvars)){

    tmp <- dplyr::group_by_at(dfnew, subvars[i]) %>%
      dplyr::do(as.data.frame(stats::t.test(.$V2 ~ .$V1, var.equal = TRUE, alternative = alternative)$p.value))

    ps[[i]] <- tmp[[2]]

  }

  ps <- c(p.orig, unlist(ps))

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-hacking with multiple subgroups
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @param iter Number of simulation iterations
#' @export

sim.subgroupHack <- function(nobs.group, nsubvars, alternative = "two.sided", strategy = "firstsig", alpha = 0.05, iter = 1000){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.subgroup(nobs.group = nobs.group, nsubvars = nsubvars)
  }

  # Apply p-hacking procedure to each dataset
  .subgroupHackList <- function(x){
    .subgroupHack(df = x, iv = 1, dv = 2, subvars = c(3:(2+length(nsubvars))),
                  alternative = alternative, strategy = strategy, alpha = alpha)
  }

  res <- lapply(dat, .subgroupHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}

