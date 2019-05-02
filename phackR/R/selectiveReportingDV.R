# ==============================================================================
# Selective Reporting of the Dependent Variable
# ==============================================================================

#' Simulate dataset with multiple dependent variables
#' @description Outputs data frame with a grouping variable and multiple correlated dependent variables
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables in the data frame
#' @param r Desired correlation between the dependent variables (scalar)

.sim.multDV <- function(nobs.group, nvar, r){

  # Generate group vector
  group <- rep(1:length(nobs.group), nobs.group)

  # Generate dependent variables
  dvs <- .sim.multcor(nobs = sum(nobs.group), nvar = nvar, r = r)

  # Generate data frame
  res <- cbind(group, dvs)

  return(res)
}

#' P-Hacking function for multiple dependent variables
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame with one group variable and multiple dependent variables
#' @param dvs Vector defining the DV columns (will be checked in given order)
#' @param group Scalar defining grouping column
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @importFrom stats t.test

.multDVhack <- function(df, dvs, group, ambitious = FALSE, alternative = "two.sided"){

  # Prepare data frame
  colnames(df)[group] <- "group"
  g1 <- unique(df$group)[1]
  g2 <- unique(df$group)[2]
  ps <- NULL

  # Compute t-tests
  for(i in 1:length(dvs)){

    ps[i] <- stats::t.test(df[df$group == g1, dvs[i]], df[df$group == g2, dvs[i]],
                           var.equal = TRUE, alternative = alternative)$p.value
  }

  ps <- unlist(ps)

  # select p-value ambitious
  if(ambitious == TRUE){

    if(min(ps) < 0.05){
      p.final <- min(ps)
    } else {
      p.final <- ps[1]
    }

  # select p-value "normal" p-hacking
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

#' Simulate p-Hacking with multiple dependent variables
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param seed Initial seed for the random process
#' @export

sim.multDVhack <- function(nobs.group, nvar, r, ambitious = FALSE, iter = 1000, alternative = "two.sided", seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.multDV(nobs.group = nobs.group, nvar = nvar, r = r)
  }

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, .multDVhack, dvs = c(2:(nvar+1)), group = 1,
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

