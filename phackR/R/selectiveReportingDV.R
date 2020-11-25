# ==============================================================================
# Selective Reporting of the Dependent Variable
# ==============================================================================

#' Simulate dataset with multiple dependent variables
#' @description Outputs data frame with a grouping variable and multiple correlated dependent variables
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables in the data frame
#' @param r Desired correlation between the dependent variables (scalar)

.sim.multDV <- function(nobs.group, nvar, r){

  # Observations per group
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)

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
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test
#' @importFrom stats t.test

.multDVhack <- function(df, dvs, group, strategy = "firstsig", alternative = "two.sided", alpha = 0.05){

  # Prepare data frame
  dvs <- df[, dvs]
  group <- df[, group]
  mod <- list()
  r2s <- NULL

  # Compute t-tests
  for(i in 1:length(dvs)){

    mod[[i]] <- stats::t.test(dvs[, i] ~ group,
                           var.equal = TRUE, alternative = alternative)
    r2s[i] <- .compR2t(dvs[group == unique(group)[1], i],
                       dvs[group == unique(group)[2], i])
  }

  ps <- unlist(simplify2array(mod)["p.value", ])
  ds <- .compCohensD(unlist(simplify2array(mod)["statistic", ]), length(df[, group])/2)

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- unique(r2s[ps == p.final])
  d.final <- unique(ds[ps == p.final])

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s,
              d.final = d.final,
              ds = ds))

}

#' Simulate p-Hacking with multiple dependent variables
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nvar Number of dependent variables (columns) in the data frame
#' @param r Desired correlation between the dependent variables (scalar)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param iter Number of simulation iterations
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.multDVhack <- function(nobs.group, nvar, r, strategy = "firstsig", iter = 1000, alternative = "two.sided", alpha = 0.05, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multDV(nobs.group = nobs.group, nvar = nvar, r = r)
  }

  # Apply p-hacking procedure to each dataset

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .multDVhack, dvs = c(2:(nvar+1)), group = 1,
                  strategy = strategy, alternative = alternative, alpha = alpha)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .multDVhack(df=x, dvs = c(2:(nvar+1)), group = 1,
                    strategy = strategy, alternative = alternative, alpha = alpha)
      })
    })
  }

  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL
  ds.hack <- NULL
  ds.orig <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
    ds.hack[i] <- res[[i]][["d.final"]]
    ds.orig[i] <- res[[i]][["ds"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig, ds.hack, ds.orig)

  return(res)
}

