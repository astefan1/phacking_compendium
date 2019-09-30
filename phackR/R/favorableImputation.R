# ==============================================================================
# p-Hacking through Favorable Imputation
# ==============================================================================

# ------------------------------------------------------------------------------
# Some local helper functions
# ------------------------------------------------------------------------------

#' Single value imputation function
#' Imputes NA values in a single variable using the function specified in fun
#' @param x The target variable of the imputation
#' @param fun The function used to replace missing values that takes x as an argument (e.g., mean)
#' @param ... Additional arguments to fun

.easyimpute <- function(x, fun, ...){
  x[is.na(x)] <- fun(x, ...)
  return(x)
}

#' Estimate mode of continuous variables
#' Estimates mode of continuous variables using the density() function
#' @param x The target variable for which the mode should be searched
#' @importFrom stats density

.estimate_mode <- function(x) {
  d <- stats::density(x, na.rm = T)
  d$x[which.max(d$y)]
}

#' Use mice::mice function without output being sent to the console
#' @param ... All arguments sent to the mice function
#' @importFrom mice mice

.miceNoOutput <- function(...){
  sink("/dev/null")
  res <- mice::mice(...)
  sink()
  invisible(res)
}

# ------------------------------------------------------------------------------
# P-Hacking functions
# ------------------------------------------------------------------------------

# Simulation function: Data can be simulated using .sim.multcor with r = 0.
# Proportion of missing values can be controlled through the argument "missing"

#' P-Hacking function favorable imputation in univariate linear regression
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame containing x and y variables as columns
#' @param x Location of x variable (predictor) in the data frame
#' @param y Location of y variable (criterion) in the data frame
#' @param which Which missing value handling method? 1: delete missing, 2: mean imputation, 3: median imputation, 4: mode imputation, 5: predictive mean matching, 6: weighted predictive mean matching, 7: sample from observed values, 8: Bayesian linear regression, 9: linear regression ignoring model error, 10: linear regression predicted values
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param seed Random seed for imputation processes that require a random process
#' @importFrom stats median lm
#' @importFrom mice complete


.impHack <- function(df, x, y, which = c(1:10), ambitious = FALSE, alpha = 0.05, seed = 1234){

  x <- df[,x]
  y <- df[,y]

  # Stop if imputation methods are not defined
  stopifnot(any(c(1:10) %in% which))

  # Initialize result vector
  ps <- rep(NA, 10)

  # p-value when missing values are deleted
  if(1 %in% which){
    ps[1] <- summary(stats::lm(y ~ x, na.action = "na.omit"))$coefficients[2, 4]
  }

  # Mean imputation
  if(2 %in% which){
    newx <- .easyimpute(x, mean, na.rm = T)
    newy <- .easyimpute(y, mean, na.rm = T)
    ps[2] <- summary(stats::lm(newy ~ newx))$coefficients[2, 4]
  }

  # Median imputation
  if(3 %in% which){
    newx <- .easyimpute(x, mean, na.rm = T)
    newy <- .easyimpute(y, mean, na.rm = T)
    ps[3] <- summary(stats::lm(newy ~ newx))$coefficients[2, 4]
  }

  # Mode imputation
  if(4 %in% which){
    newx <- .easyimpute(x, .estimate_mode)
    newy <- .easyimpute(y, .estimate_mode)
    ps[4] <- summary(stats::lm(newy ~ newx))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): predictive mean matchihng
  dfnew <- as.data.frame(cbind(x, y))
  if(5 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "pmm")
    ps[5] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): Weighted predictive mean matching
  if(6 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "midastouch")
    ps[6] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): Sample from observed values
  if(7 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "sample")
    ps[7] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): Bayesian linear regression
  if(8 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "norm")
    ps[8] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression ignoring model error
  if(9 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "norm.nob")
    ps[9] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression predicted values
  if(10 %in% which){
    imp <- .miceNoOutput(dfnew, seed = seed, m = 1, method = "norm.predict")
    ps[10] <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))$coefficients[2, 4]
  }

  ps <- ps[!is.na(ps)]

  # Select p-value "ambitious" p-hacking
  if(ambitious == TRUE){

    if(min(ps) < alpha){
      p.final <- min(ps)
    } else {
      p.final <- ps[1]
    }

    # Select p-value "normal" p-hacking
  } else if (ambitious == FALSE) {

    if(min(ps) < alpha){
      p.final <- sample(ps[which(ps < alpha)], 1)
    } else {
      p.final <- ps[1]
    }
  }

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-Hacking with different sorts of outlier definition missing value imputation
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param missing Percentage of missing values (e.g., 0.1 for 10 percent)
#' @param which Which imputation methods?  Either 5 random methods are chosen ("random") or a numeric vector containing the chosen methods (1: delete missing, 2: mean imputation, 3: median imputation, 4: mode imputation, 5: predictive mean matching, 6: weighted predictive mean matching, 7: sample from observed values, 8: Bayesian linear regression, 9: linear regression ignoring model error, 10: linear regression predicted values)
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param seed Initial seed for random processes
#' @export

sim.impHack <- function(nobs, missing, which = c(1:10), ambitious = FALSE, alpha = 0.05, iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0, missing = missing)
  }

  set.seed(seed)
  if(any(which == "random")) which <- sample(c(1:10), 5)

  # Apply p-hacking procedure to each dataset
  .impHackList <- function(x){
    .impHack(df = x, x = 1, y = 2,
             which = which, ambitious = ambitious, alpha = alpha, seed = seed)
  }

  res <- lapply(dat, .impHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  ps.all <- list()
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}

