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
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats median lm
#' @importFrom mice complete


.impHack <- function(df, x, y, which = c(1:10), strategy = "firstsig", alpha = 0.05){

  x <- df[,x]
  y <- df[,y]

  # Stop if imputation methods are not defined
  stopifnot(any(c(1:10) %in% which))

  # Initialize result vector
  ps <- rep(NA, 10)
  r2s <- rep(NA, 10)

  # p-value when missing values are deleted
  if(1 %in% which){
    mod1 <- summary(stats::lm(y ~ x, na.action = "na.omit"))
    ps[1] <- mod1$coefficients[2, 4]
    r2s[1] <- mod1$r.squared
  }

  # Mean imputation
  if(2 %in% which){
    newx <- .easyimpute(x, mean, na.rm = T)
    newy <- .easyimpute(y, mean, na.rm = T)
    mod2 <- summary(stats::lm(newy ~ newx))
    ps[2] <- mod2$coefficients[2, 4]
    r2s[2] <- mod2$r.squared
  }

  # Median imputation
  if(3 %in% which){
    newx <- .easyimpute(x, mean, na.rm = T)
    newy <- .easyimpute(y, mean, na.rm = T)
    mod3 <- summary(stats::lm(newy ~ newx))
    ps[3] <- mod3$coefficients[2, 4]
    r2s[3] <- mod3$r.squared
  }

  # Mode imputation
  if(4 %in% which){
    newx <- .easyimpute(x, .estimate_mode)
    newy <- .easyimpute(y, .estimate_mode)
    mod4 <- summary(stats::lm(newy ~ newx))
    ps[4] <- mod4$coefficients[2, 4]
    r2s[4] <- mod4$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): predictive mean matchihng
  dfnew <- as.data.frame(cbind(x, y))
  if(5 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "pmm")
    mod5 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[5] <- mod5$coefficients[2, 4]
    r2s[5] <- mod5$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): Weighted predictive mean matching
  if(6 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "midastouch")
    mod6 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[6] <- mod6$coefficients[2, 4]
    r2s[6] <- mod6$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): Sample from observed values
  if(7 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "sample")
    mod7 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[7] <- mod7$coefficients[2, 4]
    r2s[7] <- mod7$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): Bayesian linear regression
  if(8 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "norm")
    mod8 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[8] <- mod8$coefficients[2, 4]
    r2s[8] <- mod8$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression ignoring model error
  if(9 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "norm.nob")
    mod9 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[9] <- mod9$coefficients[2, 4]
    r2s[9] <- mod9$r.squared
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression predicted values
  if(10 %in% which){
    imp <- .miceNoOutput(dfnew, m = 1, method = "norm.predict")
    mod10 <- summary(stats::lm(y ~ x, data = mice::complete(imp, 1)))
    ps[10] <- mod10$coefficients[2, 4]
    r2s[10] <- mod10$r.squared
  }

  ps <- ps[!is.na(ps)]
  r2s <- r2s[!is.na(r2s)]

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- r2s[ps == p.final]

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s))

}

#' Simulate p-Hacking with different sorts of outlier definition missing value imputation
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param missing Percentage of missing values (e.g., 0.1 for 10 percent)
#' @param which Which imputation methods?  Either 5 random methods are chosen ("random") or a numeric vector containing the chosen methods (1: delete missing, 2: mean imputation, 3: median imputation, 4: mode imputation, 5: predictive mean matching, 6: weighted predictive mean matching, 7: sample from observed values, 8: Bayesian linear regression, 9: linear regression ignoring model error, 10: linear regression predicted values)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.impHack <- function(nobs, missing, which = c(1:10), strategy = "firstsig", alpha = 0.05, iter = 1000, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0, missing = missing)
  }

  if(any(which == "random")) which <- sample(c(1:10), 5)

  # Apply p-hacking procedure to each dataset

  if(!shinyEnv){
    .impHackList <- function(x){
      .impHack(df = x, x = 1, y = 2,
               which = which, strategy = strategy, alpha = alpha)
    }

    res <- pbapply::pblapply(dat, .impHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2)))
        .impHack(df = x, x = 1, y = 2,
                 which = which, strategy = strategy, alpha = alpha)
      })
    })
  }

  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL
  ps.all <- list()
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)

}

