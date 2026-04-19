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

  analyses <- list()

  # p-value when missing values are deleted
  if(1 %in% which){
    report <- .report.association(x = x, y = y, method = "delete.missing")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Mean imputation
  if(2 %in% which){
    newx <- .easyimpute(x, mean, na.rm = T)
    newy <- .easyimpute(y, mean, na.rm = T)
    report <- .report.association(x = newx, y = newy, method = "impute.mean")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Median imputation
  if(3 %in% which){
    newx <- .easyimpute(x, median, na.rm = T)
    newy <- .easyimpute(y, median, na.rm = T)
    report <- .report.association(x = newx, y = newy, method = "impute.median")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Mode imputation
  if(4 %in% which){
    newx <- .easyimpute(x, .estimate_mode)
    newy <- .easyimpute(y, .estimate_mode)
    report <- .report.association(x = newx, y = newy, method = "impute.mode")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): predictive mean matchihng
  dfnew <- as.data.frame(cbind(x, y))
  if(5 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "pmm", silent = TRUE, print = FALSE)
    dat5 <- mice::complete(imp, 1)
    report <- .report.association(x = dat5$x, y = dat5$y, method = "mice.pmm")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): Weighted predictive mean matching
  if(6 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "midastouch", silent = TRUE, print = FALSE)
    dat6 <- mice::complete(imp, 1)
    report <- .report.association(x = dat6$x, y = dat6$y, method = "mice.midastouch")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): Sample from observed values
  if(7 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "sample", silent = TRUE, print = FALSE)
    dat7 <- mice::complete(imp, 1)
    report <- .report.association(x = dat7$x, y = dat7$y, method = "mice.sample")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): Bayesian linear regression
  if(8 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "norm", silent = TRUE, print = FALSE)
    dat8 <- mice::complete(imp, 1)
    report <- .report.association(x = dat8$x, y = dat8$y, method = "mice.norm")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression ignoring model error
  if(9 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "norm.nob", silent = TRUE, print = FALSE)
    dat9 <- mice::complete(imp, 1)
    report <- .report.association(x = dat9$x, y = dat9$y, method = "mice.norm.nob")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  # Multivariate imputations by chained equations ("mice" package): Linear regression predicted values
  if(10 %in% which){
    imp <- mice::mice(dfnew, m = 1, method = "norm.predict", silent = TRUE, print = FALSE)
    dat10 <- mice::complete(imp, 1)
    report <- .report.association(x = dat10$x, y = dat10$y, method = "mice.norm.predict")
    analyses[[length(analyses)+1]] <- list(report = report,
                                           r2 = tanh(report[["effect"]])^2)
  }

  ps <- vapply(analyses, function(x) x[["report"]][["p"]], numeric(1))

  # Select final p-hacked p-value based on strategy
  final.index <- .selectanalysis(ps = ps, strategy = strategy, alpha = alpha)

  return(list(ps.hack = analyses[[final.index]][["report"]][["p"]],
              ps.orig = analyses[[1]][["report"]][["p"]],
              r2s.hack = analyses[[final.index]][["r2"]],
              r2s.orig = analyses[[1]][["r2"]],
              report.initial = analyses[[1]][["report"]],
              report.final = analyses[[final.index]][["report"]]))

}

#' Simulate p-Hacking with different sorts of outlier definition missing value imputation
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs Integer giving number of observations
#' @param missing Percentage of missing values (e.g., 0.1 for 10 percent)
#' @param which Which imputation methods?  Either 5 random methods are chosen ("random") or a numeric vector containing the chosen methods (1: delete missing, 2: mean imputation, 3: median imputation, 4: mode imputation, 5: predictive mean matching, 6: weighted predictive mean matching, 7: sample from observed values, 8: Bayesian linear regression, 9: linear regression ignoring model error, 10: linear regression predicted values)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies on the Fisher-z scale
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.impHack <- function(nobs, missing, which = c(1:10), strategy = "firstsig", effect = 0, heterogeneity = 0, alpha = 0.05, iter = 1000, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.association(nobs = nobs, effect = effect, heterogeneity = heterogeneity, missing = missing)
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
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .impHack(df = x, x = 1, y = 2,
                 which = which, strategy = strategy, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig"))

}

