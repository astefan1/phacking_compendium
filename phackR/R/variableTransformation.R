# ==============================================================================
# p-Hacking through Variable Transformation
# ==============================================================================

# Simulation function: Data can be simulated with .sim.multcor where r = 0

#' P-Hacking function variable transformation in univariate linear regression
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame containing x and y variables as columns
#' @param x Location of x variable (predictor) in the data frame
#' @param y Location of y variable (criterion) in the data frame
#' @param transvar Which variables should be transformed? Either "x" (for x variable), "y" (for y variable), or "xy" (for both)
#' @param testnorm Should variables only be transformed after a significant test for normality of residuals?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)

.varTransHack <- function(df, x, y, transvar, testnorm = FALSE, strategy = "firstsig", alpha = 0.05){

  x <- df[, x]
  y <- df[, y]
  
  # Test normality of residuals first
  normality <- FALSE
  if(testnorm){
    mod <- stats::lm(y ~ x)
    normality <- stats::shapiro.test(stats::residuals(mod))$p.value > alpha
  }
  
  # Transform all variables that should be transformed

  Xtrans <- matrix(NA, nrow = nrow(df))
  Xtrans[,1] <- x
  xlabels <- "x.orig"
  Ytrans <- matrix(NA, nrow = nrow(df))
  Ytrans[,1] <- y
  ylabels <- "y.orig"

  if(transvar != "y" && normality == FALSE){
    Xtrans <- cbind(Xtrans,
                    log(x+abs(min(x))+1e-10),        # log transformation
                    sqrt(x+abs(min(x))+1e-10),       # square root transformation
                    1/x                              # inverse
    )
    xlabels <- c(xlabels, "x.log", "x.sqrt", "x.inv")
  }


  if(transvar != "x" && normality == FALSE){
    Ytrans <- cbind(Ytrans,
                    log(y+abs(min(y))+1e-10),        # log transformation
                    sqrt(y+abs(min(y))+1e-10),       # square root transformation
                    1/y                              # inverse
    )
    ylabels <- c(ylabels, "y.log", "y.sqrt", "y.inv")
  }

  # Calculate p-values for all transformed variables

  analyses <- list()

  for(i in 1:ncol(Xtrans)){
    for(j in 1:ncol(Ytrans)){
      report <- .report.association(x = Xtrans[,i], y = Ytrans[,j],
                                    method = paste(xlabels[i], ylabels[j], sep = "_"))
      analyses[[length(analyses)+1]] <- list(report = report,
                                             r2 = tanh(report[["effect"]])^2)
    }
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

#' Simulate p-hacking with variable transformations
#' @description Outputs a data frame containing the p-hacked p-values (\code{ps.hack}), the original p-values (\code{ps.orig}), and a normalized reporting block from all iterations
#' @param nobs Integer giving number of observations
#' @param transvar Which variables should be transformed? Either "x" (for x variable), "y" (for y variable), or "xy" (for both)
#' @param testnorm Should variables only be transformed after a significant test for normality of residuals?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param effect Mean effect size across studies on the Fisher-z scale
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @param empirical Should the observed initial effect be fixed to \code{effect}? If \code{TRUE}, \code{heterogeneity} is ignored.
#' @export

sim.varTransHack <- function(nobs, transvar, testnorm = FALSE, strategy = "firstsig", effect = 0, heterogeneity = 0, alpha = 0.05, iter = 1000, shinyEnv = FALSE, empirical = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.association(nobs = nobs, effect = effect,
                                 heterogeneity = heterogeneity,
                                 empirical = empirical)
  }

  # Apply p-hacking procedure to each dataset
  .varTransHackList <- function(arg){
    .varTransHack(df = arg, x = 1, y = 2, testnorm = testnorm, transvar = transvar,
                  strategy = strategy, alpha = alpha)
  }

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .varTransHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .varTransHack(df = x, x = 1, y = 2, transvar = transvar,
                      strategy = strategy, alpha = alpha)
      })
    })
  }

  .combine.phase1.results(res = res,
                          legacy.fields = c("ps.hack", "ps.orig", "r2s.hack", "r2s.orig"))

}
