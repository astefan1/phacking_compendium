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
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)

.varTransHack <- function(df, x, y, transvar, strategy = "firstsig", alpha = 0.05){

  x <- df[, x]
  y <- df[, y]

  # Transform all variables that should be transformed

  Xtrans <- matrix(NA, nrow = nrow(df))
  Xtrans[,1] <- x
  Ytrans <- matrix(NA, nrow = nrow(df))
  Ytrans[,1] <- y

  if(transvar != "y"){
    Xtrans <- cbind(Xtrans,
                    log(x+abs(min(x))+1e-10),        # log transformation
                    sqrt(x+abs(min(x))+1e-10),       # square root transformation
                    1/x                              # inverse
    )
  }


  if(transvar != "x"){
    Ytrans <- cbind(Ytrans,
                    log(y+abs(min(y))+1e-10),        # log transformation
                    sqrt(y+abs(min(y))+1e-10),       # square root transformation
                    1/y                              # inverse
    )
  }

  # Calculate p-values for all transformed variables

  ps <- matrix(NA, nrow = dim(Xtrans)[2], ncol = dim(Ytrans)[2])
  r2s <- matrix(NA, nrow = dim(Xtrans)[2], ncol = dim(Ytrans)[2])

  for(i in 1:ncol(Xtrans)){
    for(j in 1:ncol(Ytrans)){
      mod <- summary(stats::lm(Ytrans[,j] ~ Xtrans[,i]))
      ps[i,j] <- mod$coefficients[2, 4]
      r2s[i,j] <- mod$r.squared
    }
  }

  ps <- as.vector(ps)
  r2s <- as.vector(r2s)

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- r2s[ps == p.final]

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s))

}

#' Simulate p-hacking with variable transformations
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param transvar Which variables should be transformed? Either "x" (for x variable), "y" (for y variable), or "xy" (for both)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @export

sim.varTransHack <- function(nobs, transvar, strategy = "firstsig", alpha = 0.05, iter = 1000){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0)
  }

  # Apply p-hacking procedure to each dataset
  .varTransHackList <- function(arg){
    .varTransHack(df = arg, x = 1, y = 2, transvar = transvar,
                  strategy = strategy, alpha = alpha)
  }

  res <- lapply(dat, .varTransHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)

}
