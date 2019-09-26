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
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats lm median

.varTransHack <- function(df, x, y, transvar, ambitious = FALSE, alpha = 0.05){

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
                    1/x,                             # inverse
                    as.numeric(x > stats::median(x)) # median split
    )
  }


  if(transvar != "x"){
    Ytrans <- cbind(Ytrans,
                    log(y+abs(min(y))+1e-10),        # log transformation
                    sqrt(y+abs(min(y))+1e-10),       # square root transformation
                    1/y,                             # inverse
                    as.numeric(y > stats::median(y)) # median split
    )
  }

  # Calculate p-values for all transformed variables

  ps <- matrix(NA, nrow = dim(Xtrans)[2], ncol = dim(Ytrans)[2])

  for(i in 1:ncol(Xtrans)){
    for(j in 1:ncol(Ytrans)){
      ps[i,j] <- summary(stats::lm(Ytrans[,j] ~ Xtrans[,i]))$coefficients[2, 4]
    }
  }

  ps <- as.vector(ps)

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
      p.final <- ps[which(ps < alpha)[1]]
    } else {
      p.final <- ps[1]
    }
  }

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-hacking with variable transformations
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param transvar Which variables should be transformed? Either "x" (for x variable), "y" (for y variable), or "xy" (for both)
#' @param ambitious Ambitious p-hacking (smallest p value): TRUE/FALSE
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param seed Initial seed for the random process
#' @export

sim.varTransHack <- function(nobs, transvar, ambitious = FALSE, alpha = 0.05, iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0)
  }

  # Apply p-hacking procedure to each dataset
  .varTransHackList <- function(arg){
    .varTransHack(df = arg, x = 1, y = 2, transvar = transvar,
                  ambitious = ambitious, alpha = alpha)
  }

  res <- lapply(dat, .varTransHackList)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}
