# ==============================================================================
# Helpers
# ==============================================================================

#' Simulate multivariate correlated data for continuous variables
#' @description Outputs a data frame with correlated variables of defined length
#' @param nobs Number of observations (rows) in the simulated data frame
#' @param nvar Number of variables (columns) in the data frame
#' @param r Desired correlation between the variables (integer)
#' @param mu Mean of the random data
#' @param sd Standard deviation of the random data
#' @param missing Proportion of missing values per variable (e.g., 0.2 = 20 percent)
#' @importFrom stats rnorm

.sim.multcor <- function(nobs, nvar, r, mu = 0, sd = 1, missing = 0){

  # set up correlation matrix
  R <- matrix(rep(r, nvar**2), nrow = nvar)
  diag(R) <- rep(1, nvar)

  # transposed Cholesky decomposition of correlation matrix
  U <- t(chol(R))

  # create random noise matrix
  random.normal <- matrix(stats::rnorm(nvar*nobs, mu, sd), nrow=nvar, ncol=nobs)

  # create raw data from matrix multiplication of U and random noise
  X <- as.data.frame(t(U %*% random.normal))

  # add missing values
  if(missing > 0){
    navalues <- as.data.frame(replicate(nvar, sample(1:nobs, missing*nobs)))
    for(i in 1:nvar){
      X[unlist(navalues[,i]),i] <- NA
    }
  }

  return(X)

}

#' Generic sampling function
#' @description Outputs a data frame with two columns
#' @param nobs.group Number of observations per group. Either a scalar or a vector with two elements.
#' @importFrom stats rnorm

.sim.data <- function(nobs.group){

  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  V1 <- stats::rnorm(nobs.group[1], 0, 1)
  V2 <- stats::rnorm(nobs.group[2], 0, 1)
  group <- c(rep(1, nobs.group[1]), rep(2, nobs.group[2]))

  res <- cbind(group, c(V1, V2))
  return(res)

}

#' Create data frames without outliers
#' @description Inputs data frame and two sets of outlier values, outputs list with three data frames
#' @param x Original vector of x values
#' @param y Original vector of y values
#' @param outsx Outlier values to be removed from x
#' @param outsy Outlier values to be removed from y


.extractoutlier <- function(x, y, outsx, outsy){

  # Remove x outliers from x and y
  if(length(outsx) > 0){
    x1 <- x[!x %in% outsx]
    y1 <- y[!x %in% outsx]
  } else {
    x1 <- x
    y1 <- y
  }
  xy1 <- unname(cbind(x1, y1))

  # Remove y outliers from x and y
  if(length(outsy) > 0){
    x2 <- x[!y %in% outsy]
    y2 <- y[!y %in% outsy]
  } else {
    x2 <- x
    y2 <- y
  }
  xy2 <- unname(cbind(x2, y2))

  # Remove x and y outliers from x and y
  if(length(outsx) > 0 && length(outsy) > 0){
    x3 <- x[!x %in% outsx & !y %in% outsy]
    y3 <- y[!x %in% outsx & !y %in% outsy]
  } else {
    x3 <- x
    y3 <- y
  }
  xy3 <- unname(cbind(x3, y3))

  # Combine results
  res <- unname(list(xy1, xy2, xy3))
  res <- unique(res)

  return(res)

}

#' Select a p-value from a vector of p-hacked p-values
#' @description Takes a vector of p-values and selects the smallest, first significant, or smallest significant p-value.
#' @param ps Vector of p values
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level (default: 0.05)
#' @param p.orig Original p-value (if already computed, otherwise it will be set to the first value in ps)

.selectpvalue <- function(ps, strategy, alpha){

  p.final <- NA
  p.orig <- ps[1]

  # Select smallest significant p-value
  if(strategy == "smallest.sig"){

    if(min(ps) < alpha){
      p.final <- min(ps)
    } else {
      p.final <- p.orig
    }

    # Select first significant p-value
  } else if (strategy == "firstsig") {

    if(min(ps) < alpha){
      p.final <- ps[which(ps < alpha)[1]]
    } else {
      p.final <- p.orig
    }

    # Select smallest p-value
  } else if (strategy == "smallest") {
    p.final <- min(ps)
  }

  return(p.final)

}

#' Compute R squared for the t-test
#' @param x values of group 1
#' @param y values of group 2

.compR2t <- function(x, y){
  grandmean <- mean(c(x, y))
  sst <- sum((c(x,y)-grandmean)^2)
  sse <- sum((x-mean(x))^2)+sum((y-mean(y))^2)
  return(1-(sse/sst))
}

#' Compute Cohen's d
#' @description Compute Cohen's d from t-value with equal sized groups of size n
#' @param t t-value
#' @param n sample size per group

.compCohensD <- function(t, n){
  t*sqrt(2/n)
}
