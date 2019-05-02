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
