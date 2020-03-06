# ==============================================================================
# Outlier exclusion
# ==============================================================================
# NOTE: This script differs from the other insofar as that outlier definition
# functions are first defined, then combined in a p-hacking function.

# ------------------------------------------------------------------------------
# Outlier definition functions
# ------------------------------------------------------------------------------

#' Box plot outlier definition
#' @description Box plot outlier definition function
#' @param x Vector of values from which outliers should be excluded
#' @param y Vector of values from which outliers should be excluded
#' @importFrom graphics boxplot

.out.boxplot <- function(x, y){

  # Define outliers with boxplot function
  outsx <- graphics::boxplot(x, plot = FALSE)$out
  outsy <- graphics::boxplot(y, plot = FALSE)$out

  # Create updated vector
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)

}

#' Stem and Leaf plot outlier definition
#' @description Stem and Leaf plot outlier definition function
#' @param x Vector of values from which outliers should be excluded
#' @param y Vector of values from which outliers should be excluded
#' @importFrom graphics boxplot
#' @importFrom aplpack stem.leaf
#' @importFrom utils capture.output

.out.stemleaf <- function(x, y){

  # Define outliers with the stem and leaf plot
  stemleafx <- utils::capture.output(aplpack::stem.leaf(x))
  stemleafy <- utils::capture.output(aplpack::stem.leaf(y))

  outsx <- unlist(strsplit(stemleafx[c(grep("LO: ", stemleafx), grep("HI: ", stemleafx))], " "))
  outsx <- as.numeric(outsx[-which(outsx %in% c("LO:", "HI:"))])

  outsy <- unlist(strsplit(stemleafy[c(grep("LO: ", stemleafy), grep("HI: ", stemleafy))], " "))
  outsy <- as.numeric(outsy[-which(outsy %in% c("LO:", "HI:"))])

  # Create updated vector
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)

}

#' Standard deviation outlier definition
#' @description Standard deviation outlier definition function: Takes a vector x, tries different standard deviation outlier rules (x > 2, 2.5, 3, ...) and returns all differing result vectors without the outliers.
#' @param x Vector of values from which outliers should be excluded
#' @param y Vector of values from which outliers should be excluded
#' @importFrom stats sd

.out.sdrule <- function(x, y){

  # Convert x to z values and determine sd steps
  zvalx <- (x-mean(x))/stats::sd(x)
  zvaly <- (y-mean(y))/stats::sd(y)
  zval <- c(zvalx, zvaly)

  if(max(abs(zval)) > 2){
    sdout <- seq(2, max(abs(zval)), by = 0.5)
  } else {
    sdout <- 2
  }

  # Initiate results list
  res <- list()

  for(i in 1:length(sdout)){

    # Define outliers
    boundsx <- c(mean(x) - sdout[i] * stats::sd(x), mean(x) + sdout[i] * stats::sd(x))
    outsx <- x[which(x < boundsx[1] | x > boundsx[2])]

    boundsy <- c(mean(y) - sdout[i] * stats::sd(y), mean(y) + sdout[i] * stats::sd(y))
    outsy <- y[which(y < boundsx[1] | y > boundsy[2])]

    # Create updated vector and add it to the results list
    res[[i]] <- .extractoutlier(x, y, outsx, outsy)

  }

  # Simplify results list to 1-dimensional list and keep only results that differ
  res <- unlist(res, recursive = FALSE)
  res <- unique(res)

  return(res)

}

#' Percentage outlier definition
#' @description Percentage outlier definition function
#' @param x Vector of values from which outliers should be excluded
#' @param y Vector of values from which outliers should be excluded

.out.percentrule <- function(x, y){

  # Convert values to quantiles and determine percentage steps
  quant <- c(1:length(x))/length(x)

  if(min(quant) > 0.05){
    percout <- 0.05
  } else {
    percout <- seq(1/length(x), 0.05, by = 0.005)
  }

  # Initiate results list
  res <- list()

  for(i in 1:length(percout)){

    # Define outliers
    outsx <- sort(x)[which(quant <= percout[i] | quant > (1-percout[i]))]
    outsy <- sort(y)[which(quant <= percout[i] | quant > (1-percout[i]))]

    # create updated vector and add it to the results list
    res[[i]] <- .extractoutlier(x, y, outsx, outsy)

  }

  # Simplify results list to 1-dimensional list and keep only results that differ
  res <- unlist(res, recursive = FALSE)
  res <- unique(res)

  return(res)

}

#' Residuals outlier definition
#' @description Excludes values with high standardized / studentized residuals. If the largest residual > 2, values with residuals larger than 2, 2.5, 3, ... are excluded. If the largest residual < 2, values with 1:3 largest residuals are excluded (largest 3 standardized residuals is equivalent to the q-q plot definition of outliers in the regression diagnostics in the lm package)
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @param type What type of residuals \code{"stan"}, \code{"stud"}
#' @importFrom stats lm rstandard rstudent

.out.residual <- function(x, y, type){

  stopifnot(length(x) == length(y))

  #Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute standardized residuals
  if(type == "stan"){
    residuals <- unname(stats::rstandard(mod))
  } else if(type == "stud"){
    residuals <- unname(stats::rstudent(mod))
  }

  if(max(abs(residuals)) < 2){
    resout <- sort(abs(residuals), decreasing = TRUE)[1:3]
  } else {
    resout <- seq(2, max(abs(residuals)), by = 0.5)
  }

  # Initialize results list
  res <- list()

  for(i in 1:length(resout)){

    # Define outliers
    outsx <- x[abs(residuals) >= resout[i]]
    outsy <- y[abs(residuals) >= resout[i]]

    # create updated vector and add it to the results list
    res[[i]] <- .extractoutlier(x, y, outsx, outsy)

  }

  # Simplify results list to 1-dimensional list and keep only results that differ
  res <- unlist(res, recursive = FALSE)
  res <- unique(res)

  return(res)

}

#' DFBETAS outlier definition
#' @description Excludes the 1-3 values that have the highest influence on the regression slope
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm dfbeta

.out.dfbeta <- function(x, y){

  stopifnot(length(x) == length(y))

  # Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute absolute deviations
  devs <- abs(unname(stats::dfbeta(mod)[,2]))

  # Initialize results list
  res <- list()

  for(i in 1:3){

    # Define outliers
    highdevs <- sort(devs, decreasing = TRUE)[1:i]
    outsx <- x[devs %in% highdevs]
    outsy <- y[devs %in% highdevs]

    # create updated vector and add it to the results list
    res[[i]] <- .extractoutlier(x, y, outsx, outsy)
  }

  # Simplify results list to 1-dimensional list and keep only results that differ
  res <- unlist(res, recursive = FALSE)
  res <- unique(res)

  return(res)

}

#' DFFITS outlier definition
#' @description Excludes values that have absolute DFFIT values larger than 2*sqrt(2/n) (see Wikipedia page for DFFITS for the cutoff)
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm dffits

.out.dffits <- function(x, y){

  stopifnot(length(x) == length(y))

  # Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute absolute dffit values
  dffitval <- abs(unname(stats::dffits(mod)))

  # Define outliers
  outsx <- x[dffitval >= 2*sqrt(2/length(x))]
  outsy <- y[dffitval >= 2*sqrt(2/length(y))]

  # Create results list
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)

}

#' Cook's Distance outlier definition
#' @description Excludes values that have a Cook's distance larger than the median of an F distribution with p and n-p degrees of freedom or larger than 1 (see Wikipedia for Cook's distance for the cutoff)
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm cooks.distance qf

.out.cook <- function(x, y){

  stopifnot(length(x) == length(y))

  # Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute Cook's distance values and cutoff values
  cookval <- unname(stats::cooks.distance(mod))
  cutoff <- c(stats::qf(0.5, df1 = 2, df2 = length(x)-2), 1) # Cutoff either median of F Dist or 1

  # Initialize results list
  res <- list()

  for(i in 1:2){
    # Define outliers
    outsx <- x[cookval >= cutoff[i]]
    outsy <- y[cookval >= cutoff[i]]

    # Create results list
    res[[i]] <- .extractoutlier(x, y, outsx, outsy)
  }

  # Simplify results list to 1-dimensional list and keep only results that differ
  res <- unlist(res, recursive = FALSE)
  res <- unique(res)

  return(res)
}

#' Covariance ratio outlier definition
#' @description Excludes values that have a covariance ratio differing from 1 (cutoff: influence.measues function internal)
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm cooks.distance influence.measures

.out.covratio <- function(x, y){

  stopifnot(length(x) == length(y))

  # Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute covariance ratios
  covr <- stats::influence.measures(mod)

  # Define outliers
  outsx <- x[unname(covr$is.inf[,"cov.r"])]
  outsy <- y[unname(covr$is.inf[,"cov.r"])]

  # Create results list
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)
}

#' Robust Mahalanobis Distance outlier definition
#' @description Excludes values that have a high robust Mahalanobis Distance (cutoff: squared MD > qchisq(0.98, 2), see Filzmoser et al. (2005))
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm
#' @importFrom R.devices suppressGraphics
#' @importFrom mvoutlier uni.plot

.out.mahalanobis <- function(x, y){

  # Create matrix from x and y
  dat <- matrix(c(x, y), ncol = 2, byrow = F)

  # Compute Mahalanobis distance
  mahal <- R.devices::suppressGraphics(mvoutlier::uni.plot(dat))

  # Define outliers
  outsx <- x[unname(mahal$outliers)]
  outsy <- y[unname(mahal$outliers)]

  # Create results list
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)
}

#' Leverage values outlier definition
#' @description Excludes values that have high leverage values (3 times larger than the mean leverage value 3*(p/n), see 'https://newonlinecourses.science.psu.edu/stat501/node/338/' for the cutoff)
#' @param x Vector of x values (predictor in linear regression)
#' @param y Vector of y values (criterion in linear regression)
#' @importFrom stats lm hatvalues

.out.leverage <- function(x, y){

  stopifnot(length(x) == length(y))

  # Compute regression from x to y
  mod <- stats::lm(y ~ x)

  # Compute leverage values and cutoff
  levs <- unname(stats::hatvalues(mod))
  cutoff <- 3*(2/length(x))

  # Define outliers
  outsx <- x[levs >= cutoff]
  outsy <- y[levs >= cutoff]

  # Create results list
  res <- .extractoutlier(x, y, outsx, outsy)

  return(res)

}

# ------------------------------------------------------------------------------
# P-Hacking functions
# ------------------------------------------------------------------------------

# Simulation function: data can be simulated with .sim.multcor where r = 0

#' P-Hacking function for outlier exclusion in univariate linear regression
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df Data frame containing x and y variables as columns
#' @param x Location of x variable (predictor) in the data frame
#' @param y Location of y variable (criterion) in the data frame
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param which Which outlier definition methods? A numeric vector containing the chosen methods (1: boxplot, 2: stem&leaf, 3: standard deviation, 4: percentile, 5: studentized residuals, 6: standardized residuals, 7: DFBETA, 8: DFFITS, 9: Cook's D, 10: Mahalanobis distance, 11: Leverage values, 12: Covariance ratio)

.outHack <- function(df, x, y, which = c(1:12), strategy = "firstsig", alpha = 0.05){

  # Stop if outlier exclusion methods are not defined
  stopifnot(any(c(1:12) %in% which))

  # define x and y for regression in dataframe
  x <- df[,x]
  y <- df[,y]

  # initialize p value list (one level for each outlier method)
  ps <- vector("list", 12)
  r2s <- vector("list", 12)

  #### Go through each outlier detection method and calculate p values ####

  # Boxplot
  if(1 %in% which){

    dat <- .out.boxplot(x, y)
    ps[[1]] <- rep(NA, length(dat))
    r2s[[1]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[1]][i] <- mod$coefficients[2,4]
      r2s[[1]][i] <- mod$r.squared
    }
  }

  # Stem & Leaf
  if(2 %in% which){

    dat <- .out.stemleaf(x, y)
    ps[[2]] <- rep(NA, length(dat))
    r2s[[2]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[2]][i] <- mod$coefficients[2,4]
      r2s[[2]][i] <- mod$r.squared
    }
  }

  # Standard deviation
  if(3 %in% which){

    dat <- .out.sdrule(x, y)
    ps[[3]] <- rep(NA, length(dat))
    r2s[[3]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[3]][i] <- mod$coefficients[2,4]
      r2s[[3]][i] <- mod$r.squared
    }
  }

  # Percentile
  if(4 %in% which){

    dat <- .out.percentrule(x, y)
    ps[[4]] <- rep(NA, length(dat))
    r2s[[4]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[4]][i] <- mod$coefficients[2,4]
      r2s[[4]][i] <- mod$r.squared
    }
  }

  # Studentized residuals
  if(5 %in% which){

    dat <- .out.residual(x, y, type = "stud")
    ps[[5]] <- rep(NA, length(dat))
    r2s[[5]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[5]][i] <- mod$coefficients[2,4]
      r2s[[5]][i] <- mod$r.squared
    }
  }

  # Standardized residuals
  if(6 %in% which){

    dat <- .out.residual(x, y, type = "stan")
    ps[[6]] <- rep(NA, length(dat))
    r2s[[6]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[6]][i] <- mod$coefficients[2,4]
      r2s[[6]][i] <- mod$r.squared
    }
  }

  # DFBETA
  if(7 %in% which){

    dat <- .out.dfbeta(x, y)
    ps[[7]] <- rep(NA, length(dat))
    r2s[[7]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[7]][i] <- mod$coefficients[2,4]
      r2s[[7]][i] <- mod$r.squared
    }
  }

  # DFFITS
  if(8 %in% which){

    dat <- .out.dffits(x, y)
    ps[[8]] <- rep(NA, length(dat))
    r2s[[8]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[8]][i] <- mod$coefficients[2,4]
      r2s[[8]][i] <- mod$r.squared
    }
  }

  # Cook's distance
  if(9 %in% which){

    dat <- .out.cook(x, y)
    ps[[9]] <- rep(NA, length(dat))
    r2s[[9]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[9]][i] <- mod$coefficients[2,4]
      r2s[[9]][i] <- mod$r.squared
    }
  }

  # Mahalanobis distance
  if(10 %in% which){

    dat <- .out.mahalanobis(x, y)
    ps[[10]] <- rep(NA, length(dat))
    r2s[[10]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[10]][i] <- mod$coefficients[2,4]
      r2s[[10]][i] <- mod$r.squared
    }
  }

  # Leverage levels
  if(11 %in% which){

    dat <- .out.leverage(x, y)
    ps[[11]] <- rep(NA, length(dat))
    r2s[[11]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[11]][i] <- mod$coefficients[2,4]
      r2s[[11]][i] <- mod$r.squared
    }
  }

  # Covariance ratio
  if(12 %in% which){

    dat <- .out.covratio(x, y)
    ps[[12]] <- rep(NA, length(dat))
    r2s[[12]] <- rep(NA, length(dat))

    for(i in 1:length(dat)){
      mod <- summary(stats::lm(dat[[i]][,2] ~ dat[[i]][,1]))
      ps[[12]][i] <- mod$coefficients[2,4]
      r2s[[12]][i] <- mod$r.squared
    }
  }

  # Compute original p value
  mod <- summary(stats::lm(y ~ x))
  p.orig <- mod$coefficients[2,4]
  r2.orig <- mod$r.squared

  # Combine all p values and remove NAs
  psc <- unlist(ps)
  psc <- psc[!is.na(psc)]
  r2c <- unlist(r2s)
  r2c <- r2c[!is.na(psc)]

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = psc, strategy = strategy, alpha = alpha, p.orig = p.orig)
  r2.final <- unique(r2c[psc == p.final])

  ps <- c(p.orig, psc)
  r2s <- c(r2.orig, r2c)

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s))

}

#' Simulate p-Hacking with different sorts of outlier definition
#' @description Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param which Which outlier detection methods?  Either 5 random methods are chosen ("random") or a numeric vector containing the chosen methods (1: boxplot, 2: stem&leaf, 3: standard deviation, 4: percentile, 5: studentized residuals, 6: standardized residuals, 7: DFBETA, 8: DFFITS, 9: Cook's D, 10: Mahalanobis distance, 11: Leverage values, 12: Covariance ratio)
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @export

sim.outHack <- function(nobs, which = c(1:12), strategy = "firstsig", alpha = 0.05, iter = 1000){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0)
  }

  # If which = "random
  if(any(which == "random")) which <- sample(c(1:12), 5)

  # Apply p-hacking procedure to each dataset
  res <- lapply(dat, x = 1, y = 2, .outHack, which = which,
                strategy = strategy, alpha = alpha)
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
    #ps.all[[i]] <- res[[i]][["ps"]]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)
}
