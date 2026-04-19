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
    if(missing * nobs < 2){
      navalues <- as.data.frame(t(replicate(nvar, sample(1:nobs, missing*nobs))))
    } else {
      navalues <- as.data.frame(replicate(nvar, sample(1:nobs, missing*nobs)))
    }
    for(i in 1:nvar){
      X[unlist(navalues[,i]),i] <- NA
    }
  }

  return(X)

}

#' Draw one study-level effect size
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity

.draw.study.effect <- function(effect = 0, heterogeneity = 0){

  stopifnot(length(effect) == 1)
  stopifnot(length(heterogeneity) == 1)
  stopifnot(heterogeneity >= 0)

  if(heterogeneity == 0){
    return(effect)
  }

  stats::rnorm(1, mean = effect, sd = heterogeneity)

}

#' Generic sampling function
#' @description Outputs a data frame with two columns
#' @param nobs.group Number of observations per group. Either a scalar or a vector with two elements.
#' @param effect Mean effect size across studies
#' @param heterogeneity Between-study heterogeneity
#' @param theta Study-specific true effect size
#' @importFrom stats rnorm

.sim.data <- function(nobs.group, effect = 0, heterogeneity = 0, theta = NULL){

  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  if(is.null(theta)) theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity)
  V1 <- stats::rnorm(nobs.group[1], 0, 1)
  V2 <- stats::rnorm(nobs.group[2], theta, 1)
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

#' Select a p-hacked analysis from a vector of p-values
#' @description Takes a vector of p-values and returns the index of the selected analysis.
#' @param ps Vector of p values
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level (default: 0.05)

.selectanalysis <- function(ps, strategy, alpha){

  ps.clean <- ps
  ps.clean[!is.finite(ps.clean)] <- Inf

  selected <- 1

  if(strategy == "smallest.sig"){
    sig <- which(ps.clean < alpha)
    if(length(sig) > 0){
      selected <- sig[which.min(ps.clean[sig])]
    }

  } else if(strategy == "firstsig") {

    sig <- which(ps.clean < alpha)
    if(length(sig) > 0){
      selected <- sig[1]
    }

  } else if(strategy == "smallest") {
    selected <- which.min(ps.clean)
  }

  return(selected)

}

#' Select a p-value from a vector of p-hacked p-values
#' @description Takes a vector of p-values and selects the smallest, first significant, or smallest significant p-value.
#' @param ps Vector of p values
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level (default: 0.05)

.selectpvalue <- function(ps, strategy, alpha){
  ps[.selectanalysis(ps = ps, strategy = strategy, alpha = alpha)]

}

#' Convert a Fisher-z value to a correlation
#' @param effect Fisher-z effect size

.fisherz_to_r <- function(effect){
  pmax(pmin(tanh(effect), 0.999999), -0.999999)
}

#' Convert a correlation to Fisher-z
#' @param r Correlation

.r_to_fisherz <- function(r){
  atanh(pmax(pmin(r, 0.999999), -0.999999))
}

#' Compute the standard error of Fisher-z
#' @param n Sample size

.compFisherZSE <- function(n){

  if(n <= 3){
    return(NA_real_)
  }

  1/sqrt(n-3)

}

#' Simulate two continuous variables with a study-level association
#' @param nobs Number of observations
#' @param effect Mean Fisher-z effect size across studies
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param theta Study-specific true effect size
#' @param missing Proportion of missing values per variable

.sim.association <- function(nobs, effect = 0, heterogeneity = 0, theta = NULL, missing = 0){

  if(is.null(theta)) theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity)

  .sim.multcor(nobs = nobs, nvar = 2, r = .fisherz_to_r(theta), missing = missing)

}

#' Simulate one criterion and multiple predictors with shared study-level association
#' @param nobs Number of observations
#' @param nvar Number of predictor variables
#' @param r Correlation between predictor variables
#' @param effect Mean Fisher-z effect size across studies
#' @param heterogeneity Between-study heterogeneity on the Fisher-z scale
#' @param theta Study-specific true effect size
#' @importFrom stats rnorm

.sim.multregression <- function(nobs, nvar, r, effect = 0, heterogeneity = 0, theta = NULL){

  if(length(nobs) > 1) nobs <- nobs[1]
  if(is.null(theta)) theta <- .draw.study.effect(effect = effect, heterogeneity = heterogeneity)

  rho <- .fisherz_to_r(theta)
  R <- matrix(rep(r, (nvar+1)^2), nrow = nvar+1)
  diag(R) <- rep(1, nvar+1)
  R[1, -1] <- rep(rho, nvar)
  R[-1, 1] <- R[1, -1]

  cholR <- tryCatch(t(chol(R)), error = function(e) NULL)
  if(is.null(cholR)){
    R <- as.matrix(Matrix::nearPD(R, corr = TRUE)$mat)
    cholR <- t(chol(R))
  }

  random.normal <- matrix(stats::rnorm((nvar+1)*nobs), nrow = nvar+1, ncol = nobs)
  X <- as.data.frame(t(cholR %*% random.normal))
  colnames(X)[1] <- "criterion"

  return(X)

}

#' Compute R squared for the t-test
#' @param x values of group 1
#' @param y values of group 2

.compR2t <- function(x, y){
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
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

#' Compute Cohen's d from a test statistic and group sizes
#' @param statistic Test statistic
#' @param n1 Sample size in group 1
#' @param n2 Sample size in group 2

.compCohensDStat <- function(statistic, n1, n2){
  statistic*sqrt((1/n1) + (1/n2))
}

#' Compute Cohen's d from the observed data
#' @param control values of the control group
#' @param treatment values of the treatment group

.compCohensDData <- function(control, treatment){

  control <- control[!is.na(control)]
  treatment <- treatment[!is.na(treatment)]

  n1 <- length(control)
  n2 <- length(treatment)

  if(n1 < 2 || n2 < 2){
    return(NA_real_)
  }

  sp <- sqrt((((n1-1)*stats::var(control)) + ((n2-1)*stats::var(treatment)))/(n1+n2-2))

  if(!is.finite(sp) || sp == 0){
    return(NA_real_)
  }

  (mean(treatment)-mean(control))/sp

}

#' Compute the standard error of Cohen's d
#' @param d Cohen's d
#' @param n1 Sample size in group 1
#' @param n2 Sample size in group 2

.compCohensDSE <- function(d, n1, n2){

  if(!is.finite(d) || n1 < 2 || n2 < 2){
    return(NA_real_)
  }

  sqrt(((n1+n2)/(n1*n2)) + ((d^2)/(2*(n1+n2-2))))

}

#' Convert a two-sided p-value to a one-sided p-value
#' @param p.twosided Two-sided p-value
#' @param stat Test statistic
#' @param alternative Direction of the test

.onesided_from_twosided <- function(p.twosided, stat, alternative = "two.sided"){

  if(!is.finite(p.twosided) || !is.finite(stat)){
    return(NA_real_)
  }

  if(alternative == "two.sided"){
    return(p.twosided)
  }

  halfp <- p.twosided/2

  if(alternative == "greater"){
    if(stat >= 0){
      return(halfp)
    }

    return(1-halfp)
  }

  if(alternative == "less"){
    if(stat <= 0){
      return(halfp)
    }

    return(1-halfp)
  }

  stop("Unsupported alternative.")

}

#' Build one normalized reporting entry for a two-group analysis
#' @param control values of the control group
#' @param treatment values of the treatment group
#' @param method Analysis method
#' @param alternative Direction of the test. For group-comparison analyses, \code{"greater"} tests whether the treatment or second group exceeds the control or first group.
#' @param trim Trimming level for Yuen's test
#' @param p.override Optional p-value override

.report.twogroup <- function(control, treatment, method = "t.equal", alternative = "two.sided", trim = NULL, p.override = NULL){

  control <- control[!is.na(control)]
  treatment <- treatment[!is.na(treatment)]

  n1 <- length(control)
  n2 <- length(treatment)
  effect <- .compCohensDData(control = control, treatment = treatment)
  se <- .compCohensDSE(d = effect, n1 = n1, n2 = n2)
  stat <- NA_real_
  pval <- NA_real_
  method.label <- method

  if(method == "t.equal"){
    mod <- stats::t.test(treatment, control, var.equal = TRUE, alternative = alternative)
    stat <- unname(mod$statistic)
    pval <- mod$p.value
  } else if(method == "t.welch"){
    mod <- stats::t.test(treatment, control, var.equal = FALSE, alternative = alternative)
    stat <- unname(mod$statistic)
    pval <- mod$p.value
  } else if(method == "wilcox"){
    mod <- stats::wilcox.test(treatment, control, alternative = alternative)
    stat <- unname(mod$statistic)
    pval <- mod$p.value
  } else if(method == "yuen"){
    dftest <- data.frame(group = c(rep(1, n1), rep(2, n2)),
                         dv = c(control, treatment))
    mod <- WRS2::yuen(dv ~ group, tr = trim, data = dftest)
    stat <- unname(mod$test)
    pval <- .onesided_from_twosided(p.twosided = mod$p.value,
                                    stat = stat,
                                    alternative = alternative)
    method.label <- paste0("yuen.", trim)
  } else {
    stop("Unsupported method.")
  }

  if(!is.null(p.override)){
    pval <- p.override
  }

  return(list(effect = effect,
              se = se,
              n = n1+n2,
              stat = stat,
              p = pval,
              method = method.label))

}

#' Build one normalized reporting entry for a simple association analysis
#' @param x Predictor values
#' @param y Criterion values
#' @param method Analysis method
#' @param alternative Direction of the test. For association analyses, \code{"greater"} tests a positive association.
#' @param p.override Optional p-value override
#' @param stat.override Optional statistic override

.report.association <- function(x, y, method = "lm", alternative = "two.sided", p.override = NULL, stat.override = NULL){

  dat <- data.frame(x = x, y = y)
  dat <- dat[is.finite(dat$x) & is.finite(dat$y), , drop = FALSE]
  n <- nrow(dat)

  if(n < 4 || length(unique(dat$x)) < 2 || length(unique(dat$y)) < 2){
    return(list(effect = NA_real_,
                se = NA_real_,
                n = n,
                stat = NA_real_,
                p = NA_real_,
                method = method))
  }

  fit <- summary(stats::lm(y ~ x, data = dat))
  effect <- .r_to_fisherz(stats::cor(dat$x, dat$y))
  stat <- unname(fit$coefficients[2, 3])
  pval <- .onesided_from_twosided(p.twosided = fit$coefficients[2, 4],
                                  stat = stat,
                                  alternative = alternative)

  if(!is.null(stat.override)) stat <- stat.override
  if(!is.null(p.override)) pval <- p.override

  return(list(effect = effect,
              se = .compFisherZSE(n = n),
              n = n,
              stat = stat,
              p = pval,
              method = method))

}

#' Build one normalized reporting entry for a multi-category analysis
#' @param group Grouping variable
#' @param y Criterion values
#' @param method Analysis method

.report.multicat <- function(group, y, method = "anova"){

  dat <- data.frame(group = group, y = y)
  dat <- dat[!is.na(dat$group) & is.finite(dat$y), , drop = FALSE]
  dat$group <- factor(dat$group)
  n <- nrow(dat)

  if(n < 4 || length(unique(dat$group)) < 2){
    return(list(effect = NA_real_,
                se = NA_real_,
                n = n,
                stat = NA_real_,
                p = NA_real_,
                method = method))
  }

  fit <- summary(stats::lm(y ~ group, data = dat))
  if(length(fit$fstatistic) == 0){
    return(list(effect = NA_real_,
                se = NA_real_,
                n = n,
                stat = NA_real_,
                p = NA_real_,
                method = method))
  }

  fstat <- unname(fit$fstatistic["value"])
  pval <- stats::pf(fstat,
                    df1 = unname(fit$fstatistic["numdf"]),
                    df2 = unname(fit$fstatistic["dendf"]),
                    lower.tail = FALSE)
  rsign <- sign(stats::cor(as.numeric(dat$group), dat$y))
  if(!is.finite(rsign) || rsign == 0) rsign <- 1

  return(list(effect = .r_to_fisherz(rsign*sqrt(fit$r.squared)),
              se = .compFisherZSE(n = n),
              n = n,
              stat = fstat,
              p = pval,
              method = method))

}

#' Build one normalized reporting entry for a group effect in a linear model
#' @param formula Model formula
#' @param data Analysis data
#' @param groupvar Group variable name
#' @param method Analysis method

.report.group_lm <- function(formula, data, groupvar = "group", method = "ancova"){

  fit <- stats::lm(formula, data = data)
  summary.fit <- summary(fit)
  model.df <- stats::model.frame(fit)
  group <- model.df[[groupvar]]
  group <- group[!is.na(group)]
  group.levels <- sort(unique(group))
  n1 <- sum(group == group.levels[1])
  n2 <- sum(group == group.levels[2])
  coef.row <- rownames(summary.fit$coefficients)
  row.id <- grep(paste0("^", groupvar), coef.row)[1]

  if(length(group.levels) < 2 || is.na(row.id)){
    return(list(effect = NA_real_,
                se = NA_real_,
                n = nrow(model.df),
                stat = NA_real_,
                p = NA_real_,
                method = method))
  }

  stat <- unname(summary.fit$coefficients[row.id, 3])
  effect <- .compCohensDStat(statistic = stat, n1 = n1, n2 = n2)

  return(list(effect = effect,
              se = .compCohensDSE(d = effect, n1 = n1, n2 = n2),
              n = nrow(model.df),
              stat = stat,
              p = summary.fit$coefficients[row.id, 4],
              method = method))

}

#' Combine legacy simulation output with the normalized reporting block
#' @param res List of iteration results
#' @param legacy.fields Character vector defining the legacy output columns

.combine.phase1.results <- function(res, legacy.fields){

  output <- list()

  for(i in 1:length(legacy.fields)){
    output[[legacy.fields[i]]] <- vapply(res, function(x) x[[legacy.fields[i]]], numeric(1))
  }

  output[["effect.initial"]] <- vapply(res, function(x) x[["report.initial"]][["effect"]], numeric(1))
  output[["effect.final"]] <- vapply(res, function(x) x[["report.final"]][["effect"]], numeric(1))
  output[["se.initial"]] <- vapply(res, function(x) x[["report.initial"]][["se"]], numeric(1))
  output[["se.final"]] <- vapply(res, function(x) x[["report.final"]][["se"]], numeric(1))
  output[["n.initial"]] <- vapply(res, function(x) x[["report.initial"]][["n"]], numeric(1))
  output[["n.final"]] <- vapply(res, function(x) x[["report.final"]][["n"]], numeric(1))
  output[["stat.initial"]] <- vapply(res, function(x) x[["report.initial"]][["stat"]], numeric(1))
  output[["stat.final"]] <- vapply(res, function(x) x[["report.final"]][["stat"]], numeric(1))
  output[["p.initial"]] <- vapply(res, function(x) x[["report.initial"]][["p"]], numeric(1))
  output[["p.final"]] <- vapply(res, function(x) x[["report.final"]][["p"]], numeric(1))
  output[["method.initial"]] <- vapply(res, function(x) x[["report.initial"]][["method"]], character(1))
  output[["method.final"]] <- vapply(res, function(x) x[["report.final"]][["method"]], character(1))

  return(as.data.frame(output, stringsAsFactors = FALSE))

}
