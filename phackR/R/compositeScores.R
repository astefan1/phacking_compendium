# ==============================================================================
# Scale Redefinition / Composite Scores
# ==============================================================================

#' Simulate data: Correlated composite score raw variables and one non-correlated dependent variable
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables

.sim.compscore <- function(nobs, ncompv, rcomp){

  dv <- rnorm(nobs, 0, 1)

  iv <- .sim.multcor(nobs = nobs, nvar = ncompv, r = rcomp)

  res <- cbind(dv, iv)

  return(res)

}

#' P-Hacking function for scale redefinition / Composite Scores
#' @param df Data frame containing dependent variable and composite score items as columns
#' @param dv Location of dependent variable in the data frame
#' @param compv Location of composite score variables in the data frame
#' @param ndelete How many items should be deleted from the scale at maximum?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats lm
#' @importFrom performance item_reliability

.compscoreHack <- function(df, dv, compv, ndelete, strategy = "firstsig", alpha = 0.05){

  stopifnot(length(compv)-ndelete >= 2)

  # Compute original p-value and R^2 with full scale
  modres <- summary(lm(df[, dv] ~ rowMeans(df[, compv])))
  p.orig <- modres$coefficients[2, 4]
  r2.orig <- modres$r.squared

  # Prepare and initialize variables for p-hacking
  ps <- list()
  r2s <- list()
  compscale <- df[, compv]
  changescale <- df[, compv]
  out <- NULL

  # Strategically delete items from the composite scale and re-calculate the p-value
  for(i in 1:ndelete){

    pval <- rep(NA, 3)
    r2val <- rep(NA, 3)

    # Define new item to delete from the scale
    out[i] <- which(colnames(compscale) %in% colnames(changescale)[which.max(performance::item_reliability(changescale)[,2])])

    # Compute p-value for the new composite score
    newscore <- rowMeans(compscale[, -out])
    newmodres <- summary(lm(df[, dv] ~ newscore))
    pval[1] <- newmodres$coefficients[2, 4]
    r2val[1] <- newmodres$r.squared

    # Compute p-value for the item deleted from the score
    itemscore <- compscale[, out[i]]
    newmodres2 <- summary(lm(df[, dv] ~ itemscore))
    pval[2] <- newmodres2$coefficients[2, 4]
    r2val[2] <- newmodres2$r.squared

    # Compute p-value for a scale of all items deleted so far
    nonscore <- rowMeans(cbind(compscale[, out]))
    newmodres3 <- summary(lm(df[, dv] ~ nonscore))
    pval[3] <- newmodres3$coefficients[2, 4]
    r2val[3] <- newmodres3$r.squared

    changescale <- compscale[, -out]
    ps[[i]] <- pval
    r2s[[i]] <- r2val
  }

  ps <- c(p.orig, unique(unlist(ps)))
  r2s <- c(r2.orig, unique(unlist(r2s)))

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- r2s[ps == p.final]

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s))

}

#' Simulate p-hacking with composite scores
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param ncompv Integer giving number of variables to build the composite score
#' @param rcomp Correlation between the composite score variables
#' @param ndelete How many items should be deleted from the scale at maximum?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @importFrom pbapply pblapply
#' @importFrom shiny withProgress incProgress
#' @export

sim.compscoreHack <- function(nobs, ncompv, rcomp, ndelete, strategy = "firstsig", alpha = 0.05, iter = 1000, shinyEnv=FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.compscore(nobs = nobs, ncompv = ncompv, rcomp = rcomp)
  }

  # Apply p-hacking procedure to each dataset (with progress bar within or outside Shiny)
  if(!shinyEnv){
    .compscoreHackList <- function(x){
      .compscoreHack(df = x, dv = 1, compv = c(2:(ncompv+1)), ndelete = ndelete,
                     strategy = strategy, alpha = alpha)
    }

    res <- pbapply::pblapply(dat, .compscoreHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value=0, {
      res=lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2)))
        .compscoreHack(df = x, dv = 1, compv = c(2:(ncompv+1)), ndelete = ndelete,
                       strategy = strategy, alpha = alpha)
      })
    })
  }

  ps.hack <- NULL
  ps.orig <- NULL
  r2s.orig <- NULL
  r2s.hack <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)

}
