# ==============================================================================
# Selective reporting of effects
# ==============================================================================

# We can use the simulation function from compositeScores.R

#' P-Hacking function for selective reporting of effects
#' @param df Data frame containing dependent variable and composite score items as columns
#' @param dv Location of dependent variable in the data frame
#' @param ivs Location of independent variables in the data frame
#' @param interactions Should the model take interactions into account?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats lm

.selectEffects <- function(df, dv, ivs, interactions = TRUE, strategy = "firstsig", alpha = 0.05){

  colnames(df)[dv] <- "dv"
  colnames(df)[ivs] <- paste0("V", 1:length(ivs))
  interactions <- ifelse(interactions, " * ", " + ")

  mdl <- paste(paste0("V", c(1:length(ivs))), collapse = interactions)
  mdl <- paste("dv ~ ", mdl)
  ps <- unname(summary(stats::lm(stats::as.formula(mdl), data = df))$coefficients[,4])[-1]

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)

  return(list(p.final = p.final,
              ps = ps))

}

#' Simulate p-hacking with selective reporting of effects
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs Integer giving number of observations
#' @param niv Number of independent variables
#' @param riv Correlation between the independent variables
#' @param interactions Should the model take interactions into account?
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param iter Number of simulation iterations
#' @param seed Initial seed for the random process
#' @export

sim.selectEffects <- function(nobs, niv, riv, interactions = FALSE, strategy = "firstsig", alpha = 0.05, iter = 1000, seed = 1234){

  # Simulate as many datasets as desired iterations
  dat <- list()
  set.seed(seed)
  for(i in 1:iter){
    dat[[i]] <- .sim.compscore(nobs = nobs, ncompv = niv, rcomp = riv)
  }

  # Apply p-hacking procedure to each dataset
  .selectEffectsList <- function(x){
    .selectEffects(df = x, dv = 1, ivs = c(2:(niv+1)),
                   interactions = interactions, strategy = strategy, alpha = alpha)
  }

  res <- lapply(dat, .selectEffectsList)

  ps.hack <- NULL
  ps.orig <- NULL
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
  }

  res <- cbind(ps.hack, ps.orig)

  return(res)

}


