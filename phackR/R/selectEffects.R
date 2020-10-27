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
  mres <- summary(stats::lm(stats::as.formula(mdl), data = df))
  ps <- mres$coefficients[,4][-1]
  r2s <- mres$r.squared

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- r2s # final r squared is the same for all ps because model doesn't change

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s))

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
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.selectEffects <- function(nobs, niv, riv, interactions = FALSE, strategy = "firstsig", alpha = 0.05, iter = 1000, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.compscore(nobs = nobs, ncompv = niv, rcomp = riv)
  }

  # Apply p-hacking procedure to each dataset
  .selectEffectsList <- function(x){
    .selectEffects(df = x, dv = 1, ivs = c(2:(niv+1)),
                   interactions = interactions, strategy = strategy, alpha = alpha)
  }
  
  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .selectEffectsList)
  }
  
  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2)))
        .selectEffects(df=x, dv = 1, ivs = c(2:(niv+1)), interactions = interactions, strategy = strategy, alpha = alpha)
      })
    })
  }
  
  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig)

  return(res)

}


