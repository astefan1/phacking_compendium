# ==============================================================================
# Optional Stopping Based on Significance
# ==============================================================================

# Generic sampling function .sim.data() can be used

#' Optional Stopping based on existing dataset
#' @description Outputs a p-hacked p-value and a non-p-hacked p-value based on the maximum sample size
#' @param df Data frame
#' @param group group Scalar defining grouping column
#' @param dv Scalar defining location of dependent variable in the data frame
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param peek Determines how often one peeks at the data. Overrides step argument if not NULL.
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param alpha Significance level of the t-test (default: 0.05)
#' @importFrom stats t.test
#' @importFrom utils tail

.optstop <- function(df, group, dv, n.min, n.max, step = 1, peek = NULL, alternative = "two.sided", alpha = 0.05){

  # Extract group variables
  g1 <- df[df[,group] == unique(df[,group])[1], dv]
  g2 <- df[df[,group] == unique(df[,group])[2], dv]

  # Sanity check: Enough data?
  stopifnot(length(g1) >= n.max && length(g2) >= n.max)

  # Determine places of peeks
  if(is.null(peek)){
    peeks <- seq(n.min, n.max, by=step)
    if(step > (n.max-n.min)) peeks <- c(n.min, n.max)
  } else {
    peeks <- round(seq(n.min, n.max, length.out = peek))
  }

  # Compute t-tests
  mod <- sapply(peeks, FUN = function(x) {stats::t.test(g1[1:x], g2[1:x], var.equal = TRUE, alternative = alternative)})
  ps <- simplify2array(mod["p.value",])
  r2s <- sapply(peeks, FUN = function(x) {.compR2t(g1[1:x], g2[1:x])})
  ds <- .compCohensD(simplify2array(mod["statistic",]), peeks)

  # Do the p-hacking
  if(any(ps < alpha) == FALSE){
    p.final <- utils::tail(ps, 1)
    r2.final <- utils::tail(r2s, 1)
    d.final <- utils::tail(ds, 1)
  } else if (any(ps < alpha) == TRUE) {
    p.final <- ps[which(ps < alpha)][1]
    r2.final <- unique(r2s[ps == p.final])
    d.final <- unique(ds[ps == p.final])
  }

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s,
              d.final = d.final,
              ds = ds))
}

#' Simulate p-hacking with incorrect rounding
#' @param n.min Minimum sample size
#' @param n.max Maximum sample size
#' @param step Step size of the optional stopping (default is 1)
#' @param peek Determines how often one peeks at the data. Overrides step argument if not NULL.
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param iter Number of iterations
#' @param alpha Significance level of the t-test (default: 0.05)
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @importFrom utils tail
#' @export
#'

sim.optstop <- function(n.min, n.max, step = 1, peek = NULL, alternative = "two.sided", iter = 1000, alpha = 0.05, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.data(nobs.group = n.max)
  }

  # Apply p-hacking procedure to each dataset
  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .optstop, group = 1, dv = 2,
                  n.min = n.min, n.max = n.max, step = step, peek = peek,
                  alternative = alternative, alpha = alpha)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .optstop(df=x, group = 1, dv = 2,
                 n.min = n.min, n.max = n.max, step = step,
                 alternative = alternative, alpha = alpha)
      })
    })
  }

  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL
  ds.hack <- NULL
  ds.orig <- NULL

  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- utils::tail(res[[i]][["ps"]], 1)
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- utils::tail(res[[i]][["r2s"]], 1)
    ds.hack[i] <- res[[i]][["d.final"]]
    ds.orig[i] <- utils::tail(res[[i]][["ds"]], 1)
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig, ds.hack, ds.orig)

  return(res)

}
