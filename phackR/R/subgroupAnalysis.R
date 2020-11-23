# ==============================================================================
# Subgroup Analyses
# ==============================================================================

#' Simulate data with subgroups
#' @description Outputs data frame with multiple binary variables from which subgroups can be extracted
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups

.sim.subgroup <- function(nobs.group, nsubvars){

  dat <- .sim.data(nobs.group)

  # Observations per group and total observations
  if(length(nobs.group) == 1) nobs.group <- rep(nobs.group, 2)
  nobs <- sum(nobs.group)

  subvars <- matrix(NA, nrow = nobs, ncol = nsubvars)
  for(i in 1:nsubvars){
    subvars[,i] <- sample(c(0, 1), size = nobs, replace = TRUE)
  }

  res <- cbind(dat, subvars)

  return(res)

}

#' P-Hacking function for multiple subgroups analysis
#' @description Outputs a p-hacked p-value and a vector of all p-values that were computed in the process
#' @param df A matrix or data frame containing all relevant data
#' @param iv Integer specifying the location of the binary independent variable in the data frame
#' @param dv Integer specifying the location of the dependent variable in the data frame
#' @param subvars Vector specifying the location of the subgroup variables in the data frame
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @importFrom dplyr group_by_at do
#' @importFrom stats t.test
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data

.subgroupHack <- function(df, iv, dv, subvars, alternative = "two.sided", strategy = "firstsig", alpha = 0.05){

  # Prepare data frame
  ttest.df <- cbind(df[,iv], df[,dv])
  subvars.df <- cbind(df[, subvars])
  dfnew <- as.data.frame(cbind(ttest.df, subvars.df))

  # Compute p-values, R^2, Cohen's d

  # Not p-hacked
  mod.orig <- stats::t.test(ttest.df[,2] ~ ttest.df[,1], var.equal = TRUE, alternative = alternative)
  p.orig <- mod.orig$p.value
  r2.orig <- .compR2t(ttest.df[ttest.df[,1] == unique(ttest.df[,1])[1],2],
                      ttest.df[ttest.df[,1] == unique(ttest.df[,1])[2],2])
  d.orig <- .compCohensD(unname(mod.orig$statistic), nrow(ttest.df)/2)


  # p-hacked
  ps <- list()
  ds <- list()
  r2s <- list()

  for(i in 1:length(subvars)){

    tmp <- dplyr::group_by_at(dfnew, subvars[i]) %>%
      dplyr::do(as.data.frame(stats::t.test(.data$V2 ~ .data$V1, var.equal = TRUE, alternative = alternative)[c("p.value", "statistic")]))
    tmp2 <- dplyr::group_by_at(dfnew, subvars[i]) %>%
      dplyr::do(as.data.frame(table(.data$V1)))
    tmp3 <- dplyr::group_by_at(dfnew, subvars[i]) %>% do(as.data.frame(.compR2t(.data$V2[.data$V1 == unique(.data$V1)[1]], .data$V2[.data$V1 == unique(.data$V1)[2]])))

    ps[[i]] <- tmp[[2]]
    ds[[i]] <- c(tmp[[3]][1]*sqrt(sum(1/tmp2[[3]][1:2])), tmp[[3]][2]*sqrt(sum(1/tmp2[[3]][3:4])))
    r2s[[i]] <- tmp3[[2]]

  }

  ps <- c(p.orig, unlist(ps))
  r2s <- c(r2.orig, unlist(r2s))
  ds <- c(d.orig, unlist(ds))

  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = alpha)
  r2.final <- unique(r2s[ps == p.final])
  d.final <- unique(ds[ps == p.final])

  return(list(p.final = p.final,
              ps = ps,
              r2.final = r2.final,
              r2s = r2s,
              d.final = d.final,
              ds = ds))

}

#' Simulate p-hacking with multiple subgroups
#' Outputs a matrix containing the p-hacked p-values (\code{ps.hack}) and the original p-values (\code{ps.orig}) from all iterations
#' @param nobs.group Vector giving number of observations per group
#' @param nsubvars Integer specifying number of variables for potential subgroups
#' @param alternative Direction of the t-test ("two.sided", "less", "greater")
#' @param strategy String value: One out of "firstsig", "smallest", "smallest.sig"
#' @param alpha Significance level of the t-test
#' @param iter Number of simulation iterations
#' @param shinyEnv Is the function run in a Shiny session? TRUE/FALSE
#' @export

sim.subgroupHack <- function(nobs.group, nsubvars, alternative = "two.sided", strategy = "firstsig", alpha = 0.05, iter = 1000, shinyEnv = FALSE){

  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.subgroup(nobs.group = nobs.group, nsubvars = nsubvars)
  }

  # Apply p-hacking procedure to each dataset
  .subgroupHackList <- function(x){
    .subgroupHack(df = x, iv = 1, dv = 2, subvars = c(3:(2+nsubvars)),
                  alternative = alternative, strategy = strategy, alpha = alpha)
  }

  if(!shinyEnv){
    res <- pbapply::pblapply(dat, .subgroupHackList)
  }

  if(shinyEnv){
    percentage <- 0
    withProgress(message = "Running simulation", value = 0, {
      res = lapply(dat, function(x){
        percentage <<- percentage + 1/length(dat)*100
        incProgress(1/length(dat), detail = paste0("Progress: ",round(percentage,2), "%"))
        .subgroupHack(df = x, iv = 1, dv = 2, subvars = c(3:(2+nsubvars)),
                      alternative = alternative, strategy = strategy, alpha = alpha)
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
    ps.orig[i] <- res[[i]][["ps"]][1]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2s"]][1]
    ds.hack[i] <- res[[i]][["d.final"]]
    ds.orig[i] <- res[[i]][["ds"]][1]
  }

  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig, ds.hack, ds.orig)

  return(res)

}

