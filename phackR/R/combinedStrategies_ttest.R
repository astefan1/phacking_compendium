# ==============================================================================
# Impact of combined p-hacking strategies: t-test Example 
# ==============================================================================

.sim.combined.t <- function(nobs.group = 100, nDV = 5, rDV = 0.6, nCOV = 3, rCOV = 0.3, rcovdv = 0.3, nSUB = 3){
  
  # Correlation matrix between continuous variables
  
  # Dependent variables
  RDV       <- matrix(rep(rDV, nDV), ncol=nDV, nrow=nDV)
  diag(RDV) <- rep(1, nDV)
  
  # Covariates
  RCOVDV      <- matrix(rep(rcovdv, nCOV), ncol=nCOV, nrow=nDV)
  RCOV        <- matrix(rep(rCOV, nCOV), ncol=nCOV, nrow=nCOV)
  diag(RCOV)  <- rep(1, nCOV)
  
  # Combine them
  R           <- rbind(cbind(RDV, RCOVDV), cbind(t(RCOVDV), RCOV))
  colnames(R) <- c(paste0("DV", 1:nDV), paste0("COV", 1:nCOV))
  rownames(R) <- colnames(R)
  
  # If it's not positive definite, replace with nearest positive definite matrix
  R <- Matrix::nearPD(R, corr = TRUE)$mat
  
  # Transposed Cholesky decomposition of correlation matrix
  U <- t(chol(R))
  
  # Create random noise matrix
  random.normal <- matrix(stats::rnorm(ncol(R)^2, 0, 1), 
                          nrow=ncol(R), 
                          ncol=nobs.group*2)
  
  # Create raw data from matrix multiplication of U and random noise
  X <- as.data.frame(t(U %*% random.normal))
  
  # Binary variables
  group <- sample(c(1,2), nobs.group*2, replace = TRUE)
  subgroups <- replicate(nSUB, sample(c(1,0), nobs.group*2, replace = TRUE))
  colnames(subgroups) <- paste0("SUBG", 1:nSUB)
  
  # Add binary variables to data frame
  DAT.FULL <- cbind(X, group, subgroups)
  
  return(DAT.FULL)
}

.combined.t.hack <- function(df, roundinglevel = 0.051, alternative = "two.sided", strategy = "firstsig", alpha = 0.05){
  
  ####################### (1) Original p-value ###################

  control.orig <- df[df$group == 1, "DV1"]
  treatment.orig <- df[df$group == 2, "DV1"]
  
  # Original p-value and effect sizes
  report.orig <- .report.twogroup(control = control.orig,
                                  treatment = treatment.orig,
                                  method = "t.equal",
                                  alternative = alternative)
  p.orig      <- report.orig[["p"]]
  r2.orig     <- .compR2t(control.orig, treatment.orig)
  d.orig      <- report.orig[["effect"]]
  
  # If original p-value is significant stop and return original p-value
  if(p.orig <= alpha) return(list(p.final = p.orig,
                                  p.orig = p.orig,
                                  r2.final = r2.orig,
                                  r2.orig = r2.orig,
                                  d.final = d.orig,
                                  d.orig = d.orig,
                                  stage = 1))
  
  # If original p-value is smaller than rounding level stop and return alpha as p
  if(p.orig < roundinglevel) return(list(p.final = alpha,
                                          p.orig = p.orig,
                                          r2.final = r2.orig,
                                          r2.orig = r2.orig,
                                          d.final = d.orig,
                                          d.orig = d.orig,
                                          stage = 1.5))
                                         
  
  ########### (2) Exploit statistical analysis options #################
  
  # Welch test
  p.welch <- .report.twogroup(control = control.orig,
                              treatment = treatment.orig,
                              method = "t.welch",
                              alternative = alternative)[["p"]]
  
  # Mann-Whitney / Wilcoxon test
  p.wilcox <- .report.twogroup(control = control.orig,
                               treatment = treatment.orig,
                               method = "wilcox",
                               alternative = alternative)[["p"]]
  
  # Yuen test with different levels of trimming
  p.yuen <- rep(NA, 4)
  trim <- c(0.1, 0.15, 0.2, 0.25)
  for(i in 1:4) {
    p.yuen[i] <- .report.twogroup(control = control.orig,
                                  treatment = treatment.orig,
                                  method = "yuen",
                                  trim = trim[i],
                                  alternative = alternative)[["p"]]
  }
  
  ps <- c(p.orig, p.welch, p.wilcox, p.yuen)
  
  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = roundinglevel)
  
  # If final p-value is smaller than alpha, stop and return
  if(p.final < roundinglevel) return(list(p.final = p.final,
                                   p.orig = p.orig,
                                   r2.final = r2.orig,
                                   r2.orig = r2.orig,
                                   d.final = d.orig,
                                   d.orig = d.orig,
                                   stage = 2))
  
  ################# (3) Selective Reporting DV ##########################
  
  # Compute t-tests
  
  mod <- list()
  r2s <- NULL
  
  for(i in 1:sum(grepl("DV", colnames(df)))){
    control.current <- df[df$group == 1, paste0("DV", i)]
    treatment.current <- df[df$group == 2, paste0("DV", i)]
    mod[[i]]  <- .report.twogroup(control = control.current,
                                  treatment = treatment.current,
                                  method = "t.equal",
                                  alternative = alternative)
                              
    r2s[i]    <- .compR2t(control.current, treatment.current)
                       
  }
  
  ps <- vapply(mod, function(x) x[["p"]], numeric(1))
  
  ds <- vapply(mod, function(x) x[["effect"]], numeric(1))
  
  # Select final p-hacked p-value based on strategy
  p.final   <- .selectpvalue(ps = ps, strategy = strategy, alpha = roundinglevel)
  r2.final  <- unique(r2s[ps == p.final])
  d.final   <- unique(ds[ps == p.final])
  
  # If final p-value is smaller than alpha stop and return
  if(p.final < roundinglevel) return(list(p.final = p.final,
                                   p.orig = p.orig,
                                   r2.final = r2.final,
                                   r2.orig = r2.orig,
                                   d.final = d.final,
                                   d.orig = d.orig,
                                   stage = 3))
  
  #################### (4) Exploit Covariates ##############################
  
  ncov <- sum(grepl("COV", colnames(df)))
  ps <- NULL

  # Compute correlations between covariates and dependent variable 
  # and order covariates accordingly
  dvcors    <- apply(X = df[,c("DV1", paste0("COV", 1:ncov))],
                     MARGIN = 2, 
                     FUN = function(x) stats::cor(x, df$DV1))[-1]
  covorder  <- order(dvcors, decreasing = TRUE)
  
  # Define ANCOVA models (add covariates in decreasing correlation with 
  # dependent variable)
  addmodels   <- c("DV1 ~ group", rep(NA, ncov))
  singmodels  <- c("DV1 ~ group", rep(NA, ncov))
  
  for(i in 1:ncov){
    mdl <- paste("DV1 ~ group", paste0("COV", covorder[i]), sep = "+")
    singmodels[i + 1] <- mdl
  }
  
  for(i in 1:ncov){
    mdl <- paste(paste0("COV", covorder[1:i]), collapse = "+")
    mdl <- paste("DV1 ~ group", mdl, sep = "+")
    addmodels[i+1] <- mdl
  }
  
  models <- unique(c(singmodels, addmodels))
  
  # Compute ANCOVAs
  
  for(i in 1:length(models)){
    
    res <- stats::aov(stats::as.formula(models[i]), data = df)
    resanc <- car::Anova(res, type = 2)
    ps[i] <- resanc["group", "Pr(>F)"]
  }
  
  # Select final p-hacked p-value based on strategy
  p.final     <- .selectpvalue(ps = ps, strategy = strategy, alpha = roundinglevel)

  # If final p-value is smaller than alpha stop and return
  if(p.final < roundinglevel) return(list(p.final = p.final,
                                   p.orig = p.orig,
                                   r2.final = NA,
                                   r2.orig = r2.orig,
                                   d.final = NA,
                                   d.orig = d.orig,
                                   stage = 4))
  
  ################# (5) Subgroup Analysis #########################
  
  whichsub <- grep("SUB", colnames(df))
  ps <- list()
  ds <- list()
  r2s <- list()
  
  # Compute t-test for each subgroup of the subgroup variables
  for(i in 1:length(whichsub)){
    subgroup.values <- sort(unique(df[, whichsub[i]]))
    reports <- vector("list", length(subgroup.values))
    r2.current <- rep(NA_real_, length(subgroup.values))

    for(j in 1:length(subgroup.values)){
      subset.df <- df[df[, whichsub[i]] == subgroup.values[j], , drop = FALSE]
      control.sub <- subset.df[subset.df$group == 1, "DV1"]
      treatment.sub <- subset.df[subset.df$group == 2, "DV1"]
      reports[[j]] <- .report.twogroup(control = control.sub,
                                       treatment = treatment.sub,
                                       method = "t.equal",
                                       alternative = alternative)
      r2.current[j] <- .compR2t(control.sub, treatment.sub)
    }

    ps[[i]] <- vapply(reports, function(x) x[["p"]], numeric(1))
    ds[[i]] <- vapply(reports, function(x) x[["effect"]], numeric(1))
    r2s[[i]] <- r2.current
    
  }
  
  ps <- c(p.orig, unlist(ps))
  r2s <- c(r2.orig, unlist(r2s))
  ds <- c(d.orig, unlist(ds))
  
  # Select final p-hacked p-value based on strategy
  p.final <- .selectpvalue(ps = ps, strategy = strategy, alpha = roundinglevel)
  r2.final <- unique(r2s[ps == p.final])
  d.final <- unique(ds[ps == p.final])
  
  # If final p-value is smaller than alpha stop and return
  if(p.final < roundinglevel) {
    return(list(p.final = p.final,
                p.orig = p.orig,
                r2.final = r2.final,
                r2.orig = r2.orig,
                d.final = d.final,
                d.orig = d.orig,
                stage = 5))
  } else {
    return(list(p.final = p.orig,
                p.orig = p.orig,
                r2.final = r2.orig,
                r2.orig = r2.orig,
                d.final = d.orig,
                d.orig = d.orig,
                stage = 6))
  }
  
}

sim.combined.t <- function(nobs.group = 100, nDV = 5, rDV = 0.6, nCOV = 3, rCOV = 0.3, rcovdv = 0.3, nSUB = 3, roundinglevel = 0.051, alternative = "two.sided", strategy = "firstsig", alpha = 0.05, iter = 1000){
  
  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.combined.t(nobs.group = nobs.group, nDV = nDV, rDV = rDV, nCOV = nCOV, rCOV = rCOV, rcovdv = rcovdv, nSUB = nSUB)
  }
  
  # Apply p-hacking procedure to each dataset
  .combined.t.hack.list <- function(x){
    .combined.t.hack(df = x, 
                     roundinglevel = roundinglevel, 
                     alternative = alternative, 
                     strategy = strategy, 
                     alpha = alpha)
  }
  
  # Apply p-hacking procedure to each dataset
  res <- pbapply::pblapply(dat, .combined.t.hack.list)
  
  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL
  ds.hack <- NULL
  ds.orig <- NULL
  stage <- NULL
  
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["p.orig"]]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2.orig"]]
    ds.hack[i] <- res[[i]][["d.final"]]
    ds.orig[i] <- res[[i]][["d.orig"]]
    stage[i] <- res[[i]][["stage"]]
  }
  
  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig, ds.hack, ds.orig, stage)
  
  return(res)
  
  
}

