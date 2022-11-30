# ==============================================================================
# Impact of combined p-hacking strategies: t-test Example 
# ==============================================================================

.sim.combined.reg <- function(nobs = 100, missing = 0.1, ncompv = 5, rcomp = 0.75){
  
  # Sample dependent variable, variables in the score, compute score
  DV        <- rnorm(nobs, 0, 1)
  
  SCOREVAR  <- .sim.multcor(nobs = nobs, 
                            nvar = ncompv,
                            r = rcomp)
  SCORE     <- rowMeans(SCOREVAR)
  
  # Introduce missing values in score
  missingSCORE <- sample(c(TRUE, FALSE), 
                         size = nobs, 
                         prob = c(missing, 1-missing),
                         replace = TRUE)
  SCORE[missingSCORE] <- NA
  
  # Introduce missing values in DV
  missingDV <- sample(missingSCORE, 
                      length(missingSCORE))
  DV[missingDV] <- NA
  
  # Create a missing value on a score variable if value in score is missing
  whichmissing            <- matrix(c(which(missingSCORE == TRUE),
                                      sample(1:ncompv, 
                                             size = sum(missingSCORE),
                                             replace = TRUE)),
                                    ncol=2,
                                    byrow=FALSE)
  SCOREVAR[whichmissing]  <- NA
  
  # Bind them all together
  DAT.FULL <- cbind(DV, SCOREVAR, SCORE)
  
  return(DAT.FULL)
  
}

.combined.reg.hack <- function(df, roundinglevel = 0.051, nImpMethods = 5, transvar = "xy", ndelete = 3, nOutMethods = 3, strategy = "firstsig", alpha = 0.05){
  
  ####################### (1) Original p-value ###################
  
  modres <- summary(lm(df$DV ~ df$SCORE))
  p.orig <- modres$coefficients[2, 4]
  r2.orig <- modres$r.squared
  
  # If original p-value is significant stop and return original p-value
  if(p.orig <= alpha) return(list(p.final = p.orig,
                                  p.orig = p.orig,
                                  r2.final = r2.orig,
                                  r2.orig = r2.orig,
                                  stage = 1))
  
  # If original p-value is smaller than rounding level stop and return alpha as p
  if(p.orig < roundinglevel) return(list(p.final = alpha,
                                          p.orig = p.orig,
                                          r2.final = r2.orig,
                                          r2.orig = r2.orig,
                                          stage = 1.5))
  
  ####################### (2) Favorable Imputation #####################
  
  # Apply imputation methods (random selection)
  impMethods  <- sample(c(1:10), nImpMethods)
  impres      <- .impHack(df, 
                         x = 7, 
                         y = 1, 
                         which = impMethods, 
                         strategy = strategy, 
                         alpha = roundinglevel)
  
  # If p-value is significant stop and return
  if(impres$p.final < roundinglevel) return(list(p.final = impres$p.final,
                                            p.orig = p.orig,
                                            r2.final = impres$r2.final,
                                            r2.orig = r2.orig,
                                            stage = 2))
  
  ###################### (3) Variable transformation #################
  
  # Apply variable transformation (omit NA)
  transres <- .varTransHack(df[-which(is.na(df$DV) | is.na(df$SCORE)), ], 
                            x = 7, 
                            y = 1, 
                            transvar = "xy", 
                            strategy = "firstsig", 
                            alpha = roundinglevel)
  
  # If p-value is significant, stop and return
  if(transres$p.final < roundinglevel) return(list(p.final = transres$p.final,
                                              p.orig = p.orig,
                                              r2.final = transres$r2.final,
                                              r2.orig = r2.orig,
                                              stage = 3))
  
  ##################### (4) Scale redefinition ##########################
  
  # Scale redefinition
  rescaleRes <- .compscoreHack(df[-which(is.na(df$DV) | is.na(df$SCORE)), ], 
                               dv = 1,
                               compv = c(2:6),
                               ndelete = ndelete,
                               strategy = strategy,
                               alpha = roundinglevel)
  
  # If p-value is significant, stop and return
  if(rescaleRes$p.final < roundinglevel) return(list(p.final = rescaleRes$p.final,
                                                p.orig = p.orig,
                                                r2.final = rescaleRes$r2.final,
                                                r2.orig = r2.orig,
                                                stage = 4))
  
  ##################### (5) Outlier exclusion #############################
  
  # Exclude outliers
  outMethods  <- sample(c(1:12), nOutMethods)
  outlierRes <- .outHack(df[-which(is.na(df$DV) | is.na(df$SCORE)), ], 
                         x = 7, 
                         y = 1, 
                         which = outMethods, 
                         strategy = strategy, 
                         alpha = roundinglevel)
  
  # If p-value is significant, stop and return, else return original p-value
  if(outlierRes$p.final < roundinglevel){
    return(list(p.final = outlierRes$p.final,
                p.orig = p.orig,
                r2.final = outlierRes$r2.final,
                r2.orig = r2.orig,
                stage = 5)) 
  } else {
    return(list(p.final = p.orig,
                p.orig = p.orig,
                r2.final = r2.orig,
                r2.orig = r2.orig,
                stage = 6))
  }
                                          
}

sim.combined.reg <- function(nobs = 100, missing = 0.1, ncompv = 5, rcomp = 0.75, roundinglevel = 0.051, nImpMethods = 5, transvar = "xy", ndelete = 3, nOutMethods = 3, strategy = "firstsig", alpha = 0.05, iter = 1000){
  
  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.combined.reg(nobs = nobs, 
                                  missing = missing, 
                                  ncompv = ncompv, 
                                  rcomp = rcomp)
  }
  
  # Apply p-hacking procedure to each dataset
  .combined.reg.hack.list <- function(x){
    .combined.reg.hack(df = x, 
                       roundinglevel = roundinglevel, 
                       nImpMethods = nImpMethods, 
                       transvar = transvar, 
                       ndelete = ndelete, 
                       nOutMethods = nOutMethods, 
                       strategy = strategy, 
                       alpha = alpha)
  }
  
  # Apply p-hacking procedure to each dataset
  res <- pbapply::pblapply(dat, .combined.reg.hack.list)
  
  ps.hack <- NULL
  ps.orig <- NULL
  r2s.hack <- NULL
  r2s.orig <- NULL
  stage <- NULL
  
  for(i in 1:iter){
    ps.hack[i] <- res[[i]][["p.final"]]
    ps.orig[i] <- res[[i]][["p.orig"]]
    r2s.hack[i] <- res[[i]][["r2.final"]]
    r2s.orig[i] <- res[[i]][["r2.orig"]]
    stage[i] <- res[[i]][["stage"]]
  }
  
  res <- cbind(ps.hack, ps.orig, r2s.hack, r2s.orig, stage)
  
  return(res)
  
}