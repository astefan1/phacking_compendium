# ==============================================================================
# SIMULATION TO INVESTIGATE NORMALITY OF RESIDUALS UNDER TRANSFORMATIONS
# ==============================================================================

source("./simulations/00_simulation_helpers.R")

# Compute p-values of lm() and p-values and test statistics from normality tests
# under different transformations

VarTransExploration <- function(nobs, iter){
  
  final <- array(dim=c(iter, 5, 4, 4))
  
  # Simulate as many datasets as desired iterations
  dat <- list()
  for(i in 1:iter){
    dat[[i]] <- .sim.multcor(nobs = nobs, nvar = 2, r = 0)
  }
  
  # Apply transformation and test to each dataset
  for(i in 1:iter){
    
    df <- dat[[i]]
    x <- df[,1]
    y <- df[,2]
    
    Xtrans <- matrix(NA, nrow = nrow(df))
    Xtrans[,1] <- x
    Ytrans <- matrix(NA, nrow = nrow(df))
    Ytrans[,1] <- y
    
    Xtrans <- cbind(Xtrans,
                    log(x+abs(min(x))+1e-10),        # log transformation
                    sqrt(x+abs(min(x))+1e-10),       # square root transformation
                    1/x                              # inverse
    )
    
    Ytrans <- cbind(Ytrans,
                    log(y+abs(min(y))+1e-10),        # log transformation
                    sqrt(y+abs(min(y))+1e-10),       # square root transformation
                    1/y                              # inverse
    )
    
    ps.lm <- matrix(NA, nrow = 4, ncol = 4)
    ps.ks <- matrix(NA, nrow = 4, ncol = 4)
    ps.sw <- matrix(NA, nrow = 4, ncol = 4)
    stat.ks <- matrix(NA, nrow = 4, ncol = 4)
    stat.sw <- matrix(NA, nrow = 4, ncol = 4)
    
    for(j in 1:ncol(Xtrans)){
      for(k in 1:ncol(Ytrans)){
        mod <- summary(stats::lm(Ytrans[,k] ~ Xtrans[,j]))
        ps.lm[j,k] <- mod$coefficients[2, 4]
        ks <- ks.test(mod$residuals, "pnorm")
        ps.ks[j,k] <- ks$p.value
        stat.ks[j,k] <- ks$statistic
        sw <- shapiro.test(mod$residuals)
        ps.sw[j,k] <- sw$p.value
        stat.sw[j,k] <- sw$statistic
      }
    }
    
    res <- array(dim=c(5,4,4))
    res[1,,] <- ps.lm
    res[2,,] <- ps.ks
    res[3,,] <- stat.ks
    res[4,,] <- ps.sw
    res[5,,] <- stat.sw
    
    final[i, , , ] <- res
  }
  
  return(final)
}

############################ FOR N = 30 ########################################

explore30 <- VarTransExploration(nobs=30, iter=1000)

mainsX <- matrix(c("X", "log(X)", "sqrt(X)", "1/X", rep("", 12)), nrow = 4, byrow=FALSE)
mainsY <- matrix(c("Y", "log(Y)", "sqrt(Y)", "1/Y", rep("", 12)), nrow = 4, byrow=TRUE)

# plot p values from lm
par(mfrow=c(4,4), oma=c(0,0,2,0))
for(i in 1:4){
  for(j in 1:4){
    hist(explore30[, 1, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value regression", side=3, line=0, outer=TRUE, cex=2)

# plot p values from ks test
for(i in 1:4){
  for(j in 1:4){
    hist(explore30[, 2, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value Kolmogorov-Smirnov test (residuals)", side=3, line=0, outer=TRUE, cex=2)

# plot ks test statistic
for(i in 1:4){
  for(j in 1:4){
    hist(explore30[, 3, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("Test statistic Kolmogorov-Smirnov test (residuals)", side=3, line=0, outer=TRUE, cex=2)

# plot p values from shapiro wilk
for(i in 1:4){
  for(j in 1:4){
    hist(explore30[, 4, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value Shapiro-Wilk test (residuals)", side=3, line=0, outer=TRUE, cex=2)


# plot shapiro wilk test statistic
for(i in 1:4){
  for(j in 1:4){
    hist(explore30[, 5, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("Test statistic Shapiro-Wilk test (residuals)", side=3, line=0, outer=TRUE, cex=2)

############################ FOR N = 300 ########################################

explore300 <- VarTransExploration(nobs=300, iter=1000)

mainsX <- matrix(c("X", "log(X)", "sqrt(X)", "1/X", rep("", 12)), nrow = 4, byrow=FALSE)
mainsY <- matrix(c("Y", "log(Y)", "sqrt(Y)", "1/Y", rep("", 12)), nrow = 4, byrow=TRUE)

# plot p values from lm
par(mfrow=c(4,4), oma=c(0,0,2,0))
for(i in 1:4){
  for(j in 1:4){
    hist(explore300[, 1, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value regression", side=3, line=0, outer=TRUE, cex=2)

# plot p values from ks test
for(i in 1:4){
  for(j in 1:4){
    hist(explore300[, 2, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value Kolmogorov-Smirnov test (residuals)", side=3, line=0, outer=TRUE, cex=2)

# plot ks test statistic
for(i in 1:4){
  for(j in 1:4){
    hist(explore300[, 3, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("Test statistic Kolmogorov-Smirnov test (residuals)", side=3, line=0, outer=TRUE, cex=2)

# plot p values from shapiro wilk
for(i in 1:4){
  for(j in 1:4){
    hist(explore300[, 4, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("p-Value Shapiro-Wilk test (residuals)", side=3, line=0, outer=TRUE, cex=2)


# plot shapiro wilk test statistic
for(i in 1:4){
  for(j in 1:4){
    hist(explore300[, 5, i, j], main=mainsY[i,j], ylab=mainsX[i,j], xlab="", cex.lab=1.5)  
  }
}
mtext("Test statistic Shapiro-Wilk test (residuals)", side=3, line=0, outer=TRUE, cex=2)

