findFPrate <- function(simresult, alpha = 0.05, hack = TRUE){
  
  if(hack){
    FP.firstsig <- sapply(simresult$firstsig,
                          function(x) {sum(x$ps.hack < alpha) / nrow(x)})
    FP.smallest <- sapply(simresult$smallest,
                          function(x) {sum(x$ps.hack < alpha) / nrow(x)})
    FP.smallestsig <- sapply(simresult$smallestsig,
                             function(x) {sum(x$ps.hack < alpha) / nrow(x)})
  } else {
    FP.firstsig <- sapply(simresult$firstsig,
                          function(x) {sum(x$ps.orig < alpha) / nrow(x)})
    FP.smallest <- sapply(simresult$smallest,
                          function(x) {sum(x$ps.orig < alpha) / nrow(x)})
    FP.smallestsig <- sapply(simresult$smallestsig,
                             function(x) {sum(x$ps.orig < alpha) / nrow(x)})
  }
  
  FP.rates <- rowMeans(cbind(FP.firstsig, FP.smallest, FP.smallestsig))
  
  return(FP.rates)
}

