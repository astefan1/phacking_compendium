# ==============================================================================
# Figures: p-value and effect size distributions
# ==============================================================================

#' Plot p-value distributions
#' @param simdat Simulated data from one of the p-hacking simulation functions
#' @param alpha Alpha level
#' @importFrom ggplot2 ggplot geom_histogram aes theme_light xlab ggtitle theme element_text geom_vline ylim scale_color_manual scale_fill_manual layer_scales geom_density ylab
#' @importFrom rlang .data
#' @importFrom dplyr all_of

pplots <- function(simdat, alpha){
  
  simdat <- as.data.frame(simdat)
  
  phack <- ggplot(simdat, aes(x=.data$ps.hack)) +
    geom_histogram(fill="#FFAE4A", color="#C27516", bins=30) +
    theme_light() +
    xlab("p-value") +
    ggtitle("Distribution of p-hacked p-values") +
    geom_vline(xintercept = alpha, linetype= "dashed") +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))
  
  pnohack <- ggplot(simdat, aes(x=.data$ps.orig)) +
    geom_histogram(fill="#43B7C2", color="#024B7A", bins=30) +
    theme_light() +
    xlab("p-value") +
    ggtitle("Distribution of original p-values") +
    geom_vline(xintercept = alpha, linetype= "dashed") +
    ylim(layer_scales(phack)$y$range$range) +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))
  
  simdat_long <- tidyr::gather(simdat, "condition", "pval", all_of("ps.hack"):all_of("ps.orig"))
  
  pcomp <- ggplot(simdat_long, aes(.data$pval, fill=.data$condition, color=.data$condition)) +
    geom_density(alpha=0.3) +
    xlab("p-value") +
    ylab("Density") +
    ggtitle("Distribution of p-values") +
    scale_color_manual(values=c("#C27516", "#024B7A"),
                       labels=c("p-hacked", "original")) +
    scale_fill_manual(values=c("#FFAE4A", "#43B7C2"),
                      labels=c("p-hacked", "original")) +
    theme_light() +
    geom_vline(xintercept = alpha, linetype= "dashed") +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))
  
  return(list(phack=phack,
              pnohack=pnohack,
              pcomp=pcomp)) 
}

esplots <- function(simdat, EScolumn.hack, EScolumn.orig, titles = c(expression("Distribution of p-hacked effect sizes R"^2),
                                                                           expression("Distribution of original effect sizes R"^2))){
  
  simdat <- as.data.frame(simdat)
  es.hack <- colnames(simdat)[EScolumn.hack]
  es.orig <- colnames(simdat)[EScolumn.orig]
  
  eshack <- ggplot(simdat, aes(x=simdat[,es.hack])) +
    geom_histogram(fill="#FFAE4A", color="#C27516", bins=30, na.rm=FALSE) +
    theme_light() +
    xlab("Effect Size") +
    ggtitle(titles[1]) +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))
  
  esnohack <- ggplot(simdat, aes(x=simdat[, es.orig])) +
    geom_histogram(fill="#43B7C2", color="#024B7A", bins=30) +
    theme_light() +
    xlab("Effect Size") +
    ggtitle(titles[2]) +
    theme(axis.title = element_text(size=14),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))
  
  return(list(eshack=eshack,
              esnohack=esnohack))
  
}








