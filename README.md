# The p-Hacking Compendium: Simulating Different p-Hacking Strategies

## Project Description
This project contains an R-package with code to simulate and investigate the effects of different p-hacking strategies. It has the following components:
* Functions to simulate 12 different p-hacking strategies
* A Shiny app to investigate the effects of p-hacking on the distribution of p-values, the rate of false positive results, and the distribution of effect sizes
* Code to reproduce simulation results conducted in our upcoming preprint, as well as plots

## Installation
The phackR package is not on CRAN, but you can install it from GitHub:

```
library(devtools)
install_github("astefan1/phacking_compendium/phackR", build_vignettes = TRUE)
```

## Package Description
To get an overview of the structure of the code and the simulation functions in the package, read the package vignette:

```
library(phackR)
utils::vignette("phackR_vignette", "phackR")
```

## Shiny App
You can start the Shiny app directly from the package by using the following code:

```
phackR::runShinyPHack()
```

## Simulation Results
All simulation results can be reproduced using the code in the [_/simulations_ folder of this Github project](https://github.com/astefan1/phacking_compendium/tree/master/simulations). First, follow the steps above to install the phackR package. Then, run the script "00_simulation_helpers.R", followed by all R scripts with the "\_simulation.R" suffix. Results can be visualized using the scripts with the prefix "plot\_". 


```
> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS  10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods  
[7] base     

other attached packages:
[1] cartography_3.0.1 wesanderson_0.3.6 dplyr_1.0.4      
[4] ggplot2_3.3.3    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.6           pillar_1.4.7         compiler_4.0.2      
 [4] prettyunits_1.1.1    tools_4.0.2          digest_0.6.27       
 [7] packrat_0.5.0        pkgbuild_1.2.0       jsonlite_1.7.2      
[10] lifecycle_1.0.0      tibble_3.0.6         gtable_0.3.0        
[13] lattice_0.20-41      pkgconfig_2.0.3      rlang_0.4.10        
[16] cli_2.3.0            DBI_1.1.1            rstudioapi_0.13     
[19] parallel_4.0.2       curl_4.3             loo_2.4.1           
[22] gridExtra_2.3        withr_2.4.1          generics_0.1.0      
[25] vctrs_0.3.6          rgeos_0.5-9          stats4_4.0.2        
[28] grid_4.0.2           tidyselect_1.1.0     glue_1.4.2          
[31] inline_0.3.17        R6_2.5.0             processx_3.4.5      
[34] rstan_2.21.3         sp_1.4-6             farver_2.0.3        
[37] purrr_0.3.4          callr_3.5.1          magrittr_2.0.1      
[40] codetools_0.2-16     matrixStats_0.58.0   StanHeaders_2.21.0-7
[43] ps_1.5.0             scales_1.1.1         ellipsis_0.3.1      
[46] assertthat_0.2.1     colorspace_2.0-0     labeling_0.4.2      
[49] V8_3.4.0             RcppParallel_5.0.2   munsell_0.5.0       
[52] crayon_1.4.1 
```


