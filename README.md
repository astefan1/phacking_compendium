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

Alternatively, you can directly access the Shiny app online via [https://shiny.psy.lmu.de/felix/ShinyPHack/](https://shiny.psy.lmu.de/felix/ShinyPHack/)

## Simulation Results
All simulation results can be reproduced using the code in the [_/simulations_ folder of this Github project](https://github.com/astefan1/phacking_compendium/tree/master/simulations). First, follow the steps above to install the phackR package. Then, run the script "00_simulation_helpers.R", followed by all R scripts with the "\_simulation.R" suffix. Results can be visualized using the scripts with the prefix "plot\_". 


```
> sessionInfo()
R version 4.2.1 (2022-06-23)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 11.6.8

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] BayesFactor_0.9.12-4.4 Matrix_1.5-1           coda_0.19-4           
 [4] phackR_0.0.0.9000      dplyr_1.0.10           ggforce_0.4.1         
 [7] R.devices_2.17.1       wesanderson_0.3.6      ggplot2_3.3.6         
[10] testthat_3.1.5        

loaded via a namespace (and not attached):
 [1] fs_1.5.2             usethis_2.1.6        devtools_2.4.5       insight_0.18.6      
 [5] rprojroot_2.0.3      tools_4.2.1          profvis_0.3.7        backports_1.4.1     
 [9] utf8_1.2.2           R6_2.5.1             colorspace_2.0-3     urlchecker_1.0.1    
[13] withr_2.5.0          tidyselect_1.1.2     prettyunits_1.1.1    processx_3.7.0      
[17] compiler_4.2.1       sgeostat_1.0-27      performance_0.10.0   cli_3.4.1           
[21] mice_3.14.0          desc_1.4.2           labeling_0.4.2       scales_1.2.1        
[25] DEoptimR_1.0-11      mvtnorm_1.1-3        robustbase_0.95-0    mc2d_0.1-21         
[29] callr_3.7.2          pbapply_1.5-0        stringr_1.4.1        digest_0.6.29       
[33] rmarkdown_2.17       R.utils_2.12.0       base64enc_0.1-3      WRS2_1.1-4          
[37] pkgconfig_2.0.3      htmltools_0.5.3      sessioninfo_1.2.2    fastmap_1.1.0       
[41] htmlwidgets_1.5.4    rlang_1.0.6          rstudioapi_0.14      shiny_1.7.2         
[45] generics_0.1.3       farver_2.1.1         car_3.1-1            R.oo_1.25.0         
[49] magrittr_2.0.3       Rcpp_1.0.9           munsell_0.5.0        fansi_1.0.3         
[53] abind_1.4-5          lifecycle_1.0.3      R.methodsS3_1.8.2    yaml_2.3.5          
[57] stringi_1.7.8        carData_3.0-5        MASS_7.3-57          brio_1.1.3          
[61] pkgbuild_1.3.1       plyr_1.8.7           grid_4.2.1           parallel_4.2.1      
[65] promises_1.2.0.1     shinydashboard_0.7.2 forcats_0.5.2        crayon_1.5.2        
[69] miniUI_0.1.1.1       lattice_0.20-45      knitr_1.40           aplpack_1.3.5       
[73] ps_1.7.1             pillar_1.8.1         tcltk_4.2.1          pkgload_1.3.1       
[77] glue_1.6.2           evaluate_0.17        remotes_2.4.2        vctrs_0.4.2         
[81] tweenr_2.0.2         httpuv_1.6.6         MatrixModels_0.5-1   gtable_0.3.1        
[85] purrr_0.3.5          polyclip_1.10-4      tidyr_1.2.1          reshape_0.8.9       
[89] cachem_1.0.6         xfun_0.33            mime_0.12            xtable_1.8-4        
[93] broom_1.0.1          later_1.3.0          tibble_3.1.8         memoise_2.0.1       
[97] mvoutlier_2.1.1      ellipsis_0.3.2 
```


