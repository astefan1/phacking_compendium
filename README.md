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





