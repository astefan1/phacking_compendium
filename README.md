# The p-Hacking Compendium: Simulating Different p-Hacking Strategies

## Project Description
This project contains an R-package with code to simulate and investigate the effects of different p-hacking strategies. It has the following components:
* Functions to simulate 13 different p-hacking strategies
* A Shiny app to investigate the effects of p-hacking on the distribution of p-values, the rate of false positive results, and the distribution of effect sizes

## Installation
The phackR package is not on CRAN, but you can install it from GitHub:

```
library(devtools)
install_github("nicebread/phacking_compendium/phackR", build_vignettes = TRUE)
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





