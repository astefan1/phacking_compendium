### Incorrect Rounding

This p-hacking strategy is not based on tinkering with the data or the analyses, but on misreporting the analysis outcome. Usually, the result of a hypothesis test is significant if p &leq; &alpha;. However, as has been shown (e.g., Hartgerink, van Aert, van Nuijten, Wicherts, & van Assen, 2016), sometimes p-values that are slightly larger than the significance level are reported as significant, that is, p-values are incorrectly rounded down to p = &alpha;. 

In the simulation function in this Shiny app, the user can specify the margin in which p-values should be rounded down, as well as the significance level. For example, if the significance level is specified as &alpha; = 0.05, and the margin is specified as 0.001, then all p-values below 0.05+0.001=0.051 will be reported as significant and rounded down to p = 0.05. Additionally, users can specify the direction of the test, and the number of simulation iterations.
