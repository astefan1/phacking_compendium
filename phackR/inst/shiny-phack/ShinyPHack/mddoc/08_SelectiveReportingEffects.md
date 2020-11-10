### Selective Reporting of Effects

This p-hacking strategy is not based on tinkering with the data or the analyses, but on selective reporting of results. Specifically, in statistical models that contain multiple independent variables (e.g., ANOVA, multiple linear regression), only significant effects are reported. Here, we simulate this p-hacking strategy using multiple linear regression.

In the simulation function in this Shiny app, users can define the number of independent variables and their correlation. Additionally, they can specify whether the multiple linear regression should include interaction terms. Other simulation parameters that can be specified are the sample size, the p-value selection method, the significance level &alpha;, and the number of simulation iterations.


