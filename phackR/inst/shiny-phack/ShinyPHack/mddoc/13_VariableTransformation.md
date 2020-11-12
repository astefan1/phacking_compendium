### Exploiting Variable Transformations

This p-hacking strategy assumes that if an initial hypothesis test does not yield significant results, a researcher would apply transformations to the variables involved in the test. Here, we assume that the test in question is a univariate linear regression, and that the transformations are a natural log transformation (ln(x)), a square root transformation (&radic;x), and an inverse transformation (1/x). Transformations can be applied to the predictor variable, to the outcome variable, or both.

In the simulation function in this Shiny app, users can specify which of the variables should be transformed. Additionally, they can specify the number of observations, the p-value selection method, the significance level &alpha;, and the number of simulation iterations. 
