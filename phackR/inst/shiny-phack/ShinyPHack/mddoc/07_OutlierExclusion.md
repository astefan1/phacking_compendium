### Outlier Exclusion

In this p-hacking strategy, a researcher applies different outlier exclusion criteria to their data with the goal of obtaining a significant result in a focal hypothesis test. Here, we assume that the hypothesis test in question is a univariate linear regression. Further, we assume that the researcher first checks for potential outliers in the predictor variable (x) and in the outcome variable (y), and then reruns the analysis (1) without the xy pairs where x is an outlier, (2) without the xy pairs where y is an outlier, (3) without the xy pairs where x *and* y are outliers. We assume that this is done for each outlier exclusion method.

In the simulation function provided in this Shiny app, users can define the outlier exclusion methods that are applied, as well as the sample size, the p-value selection method, the significance level &alpha;, and the number of simulation iterations.


