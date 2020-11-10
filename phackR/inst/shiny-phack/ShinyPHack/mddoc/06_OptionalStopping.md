### Optional Stopping

Researchers engaging in optional stopping repeatedly inspect the results of the statistical tests during data collection. They stop data collection as soon as a significant result has been obtained or a maximum sample size is reached. Here, we assume that the underlying statistical test is an independent-samples t-test.

In the simulation function provided in this Shiny app, the user can specify the minimum sample size (per group), the maximum sample size (per group), and the number of observations that are collected at each step of the sampling process (*step size*). For example, if the minimum sample size is specified to be 10, the maximum sample size 30, and the step size 5, then interim analyses will be conducted at N = 10, N = 15, N = 20, N = 25, and N = 30. Additionally, users can define the direction of the hypothesis test, the significance level &alpha;, and the number of simulation iterations.
