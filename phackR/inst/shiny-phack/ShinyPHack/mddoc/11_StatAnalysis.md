### Exploiting Alternative Hypothesis Tests

Often, different statistical analysis techniques can be used to answer the same research question. This p-hacking strategy assumes that a researcher tries out different statistical analysis options and decides for the one yielding a significant result. Here, we assume that the hypothesis tests in question are an independent-samples t-test, a Welch test, a Wilcoxon test, and a Yuen test (with different levels of trimming).

The simulation function in this Shiny app allows users to specify the number of observations per group, the direction of the test, the p-value selection method, the significance level &alpha;, and the number of simulation iterations.
