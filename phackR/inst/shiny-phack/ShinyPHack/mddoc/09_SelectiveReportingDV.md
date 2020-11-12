### Selective Reporting of the Dependent Variable

This p-hacking strategy assumes that the dataset contains multiple candidate dependent variables. For example, in a clinical trial, the treatment and control group could be compared on different outcome variables, such as mental and physical well-being. A researcher engaging in p-hacking would conduct one hypothesis test for each dependent variable, and selectively report the significant results. Here, we assume that the hypothesis test in question is an independent-samples t-test.

The simulation function in this Shiny app allows the specification of the number of dependent variables as well as their correlation. Additionally, users can define the number of observations per group, the direction of the test, the p-value selection method, the significance level &alpha;, and the number of simulation iterations.
