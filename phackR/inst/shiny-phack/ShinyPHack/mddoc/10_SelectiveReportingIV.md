### Selective Reporting of the Independent Variable

This p-hacking strategy assumes that an experiment or clinical trial contains multiple experimental groups and one control group. A researcher engaging in p-hacking statistically compares all experimental groups to the control group, and only report the significant results. Here, we assume that all conducted hypothesis tests are t-tests.

The simulation function in this Shiny app allows the specification of the number of experimental groups (independent variables), and their correlation. Additionally, users can set the number of observations per group, the direction of the test, the p-value selection method, the significance level &alpha;, and the number of simulation iterations. 
