# Gazing into the Abyss of p-Hacking: A Shiny App for p-Hacking Simulation

## What is p-Hacking?
The p-value is a core component of null hypothesis significance testing (NHST), a statistical framework that has found ubiquitous use across many scientific disciplines. A p-value is defined as the probability to obtain a result at least as extreme as the observed one if the null hypothesis is true (i.e., if there is no effect). If the p-value is smaller than a certain threshold called alpha level, then the test result is labeled "significant" and the null hypothesis is rejected. Researchers who are interested in showing an effect in their data (e.g., that a new medicine improved the health of patients) are therefore eager to obtain small p-values that allow them to reject the null hypothesis and claim the existence of an effect.

In recent years, failed attempts to replicate experiments have instigated investigations into how researchers use NHST in practice. Studies found that many researchers apply questionable research practices to render previously non-significant results significant. We summarize these practices under the term of *p-hacking*.

## How Does p-Hacking Work?
All p-hacking strategies are based on the principle of alpha error accumulation. Basically, alpha error accumulation means that more and more hypothesis tests are conducted, the probability of making at least one false decision increases. Therefore, even if there is no effect in the population, the probability is very high that at least one hypothesis test will (erroneously) show a significant result, if a sufficiently large number of tests are conducted. Researchers then report this significant result, and claim to have found an effect.

## Obvious Warning: Thou Shalt Not p-Hack!
Given the explanation above, it almost seems needless to say that p-hacking is detrimental and you should not do it. P-hacking slows down scientific progress by increasing the amount of false positive results in the literature. Additionally, p-hacking leads to an inflation of effect sizes that are published in the literature because only "extreme" results are reported. This means that p-hacking increases the number of cases where research wrongly claims an effect, and even if an effect exists, the reported effect size is likely to be larger than the true effect size. 

Sounds bad? It actually is. What makes it even worse is that it is difficult to discover p-hacking in the literature. How can we tell whether a reported effect is real or p-hacked? How can we tell that a p-hacked significant result (i.e., a significant finding that a researcher found after running many hypothesis tests) is not actually a true effect that was discovered? The truth is, for a single finding, it is impossible to know. However, if we know what p-hacking strategies researchers employ, it is possible to predict what distributions of p-values and effect sizes will look like, and how the rate of false positive results will be changed compared to a situation without p-hacking. The purpose of this app is to showcase these scenarios using simulated data.

## A Compendium of p-Hacking Strategies
In the literature, p-hacking has typically been described as being comprised of different strategies that researchers can use to tinker with their statistical results to achieve statistical significance. In order to learn more about the effects of p-hacking, it is important to understand all strategies and their effects on the reported scientific results. However, a comprehensive description of these strategies has been missing so far.

Here, we provide an overview of different p-hacking strategies that have been mentioned in the literature, together with a Shiny app that lets users explore the effects of p-hacking on the distribution of hypothesis testing results.

## Exploring the Effects of P-Hacking
Each tab of this Shiny app lets the user explore the effects of a different p-hacking strategy. All tabs have the same structure: First, we describe the p-hacking strategy, and how we applied it in our simulations. Below, we present simulation results, specifically the distribution of p-values, the distribution of effect sizes (if applicable), and the rate of false positive results. On a panel on the right side, the user can adjust the settings of the simulation, including the severity of the p-hacking.

### Common Settings
Several settings are common to the simulation of (almost) all p-hacking strategies. To avoid unnecessary repetition, we will describe these settings here.

#### p-Value selection method
In all simulation functions, it is necessary to specify how the final p-value is determined. There are three options: *first significant* simulates a situation where the researcher conducts a series of hypothesis tests, and stops as soon as the result is significant, that is, at the first significant p-value. In a comment on Simonsohn et al. (2014), Ulrich and Miller (2015) argued that researchers might instead engage in "ambitious" p-hacking, where the researcher conducts a series of hypothesis tests and selects the smallest significant p-value from the set. This strategy is implemented in the *smallest significant* option. Simonsohn (private comm.) argues that there might exist a third p-hacking strategy where the researcher tries a number of different analysis options, and selects the smallest p-value, no matter if it is significant or not. This strategy is implemented in the option *smallest*. The default strategy is *first significant*.

#### True effect size
The true effect size in all simulations is equal to zero. 

#### Significance level
The significance level &alpha; determines the significance level for each hypothesis test. For example, if the significance level is set to &alpha; = 0.05 (the default), the simulation assumes that a researcher would call the result of a hypothesis test significant if p < 0.05.

#### Iterations
The *iterations* option determines the number of iterations in the simulation. The default setting is 1000.

#### Alternative
Whenever the simulations are based on t-tests, the option *alternative* can be specified. This option relates to the sidedness of the alternative hypothesis in the t-test. It can either be *two-sided* or *greater*. The default setting is *two-sided*.

#### Number of observations
The number of observations determines the sample size in the test. In the case of a t-test, the specified number refers to the observations *per group*. In the case of a linear regression, the specified number refers to the overall sample size. 

#### Start simulation
A new simulation will be started when you click the *Start simulation* button on the bottom of the options panel in each tab. The progress of the simulation will be displayed in a small progress bar in the bottom right corner of the screen.

## Resources
The code for this Shiny app as well as for the simulations can be found on [https://github.com/nicebread/phacking_compendium](https://github.com/nicebread/phacking_compendium).
