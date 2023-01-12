Welcome to appRiori!
=======================

This Shiny App is programmed to help you in defining planned comparisons for specific variables and/ or for interactions among variables. 


Once you defined your planned comparisons, appRiori will provide you the corresponding code that you can insert within your R/ Rstudio before running your analysis!


## Why Should I use appRiori?
-----------------------
Usually, scholars and researchers have specific hypotheses about the differences among groups of means. Nonetheless, to test such hypotheses, it is common practice to apply the following strategy: checking the statistics related to the simple and/or interaction effects and then apply post-hoc comparisons to observe if the specific effect is statistically significant.


Such strategy has two cons: 1) An omnibus test (like $F$ or $\chi^2$) tells us only that it is likely that one or more statistically-significant differences among groups occur, but not which specific ones. 2) Post-hoc comparisons are very useful, since each condition is compared to each other, at the cost of low statistical power.


Should not be better to code the hypotheses before running the analysis? In this way, the answers would be directly available from the summary of the model, without sacrificing statistical power.

The problem is that coding planned comparisons, or contrasts, is not straightforward..until now.



The appRiori helps the user to:

* Understand the logic of contrasts.
* Directly plan a priori specific contrasts from real databases.
* Plan those contrasts not only for simple effects, but also for two-way and three-way interaction effects.
* Obtain the corresponding and ready-to-use R code to be applied directly before running the analysis.

The generated contrasts can be used for ANOVAs, linear or generalized (mixed effects) models etc.


Please, have a look at the other Panels to get started with contrasts, to learn about which kind of contrasts can be made and to have a look at some examples!
