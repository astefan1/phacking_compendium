### Scale Redefinition

The *scale redefinition* strategy assumes that one of the variables in the hypothesis test in question is a composite score (e.g., the mean of items in a personality inventory), and that a researcher manipulates which items are included in the composite score to obtain a significant result.

Here, we assume that the focal hypothesis test is a univariate linear regression, and that items are excluded based on the reliability coefficient Cronbach's &alpha; in an iterative fashion. The underlying idea is to delete the item that contributes least to a reliable score, i.e., the item leading to the highest Cronbach's &alpha; when deleted. After a candidate item for deletion has been found, the regression is recomputed with (1) the reduced score as a predictor, (2) the deleted item as a predictor, and (3) the score of all deleted items as a predictor, and the p-values are recorded.

The simulation function in this Shiny app allows the specification of the total number of items in the score, as well as their correlation. Users can also specify the maximum number of items deleted from the score. Naturally, this number should be smaller than the total number of items. Other options users can specify are the number of observations, the p-value selection method, the significance level &alpha;, and the number of simulation iterations.


