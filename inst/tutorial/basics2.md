```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


How many contrasts?
===================

In R, the contrast matrix of a specific variable can be observed by using the `contrasts()` function.

Moreover, it is possible to create or assign a contrast matrix to each variable by using the ``contrasts.'' family of functions. The following paragraphs will describe the kind of contrasts that can be generated in R and how can they be interpreted.

Dummy contrasts
---------------

```{r}
contr.treatment(4)
```

`r contr.treatment(4)`

Assumed n levels of a categorical variable, in case of dummy contrasts (or treatment contrasts), the aim is to compare n-1 levels to a level chosen as references or baseline. In R, the contrast matrix for dummy contrasts is contr.tratment(), setting as argument the number of levels of the variable of interest. If n = 4:
NB: in R, the reference level is automatically assigned to the first level in alphabetical order. In case of levels coded with alphanumerical string, the reference will be the one containing the lowest number. If you want to change it, you should reorder the variable's level. With appPiori, you can reassign the reference level by using the "New reference level" widget!
Formal note: dummy contrasts are the only ones that, by default, are not centered. In other words, for this kind contrast, each column of the contrast matrix does not sum to zero. Centering contrast is advantageous for interaction. We will briefly analyze this aspect in the "Contrasts for interactions" section.
