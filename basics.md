Basics of contrasts
===================

Contrasts are weighted linear functions, usually of means (Baguley, 2012). They allow to code and quantify the performance or the comparisons among a set of means.

In other words, contrasts make it possible to collapse several comparisons among the levels of a categorical variable into a unique function (as if they were a single effect).

Have you ever tested the means' differences in case of variable with two (or more) levels? 

Spoiler alert: the pattern of differences tested by your model were expressed as (a set of) contrasts inside your statistical software! 

Interested? The following section will briefly explain what a contrast is, within the frame of a linear regression model.

## Where are my contrasts?
-----------------------
Assume a linear model $Y = X \hat{\beta} + \hat{E}$, where $Y$ is the response variable, $\hat{\beta}$ is the set of expected regression coefficients and $E$ the expected error. $X$ is the design matrix of our model. The aforementioned comparisons can be observed by assigning, for each value of $Y$, a number encoding the condition for each of those points.


For instance, let's assume to have a single predictor with three levels (A, B and C) and nine subjects. We want to test the means differences between:

* levels A and B 
* levels A and C.


<center>
<img src="www/tab0.png" alt="drawing" width="300"/>
</center>

<p>&nbsp;</p>

In this design, we expect two effects. The matrix $\textbf{X}$ encodes all of them and adds, as the first column, the constant intercept term. The second column encodes the contrast representing the difference between group A and B: subject 1, 2 and 3 are assigned to group A and are coded with $\textbf{0}$; subjects 4, 5 and 6 are assigned to group B and are coded with $\textbf{1}$. The third column encodes the contrast representing the difference between group A and C: subject 7, 8 and 9 are assigned to group C and are coded with $\textbf{1}$ (assuming a dummy coding, see the following descriptions).

<p>&nbsp;</p>
The design matrix X can be simplified into the following contrast matrix:
<p>&nbsp;</p>

<center>
<img src="www/tab1.png" alt="drawing" width="200"/>
</center>
<p>&nbsp;</p>
 -----------------------
 
Now, let's assume to work with an ANOVA with a 2 X 2 design and eight subjects.

<p>&nbsp;</p>
<center>
<img src="www/tab2.png" alt="drawing" width="300"/>
</center>

<p>&nbsp;</p>

In this design, we expect three effects: two main and one interaction effects. The matrix $\textbf{X}$ encodes all of them and adds, as the first column, the constant intercept term. The second column encodes the contrast of the first predictor (according an effect coding, see the next Panel): the first four subjects are assigned to the first level of the predictor one and are coded with $\textbf{-1}$. The third column encodes the contrast of the second predictor (according an effect coding): subject 1,2, 5 and 6 are assigned to the first level of the predictor two and are coded with $\textbf{-1}$. The last column encodes the contrasts of the interaction between the two predictors: each subject is assigned to a specific group by multiplying the previous two contrasts columns.
The simplified contrast matrix is:


<p>&nbsp;</p>
<center>
<img src="www/tab3.png" alt="drawing" width="200"/>
</center>
<p>&nbsp;</p>



From these two examples emerges a golden rule of contrast coding: assumed a variable with $n$ levels, the contrast that can be tested must be $n-1$, since each contrast consumes one degree of freedom (for more explanations, see Baguley, 2012). The same applies for interactions, where given $(n_{1}  \times n_{2})$ levels, the contrast can be $(n_{1}  \times n_{2})-1$.

<p>&nbsp;</p>

In the next Panel, all the types of contrast provided by R and by appRiori are presented!

## References
-----------------------
Baguley, T. (2012). Contrasts. In *Serious stats: A guide to advanced statistics for the behavioral sciences*. Macmillan International Higher Education.

