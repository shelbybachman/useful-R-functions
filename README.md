This repository contains a few handy R functions. The section below is under construction but illustrates how to use some of the functions.

## Create publication-ready tables summarizing a linear model fit

Use these functions if you have fit a linear model in R using the `lm` function and would like to compile the results into a concise set of metrics. To illustrate how to use these functions, I will use data from the `iris` dataset that comes built-in with R:


```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

These functions have a few dependencies, which I load below:


```r
library(dplyr)
library(weights)
library(stringr)
library(tidyr)
```

Consider the case of a simple multiple linear regression analysis. For the `iris` dataset, I examine whether petal length and/or petal width are associated with sepal length:


```r
lm.overall <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
```

The call to `create_lm_table` below generates a dataframe displaying the coefficients, their standard errors and confidence intervals, and associated *t*- and *p*-values. The first argument is the output of the call to the `lm` function. The second argument `include.intercept = 1` is included to indicate that I would like a row indicating these values for the intercept term. The third argument `vector.of.term.names` allows me to customize the names of the terms that will be displayed in the first column of the dataframe


```r
create_lm_table(lm.object = lm.overall, 
                include.intercept = 1, 
                vector.of.term.names = c('Intercept', 'Petal Length', 'Petal Width'))
```

```
##      Predictor  Beta    SE         95% CI     t     p
## 1    Intercept 4.191 0.097   4.382, 3.999 43.18 <.001
## 2 Petal Length 0.542 0.069   0.679, 0.405  7.82 <.001
## 3  Petal Width -0.32  0.16 -0.002, -0.637 -1.99  .048
```

The call to `create_lm_summary` below generates a string summarizing the fit of the regression model. This function takes only one argument, the output of the call to the `lm` function: 


```r
create_lm_summary(lm.object = lm.overall)
```

```
## [1] "F(2, 147) = 240.95, p < .001, R^2 = 0.766, adjusted R^2 = 0.763"
```

Finally, I use the `kableExtra` package to combine these outputs into a single, publication-ready table:


```r
library(kableExtra)
table_overall <- create_lm_table(lm.object = lm.overall, 
                                 include.intercept = 1, 
                                 vector.of.term.names = c('Intercept', 'Petal Length', 'Petal Width'))
kable(table_overall) %>%
  kable_styling(full_width = F,
                'striped',
                position = 'left') %>%
  pack_rows(str_c(create_lm_summary(lm.object = lm.overall)),
            1, nrow(table_overall))
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Predictor </th>
   <th style="text-align:left;"> Beta </th>
   <th style="text-align:left;"> SE </th>
   <th style="text-align:left;"> 95% CI </th>
   <th style="text-align:left;"> t </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="6" style="border-bottom: 1px solid;"><strong>F(2, 147) = 240.95, p &lt; .001, R^2 = 0.766, adjusted R^2 = 0.763</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:left;"> 4.191 </td>
   <td style="text-align:left;"> 0.097 </td>
   <td style="text-align:left;"> 4.382, 3.999 </td>
   <td style="text-align:left;"> 43.18 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Length </td>
   <td style="text-align:left;"> 0.542 </td>
   <td style="text-align:left;"> 0.069 </td>
   <td style="text-align:left;"> 0.679, 0.405 </td>
   <td style="text-align:left;"> 7.82 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Width </td>
   <td style="text-align:left;"> -0.32 </td>
   <td style="text-align:left;"> 0.16 </td>
   <td style="text-align:left;"> -0.002, -0.637 </td>
   <td style="text-align:left;"> -1.99 </td>
   <td style="text-align:left;"> .048 </td>
  </tr>
</tbody>
</table>

Next, I consider the case of a regression analysis with multiple groups, where I might want to create a table in which coefficients, etc. for each group are stacked on top of one another. I use the `iris` dataset again, this time performing a separate regression of petal length and width on sepal length, for each iris species separately:


```r
lm.setosa <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris %>% filter(Species == 'setosa'))
lm.versicolor <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris %>% filter(Species == 'versicolor'))
lm.virginica <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris %>% filter(Species == 'virginica'))
```

I consolidate the steps above to create a single, composite table summarizing the output of all three model fits:


```r
table_setosa <- create_lm_table(lm.object = lm.setosa, 
                                 include.intercept = 1, 
                                 vector.of.term.names = c('Intercept', 'Petal Length', 'Petal Width'))
table_versicolor <- create_lm_table(lm.object = lm.versicolor, 
                                 include.intercept = 1, 
                                 vector.of.term.names = c('Intercept', 'Petal Length', 'Petal Width'))
table_virginica <- create_lm_table(lm.object = lm.virginica, 
                                 include.intercept = 1, 
                                 vector.of.term.names = c('Intercept', 'Petal Length', 'Petal Width'))

kable(as.data.frame(rbind(table_setosa,
                          table_versicolor,
                          table_virginica))) %>%
  kable_styling(full_width = F, 'striped', position = 'left') %>%
  pack_rows(str_c('Setosa, ', create_lm_summary(lm.object = lm.setosa), sep = ''), 1, nrow(table_setosa)) %>%
  pack_rows(str_c('Versicolor, ', create_lm_summary(lm.object = lm.versicolor), sep = ''), nrow(table_setosa)+1, nrow(table_setosa)+nrow(table_versicolor)) %>%
  pack_rows(str_c('Virginica, ', create_lm_summary(lm.object = lm.virginica), sep = ''), nrow(table_setosa)+nrow(table_versicolor)+1, nrow(table_setosa)+nrow(table_versicolor)+nrow(table_virginica))
```

<table class="table table-striped" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Predictor </th>
   <th style="text-align:left;"> Beta </th>
   <th style="text-align:left;"> SE </th>
   <th style="text-align:left;"> 95% CI </th>
   <th style="text-align:left;"> t </th>
   <th style="text-align:left;"> p </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="6" style="border-bottom: 1px solid;"><strong>Setosa, F(2, 47) = 2.96, p = , R^2 = 0.112, adjusted R^2 = 0.074</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:left;"> 4.248 </td>
   <td style="text-align:left;"> 0.411 </td>
   <td style="text-align:left;"> 5.075, 3.42 </td>
   <td style="text-align:left;"> 10.32 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Length </td>
   <td style="text-align:left;"> 0.399 </td>
   <td style="text-align:left;"> 0.296 </td>
   <td style="text-align:left;"> 0.994, -0.196 </td>
   <td style="text-align:left;"> 1.35 </td>
   <td style="text-align:left;"> .184 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Width </td>
   <td style="text-align:left;"> 0.712 </td>
   <td style="text-align:left;"> 0.487 </td>
   <td style="text-align:left;"> 1.693, -0.268 </td>
   <td style="text-align:left;"> 1.46 </td>
   <td style="text-align:left;"> .151 </td>
  </tr>
  <tr grouplength="3"><td colspan="6" style="border-bottom: 1px solid;"><strong>Versicolor, F(2, 47) = 31.71, p &lt; .001, R^2 = 0.574, adjusted R^2 = 0.556</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:left;"> 2.381 </td>
   <td style="text-align:left;"> 0.449 </td>
   <td style="text-align:left;"> 3.284, 1.477 </td>
   <td style="text-align:left;"> 5.3 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Length </td>
   <td style="text-align:left;"> 0.934 </td>
   <td style="text-align:left;"> 0.169 </td>
   <td style="text-align:left;"> 1.275, 0.594 </td>
   <td style="text-align:left;"> 5.52 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Width </td>
   <td style="text-align:left;"> -0.32 </td>
   <td style="text-align:left;"> 0.402 </td>
   <td style="text-align:left;"> 0.489, -1.129 </td>
   <td style="text-align:left;"> -0.8 </td>
   <td style="text-align:left;"> .430 </td>
  </tr>
  <tr grouplength="3"><td colspan="6" style="border-bottom: 1px solid;"><strong>Virginica, F(2, 47) = 69.35, p &lt; .001, R^2 = 0.747, adjusted R^2 = 0.736</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:left;"> 1.052 </td>
   <td style="text-align:left;"> 0.514 </td>
   <td style="text-align:left;"> 2.085, 0.018 </td>
   <td style="text-align:left;"> 2.05 </td>
   <td style="text-align:left;"> .046 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Length </td>
   <td style="text-align:left;"> 0.995 </td>
   <td style="text-align:left;"> 0.089 </td>
   <td style="text-align:left;"> 1.174, 0.815 </td>
   <td style="text-align:left;"> 11.14 </td>
   <td style="text-align:left;"> &lt;.001 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Petal Width </td>
   <td style="text-align:left;"> 0.007 </td>
   <td style="text-align:left;"> 0.179 </td>
   <td style="text-align:left;"> 0.368, -0.354 </td>
   <td style="text-align:left;"> 0.04 </td>
   <td style="text-align:left;"> .969 </td>
  </tr>
</tbody>
</table>
