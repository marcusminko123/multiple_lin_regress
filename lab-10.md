Lab 10 - Grading the professor, Pt. 2
================
Marcus Minko
3/20/2022

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
?evals
view(evals)

#Simple Linear Regression
m_bty = lm(score ~ bty_avg, data = evals)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

1- m_bty = 3.88 +.07(bty_avg) R-squared = .04, Adj R sq = .03 2- Only
about 3-4% of the variance in scores is explained by this model.

### Multiple Linear Regression

``` r
m_bty_gen = lm(score~bty_avg + gender, data = evals)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

``` r
#m_bty_gen = 3.88 +.07(bty_avg) + (.17)(gender) 
#3- All else being equal, men attain scores .17 higher than females on ratings
#4- Roughly 6% of the variance in scores is explained by gender and beauty rating
#5- m_bty_gen = 3.88 +.07(bty_avg) + .17
#6- Males
#7- Males tend to recieve slightly higher scores than females despite being rated equally beautiful
#8- The R-squared of m_bty_gen is about .03 greater then the R-squared of m_bty meaning that gender has about the same influence on scores as beauty, though both are quite low around 3% of variance explained
#9- The slopes for bty_avg are about the same 
#10-
m_bty_rank = lm(score~bty_avg + rank, data = evals)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

``` r
#m_bty_rank = 3.98 + .07(bty_avg) -.16(tenure track) -.13(tenured)
#Assuming equal beauty ratings, teaching faculty received scores .16 and .13 higher than tenure-track and tenured faculty, respectively.
```

### More Predictors

``` r
#11- I would not expect class credits to have much influence on scores
#12- 
m_cls = lm(score~cls_credits, data = evals)
summary(m_cls)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_credits, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84702 -0.34702  0.05298  0.35298  0.85298 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            4.14702    0.02552 162.494  < 2e-16 ***
    ## cls_creditsone credit  0.47520    0.10568   4.496 8.75e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5329 on 461 degrees of freedom
    ## Multiple R-squared:  0.04202,    Adjusted R-squared:  0.03994 
    ## F-statistic: 20.22 on 1 and 461 DF,  p-value: 8.751e-06

``` r
# m_cls = 4.15 + (.48)(cls_credits) R-squared is about the same as gender and beauty
#13- It seems unnecessary to include cls_did_eval because if you know the number of students and the percentage that did evals, you don't need cls_did_eval
#14-
m_all= lm(score~ gender + rank + ethnicity + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data=evals)
summary(m_all)
```

    ## 
    ## Call:
    ## lm(formula = score ~ gender + rank + ethnicity + language + age + 
    ##     cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84482 -0.31367  0.08559  0.35732  1.10105 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.5305036  0.2408200  14.660  < 2e-16 ***
    ## gendermale             0.1786166  0.0515346   3.466 0.000579 ***
    ## ranktenure track      -0.1070121  0.0820250  -1.305 0.192687    
    ## ranktenured           -0.0450371  0.0652185  -0.691 0.490199    
    ## ethnicitynot minority  0.1869649  0.0775329   2.411 0.016290 *  
    ## languagenon-english   -0.1268254  0.1080358  -1.174 0.241048    
    ## age                   -0.0066498  0.0030830  -2.157 0.031542 *  
    ## cls_perc_eval          0.0056996  0.0015514   3.674 0.000268 ***
    ## cls_students           0.0004455  0.0003585   1.243 0.214596    
    ## cls_levelupper         0.0187105  0.0555833   0.337 0.736560    
    ## cls_profssingle       -0.0085751  0.0513527  -0.167 0.867458    
    ## cls_creditsone credit  0.5087427  0.1170130   4.348  1.7e-05 ***
    ## bty_avg                0.0612651  0.0166755   3.674 0.000268 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.504 on 450 degrees of freedom
    ## Multiple R-squared:  0.1635, Adjusted R-squared:  0.1412 
    ## F-statistic: 7.331 on 12 and 450 DF,  p-value: 2.406e-12

``` r
#15-
m_subset = lm(score~ bty_avg + gender + ethnicity + cls_perc_eval + cls_credits, data=evals)
summary(m_subset)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender + ethnicity + cls_perc_eval + 
    ##     cls_credits, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8857 -0.3294  0.1066  0.3774  1.0540 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3.137381   0.146450  21.423  < 2e-16 ***
    ## bty_avg               0.073644   0.015773   4.669 3.98e-06 ***
    ## gendermale            0.157832   0.048493   3.255 0.001219 ** 
    ## ethnicitynot minority 0.233794   0.071275   3.280 0.001117 ** 
    ## cls_perc_eval         0.005208   0.001443   3.608 0.000343 ***
    ## cls_creditsone credit 0.541067   0.104669   5.169 3.52e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5053 on 457 degrees of freedom
    ## Multiple R-squared:  0.146,  Adjusted R-squared:  0.1366 
    ## F-statistic: 15.62 on 5 and 457 DF,  p-value: 3.338e-14

``` r
#score = 3.14 +.16(gender) + .23(notminority) + .07(bty_avg) + .005(cls_perc_eval) + .54(one credit)
#16- All else held constant, males score .16 points higher than females on average. All others held constant, a 1 point increase in beauty average is associated with an increase in .07 in their scores. 
#17- Professor would be an attractive, majority male teaching a one-credit course with a higher percentage of the students completing evaluations.
#18- I think these results could be generalized to schools with comparable demographics to the University of Texas.
```
