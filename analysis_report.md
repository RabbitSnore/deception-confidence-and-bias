Confidence and Bias – Analysis
================
Timothy J. Luke
2023-04-19

# Does sender confidence influence bias?

## Raw judgments

To assess whether sender confidence (a manipulated variable) increases
truth bias, we conducted a mixed effects logistic regression analysis.
We began with a model in which veracity judgments (0 = lie, 1 = truth)
were predicted by actual sender veracity (lie vs. truth) and by message
content (holiday, bereavement, accident, and quarrel; dummy coded with
holiday as the reference group), as well as random intercepts for each
receiver and each sender. We then fit a model that added confidence
condition and a third model that added the interaction term between
content and confidence condition. Then we compared the models to select
one using a likelihood ratio test. As can be seen below, the test
suggested better fit with the addition of the interaction term.

``` r
lrt_judge_fixed
```

    ## Data: judge_long
    ## Models:
    ## judge_fixed_base: judgment ~ veracity + content + (1 + veracity | id) + (1 | sender)
    ## judge_fixed_conf: judgment ~ veracity + content + confidence_condition + (1 + veracity | id) + (1 | sender)
    ## judge_fixed_conf_int: judgment ~ veracity + content * confidence_condition + (1 + veracity | id) + (1 | sender)
    ## judge_fixed_conf_int_2: judgment ~ (veracity + content + confidence_condition)^2 + (1 + veracity | id) + (1 | sender)
    ## judge_fixed_conf_int_3: judgment ~ (veracity + content + confidence_condition)^3 + (1 + veracity | id) + (1 | sender)
    ##                        npar    AIC    BIC  logLik deviance   Chisq Df
    ## judge_fixed_base          9 4549.2 4604.5 -2265.6   4531.2           
    ## judge_fixed_conf         10 4543.2 4604.6 -2261.6   4523.2  7.9849  1
    ## judge_fixed_conf_int     13 4532.4 4612.2 -2253.2   4506.4 16.8078  3
    ## judge_fixed_conf_int_2   17 4517.9 4622.2 -2241.9   4483.9 22.5530  4
    ## judge_fixed_conf_int_3   20 4518.6 4641.4 -2239.3   4478.6  5.2262  3
    ##                        Pr(>Chisq)    
    ## judge_fixed_base                     
    ## judge_fixed_conf        0.0047168 ** 
    ## judge_fixed_conf_int    0.0007741 ***
    ## judge_fixed_conf_int_2  0.0001555 ***
    ## judge_fixed_conf_int_3  0.1559610    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The coefficient for sender confidence was significant and positive,
indicating that more confident senders were viewed as more likely to be
truthful when talking about holidays. Additionally, the interaction
terms for sender confidence with accident messages and with quarrel
messages were significant and negative. These results suggest that
sender confidence had little or no effect on veracity judgments for
messages about accidents, and higher sender confidence was associated
with fewer truth judgments for messages about quarrels. In short, the
confidence with which senders presented their stories appeared to
influence receivers’ veracity judgments, but this relationship was
moderated by the content of the message. Additionally, sender veracity
significantly interacted with message content, such that truthful
messages about bereavements and quarrels were more likely to be judged
as truths, compared to messages about holidays. Table 2 displays the raw
rates of truth judgments as well as mean predicted truth judgment rates
derived from the retained model for each condition.

``` r
summary(judge_fixed_conf_int)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: 
    ## judgment ~ veracity + content * confidence_condition + (1 + veracity |  
    ##     id) + (1 | sender)
    ##    Data: judge_long
    ## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4532.4   4612.2  -2253.2   4506.4     3411 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7806 -1.0376  0.6050  0.8531  1.4825 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr
    ##  id     (Intercept) 0.018082 0.13447      
    ##         veracityt   0.009075 0.09527  1.00
    ##  sender (Intercept) 0.052472 0.22907      
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##                                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              0.33208    0.20994   1.582 0.113693
    ## veracityt                                0.21550    0.17715   1.217 0.223788
    ## contentbereavement                      -0.01598    0.26868  -0.059 0.952571
    ## contentaccident                         -0.47780    0.26785  -1.784 0.074453
    ## contentquarrel                          -0.51647    0.26859  -1.923 0.054491
    ## confidence_condition                     0.57864    0.14923   3.878 0.000105
    ## contentbereavement:confidence_condition -0.21985    0.20688  -1.063 0.287935
    ## contentaccident:confidence_condition    -0.42691    0.20225  -2.111 0.034792
    ## contentquarrel:confidence_condition     -0.79927    0.20469  -3.905 9.43e-05
    ##                                            
    ## (Intercept)                                
    ## veracityt                                  
    ## contentbereavement                         
    ## contentaccident                         .  
    ## contentquarrel                          .  
    ## confidence_condition                    ***
    ## contentbereavement:confidence_condition    
    ## contentaccident:confidence_condition    *  
    ## contentquarrel:confidence_condition     ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) vrctyt cntntb cntntc cntntq cnfdn_ cntntb:_ cntntc:_
    ## veracityt   -0.421                                                     
    ## cntntbrvmnt -0.641  0.003                                              
    ## contntccdnt -0.642  0.001  0.501                                       
    ## contentqrrl -0.640  0.000  0.500  0.502                                
    ## cnfdnc_cndt -0.318 -0.002  0.246  0.246  0.245                         
    ## cntntbrvm:_  0.225  0.003 -0.355 -0.177 -0.177 -0.711                  
    ## cntntccdn:_  0.231  0.001 -0.181 -0.355 -0.181 -0.727  0.524           
    ## cntntqrrl:_  0.229 -0.002 -0.179 -0.179 -0.357 -0.719  0.518    0.530  
    ## optimizer (bobyqa) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

### Mean predicted truth judgment rates

For a more intuitive examination of the results of the model above, we
can extract predicted truth judgment rates and calculate means for each
condition, sender, and content type. For each mean predicted rate of
truth judgments, I computed 95% parametric bootstrap confidence
intervals using 500 simulations.

``` r
boot_ci_fixed_inter_2
```

    ##                     estimate     ci_lb     ci_ub
    ## accident_low_t     0.4584469 0.4012599 0.5331768
    ## accident_low_f     0.5224657 0.4615928 0.5816012
    ## bereavement_low_t  0.6495406 0.5895720 0.7069806
    ## bereavement_low_f  0.5569975 0.4955340 0.6259484
    ## holiday_low_t      0.5774612 0.5176668 0.6389609
    ## holiday_low_f      0.6379456 0.5750714 0.6964053
    ## quarrel_low_t      0.5967389 0.5371046 0.6570793
    ## quarrel_low_f      0.3665838 0.3078988 0.4288884
    ## accident_high_t    0.4990451 0.4340169 0.5654272
    ## accident_high_f    0.5574281 0.4970595 0.6171265
    ## bereavement_high_t 0.7285242 0.6682470 0.7787783
    ## bereavement_high_f 0.6404329 0.5802357 0.7011472
    ## holiday_high_t     0.7112198 0.6585390 0.7659205
    ## holiday_high_f     0.7565163 0.7017986 0.8069508
    ## quarrel_high_t     0.5448146 0.4737345 0.6024823
    ## quarrel_high_f     0.3140602 0.2574792 0.3641243

## Signal detection

As a robustness check, I assessed whether confidence condition predicted
the signal detection index “c” – a measure of bias. To perform this
check, after calculating c for each receiver (collapsed across the
content types), I fit a linear regression model predicting c from
confidence condition. As can be seen below, the results of this analysis
support the results of the models examining raw judgments.

``` r
summary(sdt_model_base)
```

    ## 
    ## Call:
    ## lm(formula = c ~ confidence_condition, data = sdt_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.38534 -0.21321  0.04899  0.31119  1.17777 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.10378    0.02804   3.702 0.000242 ***
    ## confidence_condition  0.10943    0.03974   2.753 0.006151 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4111 on 426 degrees of freedom
    ## Multiple R-squared:  0.01748,    Adjusted R-squared:  0.01518 
    ## F-statistic: 7.581 on 1 and 426 DF,  p-value: 0.006151

We think the analyses of raw judgments are more informative (and more
intuitive) than the signal detection analysis. But it’s nice to see this
kind of consistency.

We can see the results in the descriptive statistics below as well.

``` r
sdt_data %>% 
  group_by(confidence_condition) %>% 
  summarise(
    mean_c   = mean(c),
    sd_c     = sd(c),
    median_c = median(c),
  )
```

    ## # A tibble: 2 × 4
    ##   confidence_condition mean_c  sd_c median_c
    ##                  <dbl>  <dbl> <dbl>    <dbl>
    ## 1                    0  0.104 0.412    0    
    ## 2                    1  0.213 0.410    0.262

## Message content as a random effect

Another way of analyzing these data is to model message content as a
random effect, rather than a fixed effect (as above). This approach
would imply that the four content types represent a random sample from a
population of possible narrative types. Whether this approach is more or
less appropriate than the fixed effect approach taken above can be
debated. However, the results are substantively the same as the fixed
effect approach presented above.

We start by comparing three models: (1) an initial model with actual
sender veracity as a predictor of veracity judgments, with random
intercepts for receivers and for senders nested in content types, (2) a
model which adds confidence condition as a predictor, and (3) a model
that adds random slopes for confidence condition for each content type
(which represent an interaction between content and confidence). We
compared these models with a likelihood ratio test.

As above, the model implying an interaction between content and
confidence appears to be the best fitting of the three.

``` r
lrt_judge
```

    ## Data: judge_long
    ## Models:
    ## judge_model_base: judgment ~ veracity + (1 | id) + (1 | content/sender)
    ## judge_model_conf: judgment ~ veracity + confidence_condition + (1 | id) + (1 | content/sender)
    ## judge_model_conf_int: judgment ~ veracity + confidence_condition + (1 | id) + (1 + confidence_condition | content) + (1 | content:sender)
    ##                      npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## judge_model_base        5 4550.4 4581.1 -2270.2   4540.4                     
    ## judge_model_conf        6 4544.3 4581.2 -2266.2   4532.3  8.051  1   0.004548
    ## judge_model_conf_int    8 4535.8 4584.9 -2259.9   4519.8 12.580  2   0.001855
    ##                        
    ## judge_model_base       
    ## judge_model_conf     **
    ## judge_model_conf_int **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The model with random slopes fits the data the best, even though none of
the fixed effects are significant (see below). In the model with only
random intercepts, we see that there is a tendency for sender confidence
to increase truth judgments on average, but adding the interaction
substantially increases the standard error for this coefficient. Thus,
we see evidence for an interaction between content type and confidence
condition, such that sender confidence has effects of different sizes
and/or directions depending on the content of the message.

These models are generally consistent with the fixed content effect
approach above, as well as the signal detection model.

``` r
summary(judge_model_conf)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: judgment ~ veracity + confidence_condition + (1 | id) + (1 |  
    ##     content/sender)
    ##    Data: judge_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4544.3   4581.2  -2266.2   4532.3     3418 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6103 -1.0703  0.6445  0.8275  1.4705 
    ## 
    ## Random effects:
    ##  Groups         Name        Variance Std.Dev.
    ##  id             (Intercept) 0.02480  0.1575  
    ##  sender:content (Intercept) 0.11418  0.3379  
    ##  content        (Intercept) 0.07977  0.2824  
    ## Number of obs: 3424, groups:  id, 428; sender:content, 8; content, 4
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)           0.08086    0.22883   0.353  0.72381   
    ## veracityt             0.21241    0.24927   0.852  0.39413   
    ## confidence_condition  0.20673    0.07264   2.846  0.00443 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) vrctyt
    ## veracityt   -0.545       
    ## cnfdnc_cndt -0.156  0.002

``` r
summary(judge_model_conf_int)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: judgment ~ veracity + confidence_condition + (1 | id) + (1 +  
    ##     confidence_condition | content) + (1 | content:sender)
    ##    Data: judge_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4535.8   4584.9  -2259.9   4519.8     3416 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7543 -1.0669  0.6180  0.8591  1.4574 
    ## 
    ## Random effects:
    ##  Groups         Name                 Variance Std.Dev. Corr
    ##  id             (Intercept)          0.02753  0.1659       
    ##  content:sender (Intercept)          0.07529  0.2744       
    ##  content        (Intercept)          0.04589  0.2142       
    ##                 confidence_condition 0.07329  0.2707   1.00
    ## Number of obs: 3424, groups:  id, 428; content:sender, 8; content, 4
    ## 
    ## Fixed effects:
    ##                      Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)           0.07986    0.18478   0.432    0.666
    ## veracityt             0.21244    0.20672   1.028    0.304
    ## confidence_condition  0.21616    0.15386   1.405    0.160
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) vrctyt
    ## veracityt   -0.560       
    ## cnfdnc_cndt  0.420  0.000
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

### Raw veracity judgment rates

Here are the overall veracity judgment rates. People were somewhat
truth-biased.

``` r
table(judge_long$judgment)/sum(table(judge_long$judgment))
```

    ## 
    ##         0         1 
    ## 0.4307827 0.5692173

People were more truth-biased when the sender was more confident, across
the content types.

``` r
judge_long %>% 
  group_by(confidence_condition) %>% 
  summarise(
    truth_rate = sum(judgment == 1) / n()
  ) %>% 
  knitr::kable()
```

| confidence_condition | truth_rate |
|---------------------:|-----------:|
|                    0 |  0.5453488 |
|                    1 |  0.5933099 |

``` r
judge_long %>% 
  group_by(content) %>% 
  summarise(
    truth_rate = sum(judgment == 1) / n()
  ) %>% 
  knitr::kable()
```

| content     | truth_rate |
|:------------|-----------:|
| holiday     |  0.6693925 |
| bereavement |  0.6425234 |
| accident    |  0.5093458 |
| quarrel     |  0.4556075 |

``` r
judge_long %>% 
  group_by(content, confidence_condition) %>% 
  summarise(
    truth_rate = sum(judgment == 1) / n()
  ) %>% 
  knitr::kable()
```

    ## `summarise()` has grouped output by 'content'. You can override using the
    ## `.groups` argument.

| content     | confidence_condition | truth_rate |
|:------------|---------------------:|-----------:|
| holiday     |                    0 |  0.6069767 |
| holiday     |                    1 |  0.7323944 |
| bereavement |                    0 |  0.6023256 |
| bereavement |                    1 |  0.6830986 |
| accident    |                    0 |  0.4906977 |
| accident    |                    1 |  0.5281690 |
| quarrel     |                    0 |  0.4813953 |
| quarrel     |                    1 |  0.4295775 |

``` r
judge_long %>% 
  group_by(content, confidence_condition, veracity) %>% 
  summarise(
    truth_rate = sum(judgment == 1) / n()
  ) %>% 
  knitr::kable()
```

    ## `summarise()` has grouped output by 'content', 'confidence_condition'. You can
    ## override using the `.groups` argument.

| content     | confidence_condition | veracity | truth_rate |
|:------------|---------------------:|:---------|-----------:|
| holiday     |                    0 | f        |  0.6418605 |
| holiday     |                    0 | t        |  0.5720930 |
| holiday     |                    1 | f        |  0.7511737 |
| holiday     |                    1 | t        |  0.7136150 |
| bereavement |                    0 | f        |  0.5255814 |
| bereavement |                    0 | t        |  0.6790698 |
| bereavement |                    1 | f        |  0.6713615 |
| bereavement |                    1 | t        |  0.6948357 |
| accident    |                    0 | f        |  0.5395349 |
| accident    |                    0 | t        |  0.4418605 |
| accident    |                    1 | f        |  0.5399061 |
| accident    |                    1 | t        |  0.5164319 |
| quarrel     |                    0 | f        |  0.3767442 |
| quarrel     |                    0 | t        |  0.5860465 |
| quarrel     |                    1 | f        |  0.3051643 |
| quarrel     |                    1 | t        |  0.5539906 |

# Does sender confidence influence receiver accuracy?

## Raw judgments

To assess whether sender confidence influenced receiver accuracy, we
conducted a mixed effects logistic regression analysis. As with the
analysis of truth judgments, we began with a model in which judgment
accuracy (0 = incorrect, 1 = correct) were predicted by actual sender
veracity (lie vs. truth) and by message content (holiday, bereavement,
accident, and quarrel; dummy coded with holiday as the reference group),
as well as random intercepts for each receiver and each sender. We then
fit a model that added confidence condition and a third model that added
the interaction term between content and confidence condition. Then we
compared the models to select one using a likelihood ratio test. As can
be seen below, the test suggested the two subsequent models after the
initial model offered no significant improvement, but the model adding
the three-way interactions fit best.

``` r
lrt_acc
```

    ## Data: accuracy_long
    ## Models:
    ## acc_model_base: accuracy ~ veracity + content + (1 + veracity | id) + (1 | sender)
    ## acc_model_conf: accuracy ~ veracity + content + confidence_condition + (1 + veracity | id) + (1 | sender)
    ## acc_model_conf_int: accuracy ~ veracity + content * confidence_condition + (1 + veracity | id) + (1 | sender)
    ## acc_model_conf_int_2: accuracy ~ (veracity + content + confidence_condition)^2 + (1 + veracity | id) + (1 | sender)
    ## acc_model_conf_int_3: accuracy ~ (veracity + content + confidence_condition)^3 + (1 + veracity | id) + (1 | sender)
    ##                      npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)
    ## acc_model_base          9 4555.8 4611.1 -2268.9   4537.8                      
    ## acc_model_conf         10 4557.8 4619.2 -2268.9   4537.8  0.0057  1  0.9399545
    ## acc_model_conf_int     13 4558.1 4637.9 -2266.0   4532.1  5.7615  3  0.1238108
    ## acc_model_conf_int_2   17 4529.3 4633.7 -2247.7   4495.3 36.7648  4  2.014e-07
    ## acc_model_conf_int_3   20 4518.6 4641.4 -2239.3   4478.6 16.6721  3  0.0008254
    ##                         
    ## acc_model_base          
    ## acc_model_conf          
    ## acc_model_conf_int      
    ## acc_model_conf_int_2 ***
    ## acc_model_conf_int_3 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

In the retained model, the coefficient for sender veracity was
significant and positive, indicating that for messages about holidays,
receivers judged truthful messages more accurately. The coefficient for
sender confidence was significant and negative, suggesting a decrease in
accuracy for messages about holidays when senders communicated more
confidently. The significant interaction between sender confidence and
veracity indicates that this increase was especially pronounced for
truthful holiday messages communicated confidently. We also see
significant increases in accuracy for messages about accidents and about
quarrels. Accuracy for deceptive messages about quarrels was further,
though only moderately, increased when senders conveyed their messages
more confidently. Finally, the significant three-way interactions
indicate that the improvement in accuracy conferred by sender confidence
observed for messages for holidays was mitigated for messages about
accidents and quarrels. Raw accuracy rates and mean predicted accuracy
rates derived from the retained model for each condition are displayed
in Table 4. Given the pattern of response biases we observed above, all
these changes in accuracy are likely largely the result of shifting
response bias, rather than improved discrimination.

``` r
summary(acc_model_conf_int_3)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: accuracy ~ (veracity + content + confidence_condition)^3 + (1 +  
    ##     veracity | id) + (1 | sender)
    ##    Data: accuracy_long
    ## Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+05))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4518.6   4641.4  -2239.3   4478.6     3404 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.6267 -0.9321  0.6243  0.8661  1.7957 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  id     (Intercept) 0.02031  0.1425        
    ##         veracityt   0.13985  0.3740   -1.00
    ##  sender (Intercept) 0.00000  0.0000        
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##                                                   Estimate Std. Error z value
    ## (Intercept)                                       -0.58634    0.14297  -4.101
    ## veracityt                                          0.88062    0.20067   4.389
    ## contentbereavement                                 0.48341    0.19768   2.445
    ## contentaccident                                    0.42706    0.19785   2.159
    ## contentquarrel                                     1.09223    0.20077   5.440
    ## confidence_condition                              -0.52366    0.21388  -2.448
    ## veracityt:contentbereavement                      -0.01856    0.28272  -0.066
    ## veracityt:contentaccident                         -0.95811    0.27852  -3.440
    ## veracityt:contentquarrel                          -1.03424    0.28099  -3.681
    ## veracityt:confidence_condition                     1.15392    0.29906   3.859
    ## contentbereavement:confidence_condition           -0.09126    0.29267  -0.312
    ## contentaccident:confidence_condition               0.52216    0.28873   1.808
    ## contentquarrel:confidence_condition                0.84457    0.29618   2.852
    ## veracityt:contentbereavement:confidence_condition -0.46477    0.41492  -1.120
    ## veracityt:contentaccident:confidence_condition    -0.84903    0.40500  -2.096
    ## veracityt:contentquarrel:confidence_condition     -1.60738    0.41114  -3.910
    ##                                                   Pr(>|z|)    
    ## (Intercept)                                       4.11e-05 ***
    ## veracityt                                         1.14e-05 ***
    ## contentbereavement                                0.014469 *  
    ## contentaccident                                   0.030887 *  
    ## contentquarrel                                    5.32e-08 ***
    ## confidence_condition                              0.014347 *  
    ## veracityt:contentbereavement                      0.947650    
    ## veracityt:contentaccident                         0.000582 ***
    ## veracityt:contentquarrel                          0.000233 ***
    ## veracityt:confidence_condition                    0.000114 ***
    ## contentbereavement:confidence_condition           0.755186    
    ## contentaccident:confidence_condition              0.070535 .  
    ## contentquarrel:confidence_condition               0.004350 ** 
    ## veracityt:contentbereavement:confidence_condition 0.262652    
    ## veracityt:contentaccident:confidence_condition    0.036050 *  
    ## veracityt:contentquarrel:confidence_condition     9.25e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 16 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## optimizer (bobyqa) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

### Accuracy rates

It can be informative to examine the raw accuracy rates across and
between conditions.

The overall accuracy rate was approximately 52.5%.

``` r
table(accuracy_long$accuracy)/sum(table(accuracy_long$accuracy))
```

    ## 
    ##         0         1 
    ## 0.4745911 0.5254089

The effect of the truth bias on accuracy is evident when splitting
accuracy by veracity condition.

``` r
accuracy_long %>% 
  group_by(veracity) %>% 
  summarise(
    acc_rate = sum(accuracy == 1) / n()
  ) %>% 
  knitr::kable()
```

| veracity |  acc_rate |
|:---------|----------:|
| f        | 0.4561916 |
| t        | 0.5946262 |

Overall accuracy between the content types varied slightly, but the
model above suggests that these differences were not estimated precisely
enough for us to conclude that they are reliable.

``` r
accuracy_long %>% 
  group_by(content) %>% 
  summarise(
    acc_rate = sum(accuracy == 1) / n()
  ) %>% 
  knitr::kable()
```

| content     |  acc_rate |
|:------------|----------:|
| holiday     | 0.4731308 |
| bereavement | 0.5443925 |
| accident    | 0.4696262 |
| quarrel     | 0.6144860 |

Differences across content in the extent to which truth bias influenced
accuracy are what you would expect given the differences in truth bias
across content. That is, when there is greater truth bias, there is
better accuracy for truthful messages. No surprises there.

``` r
accuracy_long %>% 
  group_by(content, veracity) %>% 
  summarise(
    acc_rate = sum(accuracy == 1) / n()
  ) %>% 
  knitr::kable()
```

    ## `summarise()` has grouped output by 'content'. You can override using the
    ## `.groups` argument.

| content     | veracity |  acc_rate |
|:------------|:---------|----------:|
| holiday     | f        | 0.3037383 |
| holiday     | t        | 0.6425234 |
| bereavement | f        | 0.4018692 |
| bereavement | t        | 0.6869159 |
| accident    | f        | 0.4602804 |
| accident    | t        | 0.4789720 |
| quarrel     | f        | 0.6588785 |
| quarrel     | t        | 0.5700935 |

``` r
accuracy_long %>% 
  group_by(content, confidence_condition, veracity) %>% 
  summarise(
    acc_rate = sum(accuracy == 1) / n()
  ) %>% 
  knitr::kable()
```

    ## `summarise()` has grouped output by 'content', 'confidence_condition'. You can
    ## override using the `.groups` argument.

| content     | confidence_condition | veracity |  acc_rate |
|:------------|---------------------:|:---------|----------:|
| holiday     |                    0 | f        | 0.3581395 |
| holiday     |                    0 | t        | 0.5720930 |
| holiday     |                    1 | f        | 0.2488263 |
| holiday     |                    1 | t        | 0.7136150 |
| bereavement |                    0 | f        | 0.4744186 |
| bereavement |                    0 | t        | 0.6790698 |
| bereavement |                    1 | f        | 0.3286385 |
| bereavement |                    1 | t        | 0.6948357 |
| accident    |                    0 | f        | 0.4604651 |
| accident    |                    0 | t        | 0.4418605 |
| accident    |                    1 | f        | 0.4600939 |
| accident    |                    1 | t        | 0.5164319 |
| quarrel     |                    0 | f        | 0.6232558 |
| quarrel     |                    0 | t        | 0.5860465 |
| quarrel     |                    1 | f        | 0.6948357 |
| quarrel     |                    1 | t        | 0.5539906 |

## Mean predicted accuracy

We can also calculate predicted accuracy rates for each condition based
on the retained model.

``` r
boot_ci_fixed_acc_inter_3
```

    ##                     estimate     ci_lb     ci_ub
    ## accident_low_t     0.4410930 0.3791267 0.5040982
    ## accident_low_f     0.4603044 0.3964070 0.5348836
    ## bereavement_low_t  0.6809963 0.6182056 0.7424653
    ## bereavement_low_f  0.4743246 0.4019499 0.5444064
    ## holiday_low_t      0.5729440 0.5095166 0.6418686
    ## holiday_low_f      0.3575416 0.2929529 0.4183365
    ## quarrel_low_t      0.5870583 0.5212606 0.6540393
    ## quarrel_low_f      0.6238316 0.5583198 0.6872415
    ## accident_high_t    0.5165668 0.4494919 0.5875054
    ## accident_high_f    0.4599492 0.3938083 0.5307914
    ## bereavement_high_t 0.6968592 0.6356615 0.7621520
    ## bereavement_high_f 0.3279560 0.2686042 0.3956610
    ## holiday_high_t     0.7157551 0.6589393 0.7721610
    ## holiday_high_f     0.2479663 0.1881606 0.3157302
    ## quarrel_high_t     0.5545953 0.4833601 0.6247683
    ## quarrel_high_f     0.6956615 0.6270206 0.7602191

## Signal detection

Consistent with the analysis of the raw judgments, when we fit a linear
model to predict d’ (a measure of discrimination) with sender confidence
as a predictor, we see no evidence that sender confidence influences
receiver accuracy.

``` r
summary(sdt_model_acc)
```

    ## 
    ## Call:
    ## lm(formula = dprime ~ confidence_condition, data = sdt_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9386 -0.6463 -0.1219  0.4025  2.4412 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)           0.12193    0.05050   2.415   0.0162 *
    ## confidence_condition  0.01072    0.07158   0.150   0.8811  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7404 on 426 degrees of freedom
    ## Multiple R-squared:  5.261e-05,  Adjusted R-squared:  -0.002295 
    ## F-statistic: 0.02241 on 1 and 426 DF,  p-value: 0.8811

# Does confidence predict accuracy?

To assess the extent to which confidence predicted accuracy (viz. are
correct judgments made more confidently?), we fit an additional logistic
mixed effects model predicting accuracy with sender veracity, content
type, and receiver confidence. We compared this model to the retained
model predicting accuracy above. Adding recevier confidence offered no
significant improvement to the model.

``` r
lrt_confidence
```

    ## Data: accuracy_long
    ## Models:
    ## acc_model_conf_int_3: accuracy ~ (veracity + content + confidence_condition)^3 + (1 + veracity | id) + (1 | sender)
    ## acc_recconf_model_comp: accuracy ~ (veracity + content + confidence_condition)^3 + confidence + (1 + veracity + confidence | id) + (1 | sender)
    ## acc_recconf_model_int: accuracy ~ (veracity + content + confidence_condition)^3 + confidence * confidence_condition + (1 + veracity + confidence | id) + (1 | sender)
    ##                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## acc_model_conf_int_3     20 4518.6 4641.4 -2239.3   4478.6                     
    ## acc_recconf_model_comp   24 4526.3 4673.6 -2239.1   4478.3 0.3505  4     0.9863
    ## acc_recconf_model_int    25 4526.2 4679.7 -2238.1   4476.2 2.0792  1     0.1493

For the sake of completeness, here is the main model output. As can be
seen, confidence did not appear to predict accuracy at all.

``` r
summary(acc_conf_model_base)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: accuracy ~ confidence + (1 + confidence | id) + (1 | sender)
    ##    Data: accuracy_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4556.4   4593.2  -2272.2   4544.4     3418 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4928 -0.9347  0.6803  0.8659  1.5299 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  id     (Intercept) 0.197927 0.44489       
    ##         confidence  0.002426 0.04926  -1.00
    ##  sender (Intercept) 0.275158 0.52456       
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 0.052330   0.245364   0.213    0.831
    ## confidence  0.006983   0.020410   0.342    0.732
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## confidence -0.638
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')
