Confidence and Bias – Analysis
================
Timothy J. Luke
2022-11-25

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
    ## judge_fixed_base: judgment ~ veracity + content + (1 | id) + (1 | sender)
    ## judge_fixed_conf: judgment ~ veracity + content + confidence_condition + (1 | id) + (1 | sender)
    ## judge_fixed_conf_int: judgment ~ veracity + content * confidence_condition + (1 | id) + (1 | sender)
    ## judge_fixed_conf_int_2: judgment ~ (veracity + content + confidence_condition)^2 + (1 | id) + (1 | sender)
    ## judge_fixed_conf_int_3: judgment ~ (veracity + content + confidence_condition)^3 + (1 | id) + (1 | sender)
    ##                        npar    AIC    BIC  logLik deviance   Chisq Df
    ## judge_fixed_base          7 4545.6 4588.5 -2265.8   4531.6           
    ## judge_fixed_conf          8 4539.5 4588.6 -2261.8   4523.5  8.0506  1
    ## judge_fixed_conf_int     11 4528.7 4596.3 -2253.4   4506.7 16.7776  3
    ## judge_fixed_conf_int_2   15 4514.2 4606.2 -2242.1   4484.2 22.5784  4
    ## judge_fixed_conf_int_3   18 4514.9 4625.4 -2239.5   4478.9  5.2444  3
    ##                        Pr(>Chisq)    
    ## judge_fixed_base                     
    ## judge_fixed_conf        0.0045488 ** 
    ## judge_fixed_conf_int    0.0007852 ***
    ## judge_fixed_conf_int_2  0.0001537 ***
    ## judge_fixed_conf_int_3  0.1547482    
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
    ## Formula: judgment ~ veracity + content * confidence_condition + (1 | id) +  
    ##     (1 | sender)
    ##    Data: judge_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4528.7   4596.3  -2253.4   4506.7     3413 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7829 -1.0359  0.6105  0.8491  1.4795 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  id     (Intercept) 0.02774  0.1665  
    ##  sender (Intercept) 0.05264  0.2294  
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##                                         Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                              0.33282    0.21023   1.583 0.113400
    ## veracityt                                0.21281    0.17721   1.201 0.229807
    ## contentbereavement                      -0.01699    0.26897  -0.063 0.949619
    ## contentaccident                         -0.47680    0.26816  -1.778 0.075394
    ## contentquarrel                          -0.51756    0.26887  -1.925 0.054230
    ## confidence_condition                     0.57778    0.14893   3.880 0.000105
    ## contentbereavement:confidence_condition -0.21930    0.20677  -1.061 0.288877
    ## contentaccident:confidence_condition    -0.42618    0.20207  -2.109 0.034941
    ## contentquarrel:confidence_condition     -0.79777    0.20447  -3.902 9.56e-05
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
    ## veracityt   -0.423                                                     
    ## cntntbrvmnt -0.641  0.003                                              
    ## contntccdnt -0.642  0.001  0.501                                       
    ## contentqrrl -0.640  0.000  0.500  0.502                                
    ## cnfdnc_cndt -0.317 -0.002  0.246  0.246  0.245                         
    ## cntntbrvm:_  0.225  0.004 -0.354 -0.177 -0.177 -0.711                  
    ## cntntccdn:_  0.231  0.001 -0.181 -0.355 -0.181 -0.728  0.524           
    ## cntntqrrl:_  0.229 -0.002 -0.179 -0.179 -0.356 -0.720  0.518    0.530

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
    ## accident_low_t     0.4587113 0.4102315 0.5227302
    ## accident_low_f     0.5225064 0.4647090 0.5839228
    ## bereavement_low_t  0.6487541 0.5923026 0.7009551
    ## bereavement_low_f  0.5572086 0.4920545 0.6121546
    ## holiday_low_t      0.5769142 0.5125063 0.6343389
    ## holiday_low_f      0.6384112 0.5818410 0.6932241
    ## quarrel_low_t      0.5964522 0.5286920 0.6552050
    ## quarrel_low_f      0.3661006 0.3104107 0.4241640
    ## accident_high_t    0.4990700 0.4472081 0.5663601
    ## accident_high_f    0.5576057 0.4928642 0.6157483
    ## bereavement_high_t 0.7276269 0.6775288 0.7807885
    ## bereavement_high_f 0.6407061 0.5857073 0.6953966
    ## holiday_high_t     0.7104517 0.6559212 0.7570780
    ## holiday_high_f     0.7568530 0.7004193 0.7962496
    ## quarrel_high_t     0.5444076 0.4836619 0.6054124
    ## quarrel_high_f     0.3138813 0.2582041 0.3696439

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

    ## # A tibble: 2 x 4
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
    ## acc_model_base: accuracy ~ veracity + content + (1 | id) + (1 | sender)
    ## acc_model_conf: accuracy ~ veracity + content + confidence_condition + (1 | id) + (1 | sender)
    ## acc_model_conf_int: accuracy ~ veracity + content * confidence_condition + (1 | id) + (1 | sender)
    ## acc_model_conf_int_2: accuracy ~ (veracity + content + confidence_condition)^2 + (1 | id) + (1 | sender)
    ## acc_model_conf_int_3: accuracy ~ (veracity + content + confidence_condition)^3 + (1 | id) + (1 | sender)
    ##                      npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)
    ## acc_model_base          7 4553.0 4596.0 -2269.5   4539.0                      
    ## acc_model_conf          8 4555.0 4604.1 -2269.5   4539.0  0.0145  1  0.9041281
    ## acc_model_conf_int     11 4555.2 4622.8 -2266.6   4533.2  5.7360  3  0.1251871
    ## acc_model_conf_int_2   15 4526.1 4618.2 -2248.1   4496.1 37.1234  4  1.699e-07
    ## acc_model_conf_int_3   18 4515.5 4626.0 -2239.8   4479.5 16.5622  3  0.0008694
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
    ## Formula: accuracy ~ (veracity + content + confidence_condition)^3 + (1 |  
    ##     id) + (1 | sender)
    ##    Data: accuracy_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4515.5   4626.0  -2239.8   4479.5     3406 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.5785 -0.9238  0.6335  0.8649  1.7375 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  id     (Intercept) 4e-14    2e-07   
    ##  sender (Intercept) 0e+00    0e+00   
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##                                                   Estimate Std. Error z value
    ## (Intercept)                                       -0.58345    0.14221  -4.103
    ## veracityt                                          0.87384    0.19802   4.413
    ## contentbereavement                                 0.48103    0.19718   2.440
    ## contentaccident                                    0.42498    0.19734   2.154
    ## contentquarrel                                     1.08684    0.20006   5.432
    ## confidence_condition                              -0.52143    0.21293  -2.449
    ## veracityt:contentbereavement                      -0.02193    0.28144  -0.078
    ## veracityt:contentaccident                         -0.94899    0.27710  -3.425
    ## veracityt:contentquarrel                          -1.02959    0.27960  -3.682
    ## veracityt:confidence_condition                     1.14404    0.29545   3.872
    ## contentbereavement:confidence_condition           -0.09050    0.29201  -0.310
    ## contentaccident:confidence_condition               0.51994    0.28802   1.805
    ## contentquarrel:confidence_condition                0.84087    0.29543   2.846
    ## veracityt:contentbereavement:confidence_condition -0.45879    0.41316  -1.110
    ## veracityt:contentaccident:confidence_condition    -0.84318    0.40319  -2.091
    ## veracityt:contentquarrel:confidence_condition     -1.59432    0.40914  -3.897
    ##                                                   Pr(>|z|)    
    ## (Intercept)                                       4.08e-05 ***
    ## veracityt                                         1.02e-05 ***
    ## contentbereavement                                0.014704 *  
    ## contentaccident                                   0.031277 *  
    ## contentquarrel                                    5.56e-08 ***
    ## confidence_condition                              0.014332 *  
    ## veracityt:contentbereavement                      0.937893    
    ## veracityt:contentaccident                         0.000615 ***
    ## veracityt:contentquarrel                          0.000231 ***
    ## veracityt:confidence_condition                    0.000108 ***
    ## contentbereavement:confidence_condition           0.756622    
    ## contentaccident:confidence_condition              0.071045 .  
    ## contentquarrel:confidence_condition               0.004424 ** 
    ## veracityt:contentbereavement:confidence_condition 0.266820    
    ## veracityt:contentaccident:confidence_condition    0.036504 *  
    ## veracityt:contentquarrel:confidence_condition     9.75e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 16 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
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
    ## accident_low_t     0.4418605 0.3782882 0.5072764
    ## accident_low_f     0.4604651 0.3980258 0.5303691
    ## bereavement_low_t  0.6790698 0.6212207 0.7357712
    ## bereavement_low_f  0.4744186 0.4081906 0.5389967
    ## holiday_low_t      0.5720930 0.5102081 0.6352065
    ## holiday_low_f      0.3581395 0.3061222 0.4155937
    ## quarrel_low_t      0.5860465 0.5160317 0.6437805
    ## quarrel_low_f      0.6232558 0.5606324 0.6826655
    ## accident_high_t    0.5164319 0.4516356 0.5878268
    ## accident_high_f    0.4600939 0.3992246 0.5262937
    ## bereavement_high_t 0.6948357 0.6344739 0.7578756
    ## bereavement_high_f 0.3286385 0.2683483 0.3838897
    ## holiday_high_t     0.7136150 0.6560116 0.7670234
    ## holiday_high_f     0.2488263 0.1977867 0.3070252
    ## quarrel_high_t     0.5539906 0.4892694 0.6250900
    ## quarrel_high_f     0.6948357 0.6398393 0.7557898

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
    ## acc_model_conf_int_3: accuracy ~ (veracity + content + confidence_condition)^3 + (1 | id) + (1 | sender)
    ## acc_recconf_model_comp: accuracy ~ (veracity + content + confidence_condition)^3 + confidence + (1 | id) + (1 | sender)
    ## acc_recconf_model_int: accuracy ~ (veracity + content + confidence_condition)^3 + confidence * confidence_condition + (1 | id) + (1 | sender)
    ##                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
    ## acc_model_conf_int_3     18 4515.5 4626.0 -2239.8   4479.5                     
    ## acc_recconf_model_comp   19 4517.4 4634.0 -2239.7   4479.4 0.1846  1     0.6675
    ## acc_recconf_model_int    20 4517.3 4640.1 -2238.7   4477.3 2.0224  1     0.1550

For the sake of completeness, here is the main model output. As can be
seen, confidence did not appear to predict accuracy at all.

``` r
summary(acc_conf_model_base)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: accuracy ~ confidence + (1 | id) + (1 | sender)
    ##    Data: accuracy_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   4552.5   4577.1  -2272.3   4544.5     3420 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4726 -0.9358  0.6814  0.8692  1.5280 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  id     (Intercept) 0.0000   0.0000  
    ##  sender (Intercept) 0.2733   0.5227  
    ## Number of obs: 3424, groups:  id, 428; sender, 8
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 0.051967   0.242397   0.214    0.830
    ## confidence  0.006994   0.020007   0.350    0.727
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr)
    ## confidence -0.630
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')
