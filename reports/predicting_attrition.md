Modeling Attrition , Chile Reentry Study
================
August 09, 2017

We use the baseline dataset to explore which factors seems to predict attrition, and to identify potential biases of the data.

Descriptives
============

The total sample is 225.

``` r
summary(dat[, .(age, kids, edu, mhealth)])
```

    ##       age             kids             edu            mhealth     
    ##  Min.   :19.00   Min.   : 0.000   Min.   : 0.000   Min.   :1.000  
    ##  1st Qu.:27.00   1st Qu.: 1.000   1st Qu.: 7.000   1st Qu.:2.000  
    ##  Median :34.00   Median : 2.000   Median : 8.000   Median :2.000  
    ##  Mean   :36.12   Mean   : 2.573   Mean   : 8.511   Mean   :2.262  
    ##  3rd Qu.:42.00   3rd Qu.: 4.000   3rd Qu.:12.000   3rd Qu.:3.000  
    ##  Max.   :68.00   Max.   :11.000   Max.   :16.000   Max.   :5.000  
    ##                                                    NA's   :4

Modeling response first week
============================

### Is variance of response explained by the Interviewer?

``` r
fit1 <- stan_glmer(c2 ~+ (1|id_int),
                   data = dat,  family = binomial(link = "logit"))
stan_caterpillar(fit1, pars = "b\\[\\(Intercept\\) id_int\\:[0-9]\\]",
                 pars_label = paste0("Interviewer", 1:5))
```

![](plots/predict-attrition-interviewers-1.png)

It doesn't seem to be the case!

### Predicting response using covariates

``` r
fit1 <- stan_glmer(c2 ~ age + kids + edu +  mhealth + (1|id_int),
                   data = dat,  family = binomial(link = "logit"))
```

``` r
color_scheme_set("blue")
posterior <- as.matrix(fit1)
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("age", "kids", "edu", "mhealth"),
           prob = 0.8) + plot_title
```

![](plots/predict-attrition-explore%20model-1.png)

``` r
ppc_dens_overlay(y = fit1$y,
                 yrep = posterior_predict(fit1, draws = 50))
```

![](plots/predict-attrition-explore%20model-2.png)
