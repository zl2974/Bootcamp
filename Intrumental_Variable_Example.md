Instrumental Variable Example
================
Jeffrey Zhuohui Liang
7/25/2021

# Background

This example is a modification of the example in Textbook Chapter 19 and
its code\[1\].

Assumption: Patient using medical service admission normally will
require a MICU unit. We want to look at the association of using other
icus instead of micu for patients who normally require MICU and the
probability of death within 24 hours of discharge from ICU or death
during ICU stay.

# Method

## Data Exploration

Whether a patient is cared by a `micu team` is our primary interest
predictor, but we assume that there is unobserved confounding that
related to both variable and outcome.So the number of `remaining beds`
in MICU is defined as our Instrumental Variable.

*R**e**m**a**i**n**i**n**g* *B**e**d**s* = (*M**I**C**U* *C**a**p**a**c**i**t**y* − *N**o*.*o**f* *I**n**b**o**a**r**d**e**r**s*) − (*T**e**a**m* *C**e**n**s**u**s* − *N**o*.*o**f* *B**o**a**r**d**e**r**s*)

where `inboarders` that represent a MICU bed occupied by a non-MICU
patient at the time a MICU patient began their ICU stay and we also
determine whether the new MICU patient was assigned a bed outside the
geographic confines of the MICU, in which case they were classified as a
`boarder`.

Our instrument is controlled for `team census`(how many patients were
assigned to the MICU) size since there is an intuitive inverse
relationship between team census size and the number of remaining beds,
and it is conceivable that team census size could affect the outcome.

We also control for :

and our outcome is expire `within_24_hour` of callout or during ICU
stay.

``` r
example_df = read_csv("data/example_data.csv") %>% 
  janitor::clean_names() %>% 
  select(
    micu_team,
    age,
    gender,
    oasis,
    remaining_beds,
    team_census,
    congestive_heart_failure:depression,
    num_icustay,
    within_24_hours
  ) %>%
  mutate(across(
    c(
      gender,
      congestive_heart_failure:depression,
      within_24_hours,
      micu_team
    ),
    as.factor
  )) %>% # change integer to factor
  select_if(function(x)
    length(unique(x)) > 1)   # drop factors that have only 1 value


drop_col = example_df %>%  
  select(where(is.factor)) %>%
  apply(., 2, function(x)
    min(plyr::count(x)$freq)) %>% 
  .[.<=30] %>% 
  names()

# Combine condition that have less than 10 obervation in group into a combined group
example_df["other_condition"] = as.factor(apply(example_df[,drop_col],1,function(x) max(as.numeric(x))))


# drop factors that contains less than 10 observation per groups
example_df = example_df[,!colnames(example_df) %in% drop_col]

# Over Sampling to create balance sample
# example_df = downSample(example_df %>% select(-within_24_hours),example_df$within_24_hours,
#                       yname = "within_24_hours")
  
skimr::skim_without_charts(example_df) # statistics of our example data
```

|                                                  |             |
|:-------------------------------------------------|:------------|
| Name                                             | example\_df |
| Number of rows                                   | 12943       |
| Number of columns                                | 16          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| factor                                           | 11          |
| numeric                                          | 5           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: factor**

| skim\_variable         | n\_missing | complete\_rate | ordered | n\_unique | top\_counts       |
|:-----------------------|-----------:|---------------:|:--------|----------:|:------------------|
| micu\_team             |          0 |              1 | FALSE   |         2 | 1: 11187, 0: 1756 |
| gender                 |          0 |              1 | FALSE   |         2 | M: 6719, F: 6224  |
| pulmonary\_circulation |          0 |              1 | FALSE   |         2 | 0: 12705, 1: 238  |
| peripheral\_vascular   |          0 |              1 | FALSE   |         2 | 0: 12909, 1: 34   |
| chronic\_pulmonary     |          0 |              1 | FALSE   |         2 | 0: 12870, 1: 73   |
| diabetes\_complicated  |          0 |              1 | FALSE   |         2 | 0: 12908, 1: 35   |
| liver\_disease         |          0 |              1 | FALSE   |         2 | 0: 12880, 1: 63   |
| metastatic\_cancer     |          0 |              1 | FALSE   |         2 | 0: 12908, 1: 35   |
| solid\_tumor           |          0 |              1 | FALSE   |         2 | 0: 12833, 1: 110  |
| within\_24\_hours      |          0 |              1 | FALSE   |         2 | 0: 10758, 1: 2185 |
| other\_condition       |          0 |              1 | FALSE   |         2 | 0: 12847, 1: 96   |

**Variable type: numeric**

| skim\_variable  | n\_missing | complete\_rate |  mean |    sd |  p0 | p25 | p50 | p75 | p100 |
|:----------------|-----------:|---------------:|------:|------:|----:|----:|----:|----:|-----:|
| age             |          0 |              1 | 81.15 | 66.50 |  15 |  52 |  66 |  80 |  311 |
| oasis           |          0 |              1 | 32.87 |  9.49 |   9 |  26 |  32 |  39 |   69 |
| remaining\_beds |          0 |              1 |  5.98 |  1.52 |   0 |   5 |   6 |   7 |    8 |
| team\_census    |          0 |              1 |  1.41 |  1.26 |   0 |   0 |   1 |   2 |    8 |
| num\_icustay    |          0 |              1 |  2.14 |  2.20 |   1 |   1 |   1 |   2 |   52 |

``` r
# test the association of outcome and variable of interest
chisq.test(example_df$within_24_hours,example_df$micu_team) 
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  example_df$within_24_hours and example_df$micu_team
    ## X-squared = 1.7, df = 1, p-value = 0.2

``` r
# test the association of intrumental variable and variable of interest
t.test(formula = remaining_beds~micu_team,data = example_df)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  remaining_beds by micu_team
    ## t = 69, df = 4023, p-value <2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  1.579 1.672
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##           7.384           5.758

``` r
# test the assocaition of intrumental variable and outcome
t.test(formula = remaining_beds~within_24_hours,data = example_df)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  remaining_beds by within_24_hours
    ## t = -1.2, df = 3087, p-value = 0.2
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.11316  0.02937
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##           5.972           6.014

``` r
corrplot::corrplot(
  cor(example_df %>% select(where(is.numeric)))
)
```

![](Intrumental_Variable_Example_files/figure-gfm/data_exploration-1.png)<!-- -->

``` r
featurePlot(
  x = model.matrix(within_24_hours ~ ., example_df %>% select(where(is.factor)))[,-1],
  y = example_df$within_24_hours,
  scales = list(
    x = list(relation = "free"),
    y = list(relation = "free")
  ),
  plot = "density",
  auto.key = list(columns = 2.5)
)
```

![](Intrumental_Variable_Example_files/figure-gfm/data_exploration-2.png)<!-- -->

## Model analysis

``` r
### "Bias" estimation

md_bias = glm(within_24_hours ~ .,
              family = "binomial",
              data = example_df[,!colnames(example_df) %in% "remaining_beds"])

broom::tidy(md_bias) %>%
  head() %>%
  knitr::kable(digits = 4,
               caption = "Biased Model Log Odd ratio Estimatiom")
```

| term         | estimate | std.error | statistic | p.value |
|:-------------|---------:|----------:|----------:|--------:|
| (Intercept)  |  -6.2919 |    0.1467 |  -42.8897 |  0.0000 |
| micu\_team1  |  -0.2160 |    0.0820 |   -2.6345 |  0.0084 |
| age          |   0.0005 |    0.0004 |    1.4915 |  0.1358 |
| genderM      |   0.1014 |    0.0533 |    1.9037 |  0.0570 |
| oasis        |   0.1300 |    0.0032 |   41.2572 |  0.0000 |
| team\_census |  -0.0058 |    0.0228 |   -0.2564 |  0.7977 |

Biased Model Log Odd ratio Estimatiom

The coefficient we estimate is Logarithm of Odd Ratio(OR), so we need to
apply exponential if we want to inference on OR instead of Log Odd.

``` r
broom::tidy(md_bias) %>%
  mutate(
    lower = exp(estimate + qnorm(0.025) * std.error),
    upper = exp(estimate + qnorm(0.975) * std.error),
        estimate = exp(estimate)
  ) %>%
  relocate(term, lower, estimate, upper) %>% 
  head() %>% 
  knitr::kable(digits = 4,
               caption = "Biased Model Odd ratio Estimatiom")
```

| term         |  lower | estimate |  upper | std.error | statistic | p.value |
|:-------------|-------:|---------:|-------:|----------:|----------:|--------:|
| (Intercept)  | 0.0014 |   0.0019 | 0.0025 |    0.1467 |  -42.8897 |  0.0000 |
| micu\_team1  | 0.6861 |   0.8057 | 0.9462 |    0.0820 |   -2.6345 |  0.0084 |
| age          | 0.9998 |   1.0005 | 1.0013 |    0.0004 |    1.4915 |  0.1358 |
| genderM      | 0.9970 |   1.1067 | 1.2285 |    0.0533 |    1.9037 |  0.0570 |
| oasis        | 1.1318 |   1.1388 | 1.1459 |    0.0032 |   41.2572 |  0.0000 |
| team\_census | 0.9507 |   0.9942 | 1.0396 |    0.0228 |   -0.2564 |  0.7977 |

Biased Model Odd ratio Estimatiom

## Instumental Analysis

In economics analysis, 2-stage-least-square(2SLS) is used to estimate
the adjusted effects of endogenuous variables. But the estimation is
different if we consider categorical outcome and predictors. Detailed of
estimation method is listed in the reference\[2\]. We will use `ivtools`
packages\[3\] for this example.

``` r
confounders = paste0(
  colnames(example_df)[!colnames(example_df) %in% c("micu_team","remaining_beds","within_24_hours")],
  collapse = "+")

stage_1  = as.formula(
  paste0("micu_team~remaining_beds+",confounders)
)

stage_2 = as.formula(
  paste0("within_24_hours~micu_team+",confounders)
)

md_ivtools =
  ivtools::ivglm(
    estmethod = "ts",
    fitX.LZ = glm(stage_1, family = "binomial", data = example_df),
    fitY.LX = glm(stage_2,
                  family = "binomial",
                  data = example_df),
    data = example_df
  )

bind_cols(rownames(summary(md_ivtools)$coeff), as_tibble(summary(md_ivtools)$coeff))  %>%
  rename(term = `...1`) %>% 
  janitor::clean_names() %>% 
  mutate(
    lower = exp(estimate + qnorm(0.025) * std_error),
    upper = exp(estimate + qnorm(0.975) * std_error),
    estimate = exp(estimate)
  ) %>%
  relocate(term, lower, estimate, upper) %>%
  head() %>% 
  knitr::kable(digits = 4,
               caption = "Intrumental Variable Model Odd ratio Estimatiom")
```

| term         |  lower | estimate |  upper | std\_error | z\_value |  pr\_z |
|:-------------|-------:|---------:|-------:|-----------:|---------:|-------:|
| (Intercept)  | 0.0014 |   0.0021 | 0.0033 |     0.2196 | -28.0295 | 0.0000 |
| micu\_team   | 0.4106 |   0.6686 | 1.0887 |     0.2488 |  -1.6183 | 0.1056 |
| age          | 0.9998 |   1.0005 | 1.0012 |     0.0004 |   1.4207 | 0.1554 |
| genderM      | 0.9969 |   1.1061 | 1.2273 |     0.0531 |   1.9007 | 0.0573 |
| oasis        | 1.1313 |   1.1389 | 1.1466 |     0.0034 |  38.0784 | 0.0000 |
| team\_census | 0.9488 |   1.0123 | 1.0800 |     0.0330 |   0.3693 | 0.7119 |

Intrumental Variable Model Odd ratio Estimatiom

In this example, we can see that after adjusting unobserved confounding
in our variable of interest, the effect of variable of interest became
not statistically significant, compared to a statistically significant
protective effect in the unadjust model.

# Reference

\[1\]
<https://github.com/MIT-LCP/critical-data-book/tree/master/part_iii/chapter_19>

\[2\] <http://www-stat.wharton.upenn.edu/~dsmall/BCai2009_SIM_Final.pdf>

\[3\] <https://cran.r-project.org/web/packages/ivtools/ivtools.pdf>
