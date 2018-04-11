test
================
Sara Altman
4/10/2018

``` r
source("thesis_code.R")
```

``` r
fig_num_counter
```

    ## [1] 1

``` r
three_toy_tidy
```

    ## # A tibble: 52 x 35
    ##    videoName       n condition     age ageBucket ageMonths ageBucketMonths
    ##    <chr>       <dbl> <fct>       <dbl>     <dbl>     <dbl>           <dbl>
    ##  1 YVW_161119…  1.00 Broken Toy   2.16      2.00      25.9            26.0
    ##  2 YVW_161119…  2.00 Broken But…  3.69      3.00      44.3            44.0
    ##  3 YVW_161119…  4.00 Broken But…  3.16      3.00      37.9            38.0
    ##  4 YVW_161119…  7.00 Broken But…  3.34      3.00      40.1            40.0
    ##  5 YVW_161119… 11.0  Broken But…  2.64      2.00      31.7            32.0
    ##  6 YVW_161119… 13.0  Broken But…  2.49      2.00      29.9            30.0
    ##  7 YVW_161203… 16.0  Broken Toy   3.76      3.00      45.1            45.0
    ##  8 YVW_161203… 20.0  Broken Toy   2.83      2.00      34.0            34.0
    ##  9 YVW_161221… 23.0  Broken But…  3.07      3.00      36.8            37.0
    ## 10 YVW_161221… 24.0  Broken Toy   2.32      2.00      27.8            28.0
    ## # ... with 42 more rows, and 28 more variables: gender <chr>,
    ## #   location <chr>, experimenter <chr>, confederate <chr>, toySide <chr>,
    ## #   childFlips <chr>, onLap <chr>, earlyHelp <chr>,
    ## #   `#ConfedPrompts` <chr>, `#ExpPrompts` <chr>, `#ParentPrompts` <chr>,
    ## #   parentTranslate <chr>, parentTranslation <chr>,
    ## #   parentTranslateOk <chr>, firstChoice <chr>, firstChoiceTiming <chr>,
    ## #   firstChoiceCorrect <int>, firstBehaviorDescription <chr>,
    ## #   firstBehaviorCode <chr>, firstBehaviorHelpfulness <chr>,
    ## #   helpfulCategory <chr>, ExplicitTeaching <chr>, `comments on helping
    ## #   behavior` <chr>, `other comments` <chr>, exclude <chr>,
    ## #   excludeCode <chr>, firstChoiceNum <int>, flip <lgl>
