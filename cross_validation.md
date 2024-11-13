Cross Validation
================
2024-11-12

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.9-1. For overview type 'help("mgcv-package")'.

``` r
library(SemiPar)

set.seed(1)
```

Look at LIDAR data

``` r
data("lidar")

lidar_df=
  lidar |> 
  as_tibble() |> 
  mutate(id=row_number())
```

``` r
lidar_df |> 
  ggplot(aes(x=range, y=logratio))+
  geom_point()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Try to do CV

We’ll compare 3 models–one linear, one smooth, one wiggly.

Construct training and testing df –test df is everything in lidar_df but
not in train_df

``` r
train_df=sample_frac(lidar_df, size=.8)
test_df=anti_join(lidar_df, train_df, by="id")
```

Look at these

``` r
ggplot(train_df, aes(x=range, y=logratio))+
  geom_point()+
  geom_point(data = test_df, color="red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
–red points showing missing points in train_df

Fit three models

``` r
linear_mod=lm(logratio~range, data=train_df)
smooth_mod=gam(logratio~s(range), data=train_df)
wiggly_mod=gam(logratio~s(range, k=30), sp=10e-6, data=train_df)
```

–prof saying smooth will be right one

Look at fits

``` r
train_df |> 
  add_predictions(linear_mod) |> 
  ggplot(aes(x=range, y=logratio))+
  geom_point()+
  geom_point(data = test_df, color="red")+
  geom_line(aes(y=pred, color="red"))
```

![](cross_validation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
train_df |> 
  add_predictions(wiggly_mod) |> 
  ggplot(aes(x=range, y=logratio))+
  geom_point()+
  geom_point(data = test_df, color="red")+
  geom_line(aes(y=pred, color="red"))
```

![](cross_validation_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
train_df |> 
  add_predictions(smooth_mod) |> 
  ggplot(aes(x=range, y=logratio))+
  geom_point()+
  geom_point(data = test_df, color="red")+
  geom_line(aes(y=pred, color="red"))
```

![](cross_validation_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->
—line not complex enough to capture this data –wiggly line is doing a
lot to try to follow: its too complex bc its chasing every fluctation in
the dataset –smooth mod is just right bc not doing too much

Compare these numerically using RMSE

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.127317

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.08302008

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.08848557

—rmse smallest for smooth model (wiggly model is slightly higher)

## Repeat the train/test split