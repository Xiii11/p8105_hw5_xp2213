P8105 Homework 5
================
2024-11-14

Name: Xi Peng Uni: xp2213

# Problem 1. Birthday problem

``` r
bday_sim = function(n) {
  
      bdays = sample(1:365, size = n, replace = TRUE)
      
      duplicate = length(unique(bdays)) < n

      return(duplicate)
      
}

bday_sim(100)
```

    ## [1] TRUE

``` r
sim_res =
  expand_grid(
    n = 2:50,
    iter = 1:10000
  ) |> 
  mutate(res = map_lgl(n,bday_sim)) |> 
  group_by(n) |> 
  summarize(probability = mean(res))

sim_res |> 
  ggplot(aes(x = n, y = probability)) +
  geom_line()
```

![](p8105_Homework5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The plot shows that the probability of at least two people sharing a
birthday increases steadily as the group size grows. At around the
sample size of 23 people, the probability crosses the 0.5 mark,
representing a key threshold. Beyond this point, the probability
approaches 1 as the group size exceeds 50.

# Problem 2. Power Analysis of a One-Sample T-test

``` r
sim_mean_sd = function(n, mu = 0, sigma = 5) {
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  t_test_res = t.test(sim_data$x, mu = 0) |> 
    broom::tidy() |> 
    select(estimate, p.value)
  
  return(t_test_res)
}

sim_res = 
  expand_grid(
    sample_size = 30,
    mu = c(0, 1, 2, 3, 4, 5, 6),
    iter = 1:5000
  ) |> 
  mutate(
    test_res = map(mu, \(x) sim_mean_sd(n = 30, mu = x))
  ) |>
  unnest(test_res) |>
  mutate(
    rejected = p.value < 0.05
  )

sim_q2_summary_df = sim_res |> 
  group_by(mu) |>
  summarise(
    power = mean(rejected),
    avg_estimate = mean(estimate),
    avg_estimate_rejected = mean(estimate[rejected])
  ) 

knitr::kable(sim_q2_summary_df)
```

|  mu |  power | avg_estimate | avg_estimate_rejected |
|----:|-------:|-------------:|----------------------:|
|   0 | 0.0470 |    0.0040973 |              0.223064 |
|   1 | 0.1790 |    0.9811680 |              2.256505 |
|   2 | 0.5568 |    1.9901983 |              2.627835 |
|   3 | 0.8874 |    2.9899405 |              3.178205 |
|   4 | 0.9886 |    3.9874752 |              4.014364 |
|   5 | 0.9998 |    5.0014896 |              5.002200 |
|   6 | 1.0000 |    6.0091574 |              6.009157 |

``` r
power_plot = 
  sim_q2_summary_df  |>
  ggplot(aes(x = mu, y = power)) +
  geom_line() +
  geom_point() +
  labs(
    x = "True value of μ",
    y = "Power of test",
    title = "Effect Size and Power"
  ) +
  theme_minimal()

power_plot
```

![](p8105_Homework5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
estimates_plot = 
  sim_q2_summary_df |>
  ggplot(aes(x = mu)) +
  geom_line(aes(y = avg_estimate, color = "All samples")) +
  geom_line(aes(y = avg_estimate_rejected, color = "Rejected null only")) +
  labs(
    x = "True value of μ",
    y = "Average estimate of μ",
    title = "Average Estimates vs True μ",
    color = "Sample Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

estimates_plot
```

![](p8105_Homework5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

There is a clear association between effect size and power. When the
true value of mu is small, the test has low power, meaning there is a
smaller chance of rejecting the null hypothesis. However, as mu
increases, representing a larger effect size, the power of the test also
increases. Specifically, according to the the “Effect Size and Power”
plot, from mu = 1 to mu = 3, the power rises sharply. Beyond mu = 4, the
power approaches 1, indicating that the test is highly effective at
detecting a false null hypothesis at these larger effect sizes.

The sample average of mu^hat across tests for which the null is rejected
is not exactly equal to the true value of mu.
