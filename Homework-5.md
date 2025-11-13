Homework 5
================
Ainsel Levitskaia-Collins, HL2710
2025-11-05

### Problem 1

This problem is unfinished.

``` r
#' repeat_birthdays
#'
#' @param group_size The size of the sample for which birthdays will be selected
#'
#' @returns A boolean value for whether or not there are any overlapping birthdays within the sample
repeat_birthdays = function(group_size) {
  birthday_df <- data.frame(birthdays = sample(1:365, size = group_size, replace = TRUE)) %>% 
    group_by(birthdays) %>% 
    count(birthdays) %>% 
    arrange(desc(n))
  
  overlap <- FALSE
  
  for (i in 1:ncol(birthday_df)) {
    if (pull(birthday_df, n)[[1]] > 1) {
      overlap = TRUE
    }
  }
  
  overlap
}

birthday_sim_df <-
  expand_grid(
    sample_size = 2:5,
    iter = 1:10
  ) %>% 
  mutate(
    estimate_df = map(sample_size, repeat_birthdays)
  ) %>% 
  unnest(estimate_df)
```

### Problem 2

#### Creating the dataframe

Creating a function that

1.  Generates a tibble for the model $x \sim {\sf Normal}[\mu, \sigma]$
    with a set *n* of 30
2.  Runs a t-test on that tibble
3.  Returns that tibble

``` r
normal_sim = function(n = 30, mean, sd) {
  x_vec <- tibble(x = rnorm(n, mean, sd))
  
  x_vec <- x_vec %>% 
    summarise(
      mean_true = mean,
      mean_estimate = mean(x),
      ttest = list(broom::tidy(t.test(x, mu = unique(mean)[1], conf.level = 0.95, alternative = "t")))
      ) %>% 
    unnest(ttest)
  
  x_vec
}
```

Creating a dataframe with 5,000 datasets for each value of $\mu$ = {0,
1, 2, 3, 4, 5, 6} and with a set standard deviation of 5:

``` r
normal_sim_df <-
  expand_grid(
    mean = 0:6,
    sd = 5,
    iter = 1:5000) %>% 
  mutate(
    x = map2(mean, sd, \(mean, sd) normal_sim(mean = mean, sd = sd))
  ) %>% 
  unnest(x)
```

#### Plot 1: comparing power against true mean value

A plot of the proportion of times the null was rejected (y-axis)
compared against the true value of the mean (x-axis).

``` r
plot_normal_sim_power <- normal_sim_df %>% 
  group_by(mean_true) %>% 
  mutate(
    reject = p.value < 0.05,
    reject = case_match(reject,
                        FALSE ~ "reject_no",
                        TRUE ~ "reject_yes")) %>% 
  count(reject) %>% 
  pivot_wider(
    names_from = reject,
    values_from = n
  ) %>% 
  mutate(reject_proportion = reject_yes / (reject_no + reject_yes)) %>% 
  as.data.frame() %>% 
  ggplot(aes(y = reject_proportion, x = mean_true, group = mean_true)) +
  geom_bar(stat = "identity", width = 0.6, fill = "palevioletred3") +
  theme_minimal() +
  ylab("Rejection Proportion") +
  xlab("True Mean Value")

plot_normal_sim_power
```

![](Homework-5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

There does not appear to be an association between the size of the mean
and the power of the test, as all 7 mean groups have approximately the
same proportion of times that the estimated mean was rejected.

#### Plot 2: comparing the estimated means and the true mean

``` r
normal_sim_rejects <- normal_sim_df %>% 
  filter(p.value < 0.05)

plot_normal_sim_means <- normal_sim_df %>% 
  ggplot(aes(x = mean_true, y = mean_estimate, group = mean_true)) +
  geom_boxplot() +
  geom_violin(data = normal_sim_rejects, fill = "palegreen4", color = "palegreen4", alpha = 0.5) +
  theme_minimal() +
  ylab("Estimate Mean") +
  xlab("True Mean (green = p<0.05)")

plot_normal_sim_means
```

![](Homework-5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The sample average of $\hat{\mu}$ across tests for which the null is
rejected is not approximately equal to the true value of $\mu$. I am not
sure why this is.

### Problem 3
