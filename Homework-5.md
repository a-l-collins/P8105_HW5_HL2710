Homework 5
================
Ainsel Levitskaia-Collins, HL2710
2025-11-05

### Problem 1

Function:

- [x] fixed group size
- [x] randomly draws “birthdays” for each person
- [x] checks whether there are duplicate birthdays in the group
- [x] returns `TRUE` or `FALSE` based on the result

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

# write.table(sim_results_df, "./data/birthday_sim_results.txt", col.names = T, row.names = F, quote = F, sep = "\t")
```

Analysis:

- [ ] run above function 10,000 times for each group size between 2 and
  50
- [ ] for each group size, compute the probability that at least two
  people in the group will share a birthday by averaging across the
  10,000 simulation runs
- [ ] make a plot showing the probability as a function of group size
- [ ] comment on results

``` r
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

Creating a function that generates results for the model
$x \sim {\sf Normal}[\mu, \sigma]$ with a set *n* of 30:

``` r
normal_sim = function(n = 30, mean, sd) {
  x_vec <- tibble(x = rnorm(n, mean, sd))
  
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

Extracting $\hat{\mu}$ for every iteration group:

``` r
normal_sim_means <-
  normal_sim_df %>% 
  group_by(mean, iter) %>%  
  summarize(mean_hat = mean(x))
```

Running `t.test` with a significance level of 0.05 on every iteration
group and producing an end dataframe that retains the original expected
$\mu$ value while also containing `t.test` results:

``` r
normal_sim_results <- normal_sim_df %>% 
  group_by(iter) %>% 
  summarise(
    mean_true = unique(mean),
    ttest = list(broom::tidy(t.test(x, conf.level = 0.95, mu = unique(mean)[1])))
  ) %>% 
  janitor::clean_names() %>% 
  unnest(ttest)
```

Adding the $\hat{\mu}$ values into the `t.test` results dataframe, and
organizing the `normal_sim_results` structure:

``` r
normal_sim_means <- normal_sim_means %>% rename(mean_true = mean)

normal_sim_results <- merge(normal_sim_results, normal_sim_means, by = c("mean_true", "iter"))

normal_sim_results <- normal_sim_results %>% 
  select(iter, mean_true, mean_hat, p.value, everything()) %>% 
  arrange(iter) %>% 
  arrange(mean_true)

head(normal_sim_results)
```

    ##   iter mean_true    mean_hat      p.value estimate statistic parameter conf.low
    ## 1    1         0  2.49282660 1.831566e-16 3.224628  8.959626       209 2.515115
    ## 2    2         0 -0.43925809 4.803589e-16 3.295620  8.811931       209 2.558333
    ## 3    3         0  0.77372225 8.701994e-14 3.162030  7.995009       209 2.382349
    ## 4    4         0 -0.09875799 5.962512e-16 3.371727  8.778678       209 2.614557
    ## 5    5         0  0.61457133 3.342239e-14 2.759712  8.148255       209 2.092030
    ## 6    6         0  1.01083234 1.368068e-14 2.958643  8.290040       209 2.255075
    ##   conf.high            method alternative
    ## 1  3.934140 One Sample t-test   two.sided
    ## 2  4.032906 One Sample t-test   two.sided
    ## 3  3.941712 One Sample t-test   two.sided
    ## 4  4.128898 One Sample t-test   two.sided
    ## 5  3.427393 One Sample t-test   two.sided
    ## 6  3.662212 One Sample t-test   two.sided

- [ ] make a plot showing the proportion of times the null was rejected
  (the power of the test) on the y axis and the true value of mean on
  the x axis
  - [ ] describe the association between effect size and power
- [ ] make a plot showing the average estimate of mean-hat on the y axis
  and the true value of mean on the x axis
- [ ] make a second plot, or overlay on the above, the average estimate
  of mean-hat *only in the samples for which the null was rejected* (ie
  significant samples) on the y axis and the true value of mean on the x
  axis
  - [ ] is the sample average of mean-hat across tests for which the
    null is rejected approximately equal to the true value of mean? why
    or why not?

### Problem 3
