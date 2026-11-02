library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

#2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

head(retail_raw)

# 2.3a
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(
    cols = c(RSAFS, RSAFSNA),
    names_to = "series_id",
    values_to = "sales"
  )

head(retail_raw_long)

# 2.4a 

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

# 2.4b
head(retail_ts)

has_gaps(retail_ts)
# 2.5a
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# 2.6a
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6b
can_gas_recent <- can_gas_recent |>
  mutate(
    year = year(Month),
    month = month(Month, label = TRUE),
    quarter = quarter(Month)
  )

head(can_gas_recent)

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# 2.6c
can_gas_recent |>
  mutate(
    next_month = Month + 1,
    same_month_last_year = Month - 12
  ) |>
  head()

# 2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# 2.7a
retail_ts |>
  autoplot(sales)

# 2.7b
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL,
    y = "Millions of dollars",
    color = NULL
  ) +
  theme_minimal()
# The non-seasonally adjusted retail sales series shows noticeable repeating spikes and drops throughout each year, likely reflecting seasonal shopping patterns such as increased holiday spending. The seasonally adjusted series is smoother and does not show the same consistent yearly fluctuations. Both series follow a similar long-run upward trend and experience major economic changes, including declines during recessions. Seasonal adjustment mainly removes predictable seasonal patterns rather than changing the overall direction of retail sales.

# 2.7c
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Canadian Natural Gas Production",
    x = "Year",
    y = "Billions of cubic metres"
  ) +
  theme_minimal()

# 2.7d 
can_gas |>
  autoplot(Volume) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Canadian Natural Gas Production with Trend",
    x = "Year",
    y = "Billions of cubic metres"
  ) +
  theme_minimal()
# I added geom_smooth() to the Canadian natural gas production graph to make the long-run trend easier to identify. The smoothed line helps distinguish the overall direction of production from the shorter-term seasonal fluctuations.

# 3.1a
# The seasonal swings in Canadian natural gas production generally become larger as production increases, although the pattern is not consistent throughout the entire sample. The fluctuations are relatively small in the earlier years and become more noticeable as production grows. This suggests that a logarithmic transformation may be useful for stabilizing the variation in the series.

# 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

head(can_gas)

# Original Canadian gas production
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    y = "Billions of cubic metres",
    x = "Year"
  ) +
  theme_minimal()

# Logged Canadian gas production
can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Natural Gas Production",
    y = "Log Volume",
    x = "Year"
  ) +
  theme_minimal()
# After taking the natural logarithm of Canadian gas production, the upward trend becomes less steep over time, and the seasonal fluctuations appear more consistent in size. However, some variation in the seasonal swings remains, especially during the earlier years. I think logging is useful for this series because it reduces the differences in the size of the fluctuations, although it does not completely stabilize them.

# 3.1c
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

head(retail_nsa)

# Original retail sales
retail_nsa |>
  autoplot(sales) +
  labs(
    title = "Non-Seasonally Adjusted U.S. Retail Sales",
    x = "Year",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# Logged retail sales
retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Log of Non-Seasonally Adjusted Retail Sales",
    x = "Year",
    y = "Log Sales"
  ) +
  theme_minimal()
# After taking the natural logarithm of non-seasonally adjusted retail sales, the upward trend appears more linear, and the seasonal fluctuations become more consistent in size. The repeating seasonal patterns are still visible, but the log transformation reduces the differences in their magnitude over time. I think the case for logging retail sales is stronger than for Canadian gas production because the original retail sales series shows seasonal fluctuations that increase more noticeably as sales grow.

# 3.2a
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

head(can_gas)
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Monthly Growth in Canadian Natural Gas Production",
    x = "Year",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()
can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

head(can_gas)
summary(can_gas$growth_gap)
# The log-difference approximation appears to work well for Canadian natural gas production, with an average difference of only about 0.28 percentage points between the two methods. However, the maximum difference of 3.26 percentage points shows that the approximation is less accurate during months with larger changes in production. Overall, the log-difference method provides a reasonably accurate estimate of monthly growth.

# 3.3 
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(
    growth_mom = 100 * difference(log(sales))
  ) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL,
    y = "Growth rate (%)",
    color = NULL
  ) +
  theme_minimal()
# The non-seasonally adjusted retail sales series is noticeably noisier, with much larger month-to-month fluctuations compared to the seasonally adjusted series. This is because the NSA series still includes repeating seasonal patterns, such as increases in holiday spending and decreases afterward. The SA series is smoother because these predictable seasonal changes have been removed, which is consistent with the differences observed in Section 2.7b.

# 4.1a 
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b 
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()
# The non-seasonally adjusted retail sales series shows a clear repeating seasonal pattern, with increases around March and December and noticeable decreases in February and September. In comparison, the seasonally adjusted series has much flatter lines without the same consistent monthly fluctuations. This shows that seasonal adjustment removes predictable seasonal patterns, making it easier to observe changes in retail sales that are not related to the time of year.

# 4.1c 
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Natural Gas Production",
    y = "Log Volume"
  ) +
  theme_minimal()
# Canadian natural gas production tends to be highest during the winter months, particularly December and January, and lowest during the summer months around June and July. This matches my expectations because natural gas is commonly used for heating, so production tends to be higher when temperatures are colder and lower when heating demand decreases.

# 4.2 
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()

# 4.3a 
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()
retail_nsa_x13 |>
  autoplot()
# The X-13ARIMA-SEATS decomposition separates retail sales into trend, seasonal, and irregular components. The trend shows a general increase in retail sales over time, while the seasonal component captures predictable fluctuations that repeat each year. The irregular component remains relatively stable around 1, except for larger disruptions such as the sharp decline around 2020.


# 4.3b 
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL,
    y = "Difference (%)"
  ) +
  theme_minimal()
# My seasonal adjustment closely matches the Census Bureau's official series, with most differences staying near 0%. The largest differences occur after 2020, where some differences exceed 1%, possibly due to differences in adjustment settings and unusual economic conditions.

# 4.4a 
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
# The estimated average monthly growth rate of Canadian natural gas production is approximately 0.397%. June has the most statistically significant seasonal effect, with a t-statistic of -4.777, indicating that production tends to be lower in June compared to January.

# 4.4b 
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(
    title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
    x = "Year",
    y = "log(Volume)",
    color = NULL
  ) +
  theme_minimal()
# The fitted model captures the overall upward trend and seasonal fluctuations fairly well, particularly during the 1990s. However, it overestimates production in the early 1960s and early 2000s while underestimating production during parts of the 1970s, showing that a linear trend cannot fully capture changes in growth over time.


# 4.4c
can_gas_fit |>
  gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
# The residuals do not resemble white noise because they show a clear pattern over time and strong autocorrelation across multiple lags. The Ljung–Box test produces a p-value below 0.05, so we reject the null hypothesis of no autocorrelation, suggesting that the model leaves predictable structure that could be modeled further.


# 5.1a 
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(
    snaive = SNAIVE(Volume),
    tslm = TSLM(log(Volume) ~ trend() + season())
  ) |>
  forecast(h = 1)

# Compare forecasting accuracy
gas_cv_fc |>
  accuracy(
    can_gas,
    measures = list(MAE = MAE, RMSE = RMSE)
  )
# The first evaluated forecast is January 1980, and the last is February 2005. The seasonal naïve model forecasts more accurately, with an MAE of 0.562 and RMSE of 0.702, compared to the TSLM model's MAE of 4.010 and RMSE of 4.360. Both accuracy measures agree that the seasonal naïve model performs better.

# 5.2a 
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(
    as_tibble(can_gas) |>
      select(Month, actual = Volume),
    by = "Month"
  ) |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(
  errs$snaive,
  errs$tslm,
  h = 1,
  power = 1
)
# The Diebold–Mariano test produced a test statistic of -31.278 and a p-value below 0.05. Therefore, I reject the null hypothesis that the two models have equal forecast accuracy. Combined with the MAE and RMSE results, this provides strong evidence that the seasonal naïve model performs significantly better than the expanding-window TSLM model.

# 5.3a 
gas_roll_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(
    snaive = SNAIVE(Volume),
    tslm = TSLM(log(Volume) ~ trend() + season())
  ) |>
  forecast(h = 1)

# Compare rolling-window accuracy
gas_roll_fc |>
  accuracy(
    can_gas,
    measures = list(MAE = MAE, RMSE = RMSE)
  )
# The TSLM model's accuracy improved the most when switching to a rolling window, with its MAE decreasing from 4.010 to 2.090 and RMSE decreasing from 4.360 to 2.420. This is likely because the rolling window uses more recent observations, allowing the model to better adapt to changes in the long-run trend that we observed in Part 4.4b. The seasonal naïve model's accuracy remained unchanged, and it continued to outperform the TSLM model.
