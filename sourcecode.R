# library -----------------------------------------------------------------
# library(nasapower)
library(tidyverse)
library(plotly)
library(ggplot2)
library(forecast)
library(tsibble)
library(lubridate)
library(stringr)
library(TSA)

# DATA: query (do not run again) -------------------------------------------------------------
# df <- get_power(
#   community = "re",
#   lonlat = c(114.9, 4.9),
#   pars = c("T2M", "T2M_MAX", "T2M_MIN", "TS", "TS_MAX", "TS_MIN"),
#   dates = c("1981-01-01", "2023-12-31"),
#   temporal_api = "monthly"
# )
# 
# write.csv(df, "brunei_temp.csv", row.names = FALSE)

# DATA: wrangling  --------------------------------------------------------------------
df <- read_csv("brunei_temp.csv")
df <- df %>% 
  pivot_longer(cols = 5:16, names_to = "MONTH") %>% 
  dplyr::select(-ANN, -LON, -LAT) %>% 
  pivot_wider(names_from = PARAMETER, values_from = value)
df$DATE <- ymd(paste(df$YEAR, df$MONTH, "01"))

# add anomalies
monthly_means <- df %>% 
  group_by(MONTH) %>% 
  summarise(T2M_mean = mean(T2M))

df <- df %>%
  left_join(monthly_means, by = "MONTH") %>%
  mutate(anomaly = T2M - T2M_mean) %>% 
  arrange(DATE)

# detrend
time <- 1:length(df$anomaly)
fit <- lm(df$anomaly ~ time) # Linear detrending
anomalies_detrended <- residuals(fit)

# add nino index
df <- 
  read_csv("nino34.csv") %>% 
  filter(Date > as.Date("1980-12-01") & Date < as.Date("2024-01-01")) %>% 
  right_join(df, join_by(Date == DATE)) %>% 
  rename(DATE = Date)

colnames(df)[2] <- "nino3.4"

# time series
t2m_ts <- ts(df$T2M, start = c(1981, 1), frequency = 12)
anomalies_ts <- ts(anomalies_detrended, start = c(1981, 1), frequency = 12)
nino_ts <- ts(df$nino3.4, start = c(1981, 1), frequency = 12)

# DATA: T2M vs ST ---------------------------------------------------------------
ggplot(df, aes(x = T2M, y = TS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "TS vs T2M",
    x = "T2M",
    y = "TS"
  ) +
  theme_minimal()

# EDA: Monthly Temperature --------------------------------------------------
boxplot(t2m_ts ~ cycle(t2m_ts), names = month.abb, 
        xlab = NULL, ylab = "Temperature (°C)", 
        main = "Monthly Temperature Distribution")

# EDA: Monthly Trend ----------------------------------------------------------
viz <-
  ggplot(df, aes(x = DATE, y = T2M)) +
  geom_line(color = "steelblue") + 
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "darkred") +
  labs(
    x = NULL,
    y = "Temperature (°C)"
  ) +
  theme_minimal()
ggplotly(viz)

# EDA: Yearly Trend ---------------------------------------------------------------------
# Aggregate time series by year (assuming monthly data)
t2m_agg <- aggregate(t2m_ts, FUN = mean)

# Convert to data frame for ggplot
df_t2m <- data.frame(
  Year = as.numeric(time(t2m_agg)),  # Extract time index
  Temperature = as.numeric(t2m_agg)  # Extract values
)

# plot
p <- ggplot(df_t2m, aes(x = Year, y = Temperature)) +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted",
              color = "darkred") +
  geom_point(color = "blue", size = 2) +
  labs(title = "Average Yearly Temperature (T2M)",
       x = NULL,
       y = "Temperature (°C)") +
  theme_minimal()
ggplotly(p)

# lm
lm_temp <- lm(T2M ~ YEAR, 
              data = df %>% 
                select(YEAR, T2M) %>% 
                group_by(YEAR) %>% 
                summarize(T2M = mean(T2M)))
summary(lm_temp)


# 1. Spectral Analysis-------------------------------------------------------------
library(GeneCycle)
anomalies_ts <- ts(anomalies_detrended, start = c(1981, 1), frequency = 12)
spec <- periodogram(anomalies_detrended)
periods <- 1 / spec$freq  # Convert frequencies to periods (in months)

# Create a data frame for Plotly
p <- data.frame(periods = periods, spectral_density = spec$spec)

# Filter to the desired range (2–120 months)
p <- p %>% filter(periods >= 2 & periods <= 130)

viz <- 
  ggplot(data = p, aes(x = periods, y = spectral_density)) +
  geom_line(color = "steelblue") +
  labs(
    x = "Spectral Density (Strength)",
    y = "Cycle Length (Months)"
  ) +
  theme_minimal()
ggplotly(viz)


# 2. Anomaly vs ENSO ------------------------------------------------------
ggplot(df) +
  geom_line(aes(x = DATE, y = `nino3.4`, color = "Nino 3.4 index")) +
  geom_line(aes(x = DATE, y = anomaly, color = "Temp. Anomaly")) +
  scale_color_manual(values = c("Nino 3.4 index" = "red", "Temp. Anomaly" = "steelblue")) +
  labs(
    x = "Date",
    y = "Temperature (°C)",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

df %>% 
  ggplot(aes(x = anomaly, y = `nino3.4`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(
    x = "anomaly (°C)",
    y = "nino 3.4 index (°C)"
  ) +
  theme_minimal()

fit <- lm(df$anomaly ~ df$`nino3.4`)
summary(fit)

ccf(anomalies_ts, nino_ts, ylab = "cross-correlation", main = "")


# 3. Forecast Model -------------------------------------------------------
# 516 total; test: 408 months (79%) ;predict: 108 months (9years) 
forecast_df <- df %>% mutate(lag_one = lag(df$T2M, 1), lag_two = lag(df$T2M, 2))
train <- forecast_df[1:408,] %>% dplyr::select(1,3,4,5,13,14)
train_clean <- train %>% drop_na()
test <- forecast_df[409:516,] %>% dplyr::select(1,3,4,5,13,14)
test_clean <- test %>% drop_na()
train_ts <- ts(train$T2M, start = c(1981, 1), frequency = 12)
test_ts <- ts(test$T2M, start = c(2015, 1), frequency = 12)

# A. Arima
fit <- auto.arima(train_ts)
forecast_values <- forecast(fit, h = length(test_ts))
forecast::accuracy(forecast_values, test_ts)
test$pred_ar <- as.numeric(forecast_values$mean)  

# B. linear regression
fit_lm <- lm(T2M ~ YEAR + MONTH + lag_one + lag_two, data = train)
test$pred_lm <- predict(fit_lm, test)
# why not just use use more lags as variable: similar accuracy
# why not use DATE:

# C. Random Forest
library(randomForest)
set.seed(123)
fit <- randomForest(T2M ~ YEAR + MONTH + lag_one + lag_two, data = train_clean)
# # Pre-fill column
test$pred_rf <- NA
# # Find rows with no missing predictor values
complete_rows <- complete.cases(test[c("YEAR", "MONTH", "lag_one", "lag_two")])
# Predict only on complete rows
test$pred_rf[complete_rows] <- predict(fit, newdata = test[complete_rows, ])
view(test)

# D. Gaussian Process
library(kernlab)
fit <- gausspr(T2M ~ YEAR + MONTH + lag_one + lag_two, data = train_clean)
test$pred_gp <- as.vector(predict(fit, test))

# plot
test %>%
  dplyr::select(DATE, T2M, pred_ar, pred_lm, pred_rf, pred_gp) %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "Model",
               values_to = "Forecast") %>%
  mutate(Model = recode(Model,
                        pred_ar = "ARIMA",
                        pred_lm = "Linear Regression",
                        pred_rf = "Random Forest",
                        pred_gp = "Gaussian Process")) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = T2M), color = "steelblue", linewidth = 1) +  # actual
  geom_line(aes(y = Forecast), color = "grey", linewidth = 1, linetype = "dashed") +
  facet_wrap(~Model, ncol = 1) +
  labs(title = "Forecasts vs. Actual Temperature",
       x = "Date", y = "Temperature") +
  theme_minimal()

# Accuracy
library(Metrics)
res <- tibble(error = c("Mean Absolute Error (MAE)",
                        "Root Mean Square Error (RMSE)",
                        "Mean Absolute Percentage Error (MAPE)"))
res$ar <- c(mae(test$T2M, test$pred_ar), 
            rmse(test$T2M, test$pred_ar), 
            mape(test$T2M, test$pred_ar))
res$lm <- c(mae(test$T2M, test$pred_lm), 
            rmse(test$T2M, test$pred_lm), 
            mape(test$T2M, test$pred_lm))
res$rf <- c(mae(test$T2M, test$pred_rf), 
            rmse(test$T2M, test$pred_rf), 
            mape(test$T2M, test$pred_rf))
res$gp <- c(mae(test$T2M, test$pred_gp), 
            rmse(test$T2M, test$pred_gp), 
            mape(test$T2M, test$pred_gp))
res <- bind_rows(res, tibble(
  error = "Accuracy (1 - MAPE)",
  ar = 1 - res$ar[3],
  lm = 1 - res$lm[3],
  rf = 1 - res$rf[3],
  gp = 1 - res$gp[3]
))

library(gt)
res %>%
  gt() %>%
  tab_header(
    title = "Forecast Error Metrics by Model"
  ) %>%
  fmt_number(
    columns = -error,  # Format all numeric columns except 'error'
    decimals = 3
  ) %>%
  cols_label(
    error = "Metric",
    ar = "ARIMA",
    lm = "Linear Regression",
    rf = "Random Forest",
    gp = "Gaussian Process"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(everything()),
      cells_body(rows = error == "Accuracy (1 - MAPE)")
    )
  )

# predict for 2025
date <- seq(as.Date("2024-01-01"), as.Date("2026-12-01"), by = "month")
future_df <- tibble(
  DATE = date,
  YEAR = as.integer(format(date, "%Y")),
  MONTH = toupper(format(date, "%b"))
)

last_known <- tail(forecast_df$T2M, 2)  # last two known T2M values

# Initialize lags
future_df$lag_one <- NA
future_df$lag_two <- NA
future_df$pred <- NA

# Fill in lags and predict step by step
for (i in seq_len(nrow(future_df))) {
  if (i == 1) {
    future_df$lag_one[i] <- last_known[2]
    future_df$lag_two[i] <- last_known[1]
  } else if (i == 2) {
    future_df$lag_one[i] <- future_df$pred[i - 1]  # Use previous prediction
    future_df$lag_two[i] <- last_known[2]
  } else {
    future_df$lag_one[i] <- future_df$pred[i - 1]  # Use previous prediction
    future_df$lag_two[i] <- future_df$pred[i - 2]  # Use two steps back
  }
  
  # Prepare data for prediction (ensure column names match model)
  pred_data <- data.frame(
    YEAR = future_df$YEAR[i],
    MONTH = future_df$MONTH[i],
    lag_one = future_df$lag_one[i],
    lag_two = future_df$lag_two[i]
  )
  
  # Predict using the model
  future_df$pred[i] <- predict(fit_lm, newdata = pred_data)
}

p <-
  future_df %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = pred), color = "steelblue", linewidth = 1) +
  labs(title = "3-year Forecasts",
       x = "Date", y = "Temperature") +
  theme_minimal()
ggplotly(p)
