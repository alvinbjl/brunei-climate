# library -----------------------------------------------------------------
library(nasapower)
library(tidyverse)
library(plotly)
library(ggplot2)
library(forecast)
library(tsibble)
library(lubridate)
library(stringr)

# data query (do not run again) -------------------------------------------------------------
# df <- get_power(
#   community = "re",
#   lonlat = c(114.9, 4.9),
#   pars = c("T2M", "T2M_MAX", "T2M_MIN", "TS", "TS_MAX", "TS_MIN"),
#   dates = c("1981-01-01", "2023-12-31"),
#   temporal_api = "monthly"
# )
# 
# write.csv(df, "brunei_temp.csv", row.names = FALSE)

# data --------------------------------------------------------------------
df <- read_csv("brunei_temp.csv")
df <- df %>% 
  pivot_longer(cols = 5:16, names_to = "MONTH") %>% 
  select(-ANN, -LON, -LAT) %>% 
  spread(PARAMETER, value)
df$DATE <- ymd(paste(df$YEAR, df$MONTH, "01"))
df <- df %>% arrange(DATE)

t2m_ts <- ts(df$T2M, start = c(1981, 1), frequency = 12)

# T2M vs ST ---------------------------------------------------------------
ggplot(df, aes(x = T2M, y = TS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "TS vs T2M",
    x = "T2M",
    y = "TS"
  ) +
  theme_minimal()

# 1. Trend Analysis ----------------------------------------------------------
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

lm_temp <- lm(T2M ~ YEAR, data = df)
summary(lm_temp)

plot(decompose(t2m_ts))

# 2. Yearly Temperature ---------------------------------------------------------------------
# Aggregate time series by year (assuming monthly data)
t2m_agg <- aggregate(t2m_ts, FUN = mean)

# Convert to data frame for ggplot
df_t2m <- data.frame(
  Year = as.numeric(time(t2m_agg)),  # Extract time index
  Temperature = as.numeric(t2m_agg)  # Extract values
)

# Create ggplot
p <- ggplot(df_t2m, aes(x = Year, y = Temperature)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Yearly Temperature (T2M)",
       x = "Year",
       y = "Mean Temperature (°C)") +
  theme_minimal()

# Convert to interactive plot
ggplotly(p)


# 3. Monthly Temperature --------------------------------------------------
boxplot(t2m_ts ~ cycle(t2m_ts), names = month.abb, 
        xlab = NULL, ylab = "Temperature (°C)", 
        main = "Monthly Temperature Distribution")
