# library -----------------------------------------------------------------
library(nasapower)
library(tidyverse)
library(plotly)
library(ggplot2)
library(forecast)
library(tsibble)
library(lubridate)
library(stringr)

# data --------------------------------------------------------------------
query_groupings(global = FALSE)
query_parameters(
)

t2m <- get_power(
  community = "re",
  lonlat = c(114.9, 4.9),
  pars = "T2M",
  dates = c("1981-01-01", "2023-12-31"),
  temporal_api = "monthly"
)
view(t2m)

t2m <-
  t2m %>% 
    pivot_longer(
      cols = 5:16,
      names_to = "MONTH"
    ) %>% 
    select(-ANN, -LON, -LAT)

t2m$DATE <- ymd(paste(t2m$YEAR, t2m$MONTH, "01"))

# COnver MONTH to Month
t2m$MONTH <- str_to_title(t2m$MONTH)

# Convert MONTH to a factor with the correct order 
t2m$MONTH <- factor(t2m$MONTH, levels = month.abb)

# Dont Run
# t2m %>% mutate(MONTH = month.name[match(MONTH, toupper(month.abb))])
# t2m$DATE2 <- ymd_hms(t2m$DATE)

# 1. Time series ----------------------------------------------------------
viz <-
  ggplot(t2m, aes(x = DATE, y = value)) +
    geom_line(col = "blue") + 
    labs(
      title = "Brunei BSB Monthly Temperature",
      x = "Date",
      y = "Temp"
    ) +
    theme_minimal()
ggplotly(viz)

# yearly
# ts_t2m <- as_tsibble(t2m, key = YEAR, index = DATE)

viz <-
  ggplot(t2m, aes(x = MONTH, y = value, group = YEAR)) +
  geom_line(col = "blue") +
  labs(
    title = "Brunei BSB Monthly Temperature",
    x = "Month",
    y = "Temperature (°C)",
    color = "Year"
  ) +
  theme_minimal()
ggplotly(viz)

# Is temperature increasing overtime 
t2m
ggplot(t2m, aes(x = DATE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Brunei BSB Monthly Temperature",
    x = "Month",
    y = "Temperature (°C)",
    color = "Year"
  ) +
  theme_minimal()

lm_temp <- lm(value ~ YEAR, data = t2m)
summary(lm_temp)

# Which periods are the hottest/coldest
# Extreme heat events? Matching month? Matching elnino?

# PCA

# Try raster
