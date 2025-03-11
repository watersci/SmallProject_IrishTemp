# Temp was downloaded from 
# https://waterlevel.ie/hydro-data/#/overview/Waterlevel/station/11769/Angligham/stationInfo
  
library(tidyverse)  # For data manipulation
library(ggplot2)    # For static plots
library(readr)  # For reading CSV files
library(dplyr)  # For data manipulation
library(imputeTS) # Fill in missing flow data
library(trend)  # See if there is a pattern in the annual trend
library(xts) # Need this because th start and end dates of the ts are not exact years
library(lubridate) # Date col in flow needs fixing
library(forecast) # Fill in the missing flow data

# Read the temp file, skipping the first 12 rows
temp_data <- read_csv("30089_angligham_twater_complete.csv", skip = 12)

# Convert DD/MM/YYYY to Date format, change the mean temp col header to something easier
temp_data = temp_data %>%
  mutate(date = dmy(Day),
         temp_oc = `Day Mean`)  

# the flag -1 means no data that day
temp_data = temp_data[-which(temp_data$Quality == -1),]

# Create a full sequence of dates from min to max date
full_dates <- tibble(date = seq(from = min(temp_data$date),
                                to = max(temp_data$date),
                                by = "day"))

# Join full date sequence with the existing temp data
temp_complete <- full_dates %>%
  left_join(temp_data %>% select(date, temp_oc), by = "date")

rm(temp_data,full_dates)

# Add a flag for the missing values for plotting 
temp_complete <- temp_complete %>%
  mutate(missing_flag = ifelse(is.na(temp_oc), "Missing (Imputed)", "Original Data"))

# Ensure `temp_oc` is filled using `na_kalman()`
temp_complete$temp_oc <- na_kalman(temp_complete$temp_oc, model = "StructTS")

ggplot(temp_complete, aes(x = date, y = temp_oc, color = missing_flag)) +
  geom_point(alpha = 0.6) +  # Scatter plot with some transparency
  geom_vline(xintercept = as.numeric(as.Date(paste0(unique(year(temp_complete$date)), "-01-01"))),
             linetype = "dashed", color = "black", alpha = 0.5) +  # Vertical lines for Jan 1st
  labs(title = "Lough Corrib Temperature Over Time",
       x = "Date",
       y = "Temperature (oC)",
       color = "Data Type") +
  scale_color_manual(values = c("Original Data" = "blue", "Missing (Imputed)" = "red")) +
  theme_minimal()

# Data looks good, let's start the time-series treatment

library(prophet) # load the prophet to decompose the time series

# Prepare data for Prophet
temp_prophet <- temp_complete %>%
  select(date, temp_oc) %>%
  rename(ds = date, y = temp_oc)

# Fit the model
# Unlike traditional decomposition methods (STL, classical decomposition), prophet() doesn't explicitly decompose the original dataset. Instead, it:
# fits a model to historical data, uses that model to predict future values, extracts trend & seasonality from the predicted values
# This means prophet_plot_components() needs the forecast object because trend and seasonality are derived from the model’s future predictions rather than just the historical data.

m <- prophet(weekly.seasonality = FALSE)

# Fit the model to the dataset
m <- fit.prophet(m, temp_prophet)
forecast <- predict(m, temp_prophet) # Forecast only within the dataset

# Plot the forecasted temperature
plot(m, forecast) +
  labs(title = "Temperature Forecast with Prophet",
       x = "Date",
       y = "Temperature (°C)")

prophet_plot_components(m, forecast)

# Merge the forecast with the original data
temp_complete_with_residuals <- temp_prophet %>%
  left_join(forecast %>% select(ds, yhat, trend, yearly), by = "ds") %>%
  mutate(residual = y - trend - yearly)  # Compute residuals

ggplot(temp_complete_with_residuals, aes(x = ds, y = residual)) +
  geom_line(color = "red") +
  labs(title = "Residuals (Observed - Modeled Trend & Seasonality)",
       x = "Date",
       y = "Residual (°C)") +
  theme_minimal()

# Residuals look good, I have confidence in this trend analysis

library(trend)
trend_data <- forecast %>%
  select(ds, trend) %>%
  rename(date = ds)

# Run the Mann-Kendall Test on the trend
mk_test <- mk.test(trend_data$trend)

#### Yes, the temps are statistically significantly increasing

# Convert date to numeric years
trend_data <- trend_data %>%
  mutate(year = as.numeric(format(date, "%Y")))

# Fit the linear model using years
lm_trend_year <- lm(trend ~ year, data = trend_data)

# Extract slope (change per year)
slope_per_year <- summary(lm_trend_year)$coefficients["year", "Estimate"]

# Convert slope from "per year" to "per decade"
change_per_decade <- slope_per_year * 10

# Print the result
cat("Estimated temperature change per decade:", round(change_per_decade, 1), "°C\n")

# png("tims_corrib_plot.png", width = 12, height = 6, units = "in", res = 300)  # Adjust size & resolution
ggplot(trend_data, aes(x = date, y = trend)) +
  geom_line(color = "blue", size = 1) +  # Prophet trend
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear fit
  labs(title = "Temperature Trend Analysis for Lough Corrib\nTemperature has increased approximately 0.9°C in the last decade",
       x = "Date",
       y = "Average Temperature (°C)") +
  theme_minimal()
# dev.off()
