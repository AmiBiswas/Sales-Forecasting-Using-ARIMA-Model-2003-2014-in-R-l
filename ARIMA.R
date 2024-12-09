install.packages("tidyverse",dependencies = TRUE)
library(tidyverse)
install.packages("lubridate",dependencies = TRUE)
install.packages("forecast",dependencies = TRUE)


# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)

# Load the data
setwd("C:/Users/Administrator/Desktop/ESSENSIALS/IVY/stats+R/Stats+R 2/Project/ARIMA")
getwd()
sales_data <- read.csv("Sales.csv")
summary(sales_data)
str(sales_data)
# Convert the date column to Date type
sales_data$date <- dmy(sales_data$Month.Year)
# Rename the column from Month.Year to date
library(dplyr)
sales_data <- sales_data %>%
  rename(date = Month.Year)
sales_data <- sales_data %>%
  rename(sales = Sales)

str(sales_data)

# Aggregate data by month if needed
monthly_sales <- sales_data %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(sales = sum(sales))




# View the aggregated monthly sales data
print(monthly_sales)




# Plot the sales data
install.packages("ggplot2")
library(ggplot2)
ggplot(monthly_sales, aes(x = date, y = sales)) +
  geom_line() +
  labs(title = "Monthly Sales Data", x = "Date", y = "Sales") +
  theme_minimal()


# Perform ADF test
install.packages("tseries")
library(tseries)

adf_test <- adf.test(monthly_sales$sales)
print(adf_test) #p-value: 0.8256 A high p-value means there is insufficient evidence to reject the null hypothesis of non-stationarity.


# Apply first differencing
diff_sales <- diff(monthly_sales$sales)

# Perform ADF test on differenced data
adf_test_diff <- adf.test(diff_sales)
print(adf_test_diff)

#Interpretation:
#The Dickey-Fuller statistic is significantly more negative, which suggests stronger evidence 
# against the null hypothesis of non-stationarity.

#The p-value of 0.01 is quite low, indicating that we can reject the null hypothesis at the 1% 
#significance level.



# Fit the ARIMA model
fit <- auto.arima(monthly_sales$sales)
summary(fit)
# Plot residuals to check for white noise
checkresiduals(fit)
#High p-value (0.8788): The high p-value indicates that we cannot reject the null hypothesis. This suggests that the 
# residuals do not show significant autocorrelation, meaning the ARIMA(0,1,0) with drift model fits the data well and captures the underlying structure adequately.

# Forecast future sales
forecast_sales <- forecast(fit, h = 12) # Forecast for the next 12 months
print(forecast_sales)

# Plot the forecast
autoplot(forecast_sales) +
  labs(title = "Monthly Sales Forecast", x = "Date", y = "Sales") +
  theme_minimal()




# Combine actual and forecasted data for visualization
# Load necessary library
library(lubridate)

# Correctly generate the forecast dates
forecast_df <- data.frame(
  date = seq.Date(from = tail(monthly_sales$date, 1) %m+% months(1), by = "month", length.out = 12),
  sales = as.numeric(forecast_sales$mean)
)

# Verify the data frame
print(forecast_df)

library(dplyr)
combined_data <- bind_rows(
  monthly_sales,
  forecast_df
)
combined_data
monthly_sales
# Plot the combined data
ggplot(combined_data, aes(x = date, y = sales)) +
  geom_line() +
  labs(title = "Monthly Sales Data with Forecast", x = "Date", y = "Sales") +
  theme_minimal()

