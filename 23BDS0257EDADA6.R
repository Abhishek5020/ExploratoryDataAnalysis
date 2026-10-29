# Assessment 6: Time Series Analysis
# Name: Abhishek Anand
# Reg No: 23BDS0257

# Question 1: Load the built-in AirPassengers dataset in R
cat("\nQ1 23BDS0257 Abhishek Anand\n")
data("AirPassengers")
print(AirPassengers)


# Question 2: Explore the first five and last five observations of the dataset
cat("\nQ2 23BDS0257 Abhishek Anand\n")
print(head(AirPassengers, 5))
print(tail(AirPassengers, 5))


# Question 3: Identify the starting and ending time periods of the dataset
cat("\nQ3 23BDS0257 Abhishek Anand\n")
print(start(AirPassengers))
print(end(AirPassengers))


# Question 4: Determine the frequency and time-series characteristics of the dataset
cat("\nQ4 23BDS0257 Abhishek Anand\n")
print(frequency(AirPassengers))
print(class(AirPassengers))


# Question 5: Visualize the monthly passenger trend over the entire period
cat("\nQ5 23BDS0257 Abhishek Anand\n")
plot(AirPassengers,
     main = "23BDS0257 Monthly Air Passenger Trend",
     xlab = "Year",
     ylab = "Number of Passengers",
     type = "l")


# Question 6: Decompose the time series into trend, seasonal, and random components
cat("\nQ6 23BDS0257 Abhishek Anand\n")
decomposed_data <- decompose(AirPassengers)
print(decomposed_data)


# Question 7: Plot the decomposed components to examine the underlying patterns
cat("\nQ7 23BDS0257 Abhishek Anand\n")
plot(decomposed_data)


# Question 8: Analyze the autocorrelation and partial autocorrelation of the time series
cat("\nQ8 23BDS0257 Abhishek Anand\n")
acf(AirPassengers,
    main = "23BDS0257 Autocorrelation Function of AirPassengers")

pacf(AirPassengers,
     main = "23BDS0257 Partial Autocorrelation Function of AirPassengers")


# Question 9: Fit an ARIMA model to the AirPassengers time series
cat("\nQ9 23BDS0257 Abhishek Anand\n")
arima_model <- arima(AirPassengers,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1), period = 12))
print(arima_model)


# Question 10: Display and interpret the fitted ARIMA model parameters and model summary
cat("\nQ10 23BDS0257 Abhishek Anand\n")
print(summary(arima_model))
print(arima_model$coef)
print(arima_model$sigma2)
print(arima_model$aic)


# Question 11: Forecast the number of passengers for the next 12 months using the fitted ARIMA model
cat("\nQ11 23BDS0257 Abhishek Anand\n")
forecast_result <- predict(arima_model, n.ahead = 12)
print(forecast_result$pred)


# Question 12: Visualize the actual and forecasted passenger values with appropriate labels and title
cat("\nQ12 23BDS0257 Abhishek Anand\n")

forecast_values <- ts(forecast_result$pred,
                      start = c(1961, 1),
                      frequency = 12)

plot(AirPassengers,
     xlim = c(1949, 1962),
     ylim = range(c(AirPassengers, forecast_values)),
     main = "23BDS0257 Actual and Forecasted Air Passengers",
     xlab = "Year",
     ylab = "Number of Passengers",
     type = "l")

lines(forecast_values,
      lty = 2,
      lwd = 2)

legend("topleft",
       legend = c("Actual", "Forecast"),
       lty = c(1, 2),
       lwd = c(1, 2))