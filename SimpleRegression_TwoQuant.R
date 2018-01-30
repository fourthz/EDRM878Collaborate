# Simple Regression - Two Quantitative Variables

# Read in data from CSV
storm = read.table("Tropical Storm Data.csv", header=TRUE, sep=",")

# ***OR use "Import Dataset" on Environment tab***

# ***OR manually insert values***
forecast = c(10, 11, 8, 8, 11, 7, 11, 8, 8, 11, 9, 12, 10, 11, 10, 14, 12, 12, 11, 14, 14, 15, 17, 17, 15, 11, 18, 16, 14, 18)
actual = c(12, 11, 6, 7, 12, 11, 14, 8, 6, 8, 7, 19, 13, 7, 14, 12, 14, 15, 12, 16, 14, 27, 10, 14, 16, 9, 19, 19, 19, 13)

# Save in R
save.image(file = "storm.RData")

# Group each variable from CSV file...do not need to do this step if you manually instered the values
forecast = storm$forecast
actual = storm$actual

# Descriptives
summary(forecast)
sd(forecast)

summary(actual)
sd(actual)

# Side-by-side boxplots
boxplot(forecast, actual, ylab = "Storms", names = c("Forecast", "Actual"))

# Confidence intervals for number of storms
t.test(forecast)
t.test(actual)

# Plot variables
plot(forecast, actual, xlab = "Number of Forecast Storms",
     ylab = "Number of Actual Stomrs")

# Identify linear model
storm.model = lm(actual ~ forecast)
storm.model

# Add regression line
abline(storm.model, col = "red")

# Calculate r and confidence intervals for r
cor = cor.test(forecast, actual)
cor

#Calculate r2
cor.sqr = cor$estimate*cor$estimate
cor.sqr

# Confidence interval for the slope
confint(storm.model)

# Confidence intervals for a prediction and a conditional mean 
predict(storm.model, data.frame(forecast = 16), interval = "prediction")
predict(storm.model, data.frame(forecast = 16), interval = "confidence")
