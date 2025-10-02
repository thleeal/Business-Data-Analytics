# Preliminary study 
# Draw a scatter plot for ‘PickupCount’ (y-axis) and ‘TimeMin’ (x-axis). Is it a good 
# choice to use linear regression? Explain.

# Load necessary libraries
library(ggplot2)

# Load the data
taxi_data <- read.csv("taxi.csv", stringsAsFactors = TRUE)

# Scatter plot of PickupCount vs TimeMin
ggplot(taxi_data, aes(x = TimeMin, y = PickupCount)) +
  geom_point() +
  labs(title = "Scatter Plot of PickupCount vs TimeMin",
       x = "Time of Day (Minutes)",
       y = "Pickup Count") +
  theme_minimal()

# From the scatter plot, we can see that the relationship between 'PickupCount' and 'TimeMin' is not linear.
# The data points are scattered across the plot without any clear linear pattern. Therefore, linear regression
# may not be the best choice for modeling the relationship between these two variables. Other non-linear
# regression models or data transformation techniques may be more appropriate for capturing the underlying
# patterns in the data.

# Feature engineering 
# 1. Divide the ‘TimeMin’ into 4 uniform time intervals. Name it as ‘TimeQ’.  

# Divide TimeMin into 4 uniform intervals
taxi_data$TimeQ <- cut(taxi_data$TimeMin, breaks = 4, labels = c("Q1", "Q2", "Q3", "Q4"))

# 2. Classify the ‘WeekOfDay’ in to an indicator of weekday. Name it as 
# ‘isWeekDay’.

# Create a binary indicator for weekdays (1 = Monday to Friday, 0 = Saturday and Sunday)
taxi_data$isWeekDay <- ifelse(taxi_data$DayOfWeek %in% 1:5, 1, 0)

# Modeling   
# 1. According to the scatter plot in preliminary study, is it reasonable to model 
# PickupCount and TimeMin by a polynomial regression? If yes, what is a 
# reasonable choice of the order? Then, fit such model (model.1) and report the 
# Adjusted Rsquare 

# Yes, it is reasonable to model PickupCount and TimeMin by a polynomial regression, as the scatter plot
# suggests a non-linear relationship between the two variables and a polynomial model can capture the 
# curvature and fluctuations in the data better than simple linear regression. 

# Function to fit polynomial models and compare their performance
determine_polynomial_order <- function(data, max_degree) {
  results <- data.frame(Degree = integer(), Adj_R_Squared = numeric())
  
    for (degree in 1:max_degree) {
    # Fit polynomial regression model
    model <- lm(PickupCount ~ poly(TimeMin, degree = degree), data = data)
    
    # Extract Adjusted R-squared and AIC
    adj_r_squared <- summary(model)$adj.r.squared
    
    # Store results
    results <- rbind(results, data.frame(Degree = degree, Adj_R_Squared = adj_r_squared))
  }
  
  return(results)
}

# Set the maximum degree to test (e.g., up to degree 10)
max_degree <- 10
polynomial_results <- determine_polynomial_order(taxi_data, max_degree)

# Print the results
print(polynomial_results)

# Plot Adjusted R-squared vs Degree
ggplot(polynomial_results, aes(x = Degree, y = Adj_R_Squared)) +
  geom_line() +
  geom_point() +
  labs(title = "Adjusted R-squared vs Polynomial Degree",
       x = "Polynomial Degree",
       y = "Adjusted R-squared") +
  theme_minimal()

# Based on the results, we can see that the increase in Adjusted R-squared is the most significant
# up to a polynomial degree of 3. Therefore, since we have to strike a balance between the accuracy
# and simplicity of the model, a cubic polynomial regression model should be a reasonable choice for
# modeling the relationship between 'PickupCount' and 'TimeMin'.

# Fit a polynomial regression model (cubic)
model.1 <- lm(PickupCount ~ poly(TimeMin, degree = 3, raw = TRUE), data = taxi_data)
summary(model.1)
print(summary(model.1)$adj.r.squared)

# The Adjusted R-squared value for the cubic polynomial regression model is 0.405.

# 2. Fit a cubic spline (model.2) for ‘PickupCount’ by ‘TimeMin’ with 3 evenly 
# spaced knots (report the location of each knot). Report the Adjusted Rsquare. 

# Load the splines package
library(splines)

# Fit a cubic spline with 3 evenly spaced knots
knots <- quantile(taxi_data$TimeMin, probs = c(0.25, 0.5, 0.75))
model.2 <- lm(PickupCount ~ bs(TimeMin, knots = knots), data = taxi_data)
summary(model.2)
print(summary(model.2)$adj.r.squared)

attr(bs(taxi_data$TimeMin, knots = knots),"knots")

# The Adjusted R-squared value for the cubic spline regression model is 0.4201.

# 3. Fit a multiple linear regression model (model.3) for ‘PickupCount’ by 
# ‘TimeQ’ and ‘isWeekDay’. Report the Adjusted Rsquare. 

# Fit a multiple linear regression model
model.3 <- lm(PickupCount ~ TimeQ + isWeekDay, data = taxi_data)
summary(model.3)
print(summary(model.3)$adj.r.squared)

# The Adjusted R-squared value for the multiple linear regression model is 0.3687.

# 4. Fit a multiple linear regression model (model.4) with ‘TimeQ’, ‘isWeekDay’ 
# and their interaction term. Report the Adjusted Rsquare. 

# Fit a multiple linear regression model with interaction term
model.4 <- lm(PickupCount ~ TimeQ * isWeekDay, data = taxi_data)
summary(model.4)
print(summary(model.4)$adj.r.squared)

# The Adjusted R-squared value for the multiple linear regression model with interaction term is 0.4242.

# 5. Fit a GAM (model.5) with the following features: 
# a. Cubic spline by ‘TimeMin’ (with the same knots of model.2) 
# b. ‘TimeQ’ , ‘isWeekDay’ and their interaction terms. 
# Report the Adjusted Rsquare. 

# Fit a GAM model with cubic spline on TimeMin and parametric terms
knots <- quantile(taxi_data$TimeMin, probs = c(0.25, 0.5, 0.75))
model.5 <- lm(PickupCount ~ bs(TimeMin, knots = knots) + TimeQ * isWeekDay, data = taxi_data)
summary(model.5)

print(summary(model.5)$adj.r.squared)
# The Adjusted R-squared value for the GAM model is 0.5454.

attr(bs(taxi_data$TimeMin, knots = knots),"knots")

# Business Insights 
# 1. Pick the model with the highest Adjusted Rsquare as the final model. Give a 
# short business insight from the regression model (~ 50 words). 

# Compare Adjusted R-squared values
adj_r_squared <- c(
  summary(model.1)$adj.r.squared,
  summary(model.2)$adj.r.squared,
  summary(model.3)$adj.r.squared,
  summary(model.4)$adj.r.squared,
  summary(model.5)$adj.r.squared
)

# Find the model with the highest Adjusted R-squared
best_model <- which.max(adj_r_squared)
cat("The best model is model.", best_model, "with Adjusted R-squared =", adj_r_squared[best_model])

# Check if the dummy variables are significant in the model

summary(model.3)
# Since the p-values for both of the predictors are less than 0.05, we can reject both of the null hypothesis
# that the predictors do not contribute significantly to the model. Therefore, TimeQ and isWeekDay are both
# significant predictors in the model.

anova(model.4, model.5)
# Since the partial F-test p-value is less than 0.05, we can reject the null hypothesis that the spline term does
# not contribute significantly to the model. Therefore, the spline term is significant in the model.

anova(model.2, model.5)
# Since the partial F-test p-value is less than 0.05, we can reject the null hypothesis that the interaction term
# does not contribute significantly to the model. Therefore, the interaction term is significant in the model.


# Comparing the 5 models, the GAM model (model.5) with a cubic spline on TimeMin, and parametric terms
# TimeQ, isWeekDay, and their interaction has the highest Adjusted R-squared value of 0.5454. And, since
# all the predictors in the model are significant, the GAM model indeed captures the non-linear relationship
# between PickupCount and TimeMin, as well as the effects of TimeQ and isWeekDay on the number of pickups, 
# providing a comprehensive understanding of the factors influencing the number of pickups. That is, the
# pickup count will increase as the time of the day increases, and the effect of the time of the day on the
# pickup count is not linear. Additionally, the pickup count will increase during weekends compared to weekdays,
# vice versa. The interaction between the time of the day and the day of the week also has a significant impact
# on the pickup count. For instance, the pickup in TimeQ Q4 and TimeQ Q2 are higher than that inTimeQ Q1 on weekends 
# compared to weekdays. Therefore, the GAM model can be used to make predictions and optimize pickup services
# based on the time of the day and the day of the week, leading to better resource allocation and service. For 
# example, the company can adjust the number of drivers and vehicles available during TimeQ04 in weekends to meet 
# the demand and improve customer satisfaction.
