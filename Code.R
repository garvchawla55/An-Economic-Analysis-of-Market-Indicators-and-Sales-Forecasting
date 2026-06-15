# Load necessary libraries
library(readxl)
library(car)         # for VIF and other regression diagnostics
library(lmtest)      # for statistical tests (Breusch-Pagan, DW test)
library(sandwich)    # for robust standard errors
library(psych)       # for descriptive statistics and skewness
library(ggplot2)     # for creating layered, customizable graphs.
library(ggfortify)   # for diagnostic plots
library(dplyr)
library(tidyr)


# Load the dataset
data <- read_excel("C:/Users/Garv Chawla/Downloads/simulated_financial_forecasting_data (1).xlsx")

# Clean dataset: Remove any rows with missing values

data_clean <- na.omit(data)

# Check structure and summary of cleaned data
str(data_clean)
summary(data_clean)

# Fit the regression model
data_model <- lm(# Fit the regression model with target_sales as an independent variable
  model <- lm(sales ~ market_indicator_1 + market_indicator_2 + gdp_growth + unemployment_rate + inflation_rate + Competitor_sales, data = data)
)

# Summary of the regression
summary(data_model)

## Interpretations:

# Overall Model Fit: The model is highly significant (F-statistic p-value $< 2.2e-16$) and has an excellent fit, explaining 97% of the variance (Adjusted R-squared = 0.97)
# Coefficients: target_sales is the only highly significant predictor (p $< 2e-16$). All other variables, including the intercept, are not statistically significant (p $> 0.05$)
# Residuals: The residuals are centered close to zero (Median = 6.28) and appear symmetrical (1Q $\approx$ -3Q), which is ideal.\


## Plot histograms for visual check
data %>%
  select(sales, market_indicator_1, market_indicator_2, gdp_growth, unemployment_rate, inflation_rate, Competitor_sales) %>%
  gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "white") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  ggtitle("Distribution of Numeric Variables (Check for Skewness)")

### Interpretation For Histogram:

# Normal Distribution: All seven variables display an approximately normal distribution. They are all symmetrical and follow a classic "bell-shape," where the data is clustered around a central average.
# No Significant Skew: The primary purpose of this check is to look for skewness. These plots confirm that none of the variables are significantly skewed (meaning they don't have long tails stretching out to the left or right).
# Ready for Modeling: This is an ideal result for linear regression. Because the variables are already symmetrical, you do not need to apply any data transformations (like log or square root) to proceed with your model.


library(ggplot2)
# Load dataset

# Plot 1: Market Indicator 1 vs Sales (scatter + regression line)
ggplot(data, aes(x = market_indicator_1, y = sales)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Market Indicator 1 vs Sales", x = "Market Indicator 1", y = "Sales")


# Plot 2: Market Indicator 2 vs Sales
ggplot(data, aes(x = market_indicator_2, y = sales)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Market Indicator 2 vs Sales", x = "Market Indicator 2", y = "Sales")


# Plot 3: GDP Growth vs Sales
ggplot(data, aes(x = gdp_growth, y = sales)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GDP Growth vs Sales", x = "GDP Growth", y = "Sales")


# Plot 4: Unemployment Rate vs Sales
ggplot(data, aes(x = unemployment_rate, y = sales)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Unemployment Rate vs Sales", x = "Unemployment Rate", y = "Sales")


#plot 5: Competitor_sales vs Sales
ggplot(data, aes(x = Competitor_sales, y = sales)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Competitor_sales vs Sales", x = "Competitor_sales", y = "Sales")


#plot 6: inflation_rate vs Sales
ggplot(data, aes(x = inflation_rate, y = sales)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "inflation_ratevs Sales", x = "inflation_rate", y = "Sales")




# Check Classical Linear Regression Model (CLRM) Assumptions

fitted_vals <- fitted(model)

sales <- data_clean$sales
length(model$fitted.values)
length(data_clean$sales)

fitted_vals <- fitted(model)
actual_sales <- model$model$sales  # Extract dependent variable used in modeling

length(fitted_vals)
length(actual_sales)



# Diagnostic and assumption checking

## 1. Linearity check: Plot fitted values vs actual sales

actual_sales <- model$model$sales
plot(fitted_vals, actual_sales, xlab = "Fitted Values", ylab = "Actual Sales")
abline(a=0, b=1, col="red",lwd=2)   # Reference line

# Points should lie roughly along the red 45° line — if so, linearity holds.

# Plot Residuals vs Fitted Values
plot(model, which = 1, main = "Residuals vs Fitted (Check for Linearity)")  

# If the points are randomly scattered around the horizontal line at 0 and do not form a clear curve or pattern — linearity holds 


### Result: Satisfied 

## 2. Autocorrelation

dwtest(model)          # Durbin-Watson test for autocorrelation

bgtest(model, order=4) # Breusch-Godfrey test for higher-order autocorrelation

### Interpretation:
# H0: Residuals are independent (no autocorrelation)
# p-value > 0.05 → Fail to reject H0 → Independence holds 
# p-value < 0.05 → Residuals are correlated → Violation 
# (Usually important only for time-series data)

### DW = 2.0249, which is very close to 2, suggesting no autocorrelation. The p-value = 0.6542 > 0.05, so we fail to reject H₀ → the residuals are independent.
### The Breusch–Godfrey (BG) test checks for higher-order autocorrelation (up to 4 lags). Again, the p-value = 0.4085 > 0.05, confirming there is no serial correlation.
### The Durbin–Watson statistic (2.0249) and the Breusch–Godfrey test (p = 0.4085) indicate that the residuals are not serially correlated. Both tests fail to reject the null hypothesis of no autocorrelation, confirming that the assumption of independent errors is satisfied.

### Result: Satisfied 

## 3. Homoscedasticity

bptest(model)          # Breusch-Pagan test for constant variance

ncvTest(model)         # Non-constant variance score test

###Interpretation:
# H0: Variance of residuals is constant (homoscedastic)
# p-value > 0.05 → Homoscedasticity holds 
# p-value < 0.05 → Heteroscedasticity present 

## In both the Breusch–Pagan and NCV tests, p-values are greater than (> 0.05).

## the Breusch-Pagan (BP) test checks for heteroscedasticity (non-constant variance). The p-value = 0.1806 > 0.05, so we fail to reject H₀ → the residuals have constant variance.

## The Non-constant Variance (NCV) Score Test also checks this assumption. Again, the p-value = 0.12447 > 0.05, which also confirms there is no non-constant variance.

## both indicate that the residuals are homoscedastic. Both tests fail to reject the null hypothesis of constant variance, confirming that this key assumption is satisfied.

### Result: Satisfied

## 4. Normality of residuals

shapiro.test(residuals(model))  # Shapiro-Wilk test
qqnorm(residuals(model)); qqline(residuals(model), col="red", lwd=2)  # Q-Q plot
plot(density(residuals(model)), main = "Density plot of residuals")

## The Shapiro-Wilk test checks for the normality of the residuals. The p-value = 0.1628 > 0.05, so we fail to reject H₀ → the residuals are normally distributed.
## The Density plot of residuals provides a visual confirmation. The plot shows a symmetrical, bell-shaped curve centered around 0, which is the classic shape of a normal distribution.
## the Normal Q-Q Plot provides another visual check for the normality of the residuals. The points (Sample Quantiles) fall almost perfectly along the straight diagonal line (Theoretical Quantiles)

### Result: Satisfied

## 5. Multicollinearity
if(length(coef(model)) > 2) {
  vif_values <- vif(model)
  print(vif_values)
} else {
  print("VIF cannot be computed: fewer than 2 predictors.")
}

## A VIF score greater than 5 or 10 is typically a sign of a problem.

## The results show all VIF values are extremely close to 1 (e.g., market_indicator_1 = 1.0049, gdp_growth = 1.0053). This is the best possible outcome and indicates there is no multicollinearity in the model.

# Optional: Model diagnostic plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Summary of the final model
summary(data_model)

# R-squared and Adjusted R-squared
r2 <- summary(data_model)$r.squared
adj_r2 <- summary(data_model)$adj.r.squared
cat("R-squared:", round(r2, 3), "\nAdjusted R-squared:", round(adj_r2, 3), "\n")

# Mean Squared Error (MSE)
residuals_model <- residuals(data_model)
mse <- mean(residuals_model^2)
cat("Mean Squared Error (MSE):", round(mse, 2), "\n")

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(data_model)
par(mfrow = c(1, 1))

# Robust Standard Errors (to correct heteroscedasticity)
library(lmtest)
library(sandwich)

robust_summary <- coeftest(data_model, vcov = vcovHC(data_model, type = "HC1"))
print(robust_summary)


# Interpretation note:
# - Coefficients show direction of relationship (positive or negative)
# - p-values < 0.05 indicate statistically significant predictors

### Interpretations:

## R-squared and Adjusted R-squared:

# The model explains approximately 97% of the variance in your dependent variable (Sales), as shown by both R-squared = 0.97 and Adjusted R-squared = 0.97.
# This indicates your model has an extremely strong fit and explains nearly all of the variation in the data.

## Mean Squared Error (MSE):

# The Mean Squared Error (MSE) is 28537.21. This represents the average squared difference between your model's predictions and the actual sales values.
# Given the high R-squared, this value confirms the model's predictions are, on average, very close to the true values.

## Diagnostic Plot:

# The Residuals vs Fitted plot shows a random scatter of points around a flat red line centered at 0. This confirms the linearity assumption.
# The Normal Q-Q Plot shows the residual points falling almost perfectly on the diagonal line, which confirms the residuals are normally distributed.
# The Scale-Location plot also shows a flat red line with a random scatter, confirming the assumption of constant variance (homoscedasticity).
# The Residuals vs Leverage plot shows no points in the top-right or bottom-right corners (i.e., no points with high leverage and large residuals). All points are well within the Cook's distance lines, indicating there are no influential outliers skewing the model.

## Robust Standard Errors:

# This table shows the statistical significance and effect of each predictor variable on the dependent variable (Sales).
# The target_sales predictor is highly statistically significant at the 0.001 level (p < 2e-16, marked with ), confirming its extremely strong relationship with Sales.
# The market_indicator_1 predictor is marginally significant (p = 0.05312), just slightly outside the standard 0.05 threshold.
# All other predictors (market_indicator_2, gdp_growth, unemployment_rate, inflation_rate) and the (Intercept) are not statistically significant (all p-values are much > 0.05).
# Based on the estimates for the significant predictors:
#.target_sales has a strong positive effect on Sales (Estimate = 1.626).
#.market_indicator_1 appears to have a negative effect on Sales (Estimate = -0.527).






# Checking normality of residuals
res <- residuals(data_model)
hist(res, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightblue", border = "white")

qqnorm(res, main = "Normal Q–Q Plot of Residuals")
qqline(res, col = "red", lwd = 2)
# the histogram is right-skewed, treating it by using robust standard errors 
library(lmtest)
library(sandwich)
coeftest(data_model, vcov = vcovHC(data_model, type = "HC3"))


# Recheck normality visually
hist(residuals(data_model), main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals", col = "lightgreen", border = "white")
qqnorm(residuals(data_model))
qqline(residuals(data_model), col = "red", lwd = 2)


#Checking for Multicollinearity
library(car)
vif(data_model)
# No VIF are greater than 10, therefore there is no multicollinearity.


# Select numeric columns of interest for descriptive stats
numeric_cols <- c("sales", "market_indicator_1", "market_indicator_2", "gdp_growth", "unemployment_rate", "inflation_rate", "Competitor_sales")

# Get descriptive statistics including mean, sd, skewness, and kurtosis
skewness_results <- psych::describe(data %>% select(all_of(numeric_cols)))[, c("mean", "sd", "skew", "kurtosis")]

# Print results
print(skewness_results)



# Overall Conclusions:

#1. Statistical Validity
##The final linear regression model includes market_indicator_1, market_indicator_2, gdp_growth, unemployment_rate, inflation_rate, and target_sales as predictors.
##The target_sales coefficient is highly statistically significant (p < 2e-16), suggesting an extremely strong relationship with the target variable.
##Other predictors (e.g., market_indicator_1, gdp_growth) were not statistically significant in this model.
##R-squared = 0.97 and Adjusted R-squared = 0.97. This is an exceptionally high value, indicating that the model explains approximately 97% of the variability in the dependent variable (Sales).
##Mean Squared Error (MSE) ≈ 28537.21, which, combined with the high R-squared, indicates prediction errors are very small relative to the data's variance.


#2. Assumption Checks (CLRM)
##Linearity: Satisfied. The Residuals vs Fitted plot showed a random scatter of points around a flat line at 0.
##Independence (Autocorrelation): Satisfied. Durbin-Watson ≈ 2.0249 (p ≈ 0.65) and the Breusch-Godfrey test (p ≈ 0.40). Both tests fail to reject the null hypothesis, confirming no significant autocorrelation was detected.
##Homoscedasticity (Constant Variance): Satisfied. Both the Breusch-Pagan test (p ≈ 0.18) and the NCV test (p ≈ 0.12) were not significant. This is an ideal outcome, as it confirms the variance is constant and robust standard errors are not necessary.
##Normality of residuals: Satisfied. The Shapiro-Wilk test (p ≈ 0.16) failed to reject the null hypothesis of normality. This was visually confirmed by the perfectly linear Q-Q plot and the symmetrical Density plot.


#3. Multicollinearity
##No multicollinearity was detected. All VIF values were extremely low (all ~1.00), which is well below any problematic threshold. This confirms the predictors are independent.


#4. Reliability
##The model is highly reliable. All classical linear regression model (CLRM) assumptions were met, meaning the standard errors and p-values are valid and trustworthy.
##Residual diagnostics (Residuals vs Leverage plot) confirmed that there are no influential outliers or high-leverage points unduly affecting the model


#5. Overall Interpretation
##The model is valid, reliable, and highly predictive. It passes all key diagnostic checks with ideal, "textbook" results.
##The exceptionally high R-squared (97%) indicates the model has outstanding explanatory power, which appears to be driven almost entirely by the target_sales variable.


#6. Outputs Included
##Coefficient summary table (t-test, p-values).
##Full set of residual diagnostic plots: Residuals vs Fitted, Q-Q plot, Scale–Location plot, and Residuals vs Leverage.
##Statistical tests for assumptions: bptest, ncvTest, dwtest, bgtest, shapiro.test, and vif.
##Goodness-of-fit metrics: R-squared, Adjusted R-squared, and MSE.


