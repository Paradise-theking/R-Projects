# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(here) # here()
library(readr) # read_csv()
library(ggplot2)
library(dplyr) # piping, relocate(), mutate()
library(ggpubr) # stat_regline_equation()
library(broom) # augment()
library(lmtest) # bptest
library(texreg) # screenreg(list(model1, model2,...))

# Set ggplot2 theme
theme_set(theme_minimal())

### Load data
path.full <- here::here() #Look at our directory tree

path <- paste0(dirname(path.full), "/datasets/autompg.csv")
autompg <-  readr::read_csv(path)

# Another fast read alternative
 #autompga <- data.table::fread(path)

## Sample Data (idealized linear)
dataxy <- data.frame(x = seq(from = 1, to = 100, by = 0.25))
dataxy$y <- -3 * dataxy$x + rnorm(nrow(dataxy), sd = 10)

##### ##### ##### ##### ##### ##### 
## Linear Assumptions Idealized ##
##### ##### ##### ##### ##### ##### 
# Look at idealized data
head(dataxy)
summary(dataxy)

## Scatter plot ##
ggplot(dataxy, aes(x=x, y=y)) + 
  geom_point() +
  ggtitle("Idealized SLR Model")

### Simple linear regression model
slr_ideal <- lm(y ~ x, data = dataxy)

## Summary of model
summary(slr_ideal)

## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr_ideal)

# Scatter plot with SLR & equation
ggplot(df, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="blue") + # Add SLR
  stat_regline_equation(label.x.npc = "center", color="blue") + # Add equation
  ggtitle("Idealized SLR Model")

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
ggplot(df, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence of \nRandom Error and Linearity",
       x = "Fitted Values",
       y = "Residuals")

## Check against individual regressor variables especially in MLR models
ggplot(df, aes(x = x, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Linearity Check: \nResiduals vs Individual Regressor",
       y = "Residuals")

##### 2. Check for normality of random error #####
ggplot(df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = 'red', linetype = 2) +
  ggtitle("Normal Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

ggplot(df) +
  geom_histogram(aes(x = .resid)) +
  ggtitle("Histogram of Residuals")

# Shapiro Test
shapiro.test(df$.resid)

### 3. Check for zero mean and constant variance of random error ###
ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr_ideal))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(slr_ideal)))), col = "red", lty = 2) +
  labs(title = "Scale-Location",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))")

# Breusch-Pagan test
lmtest::bptest(slr_ideal)

### 4. Check for independence of random error ###
# scatter plot of residuals sorted by x
ggplot(df %>% arrange(-x),
       aes(x = 1:nrow(slr_ideal$model), y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence \nResiduals sorted by x",
       x = "Row Numbers",
       y = "Residuals")

# Another especially for time series data
alpha <- 0.05
ciline <- qnorm(alpha/2)/sqrt(length(df$.resid))
bacf <- acf(df$.resid, lag.max = 15, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
  labs(title = "Auto Correlation Function")

# Durbin-Watson test measures the degree of autocorrelation in the residuals
# Rule of thumb: DW values between 1.5 and 2.5 are considered normal
lmtest::dwtest(slr_ideal)

### Final diagnostic plot often presented, too
ggplot(df, aes(x = .fitted, y = y)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Observed vs Predicted Values",
       x = "Fitted Values",
       y = "Actual Values")

ggplot(df, aes(x = hatvalues(slr_ideal), y = .std.resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")

##### ##### ##### ##### ##### ##### 
# Linear Assumptions with autompg #
##### ##### ##### ##### ##### ##### 
# Look at autompg data
head(autompg)
summary(autompg)

## Scatter plot ##
ggplot(autompg, aes(x=hp, y=mpg)) + 
  geom_point() +
  ggtitle("Relationship between mpg and hp")

### Simple linear regression model
slr <- lm(mpg ~ hp, data = autompg)

## Summary of model
summary(slr)

## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr)

# Scatter plot with SLR & equation
ggplot(df, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="blue") + # Add SLR
  stat_regline_equation(label.x.npc = "center", color="blue") + # Add equation
  ggtitle("Relationship between mpg and hp") +
  xlab("hp") + ylab("mpg")

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
ggplot(df, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence of \nRandom Error and Linearity",
       x = "Fitted Values",
       y = "Residuals")

## Check against individual regressor variables especially in MLR models
ggplot(df, aes(x = hp, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Linearity Check: Residuals vs hp",
       x = "hp",
       y = "Residuals")

##### 2. Check for normality of random error #####
ggplot(df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = 'red', linetype = 2) +
  ggtitle("Normal Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

ggplot(df) +
  geom_histogram(aes(x = .resid)) +
  ggtitle("Histogram of Residuals")

# Shapiro Test
shapiro.test(df$.resid)

### 3. Check for zero mean and constant variance of random error ###
ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(slr)))), col = "red", lty = 2) +
  labs(title = "Scale-Location",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))")

# Breusch-Pagan test
lmtest::bptest(slr)

### 4. Check for independence of random error ###
# scatter plot of residuals sorted by hp
ggplot(df %>% arrange(-hp),
       aes(x = 1:nrow(slr$model), y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence \n Residuals sorted by hp",
       x = "Row Numbers",
       y = "Residuals")

# Another especially for time series data
alpha <- 0.05
ciline <- qnorm(alpha/2)/sqrt(length(df$.resid))
bacf <- acf(df$.resid, lag.max = 15, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
  labs(title = "Auto Correlation Function")

# Durbin-Watson test measures the degree of autocorrelation in the residuals
# Rule of thumb: DW values between 1.5 and 2.5 are considered normal
lmtest::dwtest(slr)

### Final diagnostic plot often presented, too
ggplot(df, aes(x = .fitted, y = mpg)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")


ggplot(df, aes(x = hatvalues(slr), y = .std.resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")
##### ##### ##### ##### ##### ##### ##### 
## Linear Assumptions Fix with BoxCox ##
##### ##### ##### ##### ##### ##### ##### 
## Check which p power transformation on response variable
## Makes relationships btw response ~ regressors more linear
a <- MASS::boxcox(slr) ## Get the transformation parameter from here
head(data.frame(a) %>% arrange(-y), 5) # Top five p's

autompg <- autompg %>%
              mutate(root.recip = mpg ^ -0.5)


# Look at new autompg data
head(autompg)
summary(autompg)

## Scatter plot ##
ggplot(autompg, aes(x=hp, y=root.recip)) + 
  geom_point() +
  ggtitle("Relationship between mpg^-0.5 and hp")

### Simple linear regression model
slr_boxcox <- lm(root.recip ~ hp, data = autompg)

## Summary of model
summary(slr_boxcox)

## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr_boxcox)

# Scatter plot with SLR & equation
ggplot(df, aes(x=hp, y=root.recip)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="blue") + # Add SLR
  stat_regline_equation(label.x.npc = "center", color="blue") + # Add equation
  ggtitle("Relationship between mpg^-0.5 and hp") +
  xlab("hp") + ylab("mpg^-0.5")

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
ggplot(df, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence of \nRandom Error and Linearity",
       x = "Fitted Values",
       y = "Residuals")

## Check against individual regressor variables especially in MLR models
ggplot(df, aes(x = hp, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Linearity Check: Residuals vs hp",
       x = "hp",
       y = "Residuals")

##### 2. Check for normality of random error #####
ggplot(df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = 'red', linetype = 2) +
  ggtitle("Normal Q-Q Plot of Residuals") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles")

ggplot(df) +
  geom_histogram(aes(x = .resid)) +
  ggtitle("Histogram of Residuals")

# Shapiro Test
shapiro.test(df$.resid)

### 3. Check for zero mean and constant variance of random error ###
ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr_boxcox))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(slr_boxcox)))), col = "red", lty = 2) +
  labs(title = "Scale-Location",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))")

# Breusch-Pagan test
lmtest::bptest(slr_boxcox)

### 4. Check for independence of random error ###
# scatter plot of residuals sorted by hp
ggplot(df %>% arrange(-hp),
       aes(x = 1:nrow(slr_boxcox$model), y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence \n Residuals sorted by hp",
       x = "Row Numbers",
       y = "Residuals")

# Another especially for time series data
alpha <- 0.05
ciline <- qnorm(alpha/2)/sqrt(length(df$.resid))


bacf <- acf(df$.resid, lag.max = 15, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
  labs(title = "Auto Correlation Function")

# Durbin-Watson test measures the degree of autocorrelation in the residuals
# Rule of thumb: DW values between 1.5 and 2.5 are considered normal
lmtest::dwtest(slr_boxcox)

### Final diagnostic plot often presented, too
ggplot(df, aes(x = .fitted, y = root.recip)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")

ggplot(df, aes(x = hatvalues(slr_boxcox), y = .std.resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")

##### ##### ##### ##### ##### 
# Comparing different models #
##### ##### ##### ##### #####
summary(slr)
summary(slr_boxcox)
#same as above in one line.
texreg::screenreg(list(slr, slr_boxcox), digits = 4)

# Better model is slr_boxcox if only because it passes most/all linear regression
# assumptions we saw above, whereas slr did not. Additionally, we find that our
# R^2 is better with our transformed model meaning the fit is more accurate.

# Scatter plot with SLR & equation
ggplot(autompg, aes(x=hp, y=mpg)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="blue") + # Add SLR
  stat_regline_equation(label.x.npc = "center", color="blue") + # Add equation
  geom_line(aes(y = slr_boxcox$fitted.values^-2), linewidth = 1, color = "red") +
  stat_regline_equation(formula = autompg$root.recip ~ autompg$hp,
                        label.x.npc = "center", vjust = 3, color="red") + # Add equation
  ggtitle("Linear Models: \nRaw and Transformed Response")

# Scatter plot with SLR_BOXCOX & equation
ggplot(autompg, aes(x=hp, y=root.recip)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="blue") + # Add SLR
  stat_regline_equation(label.x.npc = "center", color="blue") + # Add equation
  ggtitle("Relationship between mpg^-0.5 and hp") +
  ylab("mpg^-0.5")

## Predict the mpg of a car WITH BOTH MODELS
x_low <- data.frame(hp = 50)
x_mid <- data.frame(hp = 125)
x_high <- data.frame(hp = 225)

# Predict with both slr models. REMEMBER, HERE, SLR_BOXCOX IS BETTER
(pred <- predict(slr, newdata = x_low))
pred <- predict(slr_boxcox, newdata = x_low) 
pred ^ -2 # p = -2 is the inverse of transformation p = -0.5

(pred <- predict(slr, newdata = x_mid))
pred <- predict(slr_boxcox, newdata = x_mid) 
pred ^ -2 # p = -2 is the inverse of transformation p = -0.5

(pred <- predict(slr, newdata = x_high))
pred <- predict(slr_boxcox, newdata = x_high) 
pred ^ -2 # p = -2 is the inverse of transformation p = -0.5

