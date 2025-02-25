# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(here) # here()
library(readr) # read_csv()
library(ggplot2)
library(GGally)
# ggpairs(), wrap()
library(dplyr) # piping, relocate(), mutate()
library(ggpubr)# stat_regline_equation()
library(broom) # augment()
library(lmtest) # bptest
library(texreg) # screenreg(list(model1, model2,...))

# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here() #Look at our directory tree

path <- paste0(dirname(path.full), "/datasets/autompg.csv")
autompg <- readr::read_csv(path)
# autompg <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/autompg.csv")
# Another fast read alternative
# autompg <- data.table::fread(path)

# Look at data
head(autompg)
summary(autompg)
str(autompg)

# Pairs plot (EDA in one plot)
autompg %>% # pipe function (passes on previous result to next function)
  dplyr::relocate(mpg, .after = last_col()) %>% # response variable at end
  plyr::mutate(cyl = factor(cyl)) %>% #convert double type to factor
  GGally::ggpairs(mapping = aes(color = cyl), #try hashing out the mapping
                  diag=list(continuous="densityDiag",
                            discrete="barDiag"),
                  upper=list(continuous= GGally::wrap("cor", size = 3.5),
                             combo="facethist",
                             discrete="ratio"),
                  lower=list(continuous="points",
                             combo="box",
                             discrete="facetbar"))

## Scatter plot ##
ggplot(autompg, aes(x=hp, y=mpg)) + # Every plus (+) is new layer
  geom_point(color="black", size=1.5, shape=16) +
  ggtitle("Relationship between mpg and hp") +
  xlab("hp") + ylab("mpg")

### Line of best fit by hand. equation mpg ~ hp
x <- autompg$hp
y <- autompg$mpg
  
x_mean <- mean(x)
y_mean <- mean(y)

n <- nrow(autompg)
est.par <- 2 # number of estimated parameters

### Univariate Summary Statistics
## ∑(𝒙−𝒙̅)^𝟐 
# By loop
sxx <- 0
for (i in 1:n) {
  sxx <- sxx + (x[i] - x_mean)^2
}

# By list comprehension, sapply()
sxx <- sum(sapply(x, function(x) (x - x_mean)^2))

# Standard R way
sxx <- sum(x^2) - sum(x)^2 / n

## ∑(𝒚−𝒚̅)^𝟐 
# By loop
syy <- 0
for (i in 1:n) {
  syy <- syy + (y[i] - y_mean)^2
}

# By list comprehension, sapply()
syy <- sum(sapply(y, function(y) (y - y_mean)^2))

# Standard R way
syy <- sum(y^2) - sum(y)^2 / n

### Joint Summary Statistic
## ∑(𝒙−𝒙̅)(𝒚−𝒚̅) 
# By loop
sxy <- 0
for (i in 1:n) {
  sxy <- sxy + (x[i] - x_mean) * (y[i] - y_mean)
}

# By list comprehension, apply()
sxy <- sum(apply(cbind(x, y), 1,
                 function(row) (row[1] - x_mean) * (row[2] - y_mean)))

# Standard R way
sxy <- sum(x * y) -
  (sum(x) * sum(y)) / n

### Final Simple Linear Regression equation 
## y = b0 + b1*x1
b1 <- sxy/sxx
b0 <- y_mean - b1 * x_mean

fit <- b0 + b1*x

### Residuals
# Sum of squares total (SST)
SST <- syy
# Sum of squares regression (SSR)
SSR <- sum(sapply(fit, function(yhat) (yhat - y_mean)^2))
# Sum of squares error (SSE)
SSE <- syy - sxy^2/sxx

# Mean square error (MSE)
mse <- SSE/(n-est.par)
# Residual standard error
resid_se <- sqrt(mse)

### Regression coefficient error, t-score, & p-value
# Standard error beta0 and beta1
b0.err <- sqrt(mse) / sqrt(n) * sqrt(1 + (x_mean^2 / (sum((x - x_mean)^2) / n)))
b1.err <- sqrt(mse) / sqrt(sxx)

# beta0 and beta1 t-values
b0.t <- (b0 - 0) / b0.err
b1.t <- (b1 - 0) / b1.err

# p-values of beta0 and beta1
b0.p <- 2 * (1 - pt(b0.t, df = n - est.par))
b1.p <- 2 * (1 - pt(b1.t, df = n - est.par, lower.tail = FALSE))

### Goodness of fit
# Coefficient of determination R-squared
r2 <- 1 - SSE/SST

# R-squared adjusted
r2.adj <- r2 - (1 - r2) * ((est.par - 1) / (n - est.par))

## The F-Statistic
msr <- sum((fit - mean(y))^2) / 1
mse2 <- sum((y - fit)^2) / (n - est.par)
f <- msr / mse2

## p-value
p <- pf(f, 1, n - est.par, lower.tail = FALSE)

### Simple linear regression model$
slr <- lm(mpg ~ hp, data = autompg)

## Summary of model ##
summary(slr)

## Predict the mpg of a car with hp=160
x0 <- data.frame(hp = 160)
predict(slr, newdata = x0)

## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr)

# Scatter plot with SLR & equation
ggplot(df, aes(x=hp, y=mpg)) +
  geom_point(color="black", size=1.5, shape=16) +
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
  labs(title = "Check for Independence of Random Error\n and Linearity",
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

# Shapiro Test: we will discuss interpretation next class
shapiro.test(df$.resid)

### 3. Check for zero mean and constant variance of random error ###
ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(slr)))), col = "red", lty = 2) +
  labs(title = "Scale-Location",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))")

# Breusch-Pagan test: we will discuss interpretation next class
lmtest::bptest(slr)

### 4. Check for independence of random error ###
# scatter plot of residuals sorted by hp
ggplot(df %>% arrange(-hp),
       aes(x = 1:nrow(slr$model), y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Check for Independence \n Residuals sorted by hp",
       x = "Row Numbers",
       y = "Residuals")

# Another especially for time series data
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(df$.resid))
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
  labs(title = "Observed versus Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")

ggplot(df, aes(x = hatvalues(slr), y = .std.resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")

