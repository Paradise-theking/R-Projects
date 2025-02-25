# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(dplyr)
library(car)
library(corrplot)
library(texreg) 
library(yardstick)
library(DT)
library(ggpmisc)


# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here()

(path <- paste0(dirname(path.full), "/datasets/autompg.csv"))
#autompg <- readr::read_csv(path)
autompg <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/autompg.csv")
head(autompg)


### Simple linear regression model$
slr <- lm(mpg ~ hp, data = autompg)

## Summary of model ##
summary(slr)
extract_equation <- function(model, response) {
  regressors <- coef(model)[-1]
  assign("vars", names(regressors), envir = .GlobalEnv)
  assign("intercept", coef(model)[1], envir = .GlobalEnv)
  assign("vars.coef", signif(regressors, 3), envir = .GlobalEnv)
  
  equation <- paste0(response, " = ", signif(intercept, 3), " + ", paste0(vars.coef, "*", vars, collapse = " + "))
  equation <- gsub("\\+ -", "- ", equation)
  
  return(equation)
}

extract_equation(slr, "y")

## Predict the mpg of a car with hp=160
x0 <- data.frame(hp = 160)

predict(slr, newdata = x0, interval = "confidence") # narrow range
predict(slr, newdata = x0, interval = "predict") # wider range

### Confidence and prediction intervals visualization
pred_int <- data.frame(predict(slr, interval = "predict", level = 0.95))
conf_int <- data.frame(predict(slr, interval = "confidence", level = 0.95))

ggplot(autompg, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method="lm", se = F, color="black") + 
  geom_ribbon(aes(ymin = conf_int$lwr,
                  ymax = conf_int$upr), color = "blue",
              fill = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = pred_int$lwr,
                  ymax = pred_int$upr), color = "red",
              fill = "red", alpha = 0.3)

# Same, more concise
ggplot(autompg, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method="lm", se = T, color="blue") +
  geom_ribbon(aes(ymin = pred_int$lwr,
                  ymax = pred_int$upr),
              fill = "lightgray", alpha = 0.4)

## Fit MLR models
mlr1 <- lm(mpg ~ hp + wt + acc, data = autompg)
mlr2 <- lm(mpg ~ ., data = autompg) # the dot . means all other variables not the response
mlr3 <- lm(mpg ~ . - hp, data = autompg) # minus - means exclude that variable from model

## Inference on model parameters ##
summary(mlr1)
summary(mlr2)
summary(mlr3)

## Type I ANOVA ##
summary(aov(mlr1))
summary(aov(mlr2))
summary(aov(mlr3))

## Type II ANOVA ##
car::Anova(mlr1, type=2)
car::Anova(mlr2, type=2)
car::Anova(mlr3, type=2)

## Type III ANOVA ##
car::Anova(mlr1, type=3)
car::Anova(mlr2, type=3)
car::Anova(mlr3, type=3)

## General linear hypothesis
car::linearHypothesis(mlr1, c("hp = 0", "wt = 0"))
car::linearHypothesis(mlr1, c("hp = wt"))

car::linearHypothesis(mlr1, c("acc = 0")) # acc=0?,tests significance

## 95% Confidence interval for model parameters ##
confint(mlr1, level = 0.95)

## Given value for x ##
x0 <- data.frame(hp=140, wt=3220, acc = 12)

## Confidence interval for mean response ##
predict(mlr1, x0, interval = "confidence", level = 0.95)
predict(mlr1, x0, interval = "prediction", level = 0.95)

## Check multicollinearity
car::vif(mlr1)

## Check correlation FULL
(corr <- cor(autompg))

corrplot(corr, method= "color", order = "FPC", diag = FALSE,
         addCoef.col = TRUE, type = 'upper')

## Remove acc to lower vifs
mlr11 <- lm(mpg ~ hp + wt, autompg)
## Check multicollinearity
car::vif(mlr11)

### Comparing subset models to its parent
## H_1: Adding cyl, disp, year to mlr1 improves the model
anova(mlr1, mlr2) # Reject Null --> 2nd (parent) is better
## H_1: Adding hp to mlr3 improves the model
anova(mlr3, mlr2) # Fail to reject Null --> 1st (subset) is better
## H_1: Adding hp to mlr3 improves the model
anova(mlr11, mlr1) # Fail to reject Null --> 1st (subset) is better

## Compare multiple models
texreg::screenreg(list(mlr1, mlr11, mlr2, mlr3), digits = 3, single.row = T)

# More specific mlr1
autompg.mlr1 <- autompg %>% select(attr(mlr1$terms, "term.labels"))

corr <- cor(autompg.mlr1)

corrplot(corr, method= "color", order = "FPC", diag = FALSE,
         addCoef.col = TRUE, type = 'upper')

# More specific mlr3
autompg.mlr3 <- autompg %>% select(attr(mlr3$terms, "term.labels"))

corr <- cor(autompg.mlr3)

corrplot(corr, method= "color", order = "FPC", diag = FALSE,
         addCoef.col = TRUE, type = 'upper')

### Advanced Comparisons
models <- data.frame(model = c("mlr1", "mlr2", "mlr3"))
response <- autompg$mpg

for (model in models$model) {
  assign(paste0("eval.", model),
         c(RMSE = rmse_vec(response, fitted(get(model))),
           MAE = mae_vec(response, fitted(get(model))),
           BIC = BIC(get(model)),
           Adj.R2 = summary(get(model))$adj.r.squared))
}

# Create an empty list to store the data frames
df_list <- list()
# Loop over each confusion matrix and create a data frame with metrics
for (model in models$model) {
  # Add postResample outputs in df_list
  df_list[[model]] <- get(paste0("eval.", model))
}

# Combine the data frames in the list into a single data frame
result_df <- data.frame(do.call(rbind, df_list))

# apply the style to the highest value in each column
rounded <- round(result_df,3)

# Formatted datatable for reports
rounded |>
  datatable(options = list(dom = 'rt', columnDefs = list(
    list(className = 'dt-center', targets = "_all") ))) |>
  # Light gray and italicize 2nd highest values
  formatStyle(
    names(rounded)[ncol(rounded)],
    background = styleEqual(apply(rounded, 2, function(x)
      max(x[-which.max(x)])), "#E8E8E8"),
    fontStyle = styleEqual(apply(rounded, 2, function(x)
      max(x[-which.max(x)])), "italic")) |>
  # Light gray and italicize 2nd lowest values
  formatStyle(
    names(rounded)[-ncol(rounded)],
    background = styleEqual(apply(rounded, 2, function(x)
      min(x[-which.min(x)])), "#E8E8E8"),
    fontStyle = styleEqual(apply(rounded, 2, function(x)
      min(x[-which.min(x)])), "italic")) |>
  # dark gray and bold highest values
  formatStyle(
    names(rounded)[ncol(rounded)],
    background = styleEqual(apply(rounded, 2, max), "darkgray"),
    fontWeight = styleEqual(apply(rounded, 2, max), "bold") ) |>
  # dark gray and bold lowest values
  formatStyle(
    names(rounded)[-ncol(rounded)],
    background = styleEqual(apply(rounded, 2, min), "darkgray"),
    fontWeight = styleEqual(apply(rounded, 2, min), "bold") )

### Getting intuition of R Squared values
RNGkind (sample.kind = "Rounding") 
set.seed(0) 

## Create the data frame
df <- data.frame(x = c(1:100))
df$y1 <- -5 * df$x + rnorm(100, sd = 600)
df$y2 <- -7 * df$x + rnorm(100, sd = 350)
df$y3 <- -9 * df$x + rnorm(100, sd = 100)
df$y4 <- 5 * df$x + rnorm(100, sd = 350)
df$y5 <- 7 * df$x + rnorm(100, sd = 250)
df$y6 <- 9 * df$x + rnorm(100, sd = 180)

y_list <- c(df$y1, df$y2, df$y3, df$y4, df$y5, df$y6)

# Create a single data frame to facet_wrap by group
df_full <- data.frame(x = rep(df$x, ncol(df)-1),
                      y = y_list,
                      group = sort(rep(seq(ncol(df)-1), nrow(df))))



# Create the plot
ggplot(data = df_full, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(col = "red") +
  facet_wrap(~ group, scales = "free_y") +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

