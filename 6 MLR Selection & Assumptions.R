# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(dplyr)
library(car)
library(StepReg) # stepwise()
library(leaps) # regsubsets()
library(glmnet) # glmnet()
library(ncvreg) # ncvreg()
library(DT)

# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here()

(path <- paste0(dirname(path.full), "/datasets/NFL.csv"))
nfl <- readr::read_csv(path)

#### #### #### #### ####
# Stepwsie Selection  #
#### #### #### #### ####
mlr.full <- lm(games.won ~ . , nfl)

## Forward Selection with Stepreg
(mlr.forward <- StepReg::stepwise(games.won ~ ., data = nfl,
                                  selection = "forward", select = "adjRsq"))

response <- "games.won"
formula.forward <- paste0(response, " ~ ",
                          paste0(mlr.forward$`Selected Varaibles`[-1],
                                 collapse = " + "))

mlr.forward <- lm(formula(formula.forward), data = nfl)
summary(mlr.forward)

# Predict
response <- "games.won"
# Pick random values for each column
RNGkind (sample.kind = "Rounding") 
set.seed(0)

(x0 <- data.frame(t(sapply(nfl, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(response)))

(pred.forward <- predict(mlr.forward, x0, interval = "confidence", level = 0.95))

### Backward Selection
## Backward Selection with Stepreg
(mlr.backward <- StepReg::stepwise(games.won ~ ., data = nfl,
                                   selection = "backward", select = "adjRsq"))

response <- "games.won"
formula.backward <- paste0(response, " ~ ",
                          paste0(mlr.backward$`Selected Varaibles`[-1],
                                 collapse = " + "))

mlr.backward <- lm(formula(formula.backward), data = nfl)
summary(mlr.backward) # SAME AS MLR.FORWARD


### Stepwise Selection with Stepreg
(mlr.mixed <- StepReg::stepwise(games.won ~ ., data = nfl,
                                   selection = "bidirection", select = "adjRsq"))

response <- "games.won"
formula.mixed <- paste0(response, " ~ ",
                           paste0(mlr.mixed$`Selected Varaibles`[-1],
                                  collapse = " + "))

mlr.mixed <- lm(formula(formula.mixed), data = nfl)
summary(mlr.mixed) # SAME AS MLR.FORWARD

#### #### #### #### #### ####
## Best Subset with leaps ##
#### #### #### #### #### ####
## Build all combinations of linear models 
models <- leaps::regsubsets(games.won ~ ., data = nfl, nvmax = NULL,
                            method = "exhaustive", nbest=1)

plot(models, scale = "adjr2")
plot(models, scale = "bic")
plot(models, scale = "Cp")

results <- summary(models)

## Plot results
# Extract and models results
tb <- tibble(predictors = seq(1, nrow(results$outmat)),
       adj_R2 = results$adjr2,
       BIC = results$bic,
       Cp = results$cp)

# Transform to along dataframe to plot with ggplot
df <- tb %>% 
  tidyr::pivot_longer(cols = -predictors, names_to = "statistic", values_to = "value")

ggplot(df, aes(predictors, value, color = statistic)) +
  geom_line(linewidth = 1, show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")

# View models that are best per statistic
(adjr2.max <- which.max(results$adjr2))
(bic.min <- which.min(results$bic))
(cp.min <- which.min(results$cp))

adjr2.max <- which.max(results$adjr2)
bic.min <- which.min(results$bic)
cp.min <- which.min(results$cp)

(vars.adjr2 <- colnames(results$which)[results$which[adjr2.max,]]) # SAME AS MLR.FORWARD
(vars.bic <- colnames(results$which)[results$which[bic.min,]])
(vars.cp <- colnames(results$which)[results$which[cp.min,]])

# Bulding model from one of the above best statistic
response <- "games.won"
formula.bestsub <- paste0(response, " ~ ",
                           paste0(vars.cp[-1], #here, using vars.cp as my choosen stat
                                  collapse = " + "))

mlr.bestsub <- lm(formula(formula.bestsub), data = nfl)
summary(mlr.bestsub)

# Predict
response <- "games.won"
# Pick random values for each column
RNGkind (sample.kind = "Rounding") 
set.seed(0)

(x0 <- data.frame(t(sapply(nfl, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(response)))

(pred.bestsub <- predict(mlr.bestsub, x0, interval = "confidence", level = 0.95))

#### #### #### #### ####
# Regularization: Lasso #
#### #### #### #### ####
# Initialize
x <- model.matrix(games.won ~ ., nfl)[,-1]
y <- nfl$games.won

# Cross-validation to get best hyperparameter, lambda
cv.glmnet <- cv.glmnet(x, y, alpha=1, standardize=T, keep = T,
                       type.measure = "default") # alpha = 1 is lasso.

plot(cv.glmnet)

coef(cv.glmnet, c(cv.glmnet$lambda.min,
                  cv.glmnet$lambda.1se))

# Build lasso regression with above best lambda
mlr.lasso.glmnet <- glmnet(x, y, alpha=1, standardize=T,
                     lambda = cv.glmnet$lambda.1se) 

coef(mlr.lasso.glmnet)
# (coef.lasso <- predict(cv.glmnet, type = "coefficients", s = cv.glmnet$lambda.min))

response <- "games.won"
vars.lasso <- row.names(coef(mlr.lasso.glmnet))[which(coef(mlr.lasso.glmnet) != 0)][-1]
intercept <- attr(coef(mlr.lasso.glmnet), "x")[1]
vars.lasso.coef <- attr(coef(mlr.lasso.glmnet), "x")[-1]

equation <- paste0(response, " = ", round(intercept, 4), " + ",
                   paste0(round(vars.lasso.coef,5), "*", vars.lasso, collapse = " + "))
(equation <- gsub("\\+ -", "- ", equation))

# Fit glm model with chosen regressors
glm.formula <- paste0(response, " ~ ",
                      paste0(vars.lasso, collapse = " + "))

glm.glmnet <- glm(as.formula(glm.formula), data = nfl)
glm.glmnet$coefficients <- setNames(c(intercept, vars.lasso.coef),
                                    names(glm.glmnet$coefficients))
summary(glm.glmnet) # SAME AS MLR.FORWARD

# Predict
response <- "games.won"
# Pick random values for each column
RNGkind (sample.kind = "Rounding") 
set.seed(0)

(x0 <- data.frame(t(sapply(nfl, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(response)))

(pred.glmnet <- predict(glm.glmnet, x0, interval = "confidence", level = 0.95))

## Bulding lasso with ncvreg
cv.ncvreg <- cv.ncvreg(x, y, penalty="lasso")
plot(cv.ncvreg)

mlr.lasso.ncvreg <- ncvreg(x, y, penalty="lasso", 
                     lambda = cv.ncvreg$lambda.min)

summary(ncvreg(x, y, penalty="lasso"), lambda = cv.ncvreg$lambda.min)
coef(mlr.lasso.ncvreg)

response <- "games.won"
regressors <- coef(mlr.lasso.ncvreg)[which(coef(mlr.lasso.ncvreg) != 0)][-1]
vars.lasso <- names(regressors)
intercept <- coef(mlr.lasso.ncvreg)[1]
vars.lasso.coef <- signif(regressors, 5)

equation <- paste0(response, " = ", signif(intercept,5), " + ",
                   paste0(vars.lasso.coef, "*", vars.lasso, collapse = " + "))
(equation <- gsub("\\+ -", "- ", equation))

# Fit glm model with chosen regressors
glm.formula <- paste0(response, " ~ ",
                      paste0(vars.lasso, collapse = " + "))

glm.ncvreg <- glm(as.formula(glm.formula), data = nfl)
glm.ncvreg$coefficients <- setNames(c(intercept, regressors),
                                    names(glm.ncvreg$coefficients))
summary(glm.ncvreg)

# Predict
response <- "games.won"
# Pick random values for each column
RNGkind (sample.kind = "Rounding") 
set.seed(0)

(x0 <- data.frame(t(sapply(nfl, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(response)))

(pred.ncvreg <- predict(glm.ncvreg, x0, interval = "confidence", level = 0.95))

#### #### #### #### #### #### ####
## Principle Component Analysis ##
#### #### #### #### #### #### ####
# Create principal components
pca <- prcomp(x, center = TRUE, scale. = TRUE)

summary(pca)

# Component pattern profiles
df_pca <- data.frame(pca$rotation)
df_pca$PC <- rownames(df_pca)

df_pca_long <- df_pca %>% 
  tidyr::pivot_longer(cols = -PC, names_to = "variable", values_to = "value")

# By PC
ggplot(df_pca_long, aes(x = PC, y = value, color = variable)) +
  geom_line(aes(group = variable), linewidth = 1, alpha = 0.8) +
  labs(x = "", y = "Scaled Value", title = "Component Pattern Profiles") +
  scale_x_discrete(limits = df_pca$PC, expand = c(0, 0)) +
  scale_color_discrete(name = "Variables") +
  theme(legend.key.size = unit(0.4, "cm"), legend.title = element_blank())

# By regressor
ggplot(df_pca_long, aes(x = variable, y = value, color = PC)) +
  geom_line(aes(group = PC), linewidth = 1, alpha = 0.8) +
  labs(x = "", y = "Scaled Value", title = "Component Pattern Profiles") +
  scale_x_discrete(labels = names(df_pca), expand = c(0, 0)) +
  scale_color_discrete(name = "Variables") +
  theme(legend.key.size = unit(0.4, "cm"), legend.title = element_blank())

# Check correlation of PCs,you will barely need this
# pcs <- broom::augment(pca) %>% select(starts_with(".fittedPC"))
pcs <- data.frame(pca$x)
corvarpc <- cor(pcs, method="pearson")
# By definition, PCs are INDEPENDENT of one another
corrplot::corrplot(corvarpc, method= "color", order = "alphabet", type = 'upper')

### Biplot ###
# Extract the loadings and calculate arrow scaling factor
loadings <- summary(pca)$rotation[,1:2]
arrow_scale <- max(abs(pcs[,1:2])) / max(abs(summary(pca)$rotation))

# Create a data frame with segment coordinates and labels
segments_df <- data.frame(
  x = rep(0, nrow(loadings)),
  y = rep(0, nrow(loadings)),
  xend = loadings[,1] * arrow_scale,
  yend = loadings[,2] * arrow_scale,
  label = rownames(loadings))

# Plot the scatter plot and add biplot arrows
ggplot(cbind(games.won = nfl$games.won, pcs),
       aes(x = PC1, y = PC2, color = games.won)) +
  geom_point() +
  labs(x = paste0("PC1 (", 100*summary(pca)[[6]][2,1], "%)"),
       y = paste0("PC2 (", 100*summary(pca)[[6]][2,2], "%)")) +
  geom_segment(data = segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", linewidth = 0.5) +
  geom_text(data = segments_df,
            aes(x = xend, y = yend, label = label),
            color = "red", hjust = 1.2, vjust = 1.2) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_vline(xintercept = 0, linetype=2) 

# How do the arrows and mpg color scale correspond to
# regressor's correlation w response? See last column (mpg)
corr <- cor(nfl %>% relocate(games.won, .after = last_col()))
corrplot::corrplot(corr, method= "color", diag = FALSE,
                   addCoef.col = TRUE, number.cex = 0.7, type = 'upper')

# MLR using Principal component analysis
## PC importance
screeplot(pca)
# Percent variance explained by each PC
summPC <- summary(pca)

df <- data.frame(PC_index = 1:length(summPC$importance[3,]), 
                 cum_prop_var = cumsum(summPC$importance[2,]))

ggplot(df, aes(x = PC_index, y = cum_prop_var)) +
  geom_point(shape = 19) +
  geom_line() +
  labs(title = "Check for PCA Elbow Point",
       x = "Index of PC",
       y = "Cumulative Proportion of Variance")

# Create a dataset specific for our PCA data
pca.data <- cbind(games.won = nfl$games.won, pcs)

mlr.pca <- lm(games.won ~ . - PC8 - PC9, data = pca.data)
summary(mlr.pca)

vif(mlr.full)
vif(mlr.pca)


# forwad selection on PCAs
(mlr.forward.pca <- StepReg::stepwise(games.won ~ ., data = pca.data,
                                  selection = "forward", select = "adjRsq"))

response <- "games.won"
formula.forward.pca <- paste0(response, " ~ ",
                          paste0(mlr.forward.pca$`Selected Varaibles`[-1],
                                 collapse = " + "))

mlr.forward.pca <- lm(formula(formula.forward.pca), data = pca.data)
summary(mlr.forward.pca) # SAME AS MLR.PCA

# Predict
response <- "games.won"
# Pick random values for each column
RNGkind (sample.kind = "Rounding") 
set.seed(0)

(x0 <- data.frame(t(sapply(nfl, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(response)))

# Normalize based on the pca created above
x0.pca <- data.frame(predict(pca, newdata = x0))

(pred.pca <- predict(mlr.pca, x0.pca, interval = "confidence", level = 0.95))

# View DataPoint in biplot
ggplot(cbind(games.won = nfl$games.won, pcs),
       aes(x = PC1, y = PC2, color = games.won)) +
  geom_point() +
  labs(x = paste0("PC1 (", 100*summary(pca)[[6]][2,1], "%)"),
       y = paste0("PC2 (", 100*summary(pca)[[6]][2,2], "%)")) +
  geom_segment(data = segments_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", linewidth = 0.5) +
  geom_text(data = segments_df,
            aes(x = xend, y = yend, label = label),
            color = "red", hjust = 1.2, vjust = 1.2) +
  geom_hline(yintercept = 0, linetype=2) +
  geom_vline(xintercept = 0, linetype=2) +
  geom_point(aes(x = x0.pca$PC1, y = x0.pca$PC2), color = "red", size = 3)

##### ##### ##### #####  
## Model Comparisons ##
##### ##### ##### ##### 
### Advanced Comparisons
models <- data.frame(model = c("mlr.forward", "mlr.bestsub",
                               "glm.glmnet", "glm.ncvreg",
                               "mlr.pca"))

# Filter out the model with "lasso" in the name
filtered_models <- subset(models, !grepl("lasso|pca", model))

# Get the models using the get() function
model_list <- lapply(filtered_models$model, get)

texreg::screenreg(model_list, digits = 3, single.row = T)

# Initialize for loop
n <- length(y)
y <- nfl$games.won
mse_full <- mean((y - mean(y))^2)
x.pca <- predict(pca, x)

for (model in models$model) {
  # Calculate the predicted values
  if (grepl("lasso", model)) {
    ifelse(grepl("ncvreg", model),
           p <- sum(get(model)$beta != 0) - 1,
           p <- sum(get(model)$beta != 0))
    y_pred <- predict(get(model), x)
  } else {
    p <- length(coef(get(model))[-1])
    ifelse(grepl("pca", model),
           y_pred <- predict(get(model), data.frame(x.pca)),
           y_pred <- predict(get(model), data.frame(x)))
  }
  
  # Calculate the residuals
  resid <- y - y_pred
  
  # Calculate the Root Mean Square Error
  mse <- mean(resid^2)
  rmse <- sqrt(mse)
  
  # Calculate the Mean Absolute Error
  mae <- mean(abs(y - y_pred))
  
  # Estimate the Cp statistics
  cp <- (mse / mse_full) * (n - p) + 2 * p
  # cp <- (mse / mse_full) - (n -2*p)
  
  # Calculate the max vif
  vif.mx <- ifelse(p < 2, NA, max(vif(get(model))))
  
  # Calculate the adjusted R-squared
  r2 <- cor(y, y_pred)^2
  adj.r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  
  assign(paste0("eval.", model),
         c(Preds = p,
           RMSE = rmse,
           MAE = mae,
           Cp = cp,
           VIFmx = vif.mx,
           Adj.R2 = adj.r2))
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
rounded <- signif(result_df,4)

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

##### ##### ##### ##### ##### ##### ##### 
## Linear Assumptions Fix with BoxCox ##
##### ##### ##### ##### ##### ##### ##### 
## Check which p power transformation on response variable
## Makes relationships btw response ~ regressors more linear
model_selected <- "mlr.forward"
a <- car::boxCox(get(model_selected), family="yjPower") ## No need to transform

## Summary of model
summary(get(model_selected))

## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(get(model_selected))

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
# Transform to along dataframe to plot with ggplot
regress.vars <- names(coef(get(model_selected))[-1])

df2 <- df[c(regress.vars, ".resid")] %>% 
  tidyr::pivot_longer(cols = regress.vars, names_to = "statistic", values_to = "xvalue")

ggplot(df2, aes(x = xvalue, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Linearity Check: Residuals vs Regressors", x = NULL) +
  facet_wrap(~ statistic, scales = "free")

# SAME AS ABOVE WITH FULL FIT
car::residualPlots(get(model_selected))

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
ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(get(model_selected)))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(get(model_selected))))), col = "red", lty = 2) +
  labs(title = "Scale-Location",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))")

# Breusch-Pagan test
lmtest::bptest(get(model_selected))

### 4. Check for independence of random error ###
# scatter plot of residuals sorted by hp
ggplot(df2 %>% arrange(-xvalue),
       aes(x = 1:nrow(df2), y = .resid)) +
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
lmtest::dwtest(get(model_selected))

### Final diagnostic plot often presented, too
ggplot(df, aes(x = .fitted, y = get(response))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_abline(intercept  = 0, slope = 1, col = "red", lty = 2) +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")

# SIMILAR TO ABOVE
car::marginalModelPlots(get(model_selected))

# Residuals vs Leverage
ggplot(df, aes(x = hatvalues(get(model_selected)), y = .std.resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept  = 0, col = "red", lty = 2) +
  labs(title = "Residuals vs Leverage",
       x = "Leverage",
       y = "Standardized Residuals")

# SIMILAR TO ABOVE BUT MARGINALLY (PER REGRESSOR)
car::leveragePlots(get(model_selected))

