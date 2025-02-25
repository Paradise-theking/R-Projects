# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(dplyr)
library(car)
library(StepReg)# stepwise()
library(leaps) # regsubsets()
library(glmnet) # glmnet()
library(ncvreg) # ncvreg()
library(corrplot)
library(texreg)
library(DT)

# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here()

(path <- paste0(dirname(path.full), "/dataset/autompg.csv"))
autompg <- readr::read_csv(path)
autompg <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/autompg.csv")
#### #### #### #### ####
# Stepwsie Selection  #
#### #### #### #### ####
### Forward Selection
## By Hand
for (col in names(autompg %>% select(-c(mpg)))){
  mlr <- lm(mpg ~ get(col), data = autompg)
  a <- summary(mlr)
  cat("Partial F-value", col,
      "\t", a$fstatistic[1],
      "\t", "p-value: ", a$coefficients[,"Pr(>|t|)"][2], "\n")
}

mlr1 <- lm(mpg ~ wt, data = autompg)
summary(mlr1)

for (col in names(autompg %>% select(-c(mpg, wt)))){
  mlr <- lm(mpg ~ wt + get(col), data = autompg)
  a <- summary(mlr)
  cat("Partial F-value", col,
      "\t", a$fstatistic[1],
      "\t", "p-value: ", a$coefficients[,"Pr(>|t|)"][2], "\n")
}

mlr2 <- lm(mpg ~ wt + year, data = autompg)
summary(mlr2)

for (col in names(autompg %>% select(-c(mpg, wt, year)))){
  mlr <- lm(mpg ~ wt +year+ get(col), data = autompg)
  a <- summary(mlr)
  cat("Partial F-value", col,
      "\t", a$fstatistic[1],
      "\t", "p-value: ", a$coefficients[,"Pr(>|t|)"][2], "\n")
}

mlr3 <- lm(mpg ~ wt + year + acc, data = autompg)
summary(mlr3)

## Forward Selection with Stepreg
(mlr.forward <- StepReg::stepwise(mpg ~ ., data = autompg,
                                  selection = "forward", select = "adjRsq"))

response <- "mpg"
formula.forward <- paste0(response, " ~ ",
                          paste0(mlr.forward$`Selected Varaibles`[-1],
                                 collapse = " + "))

mlr.forward <- lm(formula(formula.forward), data = autompg)
summary(mlr.forward)

# Added Variable plot
car::avPlots(mlr.forward) # Gives intuition abt regressor coefficients
# Marginal Model Plot
car::marginalModelPlots(mlr.forward) # Good for Linear Assumption 1
# Marginal Residual plot
car::residualPlots(mlr.forward) # Good for Linear Assumption 1
# Component Residual plot
car::crPlots(mlr.forward) # purple = data, dotted blue = model
# Marginal Leverage plots
car::leveragePlots(mlr.forward)

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(mlr.forward, x0, interval = "confidence")
predict(mlr.forward, x0, interval = "predict")

### Backward Selection
## By Hand
mlr.full <- lm(mpg ~ ., data=autompg)
summary(mlr.full) # highest p-value comes from hp

mlr1.a <- update(mlr.full, . ~ . - hp)
summary(mlr1.a) # highest p-value comes from acc

mlr2.a <- update(mlr1.a, . ~ . - acc)
summary(mlr2.a) # highest p-value comes from disp

mlr3.a <- update(mlr2.a, . ~ . - disp)
summary(mlr3.a)

# Backward selection on arbitrary length loop.len
loop.len <- 3
variable_to_remove.p <- list()
# Iterate loop.len times
for (i in 1:loop.len) {
  # Get the summary statistics of the current model
  ifelse(i==1,
         mlr.current <- mlr.full,
         mlr.current <- get(paste0("mlr",i-1)))
  
  summary_stats <- summary(mlr.current)
  
  # Extract the p-values
  p_values <- summary_stats$coefficients[, "Pr(>|t|)"]
  
  # Identify the variable with the highest p-value
  variable_to_remove.p[[i]] <- names(p_values)[which.max(p_values)]
  
  # Create a new model by removing the variable with the highest p-value
  updated_formula <- paste0(". ~ . - ", variable_to_remove.p[[i]])
  assign(paste0("mlr",i),
         update(mlr.current,
                formula = as.formula(updated_formula)))
}

## Backward Selection with Stepreg
(mlr.backward <- StepReg::stepwise(mpg ~ ., data = autompg,
                                   selection = "backward", select = "adjRsq"))

response <- "mpg"
formula.backward <- paste0(response, " ~ ",
                          paste0(mlr.backward$`Selected Varaibles`[-1],
                                 collapse = " + "))

mlr.backward <- lm(formula(formula.backward), data = autompg)
summary(mlr.backward)

# Added Variable plot
car::avPlots(mlr.backward) # Gives intuition abt regressor coefficients
# Marginal Model Plot
car::marginalModelPlots(mlr.backward) # Good for Linear Assumption 1
# Marginal Residual plot
car::residualPlots(mlr.backward) # Good for Linear Assumption 1
# Component Residual plot
car::crPlots(mlr.backward) # purple = data, dotted blue = model
# Marginal Leverage plots
car::leveragePlots(mlr.backward)

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(mlr.backward, x0, interval = "confidence")
predict(mlr.backward, x0, interval = "predict")

### Stepwise Selection with Stepreg
(mlr.mixed <- StepReg::stepwise(mpg ~ ., data = autompg,
                                   selection = "bidirection", select = "adjRsq"))

response <- "mpg"
formula.mixed <- paste0(response, " ~ ",
                           paste0(mlr.mixed$`Selected Varaibles`[-1],
                                  collapse = " + "))

mlr.mixed <- lm(formula(formula.mixed), data = autompg)
summary(mlr.mixed)

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(mlr.mixed, x0, interval = "confidence")
predict(mlr.mixed, x0, interval = "predict")

#### #### #### #### #### ####
## Best Subset with leaps ##
#### #### #### #### #### ####
## Build all combinations of linear models 
models <- leaps::regsubsets(mpg ~ ., data = autompg, nvmax = NULL,
                            method = "exhaustive", nbest=1)

plot(models, scale = "adjr2")
plot(models, scale = "bic")
plot(models, scale = "Cp")

results <- summary(models)

## Plot results
# Extract and models results
tb <- tibble(predictors = seq(1, nrow(results$outmat)), #nrows tells me how many predictors
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
adjr2.max <- which.max(results$adjr2)
(vars.adjr2 <- colnames(results$which)[results$which[adjr2.max,]])

bic.min <- which.min(results$bic)
(vars.bic <- colnames(results$which)[results$which[bic.min,]])

cp.min <- which.min(results$cp)
(vars.cp <- colnames(results$which)[results$which[cp.min,]])

# Bulding model from one of the above best statistic
response <- "mpg"
formula.bestsub <- paste0(response, " ~ ",
                           paste0(vars.cp[-1], #here, using vars.cp as my choosen stat
                                  collapse = " + "))

mlr.bestsub <- lm(formula(formula.bestsub), data = autompg)
summary(mlr.bestsub)

x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(mlr.bestsub, x0, interval = "confidence")
predict(mlr.bestsub, x0, interval = "predict")

#### #### #### #### ####
# Regularization: Lasso #
#### #### #### #### ####
# Initialize
x <- model.matrix(mpg ~ ., autompg)[,-1]
y <- autompg$mpg

## lasso
la.eq <- glmnet(x, y, alpha=1) 
# plot
matplot(log(la.eq$lambda), t(la.eq$beta),
        type="l", main="Lasso", lwd=2)
## Ridge
ri.eq <- glmnet(x, y, alpha=0) 
# plot
matplot(log(ri.eq$lambda), t(ri.eq$beta),
        type="l", main="Ridge", lwd=2)

## Bulding lasso with glmnet
# Cross-validation to get best hyperparameter, lambda
cv.glmnet <- cv.glmnet(x, y, alpha=1, standardize=T, keep = T,
                type.measure = "default") # alpha = 1 is lasso.

plot(cv.glmnet)

coef(cv.glmnet, c(cv.glmnet$lambda.min,
               cv.glmnet$lambda.1se))

# Build lasso regression with above best lambda(BETTER)
mlr.lasso.glmnet <- glmnet(x, y, alpha=1, standardize=T,
                     lambda = cv.glmnet$lambda.1se) # try cv.glmnet$lambda.1se

coef(mlr.lasso.glmnet)
# (coef.lasso <- predict(cv.glmnet, type = "coefficients", s = cv.glmnet$lambda.min))

response <- "mpg"
vars.lasso <- row.names(coef(mlr.lasso.glmnet))[which(coef(mlr.lasso.glmnet) != 0)][-1]
intercept <- attr(coef(mlr.lasso.glmnet), "x")[1]
vars.lasso.coef <- attr(coef(mlr.lasso.glmnet), "x")[-1]

equation <- paste0(response, " = ", round(intercept, 4), " + ",
                   paste0(round(vars.lasso.coef,5), "*", vars.lasso, collapse = " + "))
(equation <- gsub("\\+ -", "- ", equation))

# Fit glm model with chosen regressors
glm.formula <- paste0(response, " ~ ",
                      paste0(vars.lasso, collapse = " + "))

glm.glmnet <- glm(as.formula(glm.formula), data = autompg)
glm.glmnet$coefficients <- setNames(c(intercept, vars.lasso.coef),
                                    names(glm.glmnet$coefficients))

summary(glm.glmnet)
# Added Variable plot
car::avPlots(glm.glmnet) # Gives intuition abt regressor coefficients
# Marginal Model Plot
car::marginalModelPlots(glm.glmnet) # Good for Linear Assumption 1
# Marginal Residual plot
car::residualPlots(glm.glmnet) # Good for Linear Assumption 1
# Component Residual plot
car::crPlots(glm.glmnet) # purple = data, dotted blue = model

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(glm.glmnet, x0, interval = "confidence") # Notice, glm can't do intervals

# Predicting lasso model
vars <- dimnames(coef(mlr.lasso.glmnet))[[1]][-1]

xM <- matrix(rep(0, length(vars)), nrow = 1)
colnames(xM) <- vars

x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)
# xM[vars %in% vars.lasso] <- c(1200) # add, in order, the value of each var in vars.lasso

predict(mlr.lasso.glmnet, as.matrix(x0), s = cv.glmnet$lambda.min)

## Bulding lasso with ncvreg
cv.ncvreg <- cv.ncvreg(x, y, penalty="lasso")
plot(cv.ncvreg)

mlr.lasso.ncvreg <- ncvreg(x, y, penalty="lasso", 
                     lambda = cv.ncvreg$lambda.min)

summary(ncvreg(x, y, penalty="lasso"), lambda = cv.ncvreg$lambda.min)
coef(mlr.lasso.ncvreg)

response <- "mpg"
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

glm.ncvreg <- glm(as.formula(glm.formula), data = autompg)
glm.ncvreg$coefficients <- setNames(c(intercept, regressors),
                                    names(glm.ncvreg$coefficients))

summary(glm.ncvreg)

# Added Variable plot
car::avPlots(glm.ncvreg) # Gives intuition abt regressor coefficients
# Marginal Model Plot
car::marginalModelPlots(glm.ncvreg) # Good for Linear Assumption 1
# Marginal Residual plot
car::residualPlots(glm.ncvreg) # Good for Linear Assumption 1
# Component Residual plot
car::crPlots(glm.ncvreg) # purple = data, dotted blue = model

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)

predict(glm.ncvreg, x0, interval = "confidence") # Notice, glm can't do intervals

# Prediciting lasso model
vars <- names(coef(mlr.lasso.ncvreg))[-1]

xM <- matrix(rep(0, length(vars)), nrow = 1)
colnames(xM) <- vars

x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)
# xM[vars %in% vars.lasso] <- c(6, 1200, 12, 77) # add, in order, the value of each var in vars.lasso

predict(mlr.lasso.ncvreg, as.matrix(x0), s = cv.ncvreg$lambda.min)

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

# Check correlation of PCs
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
ggplot(cbind(mpg = autompg$mpg, pcs),
       aes(x = PC1, y = PC2, color = mpg)) +
  geom_point() +
  labs(x = "PC1", y = "PC2") +
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
corr <- cor(autompg)
corrplot::corrplot(corr, method= "color", order = "FPC", diag = FALSE,
         addCoef.col = TRUE, type = 'upper')

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

# pca.data <- pcs %>% mutate(mpg = ctraining.data$mpg) #same
pca.data <- cbind(mpg = autompg$mpg, pcs)

mlr.pca <- lm(mpg ~ PC1 + PC2 + PC3, data = pca.data)
summary(mlr.pca)

vif(mlr.full)
vif(mlr.pca)#always 1 by definition

# Added Variable plot
car::avPlots(mlr.pca) # Gives intuition abt regressor coefficients
# Marginal Model Plot
car::marginalModelPlots(mlr.pca) # Good for Linear Assumption 1
# Marginal Residual plot
car::residualPlots(mlr.pca) # Good for Linear Assumption 1
# Component Residual plot
car::crPlots(mlr.pca) # purple = data, dotted blue = model
# Marginal Leverage plots
car::leveragePlots(mlr.pca)

# Predict
x0 <- data.frame(cyl = 6, disp = 300, hp = 250, wt = 1200, acc = 25, year = 77)
x0.pca <- data.frame(predict(pca, newdata = x0))

predict(mlr.pca, x0.pca, interval = "confidence") # Notice, glm can't do intervals

##### ##### ##### #####  
## Model Comparisons ##
##### ##### ##### ##### 
### Advanced Comparisons
models <- data.frame(model = c("mlr.forward", "mlr.backward",
                               "glm.glmnet", # "mlr.lasso.glmnet",
                               "glm.ncvreg", # "mlr.lasso.ncvreg",
                               "mlr.pca"))

# Filter out the model with "lasso" in the name
filtered_models <- subset(models, !grepl("lasso|pca", model))

# Get the models using the get() function
model_list <- lapply(filtered_models$model, get)

texreg::screenreg(model_list, digits = 3, single.row = T)

# Initialize for loop
n <- length(y)
y <- autompg$mpg
mse_full <- mean((y - mean(y))^2)
x.pca <- scale(x) %*% as.matrix(pca$rotation)

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
  vif.mx <- ifelse(p < 2,
                   NA,
                   max(vif(get(model))))
  
  # Calculate the adjusted R-squared
  r2 <- cor(y, y_pred)^2
  adj.r2 <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  
  assign(paste0("eval.", model),
         c(Preds = p,
           RMSE = rmse,
           MAE = mae,
           Cp = cp,
           VIFmx = vif.mx, # can't have with lasso models
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

