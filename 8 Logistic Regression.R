# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                 detach, character.only=TRUE, unload=TRUE, force=TRUE)))

rm(list = ls())

library(ggplot2) #Plots
library(fastDummies)
library(dplyr)
library(GGally)
library(car) #vif
library(caret)
library(bestglm)
library(MLmetrics)

library(pROC) #roc
library(gains) #gains
library(DT) #datatable

# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here()
path <- paste0(dirname(path.full), "/datasets/eBayAuctions2.csv")
path <- "C:/Users/PARADISE/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/eBayAuctions2.csv"

ebay1 <- readr::read_csv(path)
glimpse(ebay1)
ebay1$Competitive <- factor(ebay1$Competitive)

ebay <- fastDummies::dummy_cols(ebay1, select_columns = 'currency',
                    remove_first_dummy = TRUE,
                    remove_selected_columns = TRUE)

str(ebay1)
##### ##### ##### ##### ##### 
## Simple EDA on ebay data ##
##### ##### ##### ##### ##### 
## Pairs Plot
GGally::ggpairs(ebay1, mapping = aes(color = currency), #try hashing out the mapping
                diag=list(continuous="densityDiag",
                          discrete="barDiag"),
                upper=list(continuous= GGally::wrap("cor", size = 3.5),
                           combo="facethist",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="box",
                           discrete="facetbar"))

## Bar plots
# Bar plot of response variable colored by categoric regressor
ggplot(ebay1, aes(x = Competitive, fill = currency)) + 
  geom_bar() + 
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(x = "Competitive", y = "Count of Competitive", fill = "Currency") +
  stat_count(aes(label =
                   paste0(round(after_stat(count)/sum(after_stat(count)) * 100), "%")), 
             geom = "text", color = 'white',
             position = position_stack(vjust = 0.5))

# Opposite of above
ggplot(ebay1, aes(x = currency, fill = Competitive)) + 
  geom_bar() + 
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(x = "currency", y = "Count of currency", fill = "Competitive") +
  stat_count(aes(label =
                   paste0(round(after_stat(count)/sum(after_stat(count)) * 100), "%")), 
             geom = "text", color = 'white',
             position = position_stack(vjust = 0.5))

## Parallel Boxplots
# Preprocess dataframe for ggplot
df <- ebay1 %>% 
  tidyr::pivot_longer(cols = -c(Competitive, currency),
                      names_to = "variable",
                      values_to = "value")

ggplot(df, aes(x = Competitive, y = value, fill = Competitive)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + # adds whisker end vertical lines
  geom_boxplot(outlier.shape = 1) + #gray box and outlier circles
  expand_limits(y = c(-1,1)) + # expands y-axis to make box look smaller
  labs(y = NULL) +
  scale_fill_grey(start = 0.6, end = .9) +
  facet_wrap(~ variable, nrow = 2, ncol = 2, scales = "free") +
  guides(fill = FALSE)

## Histograms
ggplot(df, aes(x = value, fill = Competitive)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  labs(x = df$variable, y = "Count", fill = "Competitive") +
  facet_wrap(~ variable, nrow = 2, ncol = 2, scales = "free") +
  scale_fill_grey() +
  theme(legend.position = "top") # Put legend at top

#### ##### ##### ##### #### 
## Create Logistic Models ##
#### ##### ##### ##### #### 
### SLR ###
logistic.slr <- glm(Competitive ~ OpenPrice, family = binomial(link = 'logit'), data = ebay)
summary(logistic.slr)

confint.default(logistic.slr)
# Odds Ratio
exp(confint.default(logistic.slr))

# Extract Equation
extract_equation <- function(model, response) {
  regressors <- coef(model)[-1]
  assign("vars", names(regressors), envir = .GlobalEnv)
  assign("intercept", coef(model)[1], envir = .GlobalEnv)
  assign("vars.coef", signif(regressors, 3), envir = .GlobalEnv)

  equation <- paste0(response, " = ", signif(intercept, 3), " + ", paste0(vars.coef, "*", vars, collapse = " + "))
  equation <- gsub("\\+ -", "- ", equation)
  
  return(equation)
}

extract_equation(logistic.slr, "Competitive") # gives you equation

exp(vars.coef) #this is the odds ratio

### Visualizations
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(logistic.slr)

## Observed vs Predicted Values
ggplot(df, aes(x = .fitted, y = as.numeric(Competitive)-1)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              fullrange = T, span = 1, se = F, color = "gray66", lty = 2) +
  expand_limits(x = 5) +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")

## Response vs Regressor/s, curve is fitted
ggplot(df, aes(x = OpenPrice, y = as.numeric(Competitive)-1)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                fullrange = T, span = 1, se = F, color = "gray66", lty = 2) +
  expand_limits(x = -100) +
  labs(title = "Competitive vs OpenPrice",
         y = "Competitive")


### Evaluation of logistic regression model
c <-confusionMatrix(factor(
  ifelse(fitted(logistic.slr) > 0.5, "1", "0")),
  ebay$Competitive, positive = "1")

c$byClass['F1'] #F1 needs to be extracted explicitly

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(0)

x0 <- data.frame(t(sapply(ebay, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(Competitive))

(x0 <- type.convert(x0, as.is = TRUE))

predict(logistic.slr, newdata = x0, type = "response")

# Predict finds the Probability for us.
1/(1+exp(-(sum(x0[vars]*vars.coef)+intercept))) # Notice slight rounding error


### MLR ###
logistic.full <- glm(Competitive ~ ., family = binomial(link = 'logit'), data = ebay)
summary(logistic.full)

confint.default(logistic.full)
# Odds Ratio
exp(confint.default(logistic.full))

vif(logistic.full)

# Extract equation
extract_equation(logistic.full, "Competitive")

exp(vars.coef)
#eg Closed price is 1.15..this is beyond one 
#so the higher it is the more it becomes competitive

#us is t times more competitive thn the base line
#which is the one currency excluded there being EUR

### Visualizations
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(logistic.full)

## Observed vs Predicted Values
ggplot(df, aes(x = .fitted, y = as.numeric(Competitive)-1)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              fullrange = T, span = 1, se = F, color = "gray66", lty = 2) +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values")

## Response vs Regressor/s
## Check against individual regressor variables especially in MLR models
# Transform to along dataframe to plot with ggplot
response <- "Competitive"
regress.vars <- names(coef(logistic.full)[-1])[!grepl("currency",
                                                  names(coef(logistic.full)[-1]))]

df2 <- df[c(regress.vars, response)] %>% 
  tidyr::pivot_longer(cols = regress.vars, names_to = "statistic", values_to = "xvalue")

ggplot(df2, aes(x = xvalue, y = as.numeric(get(response))-1)) +
    geom_point() +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                fullrange = T, span = 1, se = F, color = "gray66", lty = 2) +
    # geom_text(aes(label = ifelse(color %in% c("red"), idx, NA), vjust = 1.5)) +
    theme(legend.position = "none") +
    labs(title = "Response vs Regresssors", x = NULL, y = NULL) +
    facet_wrap(~ statistic, scales = "free")

# Evaluation of logistic regression model
 c1 <-confusionMatrix(factor(
  ifelse(fitted(logistic.full) > 0.5, "1", "0")),
                ebay$Competitive, positive = "1")
 
 c1$byClass['F1']

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(2)

x0 <- data.frame(t(sapply(ebay, function(col) sample(col, 1, replace = T)))) %>%
    select(-c(Competitive))

(x0 <- type.convert(x0, as.is = TRUE))

predict(logistic.full, newdata = x0, type = "response")

# Predict finds the Probability for us.
1/(1+exp(-(sum(x0[vars]*vars.coef)+intercept))) # Notice slight rounding error

#### ##### ##### #####
## Outlier Analysis ##
#### ##### ##### #####
### Outliers ###
## RStandard
best <- "logistic.full"
rstand <- rstandard(get(best))

df <- data.frame(idx = c(1:length(rstand)),
                 rstand = rstand)

fence <- 3

(out_rstan <- which(abs(rstand) > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "rstandard")

# Create the plot for hat values
ggplot(df3, aes(x = idx, y = rstandard)) +
  geom_point(size = ifelse(df3$type %in% c("rstand") &
                             abs(df3$rstandard) > fence, 3, 1.5),
             col = ifelse(df3$type %in% c("rstand") &
                            abs(df3$rstandard) > fence, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("rstand") &
  #                                abs(rstandard) > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(fence, -fence), linetype = 2, color = "gray50") +
  labs(title = "Outlier Points (RStandard)",
       x = "Row number", y = "RStandard")

## RStudent
rstud <- rstudent(get(best))

df <- data.frame(idx = c(1:length(rstud)),
                 rstud = rstud)

fence <- 3

(out_rstud <- which(abs(rstud) > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "rstudent")

# Create the plot for hat values
ggplot(df3, aes(x = idx, y = rstudent)) +
  geom_point(size = ifelse(df3$type %in% c("rstud") &
                             abs(df3$rstudent) > fence, 3, 1.5),
             col = ifelse(df3$type %in% c("rstud") &
                            abs(df3$rstudent) > fence, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("rstud") &
  #                                   abs(rstudent) > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(fence, -fence), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Outlier Points (RStudent)",
       x = "Row number", y = "Rstudent")

### Leverage Points ###
## Hat Values
hatvals <- hatvalues(get(best))

df <- data.frame(idx = c(1:length(hatvals)),
                 hatvals = hatvals)

n <- nrow(df)
k <- length(get(best)$coefficients) - 1

fence <- (2*(k+1)/n)

lev_hv <- which(hatvals > fence)

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "hatvals")

# Create the plot for hat values
ggplot(df3, aes(x = idx, y = hatvals)) +
  geom_point(size = ifelse(df3$type %in% c("hatvals") &
                             df3$hatvals > fence, 3, 1.5),
             col = ifelse(df3$type %in% c("hatvals") &
                            df3$hatvals > fence, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("hatvals") &
  #                                             hatvals > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(fence), linetype = 2, color = "gray50") +
  labs(title = "Leverage Points (Hat Values)",
       x = "Row number", y = "Hat Values")

### Influential Points ### 
## Difference in Fits
diffits <- dffits(get(best))

df <- data.frame(idx = c(1:length(diffits)),
                 diffits = diffits)

n <- nrow(df)
k <- length(get(best)$coefficients) - 1

fences <- 2*sqrt((k+2)/(n-k-2))

(infl_d <- which(abs(diffits) > fences))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "dffits")

# Create the plot for dffits
ggplot(df3, aes(x = idx, y = dffits)) +
  geom_point(size = ifelse((df3$type %in% c("diffits")) &
                             abs(df3$dffits) > fences, 3, 1.5),
             col = ifelse(df3$type %in% c("diffits") &
                            abs(df3$dffits) > fences, "red", "black")) +
  geom_hline(yintercept = c(fences, -fences), linetype = 2, color = "gray50") +
  labs(title = "Influential Points (dffits)",
       x = "Row number", y = "Difference in Fits")

## Cook's Distance
cookd <- cooks.distance(get(best)) 

cutoff <- 0.5
cutoff <- 4/(nrow(df)-length(get(best)$coefficients)-2) #alternative cutoff for huge n 

(infl_cooks <- which(cookd > cutoff))

# Create a data frame with Cook's distance and row numbers
df <- data.frame(idx = c(1:length(cookd)),
                 cookd = cookd)

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "cooks")

# Create the plot for Cook's distance
ggplot(df3, aes(x = idx, y = cooks)) +
  geom_point(size = ifelse(df3$type %in% c("cookd") &
                             df3$cooks > cutoff, 3, 1.5),
             col = ifelse(df3$type %in% c("cookd") &
                            df3$cooks > cutoff, "red", "black")) +
  geom_hline(yintercept = cutoff, linetype = 2, color = "gray50") +
  labs(title = "Influential Points (Cook's distance)",
       x = "Row number", y = "Cook's distance")

## CovRatio
cvrt <- covratio(get(best)) 

n <- nrow(df)
k <- length(get(best)$coefficients) - 1

fence_high <- (1 + 3*(k+1)/n)
fence_low <- (1 - 3*(k+1)/n)


(infl_cvr <- c(which(cvrt > fence_high),
                       which(cvrt < fence_low)))

# Create a data frame with covratio and row numbers
df <- data.frame(idx = c(1:length(cvrt)),
                 cvrt = cvrt)

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "covratio")

# Create the plot for covratio
ggplot(df3, aes(x = idx, y = covratio)) +
  geom_point(size = ifelse((df3$type %in% c("cvrt")) &
                             (df3$covratio > fence_high |
                                df3$covratio < fence_low), 3, 1.5),
             col = ifelse(df3$type %in% c("cvrt") &
                            (df3$covratio > fence_high |
                               df3$covratio < fence_low), "red", "black")) +
  geom_hline(yintercept = c(fence_high, fence_low), linetype = 2, color = "gray50") +
  labs(title = "Influential Points (covratio)",
       x = "Row number", y = "Covratio")

### Which Points to Remove? ###
df <- data.frame(
  type = c("out_rstan", "out_rstud", "lev_hv",
             "infl_cooks", "infl_cvr", "infl_d"),
  length = c(length(out_rstan), length(out_rstud), length(lev_hv),
             length(infl_cooks), length(infl_cvr), length(infl_d)))

ggplot(df, aes(x = reorder(type, -length), y = length)) +
  geom_bar(stat = "identity")

# Above I look for the LEAST amount especially in INFLUENCE points
# In the above example, that would be infl_d points

#### ##### ##### ##### ##### ##### #####
## Visualize Points we Want to Remove ##
#### ##### ##### ##### ##### ##### ##### 
## Pre-process:
# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(get(best))

col.data <- df %>%
  dplyr::mutate(color = case_when(
    rownames(df) %in% infl_d ~ "red",
    TRUE ~ "black")) %>%
  dplyr::mutate(idx = rownames(df)) #since we choose infl_d to remove
                                    #we make a new column  for it and color it red

### Observed vs Predicted Values
(p1 <- ggplot(col.data, aes(x = .fitted, y = as.numeric(Competitive)-1, col = color)) +
  geom_point(size = ifelse(col.data$color %in% c("red"), 3, 1.5)) +
  # geom_text(aes(label = ifelse(color %in% c("red"), idx, NA), vjust = 1.5)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              span = 1, se = F, color = "gray66", lty = 2) +
  scale_colour_manual(values = c("black", "red")) +
  theme(legend.position = "none") +
  labs(title = "Observed vs Predicted Values ",
       x = "Fitted Values",
       y = "Actual Values"))

## Check against individual regressor variables especially in MLR models
# Transform to along dataframe to plot with ggplot
response <- "Competitive"
regress.vars <- names(coef(get(best))[-1])[!grepl("currency",
                                                  names(coef(get(best))[-1]))]

df2 <- col.data[c(regress.vars, response, "color", "idx")] %>% 
  tidyr::pivot_longer(cols = regress.vars, names_to = "statistic", values_to = "xvalue")

(p2 <- ggplot(df2, aes(x = xvalue, y = as.numeric(get(response))-1, col = color)) +
  geom_point(size = ifelse(df2$color %in% c("red"), 3, 1.5)) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                span = 1, se = F, color = "gray66", lty = 2) +
  # geom_text(aes(label = ifelse(color %in% c("red"), idx, NA), vjust = 1.5)) +
    scale_colour_manual(values = c("black", "red")) +
  theme(legend.position = "none") +
  labs(title = "Response vs Regresssors", x = NULL, y = NULL) +
  facet_wrap(~ statistic, scales = "free"))

# cowplot::plot_grid(p2, p1, nrow = 2)

#### ##### ##### #####
## Build More Models ##
#### ##### ##### #####
### Logistic StepWise MINUS OLI points ###
## Pre-process
# Remove OLI points
ebay.xouts <- ebay %>% slice(-c(infl_d)) #here we slice out infl_d

# Build full model w/out OLI points
logistic.full.xout <- glm(Competitive ~ ., family = binomial(link = 'logit'), data = ebay.xouts)
summary(logistic.full.xout)

logistic.forward.xout <- MASS::stepAIC(logistic.full.xout,
                                       direction = "forward",
                                       k = log(nrow(ebay.xouts)))

summary(logistic.forward.xout) # same as logistic.full.xout

vif(logistic.forward.xout)

# Evaluation of logistic regression model
confusionMatrix(factor(
  ifelse(fitted(logistic.forward.xout) > 0.5, "1", "0")),
  ebay.xouts$Competitive, positive = "1")

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(0)

x0 <- data.frame(t(sapply(ebay, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(Competitive))

(x0 <- type.convert(x0, as.is = TRUE))

predict(logistic.full, newdata = x0, type = "response")

### Logistic Best Subset ###
# Pre-process
Xy <- data.frame(ebay %>% 
  dplyr::relocate(Competitive, .after = last_col()))

logistic.bestglm <- bestglm(Xy = Xy,
                       family = binomial,
                       IC = "BIC",
                       method = "exhaustive")

(s <- summary(logistic.bestglm$BestModel))

# Extract formula
response <- "Competitive"
bestglm.formula <- paste0(response, " ~ ",
                      paste0(rownames(s$coefficients)[-1], collapse = " + "))

# Build BestGLM Model
logistic.bestglm <- glm(formula(bestglm.formula), family = binomial(link = 'logit'), data = ebay)
summary(logistic.bestglm)

vif(logistic.bestglm)

# Evaluation of logistic regression model
confusionMatrix(factor(
  ifelse(fitted(logistic.bestglm) > 0.5, "1", "0")),
  ebay$Competitive, positive = "1")

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(0)

x0 <- data.frame(t(sapply(ebay, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(Competitive))

(x0 <- type.convert(x0, as.is = TRUE))

predict(logistic.full, newdata = x0, type = "response")

#### #### #### #### #### 
### Comparing Models ###
#### #### #### #### #### 
# Create data frame of each of the model names
models <- data.frame(model = c("logistic.slr","logistic.full", 
                               "logistic.full.xout", "logistic.bestglm"))

## Evaluative metrics ##
# Create an empty list to store the data frames
df_list <- list()
cutOff <- 0.5
# Loop over each confusion matrix and create a data frame with metrics
for (model in models$model) {
  ifelse(grepl("xout", model),
         response <- ebay.xouts$Competitive,
         response <- ebay$Competitive)
  
  pos.pred.perc <- fitted(get(model))
  
  # Create dataframe for evaluation
  eval_df <- data.frame(
    obs = response,
    pred = factor(ifelse(pos.pred.perc > cutOff, "1", "0")),
    pos.pred.perc,
    1 - pos.pred.perc)
  names(eval_df)[3:4] <- c("1","0")
  
  # Alternative to confusion matrix (same values, just as vectors)
  evaluative <- append(
    append(defaultSummary(eval_df, lev = levels(response)),
           prSummary(eval_df, lev = levels(response))),
    append(twoClassSummary(eval_df, lev = levels(response)),
           mnLogLoss(eval_df, lev = levels(response))))[c(1,2,6,8,9,4,3,7,10)]
  
  df_list[[model]] <- evaluative
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
    names(rounded)[-c(ncol(rounded))],
    background = styleEqual(apply(rounded, 2, function(x)
      max(x[-which.max(x)])), "#E8E8E8"),
    fontStyle = styleEqual(apply(rounded, 2, function(x)
      max(x[-which.max(x)])), "italic")) |>
  # Light gray and italicize 2nd lowest values
  formatStyle(
    names(rounded)[ncol(rounded)],
    background = styleEqual(apply(rounded, 2, function(x)
      min(x[-which.min(x)])), "#E8E8E8"),
    fontStyle = styleEqual(apply(rounded, 2, function(x)
      min(x[-which.min(x)])), "italic")) |>
  # dark gray and bold highest values
  formatStyle(
    names(rounded)[-c(ncol(rounded))],
    background = styleEqual(apply(rounded, 2, max), "darkgray"),
    fontWeight = styleEqual(apply(rounded, 2, max), "bold") ) |>
  # dark gray and bold lowest values
  formatStyle(
    names(rounded)[ncol(rounded)],
    background = styleEqual(apply(rounded, 2, min), "darkgray"),
    fontWeight = styleEqual(apply(rounded, 2, min), "bold") )

## Create ROC curve ##
# use this for getting ROC curve on test data
(r.full <- roc(ebay$Competitive, fitted(logistic.full)))
(r.full.xouts <- roc(ebay.xouts$Competitive, fitted(logistic.full.xout)))
(r.bestglm <- roc(ebay$Competitive, fitted(logistic.bestglm)))

plot.roc(r.full, main = "ROC curve Full")
plot.roc(r.full.xouts, main = "ROC curve Rm Outliers")
plot.roc(r.bestglm, main = "ROC curve BestGLM")

## Create Lift Charts ##
lift_pairs <- list()
# Loop through each set of predictions
for (model in models$model) {
  ifelse(grepl("xout", model),
         n <- nrow(ebay.xouts),
         n <- nrow(ebay))
  
  ifelse(grepl("xout", model),
         outcome <- as.numeric(ebay.xouts$Competitive) - 1,
         outcome <- as.numeric(ebay$Competitive) - 1)
  
  pos.pred.perc <- fitted(get(model))
  # Create lift chart
  gain <- gains(outcome, pos.pred.perc)
  
  # Cumulative lift chart
  df <- data.frame(
    ncases=c(0, gain$cume.obs),
    cumComp=c(0, gain$cume.pct.of.total * sum(outcome))
  )
  
  g1 <- ggplot(df, aes(x=ncases, y=cumComp)) +
    geom_line(color = 'red') +
    geom_line(data=data.frame(ncases=c(0, n),
                              cumComp=c(0, sum(outcome))),
              color="darkgray", linetype=2) + # adds baseline
    labs(x="# cases", y="Cumulative Competitive Count", title="Cumulative Gains Chart") +
    ggtitle(paste("Lift Chart for", model))
  
  # Decile-wise lift chart
  df <- data.frame(
    percentile=gain$depth,
    meanResponse=gain$mean.resp / mean(outcome)
  )
  
  g2 <- ggplot(df, aes(x=percentile, y=meanResponse)) +
    geom_bar(stat="identity") +
    labs(x="Percentile", y="Decile Mean / Global Mean", title="Decile-wise lift chart") +
    ggtitle(paste("Lift Chart for", model))
  
  # Store lift chart pairs in lift_pairs
  lift_pairs[[model]] <- cowplot::plot_grid(g1, g2, ncol=2)
}

# Plot the lift pairs
# lapply(lift_pairs, plot)
lift_pairs
