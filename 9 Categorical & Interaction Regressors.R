# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(dplyr)
library(ggpmisc)
library(scales)
library(car)

# Set ggplot2 theme
theme_set(theme_minimal())

### Load data
path.full <- here::here()

## House price data
path <- "C:/Users/PARADISE/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/housePriceCattt.csv"
homes1 <- readr::read_csv(path)
homes1 <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/SEMESTER II/AI-Regression Analysis/datasets/housePriceCattt.csv")
##when building model with no dummy column


head(homes1)
unique(homes1$view)#viewing uniques values

homes1$waterfront <- factor(homes1$waterfront)

(homes <- fastDummies::dummy_cols(homes1, # Select all categorical regressors
                                  select_columns = c("waterfront", "view"),
                                  remove_first_dummy = TRUE,
                                  remove_selected_columns = TRUE))

head(homes)


##### ##### #####
## Simple EDA ##
##### ##### ##### 
## Pairs Plot
homes1 %>% relocate(price, .after = last_col()) %>% # response variable at end
GGally::ggpairs(mapping = aes(color = view), #try hashing out the mapping
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
ggplot(homes1, aes(x = waterfront, fill = view)) + 
  geom_bar() + 
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(x = "WaterFront", y = "Count", fill = "View") +
  stat_count(aes(label =
                   paste0(round(after_stat(count)/sum(after_stat(count)) * 100), "%")), 
             geom = "text", color = 'white',
             position = position_stack(vjust = 0.5))

# Opposite of above
ggplot(homes1, aes(x = view, fill = waterfront)) + 
  geom_bar() + 
  scale_fill_grey(start = 0.3, end = 0.7) +
  labs(x = "View", y = "Count", fill = "WaterFront") +
  stat_count(aes(label =
                   paste0(round(after_stat(count)/sum(after_stat(count)) * 100), "%")), 
             geom = "text", color = 'white',
             position = position_stack(vjust = 0.5))

## Parallel Boxplots
# Preprocess dataframe for ggplot
df <- homes1 %>% 
  tidyr::pivot_longer(cols = price,
                      names_to = "variable",
                      values_to = "value")

ggplot(df, aes(x = view, y = value, fill = view)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + # adds whisker end vertical lines
  geom_boxplot(outlier.shape = 1) + #gray box and outlier circles
  expand_limits(y = c(-1,1)) + # expands y-axis to make box look smaller
  labs(y = NULL) +
  scale_fill_grey(start = 0.6, end = .9) +
  facet_wrap(~ variable, nrow = 2, ncol = 2, scales = "free") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  guides(fill = FALSE)


ggplot(df, aes(x = waterfront, y = value, fill = waterfront)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + # adds whisker end vertical lines
  geom_boxplot(outlier.shape = 1) + #gray box and outlier circles
  expand_limits(y = c(-1,1)) + # expands y-axis to make box look smaller
  labs(y = NULL) +
  scale_fill_grey(start = 0.6, end = .9) +
  facet_wrap(~ variable, nrow = 2, ncol = 2, scales = "free") +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  guides(fill = FALSE)

##### #####
### MLR ###
##### #####
mlr.homes <- lm(price ~ ., data=homes) #same slope for categorical varibles
#you get each line for each category
summary(mlr.homes)
 
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

extract_equation(mlr.homes, "price")

##### ##### ##### ##### ##### ##### 
### The Three Plots: WaterFront ###
##### ##### ##### ##### ##### #####
## Base Plot
base <- ggplot(homes1, aes(x = sqft, y = price, color = waterfront)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  theme(legend.position = "bottom") +
  labs(title = "",
       x = "Square Feet", y = "Price", color = "WaterFront")

## Standard line of best fit,ignores waterfront
base +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              aes(group = 1), col = "black") +
  labs(title = "Full MLR: Same Intercept, Same Slope")

## Assumption of categorical regressor equations
# Different intercept, Same slope,additive or full or dot model
#if slopes are significantly statistically different,you can move on to interactive model

base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.homes))) +
  labs(title = "Full MLR: Diff Intercept, Same Slope")

## Hypothesis test: Do different views have different affect on price??
# Different intercept, Different slope
base +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  labs(title = "Full MLR: Diff Intercept, Diff Slope")

## H_0: β_2 = 0   vs    H_1: β_2 ≠ 0....joint propability
# Is there a difference in response based on categorical regressor?
#testing difference of lines
#if p-value above alpha the fail to reject
car::linearHypothesis(mlr.homes, c("waterfront_1 = 0"))#this is null hypothesis

# Conclusion: p > alpha; FAIL TO REJECT NULL.
# The regression lines corresponding to waterfront
# and non-waterfront DO NOT have different slopes.

##### ##### ##### ##### #####
### The Three Plots: View ###
##### ##### ##### ##### #####
## Base Plot
base <- ggplot(homes1, aes(x = sqft, y = price, color = view)) +
  geom_point() +
  scale_y_continuous(labels = label_number(suffix = "M", scale = 1e-6)) +
  theme(legend.position = "bottom") +
  labs(title = "",
       x = "Square Feet", y = "Price", color = "View")

## Standard line of best fit
base +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              aes(group = 1), col = "black") +
  labs(title = "Full MLR: Same Intercept, Same Slope")

## Assumption of categorical regressor equations
# Different intercept, Same slope
base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.homes))) +
  labs(title = "Full MLR: Diff Intercept, Same Slope")

## Hypothesis test: Do different views have different affect on price??
# Different intercept, Different slope
base +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  labs(title = "Full MLR: Diff Intercept, Diff Slope")
  
### Any difference in Response by Categorical Regressor?
### If either Joint or Linear Combo is rejected, the NULL is rejected.
## joint probability hypothesis for k-1 to test city(0),mountain and woods
linearHypothesis(mlr.homes, c("view_mountain = 0","view_wood = 0"))
# Conclusion: p > alpha; Therefore, FAIL TO REJECT NULL,
# view_ slopes ARE NOT statistically different...thus model 2 is better than model 3.

## linear combination of coefficients
linearHypothesis(mlr.homes, c("view_mountain - view_wood = 0"))

# Conclusion: p > alpha; Therefore, FAIL TO REJECT NULL.
# view_mountain - view_wood is not different that 0.

## Individual Dummy Regressors
# Mountain vs City (Baseline) Prices
linearHypothesis(mlr.homes, c("view_mountain = 0"))#tells us red and greens are the same
# Wood vs City (Baseline) Prices
linearHypothesis(mlr.homes, c("view_wood = 0"))#WE reject the null since blue slope is
                                            #different from red slope thus significant
# Mountain vs Wood Prices
linearHypothesis(mlr.homes, c("view_mountain = view_wood")) # Same as above

# Neither Joint nor Linear Combination assumption was rejected, but an individual
# regressor, view_wood = 0, was rejected so we can check for interactive terms.

# However, because our joint probabilities wasn't rejected, the interaction
# model I do not suspect will be successful

##### ##### ##### #####
## Interactive Models ##
##### ##### ##### #####
mlr.int <- lm(price ~ . + view_mountain*sqft + view_wood*sqft, data = homes)
summary(mlr.int)
#each sqft incease in city is 747
#add interactive terms to base terms(sqft) eg


## Assumption of categorical regressor equations
# ** Same code as above's: Different intercept, Same slope **
base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.int))) +
  labs(title = "Interactive MLR")

### Check which model is better
anova(mlr.homes, mlr.int)#h0:1st which is child in better than parent
# h0 is parent is better and we fail to reject h0 since model 2 is better
#than the interactive model.

# Conclusion: FAIL TO REJECT NULL. Not enough evidence such that we can say
# the parent (interaction) model is better than our additive model.

# Like we suspected above: While an individual dummy term suggested we look
# at an interactive model, according to our anova test, the interactive
# model is less good than our full additive model.

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(1)

x0 <- data.frame(t(sapply(homes, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(price))

# If data has mix of strings and numerics
(x0 <- type.convert(x0, as.is = TRUE))

(pred.non <- predict(mlr.homes, newdata = x0, interval = "confidence"))
(pred.int <- predict(mlr.int, newdata = x0, interval = "confidence"))

# Plot predictions
x.var <- paste0(base$mapping$x[[2]])

base +
  geom_point(aes(x = x0[[x.var]], y = pred.non[1]),
             size = 4, color = "gray50", shape = 15) +
  geom_point(aes(x = x0[[x.var]], y = pred.int[1]),
             size = 4, color = "pink", shape = 15) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  geom_smooth(method = "lm", se = F, fullrange = T, lty = 2,
              mapping = aes(y = fitted(mlr.homes))) +
  labs(title = "Two Models, Two Predictions")

##### ##### ##### ##### #####
## Example 2: Lung Capacity ##
##### ##### ##### ##### #####
## Load data
path.full <- here::here()

path <- paste0(dirname(path.full), "/datasets/LungCapacity.csv")
health1 <- readr::read_csv(path)

head(health1)

health1$sexM <- factor(health1$sexM)
health1$smoke <- factor(health1$smoke)

(health <- fastDummies::dummy_cols(health1, # Select all categorical regressors
                                  select_columns = c("sexM", "smoke"),
                                  remove_first_dummy = TRUE,
                                  remove_selected_columns = TRUE))

##### ##### #####
## Simple EDA ##
##### ##### ##### 
## Pairs Plot
health1 %>% relocate(lung.cap, .after = last_col()) %>% # response variable at end
  GGally::ggpairs(mapping = aes(color = smoke), #try hashing out the mapping
                  diag=list(continuous="densityDiag",
                            discrete="barDiag"),
                  upper=list(continuous= GGally::wrap("cor", size = 3.5),
                             combo="facethist",
                             discrete="ratio"),
                  lower=list(continuous="points",
                             combo="box",
                             discrete="facetbar"))

##### #####
### MLR ###
##### #####
mlr.health <- lm(lung.cap ~ ., data=health) 
summary(mlr.health)

# Extract Equation
extract_equation(mlr.health, "lung.cap")

##### ##### ##### ##### #####
### The Three Plots: smoke ###
##### ##### ##### ##### #####
## Base Plot
base <- ggplot(health1, aes(x = age, y = lung.cap, color = smoke)) +
  geom_point() +
  geom_jitter() +
  theme(legend.position = "bottom") +
  labs(title = "",
       x = "Age", y = "Lung Capacity", color = "Smoker")

## Standard line of best fit
base +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              aes(group = 1), col = "black") +
  labs(title = "Full MLR: Same Intercept, Same Slope")

## Assumption of categorical regressor equations
# Different intercept, Same slope (in some dimension)
base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.health))) +
  labs(title = "Full MLR: Diff Intercept, Same Slope")

## Hypothesis test: Do categorical regressors have different affects on response??
# Different intercept, Different slope
base +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  labs(title = "Full MLR: Diff Intercept, Diff Slope")

## H_0: β_2 = 0   vs    H_1: β_2 ≠ 0
# Is there a difference in response based on categorical regressor?
linearHypothesis(mlr.health, c("smoke_1 = 0"))

# Conclusion: FAIL TO REJECT NULL
# The regression lines corresponding to smoke_1
# and non-smoke DO NOT have different slopes.
#slopes are staticically the same thus no need to make
#interactive model.


##### ##### ##### ##### ##### 
### The Three Plots: SexM ###
##### ##### ##### ##### #####
## Base Plot
base <- ggplot(health1, aes(x = age, y = lung.cap, color = sexM)) +
  geom_point() +
  geom_jitter() +
  theme(legend.position = "bottom") +
  labs(title = "",
       x = "Age", y = "Lung Capacity", color = "SexMale")

## Standard line of best fit
base +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, 
              aes(group = 1), col = "black") +
  labs(title = "Full MLR: Same Intercept, Same Slope")

## Assumption of categorical regressor equations
# Different intercept, Same slope (in some dimension)
base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.health))) +
  labs(title = "Full MLR: Diff Intercept, Same Slope")

## Hypothesis test: Do categorical regressors have different affects on response??
# Different intercept, Different slope
base +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  labs(title = "Full MLR: Diff Intercept, Diff Slope")

## H_0: β_2 = 0   vs    H_1: β_2 ≠ 0
# Is there a difference in response based on categorical regressor?
linearHypothesis(mlr.health, c("sexM_1 = 0"))

# Conclusion: REJECT NULL
# The regression lines corresponding to SexM
# and SexF DO HAVE different slopes.

# Given that we found difference in slopes of sexM_1,
# we need to fit an INTERACTIVE model.

##### ##### ##### #####
## Interactive Models ##
##### ##### ##### #####
mlr.int <- lm(lung.cap ~ . + age*sexM_1 + sexM_1*smoke_1, data = health)
summary(mlr.int)
#baseline is females,lung capacity increases by 0.04 as female ages increas




## Assumption of categorical regressor equations
# ** Same code as above's: Different intercept, Same slope **
base +
  geom_smooth(method = "lm", se = F, fullrange = T, 
              mapping = aes(y = fitted(mlr.int))) +
  labs(title = "Full MLR: Diff Intercept, Same Slope")

### Check which model is better
anova(mlr.health, mlr.int)

# Conclusion: REJECT NULL. Our interaction model is an improvement on our
# NON-interaction model.

### Prediction on new data ###
RNGkind (sample.kind = "Rounding") 
set.seed(123)

x0 <- data.frame(t(sapply(health, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(lung.cap))

# If data has mix of strings and numerics
(x0 <- type.convert(x0, as.is = TRUE))

(pred.non <- predict(mlr.health, newdata = x0, interval = "confidence"))
(pred.int <- predict(mlr.int, newdata = x0, interval = "confidence"))

# Plot predictions
x.var <- paste0(base$mapping$x[[2]])

base +
  geom_point(aes(x = x0[[x.var]], y = pred.non[1]),
             size = 4, color = "gray50", shape = 15) +
  geom_point(aes(x = x0[[x.var]], y = pred.int[1]),
             size = 4, color = "pink", shape = 15) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  geom_smooth(method = "lm", se = F, fullrange = T, lty = 2,
              mapping = aes(y = fitted(mlr.health))) +
  labs(title = "Two Models, Two Predictions")

