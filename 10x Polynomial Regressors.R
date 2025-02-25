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

## Load data
path.full <- here::here()
#anova helps us see better model, child or parent model.
#regressor has to be categorical for interactive model
#pairs plot will be included in the exam
path <- paste0(dirname(path.full), "/datasets/LungCapacity.csv")
health1 <- readr::read_csv(path)

head(health1)#check factor vs dbl on data type to know which
#to make factors
str(health1)
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

##### ##### ##### ##### ##### ##### ##### ##### 
### Lung Capacity by Age Grid Smoke by Sex ###
##### ##### ##### ##### ##### ##### ##### ##### 
ggplot(health1, aes(x = age, y = lung.cap)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, fullrange = TRUE) +
  facet_grid(smoke ~ sexM, scales = "free_x",
             labeller = labeller(smoke = c("0" = "Non-Smoker", "1" = "Smoker"),
                                 sexM = c("0" = "Female", "1" = "Male")))
#facte_gris can split categorical variables,here we split by 

##### ##### ##### ##### ##### ##### ##### #####
### Regressor Polynomial Model: Non-Smokers ###
##### ##### ##### ##### ##### ##### ##### #####
## Female Non-Smokers
f.nonsmokers <- health %>% filter(sexM_1 == 0 & smoke_1 == 0) 

mlr.poly.f.non <- lm(lung.cap ~ age + I(age^2), data=f.nonsmokers) #need to use
#function 'I' if you square

summary(mlr.poly.f.non)
#significant relationship tels us that relationship is nono-linear between female non smokers
#meaning linear model not as good as a curved one.

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

extract_equation(mlr.poly.f.non, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(f.nonsmokers, aes(x = age, y = lung.cap)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              color = "red", linetype = 2, fullrange = T) +
  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2), color = "blue") +
  expand_limits(x = 25) +
  labs(x = "Age", y = "Lung Capacity") 


#geom_smooth makes the curve.
#this doesnt make it the right model but means data fits well.There is  scope problem.
#this model is only good for the scope. Start predicting outside the scope and fit gets bad.

## Male Non-Smokers

m.nonsmokers <- health %>% filter(sexM_1 == 1 & smoke_1 == 0) 
#here we making the Sex_M to be equal to 1

mlr.poly.m.non <- lm(lung.cap ~ age + I(age^2), data=m.nonsmokers) 
summary(mlr.poly.m.non)
#0.466 mean it is not significant

##polynomial term is not significant.
#little evidence of non linearity amont the male non-smokes
#CHECK:for females model was good but bad for males

extract_equation(mlr.poly.m.non, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(m.nonsmokers, aes(x = age, y = lung.cap)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              color = "red", linetype = 2, fullrange = T) +
  geom_smooth(method = "lm", se = F, formula = y ~ poly(x, 2), color = "blue") +
  expand_limits(x = 25) +
  labs(x = "Age", y = "Lung Capacity") 


##### ##### ##### ##### ##### ##### ##### #####
## Interaction Polynomial Model: Non-Smokers ##
##### ##### ##### ##### ##### ##### ##### #####
## Non-Smokers
nonsmokers <- health %>% filter(smoke_1 == 0) 

mlr.poly.int.non <- lm(lung.cap ~ (age + I(age^2))*sexM_1, data=nonsmokers) 
summary(mlr.poly.int.non)

extract_equation(mlr.poly.int.non, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(nonsmokers, aes(x = age, y = lung.cap, col = factor(sexM_1))) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Lung Capacity", color = "Male") 

#here we looking ata the different models based on the sex

##### ##### ##### ##### ##### ##### #####
## Regressor Polynomial Model: Smokers ##
##### ##### ##### ##### ##### ##### #####
## Female Smokers
f.smokers <- health %>% filter(sexM_1 == 0 & smoke_1 == 1) 

mlr.poly.f <- lm(lung.cap ~ age + I(age^2), data=f.smokers) 
summary(mlr.poly.f) #I IS 0.1382 MEANING IT IS 13% THUS NOT GOOD
#MEANING THERE IS MINIMUM RELATION BETWEEN AGE AND LING CAPACITY
#REMEMBER AS N GETS BIGGER , SIGNIFICANCE IS NORE POSSIBLE. 
#HERE EVEN THE N IS SMALL

extract_equation(mlr.poly.m.non, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(f.smokers, aes(x = age, y = lung.cap)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm",  formula = y ~ poly(x, 2)) +
  labs(x = "Age", y = "Lung Capacity") 

## Male Smokers
m.smokers <- health %>% filter(sexM_1 == 1 & smoke_1 == 1) 

mlr.poly.m <- lm(lung.cap ~ age + I(age^2), data=m.smokers) 
summary(mlr.poly.m)

extract_equation(mlr.poly.m, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(m.smokers, aes(x = age, y = lung.cap)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(x = "Age", y = "Lung Capacity") 

##### ##### ##### ##### ##### ##### ##### #####
### Interaction Polynomial Model: Smokers ###
##### ##### ##### ##### ##### ##### ##### #####
## Smokers
smokers <- health %>% filter(smoke_1 == 1) 

mlr.poly.int <- lm(lung.cap ~ (age + I(age^2))*sexM_1, data=smokers) 
summary(mlr.poly.int)

extract_equation(mlr.poly.int, "lung.cap")

# Create the plot with the polynomial model curve
ggplot(smokers, aes(x = age, y = lung.cap, col = factor(sexM_1))) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Lung Capacity", color = "Male") 

## All data facet_wrap with smoke, color by sexM
# Create the plot with the polynomial model curve
ggplot(health1, aes(x = age, y = lung.cap, col = sexM)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  facet_grid(~ smoke, labeller = labeller(smoke = c("0" = "Non-Smoker", "1" = "Smoker"))) +
  theme(legend.position = "bottom") +
  labs(x = "Age", y = "Lung Capacity", color = "Male")

# Check for a boxcox transformation
car::boxCox(health$lung.cap ~ 1, family="yjPower")

mlr.everything <- lm(log(lung.cap) ~ (age + I(age^2))*sexM_1*smoke_1, data=health)
summary(mlr.everything)
#PROJECT:TRANSFORMATIONS,POLYNOMIALS,INTERACTIONS