rm(list = ls())

library(ggplot2) #Plots
library(fastDummies)
library(yardstick)
library(dplyr)
library(GGally)
library(car) #vif
library(caret)
library(bestglm)
library(pROC) #roc
library(gains) #gains
library(DT) #datatable
library(ggpmisc)
library(tidyverse)

# Set ggplot2 theme
theme_set(theme_minimal())

path.full <- here::here()

path <- paste0(dirname(path.full), "/PROJECT/diabetes_prediction.csv")
diabetes <- readr::read_csv(path)
head(diabetes)
nrow(diabetes)
str(diabetes)

#CLEANING OUR DATA
#dropping the no info entries
unique(diabetes$smoking_history)
unique(diabetes$gender)

idx <- na.omit(which(diabetes$smoking_history == 'No Info'))
gen <- na.omit(which(diabetes$gender == 'Other'))
length(gen)
length(idx)
diabetes<-diabetes %>% slice(-c(idx))
diabetes<-diabetes %>% slice(-c(gen))
unique(diabetes$smoking_history)
unique(diabetes$gender)
nrow(diabetes)

#renaming misspelled entries
diabetes$smoking_history[diabetes$smoking_history == 'ever'] <- "never"
diabetes$smoking_history[diabetes$smoking_history == 'not current'] <- "former"
unique(diabetes$smoking_history)


str(diabetes)
#VISIALIZING THE DATA

diabetes$diabetes <- factor(diabetes$diabetes)

diabetes.sample<-sample_n(diabetes,1000)

GGally::ggpairs(diabetes.sample,mapping = aes(color = diabetes), #try hashing out the mapping
                diag=list(continuous="densityDiag",
                          discrete="barDiag"),
                upper=list(continuous= GGally::wrap("cor", size = 3.5),
                           combo="facethist",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="box",
                           discrete="facetbar"))




diabetes %>% ggplot(aes(blood_glucose_level))+
  geom_histogram(binwidth = 15,fill = "#97B3C6") +
  theme_bw()+
  #xlim()+
  labs(x = "glucose sugar levels",y = NULL,
       title = "Glucose level hist")


diabetes %>% ggplot(aes(HbA1c_level))+
  geom_histogram(binwidth = 1,fill = "#97B3C6") +
  theme_bw()+
  #xlim()+
  labs(x = "HbA1c_level",y = NULL,
       title = "Glucose level hist")



diabetes %>% ggplot(aes(bmi))+
  geom_histogram(binwidth = 1,fill = "#97B3C6") +
  theme_bw()+
  #xlim()+
  labs(x = "bmi",y = NULL,
       title = "Glucose level hist")

c <-MASS::boxcox(diabetes$blood_glucose_level~1)# close to 0; log transformation
c <-MASS::boxcox(diabetes$HbA1c_level~1) #appx 1 so no transformation
c <-MASS::boxcox(diabetes$bmi~1) #closer to 0;log transformation

(reexpressed.diabetes <- data.frame(log.blood_glucose_level = log(diabetes[8])))
  
reexpressed.diabetes %>% ggplot(aes(blood_glucose_level))+
  geom_histogram(binwidth = 0.2,fill = "#97B3C6") +
  theme_bw()+
  labs(x = "blood glucose levels",y = NULL,
       title = "Reexepressed Glucose level hist")



reexpressed.diabetes1 <- data.frame(log.bmi = log(diabetes[6]))
reexpressed.diabetes1 %>% ggplot(aes(bmi))+
  geom_histogram(binwidth = .1,fill = "#97B3C6") +
  theme_bw()+
  xlim(2,5)+
  labs(x = "bmi",y = NULL,
       title = "Reexepressed bmi level hist")


diabetes$log.bmi <- log(diabetes$bmi)

diabetes$log.bgl <- log(diabetes$blood_glucose_level)
diabetes.fulls <- diabetes %>% select(-c(bmi, blood_glucose_level))
# diabetes <- diabetes %>% select(-c(blood_glucose_level))
# str(diabetes)

# Remove sex = other. Rationale Sex = other is only 12
diabetes.full <- fastDummies::dummy_cols(diabetes.fulls,
                                         select_columns = c('smoking_history', 'gender', 
                                                                 'hypertension', 'heart_disease'),
                                     remove_first_dummy = TRUE,
                                     remove_selected_columns = TRUE)


logistic.full <- glm(diabetes ~ ., family = binomial(link = 'logit'), data = diabetes.full)
summary(logistic.full)
# Odds Ratio
exp(confint.default(logistic.full))

c <-confusionMatrix(factor(
  ifelse(fitted(logistic.full) > 0.5, "1", "0")),
  diabetes$diabetes, positive = "1")

(c$byClass['F1'])



extract_equation <- function(model, response) {
  regressors <- coef(model)[-1]
  assign("vars", names(regressors), envir = .GlobalEnv)
  assign("intercept", coef(model)[1], envir = .GlobalEnv)
  assign("vars.coef", signif(regressors, 3), envir = .GlobalEnv)
  
  equation <- paste0(response, " = ", signif(intercept, 3), " + ", paste0(vars.coef, "*", vars, collapse = " + "))
  equation <- gsub("\\+ -", "- ", equation)
  
  return(equation)
}

(eq<-extract_equation(logistic.full,"diabetes") )


###########MODEL #2#################
logistic.int.full <- glm(diabetes ~ age:smoking_history_former
                         + age:smoking_history_never +
                           age*gender_Male, family = binomial(link = 'logit'), data = diabetes.full)

summary(logistic.int.full)
(eq1<-extract_equation(logistic.int.full,"diabetes"))


 

# # ###########MODEL #3#################
 logistic.all <- glm(diabetes ~ .^2, family = binomial(link = 'logit'), data = diabetes.full)
 summary(logistic.all)
 eq0<-extract_equation(logistic.all,"diabetes")
 
 
# LOOK AT DIFFERENT LIBRARIES FOR FORWAD/BACKWARD
 
# logistic.bidirectional <- MASS::stepAIC(logistic.all,
#                            direction = "both",
#                            k = log(nrow(diabetes.full)))
# 
# (eq0.5<-extract_equation(logistic.bidirectinal,"diabetes"))
# 



#OUTLIER ANALYSIS
#1. rstudent
best <- "logistic.full"
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
                             abs(df3$rstudent) > 3, 3, 1.5),
             col = ifelse(df3$type %in% c("rstud") &
                            abs(df3$rstudent) > 3, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("rstud") &
  #                                   abs(rstudent) > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(3, -3), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Outlier Points (RStudent)",
       x = "Row number", y = "Rstudent")

#2. rstandard

rstand <- rstandard(get(best))

df <- data.frame(idx = c(1:length(rstand)),
                 rstud = rstand)

fence <- 3

(out_rstand <- which(abs(rstand) > fence))



df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "rstandard")

# Create the plot for hat values
ggplot(df3, aes(x = idx, y = rstandard)) +
  geom_point(size = ifelse(df3$type %in% c("rstud") &
                             abs(df3$rstandard) > 3, 3, 1.5),
             col = ifelse(df3$type %in% c("rstud") &
                            abs(df3$rstandard) > 3, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("rstud") &
  #                                   abs(rstudent) > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(3, -3), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Outlier Points (RStandard)",
       x = "Row number", y = "Rstandard")



#3. hatvalues
hatvals <- hatvalues(get(best))

df <- data.frame(idx = c(1:length(hatvals)),
                 hatvals = hatvals)

n <- nrow(df)
k <- length(get(best)$coefficients) - 1

fence <- (2*(k+1)/n)

(lev_hv <- which(hatvals > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -idx,
                      names_to = "type",
                      values_to = "hatvals")

# Create the plot for hat values
ggplot(df3, aes(x = idx, y = hatvals)) +
  geom_point(size = ifelse(df3$type %in% c("hatvals") &
                             df3$hatvals > fence,3, 1.5),
             col = ifelse(df3$type %in% c("hatvals") &
                            df3$hatvals > fence, "red", "black")) +
  # geom_text(col = "red", aes(label = ifelse(type %in% c("hatvals") &
  #                                             hatvals > fence, idx, NA), vjust = 1.5)) +
  geom_hline(yintercept = c(fence), linetype = 2, color = "gray50") +
  labs(title = "Leverage Points (Hat Values)",
       x = "Row number", y = "Hat Values")



#4. ddfits

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


#5.cook
cookd <- cooks.distance(get(best)) 

cutoff <- 0.5
cutoff <- 4/(nrow(df)-length(get(best)$coefficients)-2) #alternative cutoff for huge n 

(infl_cooks <- which(cookd > cutoff))

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


#6. vcratio

  cvrt <- covratio(get(best)) 

n <- nrow(df)
k <- length(get(best)$coefficients) - 1

fence_high <- (1 + 3*(k+1)/n)
fence_low <- (1 - 3*(k+1)/n)


(infl_cvr <- c(which(cvrt > fence_high),
               which(cvrt < fence_low)))

    df <- data.frame(idx = c(1:length(cvrt)),
                   cvrt = cvrt)

df3 <- df %>% tidyr::pivot_longer(cols = -idx,
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

df <- data.frame(
  type = c("out_rstand", "out_rstud", "lev_hv",
           "infl_cooks", "infl_cvr", "infl_d"),
  length = c(length(out_rstand), length(out_rstud), length(lev_hv),
             length(infl_cooks), length(infl_cvr), length(infl_d)))

ggplot(df, aes(x = reorder(type, -length), y = length)) +
  geom_bar(stat = "identity")


#REMOVING OUTLIERS
df <- broom::augment(get(best))

col.data <- df %>%
  dplyr::mutate(color = case_when(
    rownames(df) %in% infl_cooks ~ "red",
    TRUE ~ "black")) %>%
  dplyr::mutate(idx = rownames(df))


diabetes.xouts <-  diabetes.full %>% slice(-c(infl_cooks)) #here we slice out infl_d

cooks.pts <- diabetes.full %>% slice(c(infl_cooks))
#find confusion matrix,predict using outliers

logistic.full.xout <- glm(diabetes ~ ., family = binomial(link = 'logit'), data =diabetes.xouts)
summary(logistic.full.xout)
(eq3<-extract_equation(logistic.full.xout,"diabetes"))

##########BEST GLM########

diabetes.sample<-sample_n(diabetes.xouts,2000)
#LOGSISTIC,MAKE SUBSET,BEST GLM WITH SUBSET,EXTRACT FORMULA,BULD MODEL USING STANDARD FORMULA

Xy <- data.frame(diabetes.sample %>%
                   dplyr::relocate(diabetes, .after = last_col()))

logistic.bestglm <- bestglm(Xy = Xy,
                            family = binomial,
                            IC = "BIC",
                            method = "exhaustive")

(s <- summary(logistic.bestglm$BestModel))

#Extract formula
response <- "diabetes"
bestglm.formula <- paste0(response, " ~ ",
                          paste0(rownames(s$coefficients)[-1], collapse = " + "))

# Build BestGLM Model
logistic.bestglm <- glm(formula(bestglm.formula), family = binomial(link = 'logit'), data = diabetes.xouts)
summary(logistic.bestglm)



models <- data.frame(model = c("logistic.full","logistic.int.full","logistic.all","logistic.bestglm"))
df_list <- list()
cutOff <- 0.5
# Loop over each confusion matrix and create a data frame with metrics
for (model in models$model) {
  
  ifelse(grep("bestglm", model),
         response <-diabetes.xouts$diabetes,
         response <- diabetes.full$diabetes)
  
  pos.pred.perc <- fitted(get(model))
  
  # Create dataframe for evaluation
  eval_df <- data.frame(
    obs = response,
    pred = factor(ifelse(pos.pred.perc > cutOff, "1", "0"),levels =c("0","1")),
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

 


RNGkind (sample.kind = "Rounding") 
set.seed(0)

x0 <- data.frame(t(sapply(cooks.pts, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(diabetes))

(x0 <- type.convert(x0, as.is = TRUE))
(exp(x0[10]))

predict(logistic.bestglm, newdata = x0, type = "response") #prediction with outliers

x0 <- data.frame(t(sapply(diabetes.full, function(col) sample(col, 1, replace = T)))) %>%
  select(-c(diabetes))

(x0 <- type.convert(x0, as.is = TRUE))


predict(logistic.bestglm, newdata = x0, type = "response") #predict with random entry




#easier to reject null if its one tail test
#stronger hypothesis you must do a two tail test
#pvalue less than alpha means reject null 7\

