# Removes all prior variables and reads csv
rm(list = ls())

################
### Boxplots ###
###############
library(LearnEDAfunctions)
library(aplpack)

tail(boston.marathon)
# Backboned stemplot with runners of ages 20, 30, 40, 50, and 60
for (i in c(2:5)){
  stem.leaf.backback(boston.marathon[boston.marathon$age == 10*i,]$time, boston.marathon[boston.marathon$age == 10*(i+1),]$time) 
}

# Subset boston.marathon by age = 20
(runners.20 <- subset(boston.marathon, age == 20))

# Boxplot single batch to understand boxplot
boxplot(runners.20$time, horizontal = TRUE, xlab = "Time (in minutes)")

# Fivre number summary: Lo: 150, Fl: 222, M: 231, Fu: 240, Hi: 274
fivenum(runners.20$time)

# Find fences for each age group and count of OUTs
(s <- lval(boston.marathon))
(s$mids)
########
lval_plus(boston.marathon, time, age) %>%
  group_by(age) %>%
  summarise(age = median(age),
            Fence_LO = median(Fence_LO),
            Fence_HI = median (Fence_HI),
            Count_Outs = sum(OUT == TRUE),
            .groups = 'drop')

# See the individual OUT values arranged by time and group
lval_plus(boston.marathon, time, age)[-c(2:3)] %>%
  group_by(age) %>%
  filter(OUT == TRUE) %>%
  arrange(time, .by_group = T)

# Print letter values for each group to compare batches
for (age in unique(boston.marathon$age)){
  print(paste0("Age: ", age))
  print(lval(boston.marathon[boston.marathon$age == age,]$time))
}

# Compare batches with boxplot
boxplot(time ~ age, horizontal = TRUE,
        data = boston.marathon, xlab = "Time (in minutes)",
        ylab = "Age")

############################
### Spread vs Level Plot ###
############################
head(pop.densities[, -1],)

# Turn wide format pop.density to long format using stack
(stacked.data <- stack(pop.densities))
head(stacked.data)

# Compare batches with boxplot
boxplot(values ~ ind, data = stacked.data,
        horizontal = TRUE,
      ylim = c(0, 1200),
        xlab = "Density", ylab = "Year")

# Use spread vs level plot to help see potential batch comparison problems and transformation power
spread_level_plot(stacked.data, values, ind)

# Using p = 1 - b, and Tukey's ladder, we find the most appropriate transformation is a log transformation
(p <- 1-1.11)

reexpressed.data <- data.frame(log(pop.densities[, 3:8]))
#reexpressed.data <- data.frame(sqrt(pop.densities[, 3:8]))
#reexpressed.data <- data.frame((pop.densities[, 3:8]))^0.5

# Compare tranformed batches with boxplot
boxplot(reexpressed.data,
        horizontal = TRUE,
        xlab = "Transformed Densities", ylab = "Year")

# Turn wide format reexpressed.data to long format using stack
(stacked.tdata <- stack(reexpressed.data))

# Use spread vs level plot to help see potential batch comparison problems
spread_level_plot(stacked.tdata, values, ind)

for (year in unique(stacked.tdata$ind)){
  print(paste0(year))
  print(lval(stacked.tdata[stacked.tdata$ind == year,]$value))
}

############################
### In Class Exercise ###
############################

head(boston.marathon)
# 1. With boston.marathon, use a spread vs level plot to determine what p for a transformation would help to compare batches.
spread_level_plot(boston.marathon,time,age)
(p <- 1-0.6)

#######before transformation########
boxplot(time ~ age, horizontal = TRUE,
        data = boston.marathon, xlab = "Time (in minutes)",
        ylab = "Age")
# 2. Implement the transformation and create boxplots on the transformed data. Explain what you notice.
reexpressed.data <- data.frame(log(boston.marathon[, 2]))
boxplot(reexpressed.data[,1] ~ boston.marathon$age,
        horizontal = TRUE,
        xlab = "Time", ylab = "Age")

# 3. Give the interpretation of marathon times by age using the transformed data. 
time <- reexpressed.data[,1]
age <- boston.marathon[,1]
(reexpressed.dataframe <- data.frame(age,time))
spread_level_plot(reexpressed.dataframe,time,age)

for (age in unique(reexpressed.dataframe$age)){
  print(paste0(age))
  print(lval(reexpressed.dataframe[reexpressed.dataframe$time == time,]$time))
}

(c <- lval(reexpressed.dataframe))
(c$mids)

