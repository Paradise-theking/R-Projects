# Removes all prior variables and reads csv
rm(list = ls())

################
### Boxplots ###
###############
library(LearnEDAfunctions)
library(aplpack)

head(homeruns.2000)
tail(homeruns.2000)
# Backboned stemplot with homerun hitters by year from 1900 to 2000
for (year in unique(homeruns.2000$YEARS)){
  cat("\n")
  print(paste0("Year: ", year))
  stem.leaf(homeruns.2000[homeruns.2000$YEARS == year,]$HOMERUNS)
}

stem.leaf(homeruns.2000$HOMERUNS)

# Five number summary: Lo: 7, Fl: 75, M: 117.5, Fu: 156, Hi: 249
fivenum(homeruns.2000$HOMERUNS)
max(homeruns.2000$HOMERUNS) / min(homeruns.2000$HOMERUNS)

# boxplot of homeruns.2000 by YEARS
boxplot(HOMERUNS ~ YEARS, data=homeruns.2000,
        horizontal = FALSE,
        xlab="Years", ylab="Homeruns")


# Find fences for each age group and count of OUTs
lval_plus(homeruns.2000, HOMERUNS, YEARS) %>%
  group_by(YEARS) %>%
  summarise(YEARS = median(YEARS),
            Fence_LO = median(Fence_LO),
            Fence_HI = median (Fence_HI),
            Count_Outs = sum(OUT == TRUE),
            .groups = 'drop') 

# See the individual OUT values arranged by time and group
lval_plus(homeruns.2000, HOMERUNS, YEARS) %>%
  group_by(YEARS) %>%
  filter(OUT == TRUE) %>%
  arrange(YEARS, .by_group = T)

# Print letter values for each group to compare batches
for (year in unique(homeruns.2000$YEARS)){
  print(paste0("Year: ", year))
  print(lval(homeruns.2000[homeruns.2000$YEARS == year,]$HOMERUNS))
}

############################
### Spread vs Level Plot ###
############################
# Use spread vs level plot to help see potential batch comparison problems and transformation power
(raw.slp <- spread_level_plot(homeruns.2000, HOMERUNS, YEARS))
#Distributions of spreads
fivenum(raw.slp$df)
# smallest spread's multiple difference to largest spread
max(raw.slp$df) / min(raw.slp$df)

# Using p = 1 - b, and Tukey's ladder, we find the most appropriate transformation is a log or sqrt transformation
log.homeruns <- data.frame(YEARS = homeruns.2000$YEARS, HOMERUNS = log(homeruns.2000$HOMERUNS))
sqrt.homeruns <- data.frame(YEARS = homeruns.2000$YEARS, HOMERUNS = sqrt(homeruns.2000$HOMERUNS))

# Compare log transformed batches with boxplot
boxplot(HOMERUNS ~ YEARS, data = log.homeruns,
        horizontal = F,
        xlab = "Years", ylab = "Log Homeruns")

# Compare sqrt transformed batches with boxplot
boxplot(HOMERUNS ~ YEARS, data = sqrt.homeruns,
        horizontal = F,
        xlab = "Years", ylab = "Square Root Homeruns")

# Use spread vs level plot to help see potential batch comparison problems
(log.slp <- spread_level_plot(log.homeruns, HOMERUNS, YEARS))
fivenum(log.slp$df)
max(log.slp$df) / min(log.slp$df)

# Use spread vs level plot to help see potential batch comparison problems
(sqrt.slp <- spread_level_plot(sqrt.homeruns, HOMERUNS, YEARS))
fivenum(sqrt.slp$df)
max(sqrt.slp$df) / min(sqrt.slp$df)

# View all batch spreads in data.frame
data.frame('Year' = unique(homeruns.2000$YEARS), 'dF Raw' = raw.slp$df, 'dF log' = log.slp$df, 'dF sqrt' = sqrt.slp$df)

####### Check Transformation with boxcox
library(MASS)
(a <- boxcox(HOMERUNS ~ YEARS, data = homeruns.2000))
(p <- a$x[which.max(a$y)])

# Using p = 1 - b, and Tukey's ladder, we find the most appropriate transformation is a log or sqrt transformation
boxcox.homeruns <- data.frame(YEARS = homeruns.2000$YEARS, HOMERUNS = (homeruns.2000$HOMERUNS) ^ p)

# Compare sqrt transformed batches with boxplot
boxplot(HOMERUNS ~ YEARS, data = boxcox.homeruns,
        horizontal = F,
        xlab = "Years", ylab = "Homeruns to the .71 Power")

# Use spread vs level plot to help see potential batch comparison problems
detach('package:MASS', unload = TRUE)
spread_level_plot(boxcox.homeruns, HOMERUNS, YEARS)

##################################
### Reexpressing for Symmetry ###
################################
head(mortality.rates)

## RAW
# Stemplot
stem.leaf(mortality.rates$Rate)

# Letter Values
(letter.values <- lval(mortality.rates$Rate))
letter.values$mids
mean(mortality.rates$Rate)

# Plot mids
plot(letter.values$mids, main = 'Mortality Rates Raw')

# Hinkley’s quick method
hinkley(mortality.rates$Rate)

for (p in seq(1, -.5, by = -.5)){
  # Transform
  ifelse(p == 0, assign('reexpress', log(mortality.rates$Rate)), assign('reexpress', mortality.rates$Rate ^ p))
  
  # Stemplot
  cat("\n")
  print(paste0("p = ", p))
  stem.leaf(reexpress)
  
  # Letter Values
  cat("\n")
  print((letter.values <- lval(reexpress)))
  cat("\n")
  print(hinkley(reexpress))  
  
  # Plot mids
  plot(letter.values$mids, main = paste0("Power Mortality Rates\nwhere p = ", p))
  
}

