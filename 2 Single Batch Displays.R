# Install Packages tidyverse for data cleaning and aplpack for stemplot
install.packages(c('tidyverse', 'aplpack'))

################################
## 2.1 Single Batch Displays ##
###############################
# Add libraries to session
library(LearnEDAfunctions)
library(tidyverse)

# View head of dataset we're working with
head(act.scores.06.07)

# Histogram of ACT scores
hist(act.scores.06.07$ACT)
hist(act.scores.06.07$ACT, breaks=17:24) # Force the histogram range from 17 to 24

# Subset dataset by column Per.Grads >= 50
act.sub <- subset(act.scores.06.07, Per.Grads >= 50)

# Reset row index
rownames(act.sub) = seq(length = nrow(act.sub))

# Create stemplot of average ACT scores of subsetted data
# Note: Use '[library]::[function]' to call a library's function without adding it to whole session
aplpack::stem.leaf(act.sub$ACT, m = 1, depth = F) # 10 leaves per stem
aplpack::stem.leaf(act.sub$ACT, m = 2, depth = F) # 5 leaves per stem
aplpack::stem.leaf(act.sub$ACT, m = 5, depth = F) # 2 leaves per stem

# Fish weight dataset
tail(heaviest.fish)

# ordered by heaviest fishes
head(heaviest.fish[order(-heaviest.fish[,2]),])

# Convert pounds to kilograms 
fish.kg <- round(heaviest.fish$Weight / 2.2, 1)
fish.kg[order(-fish.kg)]

# Variations of stemplots 
aplpack::stem.leaf(fish.kg, m = 1, depth = F)
aplpack::stem.leaf(fish.kg, m = 2, depth = F)
aplpack::stem.leaf(fish.kg, unit = 10, m = 5, depth = F)

# Rule of thumb formula
max.act <- max(act.sub$ACT) 
# max.act <- max(act.sub[-which.max(act.sub)]) # if outlier, try 2nd largest
min.act <- min(act.sub$ACT)
(R <- max.act - min.act) # parenthesis will show the assignment value

(L <- floor(10 * log10(nrow(act.sub)))) #nrow for n of dataset

R / L

#######################################
## Your Turn try on baseball dataset ##
#####################################
# Using the below dataset from learnEDAfunctions, answer the following questions
library(aplpack)
head(baseball.attendance)
# PART A:  Working with a Single Batch – Displays
# (1)  Construct a stem plot where
# -        the breakpoint is between the 10,000 and 1000 places

baseball.sub <- subset(baseball.attendance, baseball.attendance$Avg.Home > 1000, baseball.attendance$Avg.Home < 10000)

stem.leaf(baseball.sub$Avg.Home, m = 1, depth = F)
# -        you have 10 leaves per stem
stem.leaf(baseball.attendance$Avg.Away)

# (2) Discuss what you see in this distribution of attendance numbers, including shape, “average”, spread, and any unusual characteristics.  If there are any unusual high or low attendances, identify the cities that have these unusual mean attendances.

# (3)  Redraw the stem plot using the same breakpoint and 5 leaves per stem.

# (4)  Redraw the stem plot using the same breakpoint and 2 leaves per stem.

# (5)  Discuss any features that you’ve learned about this dataset by constructing these two additional stem plots.  What is the best choice of stem plot?  Why?

#################################
## 2.2 Single Batch Summaries ##
################################
library(LearnEDAfunctions)
pop.change[, c("State", "Pct.change")]
head(pop.change)
stem.leaf(pop.change$Pct.change, depth = F)

head(baseball.attendance)
bass <-baseball.attendance[,c("Team","Avg.Home")]
stem.leaf(bass$Avg.Home, m = 1) 
stem.leaf(bass$Avg.Home, m = 2)
stem.leaf(bass$Avg.Home, m = 5)
                   