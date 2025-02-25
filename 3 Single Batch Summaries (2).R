# Load LearnEDAfunctions to session
library(LearnEDAfunctions)

# View first 10 entries in pop.change dataset
head(pop.change[, c("State", "Pct.change")], 10)

# How is this ordered?
pop.pct.change <- pop.change[, c("State", "Pct.change")]
head(pop.pct.change[order(-pop.pct.change[,2]),]) # Explain what this is saying

# Create basic stemplot of pop.change
library(aplpack)
stem.leaf(pop.change$Pct.change, m = 2, depth = F)
stem.leaf(pop.change$Pct.change, m = 5, depth = F)
# Which is better? Why?
# How would you describe the distribution?

# Find n of pop.change
(n <- nrow(pop.change))
# Finding depth of median
(M <- (n+1) / 2)
# Finding depth of fourth
(F <- (M+1)/2)

# Stemplot with depths
stem.leaf(pop.change$Pct.change, m = 5, depth = T)

# Five-number summary
fivenum(pop.change$Pct.change)

# Letter values of pop.change
(a <- lval(pop.change$Pct.change))
fu <- a[2,3]
fl <- a[2,2]

# Finding STEP
(STEP <- 1.5* (fu - fl))

# Finding fences and FENCES
(fence.lower <- fl - STEP)
(fence.higher <- fu + STEP)

(FENCE.lower <- fl - 2*STEP)
(FENCE.upper <- fu + 2*STEP)

#################
## 2nd Example ##
#################
head(gestation.periods, 15)
head(gestation.periods[order(-gestation.periods[,2]),], 10)

stem.leaf(gestation.periods$Period, m = 1)
stem.leaf(gestation.periods$Period) # notice how it defaults to m = 2

# Please type (or copy above code) to answer the following questions:
# How many entries are in the gestation.periods dataset?
########### WRITE YOUR CODE HERE ##########################

# Using the n you found above, find the depth of M and the median itself.
########### WRITE YOUR CODE HERE ##########################

# Find the depth of the fourths and the fourths themselves.
########### WRITE YOUR CODE HERE ##########################

# Find the depth of the eights and the eighths themselves.
########### WRITE YOUR CODE HERE ##########################

# Find the fence.lower and fence.upper.
########### WRITE YOUR CODE HERE ##########################

# Find the FENCE.lower and FENCE.upper.
########### WRITE YOUR CODE HERE ##########################

# Do we have any animals outside? Any animals really out? What are they?
########### WRITE YOUR CODE HERE ##########################
