# Load LearnEDAfunctions to session
install.packages('rmarkdown', dependencies = TRUE)
library(LearnEDAfunctions)

# View first 10 entries in pop.change dataset
head(pop.change[1, c("State", "Pct.change")], 10)

# How is this ordered?
pop.pct.change <- pop.change[, c("State", "Pct.change")] #assigning variable,
head(pop.pct.change[order(-pop.pct.change[,2]),]) # Explain what this is saying

# Create basic stemplot of pop.change
library(aplpack)

stem.leaf(pop.change$Pct.change, m = 2, depth = FALSE) # m represents how many splits
stem.leaf(pop.change$Pct.change, m = 5, depth = F)
# Which is better? Why?
# How would you describe the distribution?

# Find n of pop.change
(n <- nrow(pop.change)) #outside paratheses mean it should also print output
# Finding depth of median
(M <- (n+1) / 2)
# Finding depth of fourth
(F <- (floor(M)+1)/2)

# Stemplot with depths
stem.leaf(pop.change$Pct.change, m = 5, depth = T)

# Five-number summary
fivenum(pop.change$Pct.change)

# Letter values of pop.change
(a <- lval(pop.change$Pct.change))
(fu <- a[2,3]) #add paranthseses to print output
(fl <- a[2,2])

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

stem.leaf(gestation.periods$Period, m = 2)
stem.leaf(gestation.periods$Period) # notice how it defaults to m = 2

# Please type (or copy above code) to answer the following questions:
# How many entries are in the gestation.periods dataset?

########### WRITE YOUR CODE HERE ##########################
(n <- nrow(gestation.periods))


# Using the n you found above, find the depth of M and the median itself.
########### WRITE YOUR CODE HERE ##########################
(M <- (n+1) / 2)

# Find the depth of the fourths and the fourths themselves.
########### WRITE YOUR CODE HERE ##########################
(F <- (floor(M)+1)/2)

# Find the depth of the eights and the eighths themselves.
########### WRITE YOUR CODE HERE ##########################
(E <- (floor(F)+1)/2)


# Find the fence.lower and fence.upper.
########### WRITE YOUR CODE HERE ##########################
(a <- lval(gestation.periods$Period))
(fu <- a[2,3]) #add paranthseses to print output
(fl <- a[2,2])

(STEP <- 1.5* (fu - fl))

# Finding fences and FENCES
(fence.lower <- fl - STEP)
(fence.higher <- fu + STEP)

# Find the FENCE.lower and FENCE.upper.
########### WRITE YOUR CODE HERE ##########################
(FENCE.lower <- fl - 2*STEP)
(FENCE.upper <- fu + 2*STEP)


# Do we have any animals outside? Any animals really out? What are they?
########### WRITE YOUR CODE HERE ##########################

(fence.higher <- fu + STEP)
(FENCE.upper <- fu + 2*STEP)



