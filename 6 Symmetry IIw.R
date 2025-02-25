# Removes all prior variables and reads csv
rm(list = ls())

packages <- c('LearnEDAfunctions', 'aplpack')
lapply(packages, require, character.only = TRUE)

head(farms)

# Which is better? Why?
stem.leaf(farms$count)
stem.leaf(farms$count, unit=10, m=5, trim.outliers=TRUE)
stem.leaf(farms$count, unit=10, m=5, trim.outliers=FALSE)

lval(farms$count)

plot(lval(farms$count)$mids, cex = 1.5, pch = 21, bg = "red", main = "Farm Count")

symplot(farms$count)

for (p in seq(1, -.5, by = -.5)){
  # Transform
  ifelse(p == 0, assign('reexpress', log(farms$count)), assign('reexpress', farms$count ^ p))
  
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
  symplot <- symplot(reexpress)
  print(symplot)
}

# According to hinkley's number our best two transformations are RAW and ROOT
# Look at above loop output for stem.leaf, letter values, of ROOT transformation
plot(lval(farms$count ^ 0.5)$mids, cex = 1.5, pch = 21, bg = "red", main = "Farm Count Root")


####### Check Transformation with boxcox
library(MASS)
(a <- boxcox(count ~ 1, data = LearnEDAfunctions::farms)) #double colon means from the library
(p <- a$x[which.max(a$y)])

# Use spread vs level plot to help see potential batch comparison problems
detach('package:MASS', unload = TRUE)

# Check transformation, did we make it more symmetric?
stem.leaf(farms$count ^ p)
lval(farms$count ^ p)
plot(lval(farms$count ^ p)$mids, cex = 1.5, pch = 21, bg = "red", main = "Farm Count BoxCox Power")

symplot(farms$count ^ p)
hinkley(farms$count ^ p)

# Our three best symmetry measured data
raw <- farms$count
root <- farms$count ^ 0.5
boxcox <- farms$count ^ p


# Notice the difficulty of comparing transformations
boxplot(data.frame(raw, root, boxcox), main = "Unmatched")

# Use mtrans() to normalize/match according to median
matched.root <- mtrans(raw, 0.5)
matched.boxcox <- mtrans(raw, p)

# Notice the medians now match so we can discuss the distributions of the transformations
boxplot(data.frame(raw, matched.root, matched.boxcox), main = "Matched")

