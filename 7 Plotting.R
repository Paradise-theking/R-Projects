library(LearnEDAfunctions)
head(boston.marathon.wtimes)

##########################
# Boston Marathon Times #
##########################
# See y ~ x relationship
plot(boston.marathon.wtimes$year, boston.marathon.wtimes$minutes, xlab="YEAR", ylab="TIME")

# Fancier plot with car library
library(car)
scatterplot(minutes ~ year,
            data = boston.marathon.wtimes,
            xlab = "YEAR", ylab = "TIME",
            smooth = FALSE, boxplots = FALSE)


# Finding the FIT: or Extracting the slope and intercept from the above relationship
slr <- lm(minutes ~ year,
          data = boston.marathon.wtimes)
summary(slr)

# Drawing slr without car library
plot(boston.marathon.wtimes$year, boston.marathon.wtimes$minutes,
     xlab="YEAR", ylab="TIME")
abline(slr, col = 'red')

# Residual (error) analysis
actual <- boston.marathon.wtimes$minutes
fitted <- predict(slr, boston.marathon.wtimes)
residuals <- actual - fitted

plot(boston.marathon.wtimes$year, residuals,
     xlab="YEAR")
abline(h = 0, lwd = 1.5, col = "red", lty = 2)

# Fitting a smooth line
with(boston.marathon.wtimes,
     plot(year, minutes,
     xlab="YEAR", ylab="TIME"))
with(boston.marathon.wtimes,
     lines(lowess(year, minutes, f = 0.2)))

######################
### US Population ###
######################
head(us.pop)

# See y ~ x relationship
with(us.pop, plot(YEAR, POP, ylab="Population"))

# fancier plot with car library
scatterplot(POP ~ YEAR,
            data = us.pop,
            xlab = "YEAR", ylab = "POP",
            smooth = FALSE, boxplots = FALSE)

# Finding the FIT: or Extracting the slope and intercept from the above relationship
slr <- lm(POP ~ YEAR, data = us.pop)
summary(slr)

# Drawing slr without car library
with(us.pop, plot(YEAR, POP, ylab="Population"))
abline(slr, col = 'red')

# Residual analysis
actual <- us.pop$POP
fitted <- predict(slr, us.pop)
residuals <- actual - fitted

plot(us.pop$YEAR, residuals,
     xlab="YEAR")
abline(h = 0, lwd = 1.5, col = "red", lty = 2)

for (p in seq(1, -.5, by = -.5)){
  # Transform
  ifelse(p == 0, assign('reexpress', log(us.pop$POP)), assign('reexpress', us.pop$POP ^ p))
  
  # Stemplot
  cat("\n")
  print(paste0("p = ", p))
  aplpack::stem.leaf(reexpress)
  
  # Letter Values
  cat("\n")
  print((letter.values <- lval(reexpress)))
  cat("\n")
  print(hinkley(reexpress))  
  
  # Plot mids
  symplot <- symplot(reexpress)
  print(symplot)
}

# Reexpress with roots
us.pop$pop.root <- sqrt(us.pop$POP)
with(us.pop, 
     plot(YEAR, pop.root, ylab="Root Population"))

scatterplot(pop.root ~ YEAR,
            data = us.pop,
            xlab = "YEAR", ylab = "Root POP",
            smooth = FALSE, boxplots = FALSE)

# Extracting the slope and intercept from the above relationship
slr <- lm(pop.root ~ YEAR, data = us.pop)
summary(slr)

# Residual Analysis
actual <- us.pop$pop.root
fitted <- predict(slr, us.pop)
residuals <- actual - fitted

plot(us.pop$YEAR, residuals,
     xlab="YEAR")
abline(h = 0, lwd = 1.5, col = "red", lty = 2)

#################
### CLASSWORK ###
#################
# Let's look at the scatter plot piece-wise:
# Population in US before AND after 1930
with(us.pop, plot(YEAR, POP, ylab="Population"))
abline(v = 1930, lwd = 1.5, col = "red", lty = 2)


# Notice the >=1930 side looks linear and the < 1930 side looks exponential
# Let's subset both sections:
us.pop.1930 <- subset(us.pop, YEAR >= 1930)
us.pop.1790 <- subset(us.pop, YEAR < 1930)

with(us.pop, plot(YEAR, POP, ylab="Population", xlim=c(1930, 2000)))
scatterplot(POP ~ YEAR,
            data = us.pop.1930,
            xlab = "YEAR", ylab = "POP",
            smooth = FALSE, boxplots = FALSE)

with(us.pop, plot(YEAR, POP, ylab="Population", xlim=c(1790, 1920)))
scatterplot(POP ~ YEAR,
            data = us.pop.1790,
            xlab = "YEAR", ylab = "POP",
            smooth = FALSE, boxplots = FALSE)

# 1. Write the FITTED equation for us.pop.1930

slr <- lm(pop.root ~ YEAR, data = us.pop.1930)
summary(slr)

fitted <- predict(slr, us.pop.1930)


# 2. Make a residual graph from the FITTED us.pop.1930 slr.
actual <- us.pop.1930$pop.root
residuals <- actual - fitted

plot(us.pop.1930$YEAR, residuals,
     xlab="YEAR")
abline(h = 0, lwd = 1.5, col = "red", lty = 2)
# 3. Interpret the residual graph.

# 4. For us.pop.1790, show that the distribution would be better if reexpressed (use graphs/plots).
scatterplot(POP ~ YEAR,
           data = us.pop.1790,
           xlab = "YEAR", ylab = "POP",
           smooth = FALSE, boxplots = FALSE)
# 5. Name two transformations you could use to reexpress us.pop.1790. How did you get them?
#####logs or roots.
# 6. a. Transform us.pop.1790 with your two suggested reexpressions

  # Transform
  reexpress <- log(us.pop.1790$POP)
  reexpres <- sqrt(us.pop.1790$POP)
  
   # Plot mids
  symplot <- symplot(reexpress)
  print(symplot)
  
  symplot <- symplot(reexpres)
  print(symplot)


# b. Write the FITTED equation for the two transformed us.pop.1790
  slr <- lm(pop.root ~ YEAR, data = us.pop.1790)
  summary(slr)
  
  fited <- predict(slr, us.pop.1790)
# 7. Make residual graphs from the two FITTED transformed us.pop.1790 slrs.
  actual <- us.pop.1790$pop.root
  residuals <- actual - fited
  
  plot(us.pop.1790$YEAR, residuals,
       xlab="YEAR")
  abline(h = 0, lwd = 1.5, col = "red", lty = 2)

  
  
  # 8. Interpret the residual graphs.


