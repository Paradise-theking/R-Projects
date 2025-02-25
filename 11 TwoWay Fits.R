rm(list = ls())

library(LearnEDAfunctions)
head(temperatures)

# Make City names into the index 
(temps <- data.frame(temperatures[,-1], row.names = temperatures[,1]))

# Median polish
(additive.fit <- medpolish(temps))

#####################
### Additive Plot ###
####################
# Additive fit plot
Row.Part <- with(additive.fit, row + overall)
Col.Part <- additive.fit$col
plot2way(Row.Part, Col.Part,
         dimnames(temps)[[1]], dimnames(temps)[[2]])

# Residuals
additive.fit$residual
hist(c(additive.fit$residual))
aplpack::stem.leaf(c(additive.fit$residual),
                   unit=1, m=5)
# aplpack::stem.leaf(c(additive.fit$residual))
hinkley(c(additive.fit$residual))

# Two-Way Plot (more flexible)
library(twoway)

# Similar to medpolish()
(sent.2way <- twoway(temps, 'median'))

# Comparing row and column fits
with(sent.2way, roweff + overall)
with(sent.2way, coleff + overall)

# Similar to plot2way()
plot(sent.2way)
grid(col = '#bbbbbb', lwd = 1.4)

# Generate all combinations of i = City and j = Month
combinations <- expand.grid(i = row.names(temps),
                            j = colnames(temps))

fitted.add <- matrix(NA, nrow(temps), ncol(temps),
                     dimnames = dimnames(temps))
(fits.add <- data.frame())
# Loop through the rows of the data frame
for (row in 1:nrow(combinations)) {
  # Grab City
  i <- combinations$i[row]
  # Grab Month
  j <- combinations$j[row]
  # Fit formula
  FIT <- sent.2way$overall +
    sent.2way[["roweff"]][[i]] +
    sent.2way[["coleff"]][[j]]
  # Initializing what's in final data.frame
  Actual <- temps[i,j]
  Residual <- Actual - FIT
  # Add individual loops to fits.add
  fits.add <- rbind(fits.add, data.frame(City = i,
                                 Month = j,
                                 FIT = FIT,
                                 Actual = Actual,
                                 Residual = Residual))
  fitted.add[i,j] <- FIT
  # print(paste0("Fit of ", i, ".", j, " = ", FIT), quote = F)
}

fits.add <- fits.add[order(fits.add$FIT, decreasing = T), ]
fitted.add
additive.fit$residual
  
#####################
### Extended Fit ###
####################
# Calculate the median for rows and columns
row.medians <- apply(temps, 1, median)
col.medians <- apply(temps, 2, median)

# Order dataframe accordingly
temps <- temps[order(row.medians, decreasing = T),]
temps <- temps[,order(col.medians, decreasing = T)]

# To see pattern
(sent.2way.ordered <- twoway(temps, 'median'))

# Comparison values
round(sent.2way.ordered$compValue,3)

# Possible transformation
plot(sent.2way, which="diagnose") # cubed transformation

# Extended fit
p <- 3
(sent.2way.extended <- twoway(temps^p, 'median'))

# Comparing row and column fits
with(sent.2way.extended, round((roweff + overall)^(1/p), 2))
with(sent.2way.extended, round((coleff + overall)^(1/p), 2))
with(sent.2way.extended,
     apply(residuals, 2, function(x) sign(x)*abs(x)^(1/p)))

# Good transformation?
plot(sent.2way.extended, which="diagnose") # Good transformation

# Plot extended
plot(sent.2way.extended)
grid(col = '#bbbbbb', lwd = 1.4)

# Generate all combinations of i = City and j = Month
combinations <- expand.grid(i = row.names(temps),
                            j = colnames(temps))

# Intialize loop
fitted.ext <- matrix(NA, nrow(temps), ncol(temps),
                     dimnames = dimnames(temps))
raw.residuals.ext <- matrix(NA, nrow(temps), ncol(temps),
                       dimnames = dimnames(temps))
fits.ext <- data.frame()
# Loop through the rows of the data frame
for (row in 1:nrow(combinations)) {
  # Grab City
  i <- combinations$i[row]
  # Grab Month
  j <- combinations$j[row]
  # Fit formula
  FIT <- round((sent.2way.extended$overall + 
                 sent.2way.extended[["roweff"]][[i]] +
                 sent.2way.extended[["coleff"]][[j]]) ^ (1/p), 2)
  
  # Initializing what's in final data.frame
  Actual <- temps[i,j]
  Residual <- Actual - FIT
  # Add individual loops to fits.ext
  fits.ext <- rbind(fits.ext, data.frame(City = i,
                                 Month = j,
                                 FIT = FIT,
                                 Actual = Actual,
                                 Residual = Residual))
  fitted.ext[i,j] <- FIT
  raw.residuals.ext[i,j] <- Residual
  # print(paste0("Fit of ", i, ".", j, " = ", FIT), quote = F)
}

fits.ext <- fits.ext[order(fits.ext$FIT, decreasing = T), ]
fitted.ext
raw.residuals.ext

# Residual Analysis
hist(fits.ext$Residual)
aplpack::stem.leaf(c(fits.ext$Residual))
hinkley(sent.2way.extended$residuals)

## Alternative
# # k as multiple for cv
# k <- 1 - p
#
# fits.ext2 <- data.frame()
# # Loop through the rows of the data frame
# for (row in 1:nrow(combinations)) {
#   # Grab City
#   i <- combinations$i[row]
#   # Grab Month
#   j <- combinations$j[row]
#   # Fit formula
#   FIT <- round(sent.2way$overall + # Use original sent.2way, not extended
#                  sent.2way[["roweff"]][[i]] +
#                  sent.2way[["coleff"]][[j]] +
#                  k * sent.2way$compValue[i,j], 2) # bc we include the one more term
#   
#   # Initializing what's in final data.frame
#   Actual <- temps[i,j]
#   Residual <- Actual - FIT
#   # Add individual loops to fits.ext2
#   fits.ext2 <- rbind(fits.ext2, data.frame(City = i,
#                                  Month = j,
#                                  FIT = FIT,
#                                  Actual = Actual,
#                                  Residual = Residual))
#   # print(paste0("Fit of ", i, ".", j, " = ", FIT), quote = F)
# }
# 
# (fits.ext2 <- fits.ext2[order(fits.ext2$FIT, decreasing = T), ])

# Residuals

##########################
### Multiplicative Fit ###
#########################
library(LearnEDAfunctions)
olympics.swim

(times <- data.frame(olympics.swim[,-1], row.names = olympics.swim[,1]))

# Extended fit
(sent.2way.mult <- twoway(log(times), 'median'))

# Comparing row and column fits
with(sent.2way.mult, round(exp(roweff + overall), 2))
with(sent.2way.mult, round(exp(coleff + overall), 2))
with(sent.2way.mult, signif(exp(sent.2way.mult$residuals),3))

# Good transformation?
plot(sent.2way.mult, which="diagnose") # Good transformation

# Plot extended
plot(sent.2way.mult)
grid(col = '#bbbbbb', lwd = 1.4)

# Generate all combinations of i = Year and j = Distance
combinations <- expand.grid(i = row.names(times),
                            j = colnames(times))
# Initialize
fitted.mult <- matrix(NA, nrow(times), ncol(times),
                     dimnames = dimnames(times))
raw.residuals.mult <- matrix(NA, nrow(times), ncol(times),
                       dimnames = dimnames(times))
fits.mult <- data.frame()
# Loop through the rows of the data frame
for (row in 1:nrow(combinations)) {
  # Grab Year
  i <- combinations$i[row]
  # Grab Distance
  j <- combinations$j[row]
  # Fit formula
  FIT <- round(exp(sent.2way.mult$overall + 
                  sent.2way.mult[["roweff"]][[i]] +
                  sent.2way.mult[["coleff"]][[j]]), 2)
  
  # Initializing what's in final data.frame
  Actual <- times[i,j]
  Residual <- Actual - FIT
  # Add individual loops to fits.mult
  fits.mult <- rbind(fits.mult, data.frame(Year = i,
                                 Distance = j,
                                 FIT = FIT,
                                 Actual = Actual,
                                 Residual = Residual))
  fitted.mult[i,j] <- FIT
  raw.residuals.mult[i,j] <- Residual
  # print(paste0("Fit of ", i, ".", j, " = ", FIT), quote = F)
}

fits.mult <- fits.mult[order(fits.mult$FIT, decreasing = T), ]
fitted.mult
raw.residuals.mult

# Sort absolute residuals
n <- sort(abs(raw.residuals.mult), T)
# view top five residuals
(combed <- apply(raw.residuals.mult, 2,
                 function(x) ifelse(abs(x) >= min(head(n,5)), x, NA)))

# Residual Analysis
hist(fits.mult$Residual)
aplpack::stem.leaf(c(fits.mult$Residual))
hinkley(fits.mult$Residual)

## Interpretations by row axis
(fitted.year <- with(sent.2way.mult, exp(roweff + overall)))

# Plot row fit
plot(names(fitted.year), fitted.year, type = 'b',
     ylab = 'Row Fit', xlab = 'Year', pch=19,
     main = 'Plot of Year Fit')
grid()

# Generate all permutations of row names
permutations <- combn(row.names(times), 2, simplify = F)
# Convert the list to a data frame
permutations <- data.frame(matrix(unlist(permutations), nrow=length(permutations), byrow=T))

ratio.matrix.year <- matrix(NA, nrow(times), nrow(times),
                            dimnames = list(dimnames(times)[[1]],
                                            dimnames(times)[[1]]))
ratios <- data.frame()
# Loop through the rows of the data frame
for (row in 1:nrow(permutations)) {
  i <- permutations$X1[row]
  j <- permutations$X2[row]
  ratio <- fitted.year[[i]] / fitted.year[[j]]
  ratio2 <- 1/ratio
  ratios <- rbind(ratios, data.frame(Year1 = i, Year2 = j,
                                     Ratio = round(ratio,2),
                                     Ratio2 = round(ratio2, 2)))
  ratio.matrix.year[i,j] <- round(ratio2,2)
  ratio.matrix.year[j,i] <- round(ratio,2)
  # print(paste0("Ratio of fit for ", i, "/", j, " = ", ratio), quote = F)
}

# Create a new dataframe with three columns
ratios.years <- data.frame(Year1 = ratios$Year1, Year2 = ratios$Year2)
# Iterate over each row of the dataframe
for (i in 1:nrow(ratios)) {
  # Determine which of the two numeric columns is greater
  greater <- max(ratios$Ratio[i], ratios$Ratio2[i])
  
  # Assign the value of the greater ratio to the Greater column
  ratios.years$Ratio[i] <- greater
  
  # Swap the values of Year1 and Year2 if Ratio is less than Ratio2
  if (ratios$Ratio[i] < ratios$Ratio2[i]) {
    ratios.years[i, c("Year1", "Year2")] <- rev(ratios.years[i, c("Year1", "Year2")])
  }
}

# For interpretation, 1968 swim *times* on average are 1.11 times longer than 1988, 2000, and 2004's times
(ratios.years <- ratios.years[order(ratios.years$Ratio, decreasing = T), ])
ratio.matrix.year

## Interpretations by column axis
(fitted.dist <- with(sent.2way.mult, exp(coleff + overall)))

# Plot col fit
ggplot(NULL, aes(fitted.dist, names(fitted.dist), group = 1)) + 
  geom_point(stat = 'summary', fun = sum) +
  stat_summary(fun = sum, geom = 'line') + theme_bw() +
  labs(title = 'Plot Distance Fit', x = 'Col Fit', y = 'Distance')

# ggplot(NULL, aes(names(fitted.dist), fitted.dist, group = 1)) + 
#   geom_point(stat = 'summary', fun = sum) +
#   stat_summary(fun = sum, geom = 'line') + theme_bw() +
#   labs(title = 'Plot Distance Fit', y = 'Col Fit', x = 'Distance')

# Generate all permutations of column names
permutations <- combn(colnames(times), 2, simplify = F)
# Convert the list to a data frame
permutations <- data.frame(matrix(unlist(permutations), nrow=length(permutations), byrow=T))

#
ratio.matrix.dist <- matrix(NA, ncol(times), ncol(times),
                       dimnames = list(dimnames(times)[[2]],
                                       dimnames(times)[[2]]))
ratios <- data.frame()
# Loop through the rows of the data frame
for (row in 1:nrow(permutations)) {
  i <- permutations$X1[row]
  j <- permutations$X2[row]
  ratio <- fitted.dist[[i]] / fitted.dist[[j]]
  ratio2 <- 1/ratio
  ratios <- rbind(ratios, data.frame(Dist1 = i, Dist2 = j,
                                     Ratio = round(ratio,2),
                                     Ratio2 = round(ratio2,2)))
  ratio.matrix.dist[i,j] <- round(ratio2,2)
  ratio.matrix.dist[j,i] <- round(ratio,2)
  # print(paste0("Ratio of fit for ", i, "/", j, " = ", ratio), quote = F)
}

# Create a new dataframe with three columns
ratios.dist <- data.frame(Dist1 = ratios$Dist1, Dist2 = ratios$Dist2)
# Iterate over each row of the dataframe
for (i in 1:nrow(ratios)) {
  # Determine which of the two numeric columns is greater
  greater <- max(ratios$Ratio[i], ratios$Ratio2[i])
  
  # Assign the value of the greater ratio to the Greater column
  ratios.dist$Ratio[i] <- greater
  
  # Swap the values of Dist1 and Dist2 if Ratio is less than Ratio2
  if (ratios$Ratio[i] < ratios$Ratio2[i]) {
    ratios.dist[i, c("Dist1", "Dist2")] <- rev(ratios.dist[i, c("Dist1", "Dist2")])
  }
}

# For interpretation, 800m race takes on average 9.09 times longer than 100m race
(ratios.dist <- ratios.dist[order(ratios.dist$Ratio, decreasing = T), ])
ratio.matrix.dist

