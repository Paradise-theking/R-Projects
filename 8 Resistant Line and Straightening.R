rm(list = ls())

#######################
### Resistant Line ###
#######################
library(LearnEDAfunctions)
head(home.prices)
max(home.prices$y1985)
# Plot our y ~ x
with(home.prices, plot(y1985, y2000))

# Reexpress for more even spread
home.prices$log.1985 <- log(home.prices$y1985)
home.prices$log.2000 <- log(home.prices$y2000)

with(home.prices, plot(log.1985, log.2000))

# Order data for summary points
data <- data.frame(log.1985 = home.prices$log.1985,
                   log.2000 = home.prices$log.2000)
(sorted.data <- data[order(data$log.1985),])#coma means include all columns 
# rownames(sorted.data) = seq(length = nrow(sorted.data))
round(sorted.data, 2)

# is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
sorted.data$group <- as.numeric(cut_number(sorted.data$log.1985, 3)) #dividing data into three groups
length(sorted.data$log.1985)
# Counts per group
(n1 <- nrow(sorted.data[which(sorted.data$group == 1),]))# which means where
(n2 <- nrow(sorted.data[which(sorted.data$group == 2),]))
(n3 <- nrow(sorted.data[which(sorted.data$group == 3),]))

# Find divisions between groups
t1 <- mean(c(last(sorted.data[which(sorted.data$group == 1),
                              'log.1985']),
             first(sorted.data[which(sorted.data$group == 2),
                               'log.1985'])))

t2 <- mean(c(last(sorted.data[which(sorted.data$group == 2),
                              'log.1985']),
             first(sorted.data[which(sorted.data$group == 3),
                               'log.1985'])))

# find (median x, median y) and put to one data frame
# summary.points <- data.frame(x=c(1.79, 1.87, 2.03),
#                        y=c(2.08, 2.14, 2.31))
(summary.points <- as.data.frame(
  rbind(
    sapply(sorted.data[which(sorted.data$group == 1),
                       1:2], 2, FUN = median),
    sapply(sorted.data[which(sorted.data$group == 2),
                       1:2], 2, FUN = median),
    sapply(sorted.data[which(sorted.data$group == 3),
                       1:2], 2, FUN = median) #2 means column
    )
  )
)

# bild SLRs from full data vs only 3 summary points
(slr.full <- lm(log.2000 ~ log.1985,
                data = home.prices))
(slr.resistant <- lm(log.2000 ~ log.1985,  summary.points))
#is rlm model from original data, will it be similar to resistant line
library(MASS)
#r.resistant <-rlm(log.2000~log.1985, data = home.prices))




# View plot with 3rds and summary points
with(home.prices, plot(log.1985, log.2000,
     main="3 Groups, Summary Points\n and Lines of Best Fit"))
abline(v=t1)
abline(v=t2)
points(summary.points, cex=2, pch=19, col="red")

abline(slr.full, col = "blue")
abline(slr.resistant, col = "red")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Fitting a risistant line to data with rline
(myfit <- rline(log.2000 ~ log.1985, home.prices))
FIT <- with(myfit, a + b * (home.prices$log.1985 - xC))
RESIDUAL <- myfit$residual
options(digits=4) # round everything to 4 digits
data.frame(City = home.prices$City, 
           log.1985 = home.prices$log.1985, 
           log.2000 =  home.prices$log.2000,
           FIT, RESIDUAL)


# Residual Analysis
plot(home.prices$log.1985, RESIDUAL,
     main = 'Resistant SLR Residual vs log.1985')
abline(h=0, col = "red", lwd=2)

# Finding convergence with iterations from 5 to 10
Results <- data.frame(Iteration=NULL, Slope=NULL, Intercept=NULL)
for(iterations in 1:10){
  fit <- rline(log.2000 ~ log.1985, home.prices, iter=iterations)
  Results <- rbind(Results,
                   data.frame(Iteration=iterations,
                              Slope=fit$b, Intercept=fit$a))
}
Results

# Median x value
(xc <- summary.points[[2,1]])

# Build linear model
lm(log.2000 ~ I(log.1985 - xc), data = home.prices)

# Create an outlier to see difference
(home.prices$log.2000a <- home.prices$log.2000)
(home.prices$log.2000a[19])
(home.prices$log.2000a[19] <- 4.5) ##?????

r <- rline(log.2000a ~ log.1985, home.prices, 10)
r[c("a", "b", "xC")]

(slr.a <- lm(log.2000a ~ I(log.1985 - r$xC), home.prices))
(slr.a <- lm(log.2000a ~ log.1985, home.prices))

with(home.prices, plot(log.1985, log.2000a))
abline(a = r$a + r$b * -r$xC,
       b = r$b, col = "red")
abline(slr.a, col = "blue")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Resistant v Full Residual Analysis
plot(home.prices$log.1985, r$residual,
     main = 'Resistant SLR Residual vs log.1985')
abline(h=0, col = "red", lwd=2)
plot(home.prices$log.1985, slr.a$residual,
     main = 'Full SLR Residual vs log.1985')
abline(h=0, col = "blue", lwd=2)

# Predict price of a home in 2000 when the 1985 price = 45
x0 <- data.frame(y1985 = 45)
# Using: log(house.price.2000) = 0.9071∗log(house.price.1985) + 1.006
exp(r$b * log(x0[[1]]) + r$a + r$b * -r$xC)
# Using: house.price.2000 = (house.price.1985) ^ 0.9071 ∗ e ^ 1.006 
x0[[1]] ^ r$b * exp(r$a + r$b * -r$xC)

######################
### Straightening ###
#####################
library(LearnEDAfunctions)
head(car.measurements)

# Scatter plot y ~ x
with(car.measurements,
     plot(Displacement, MPG))
# Add a curve
with(car.measurements,
     lines(lowess(Displacement, MPG, f=0.5)))

# Sort data to get our summary points
sorted.data <- car.measurements[order(car.measurements$Displacement), ]

# Split data into 3equal groups
sorted.data$group <- as.numeric(cut_number(sorted.data$Displacement, 3))

# Counts per group
(n1 <- nrow(sorted.data[which(sorted.data$group == 1),]))
(n2 <- nrow(sorted.data[which(sorted.data$group == 2),]))
(n3 <- nrow(sorted.data[which(sorted.data$group == 3),]))

# Special case of many (x3) disp = 121 in group 1
(sorted.data[which(sorted.data$Displacement == 121),]$group <- 2)
# Find divisions between groups
t1 <- mean(c(last(sorted.data[which(sorted.data$group == 1),'Displacement']),
             first(sorted.data[which(sorted.data$group == 2),'Displacement'])))
t2 <- mean(c(last(sorted.data[which(sorted.data$group == 2),'Displacement']),
             first(sorted.data[which(sorted.data$group == 3),'Displacement'])))

# summary.points <- data.frame(x = c(98, 148.5, 302),
#                              y = c(31, 24.25, 18.2))
(summary.points <- as.data.frame(
  rbind(
    sapply(sorted.data[which(sorted.data$group == 1), c(7,3)], 2, FUN = median),
    sapply(sorted.data[which(sorted.data$group == 2), c(7,3)], 2, FUN = median),
    sapply(sorted.data[which(sorted.data$group == 3), c(7,3)], 2, FUN = median)
    )
  )
)

colnames(summary.points) <- c('x','y')

# Plot
with(car.measurements,
     plot(Displacement, MPG))
abline(v=t1)
abline(v=t2)

points(summary.points, cex=2, pch=19, col="red")
abline(lm(y ~ x, data = summary.points[1:2, ]), col="blue")
abline(lm(y ~ x, data = summary.points[2:3, ]), col="blue")

straightening.work <- function(sp, px, py){
  sp$tx <- with(sp, (x ^ px - 1) / px)
  sp$ty <- with(sp, (y ^ py - 1) / py)
  sp$slope[1] <- with(sp, diff(ty[1:2]) / diff(tx[1:2]))
  sp$slope[2] <- with(sp, diff(ty[2:3]) / diff(tx[2:3]))
  sp$half.slope.ratio <- with(sp, slope[2] / slope[1])
  sp$slope[3] <- NA
  sp$half.slope.ratio[2:3] <- NA
  row.names(sp) <- c("Left", "Center", "Right")
  sp
}
# raw
straightening.work(summary.points, 1, 1)
# sqrt
straightening.work(summary.points, 0.5, 1)
# effectively, log (p = 0)
straightening.work(summary.points, 0.001, 1)
# reexpress y with a log
straightening.work(summary.points, 0.001, 0.001)
# reexpress y with reciprocal
straightening.work(summary.points, 0.001, -1)
# reexpress x with p = -0.33 and y with p = -1
straightening.work(summary.points, -0.33, -1)

# Transform x's and y's to straighten plot
car.measurements$new.x <- car.measurements$Displacement ^ (- 0.33)
(car.measurements$new.y <- 1 / car.measurements$MPG)


# Fit resistant line to transformed data
fit <- rline(new.y ~ new.x, car.measurements, iter=5)
fit[c('a', 'b', 'xC')]

# Plot transformed data with resistant line
with(car.measurements, plot(new.x, new.y,
                            main = "Straightened Fit"))
abline(a = fit$a + fit$b * -fit$xC,
       b = fit$b, col = "red")

# Residual Analysis
plot(car.measurements$new.x, fit$residual, ylab="Residual")
abline(h=0, col = "red", lwd = 2)

# Predict MPG of a car when '78/'79 car's displacement = 210
x0 <- data.frame(Displacement = 210)
# Using: 1/(MPG) = fit$b * Displacement ^ -1/3 + fit$a + fit$b * -fit$xC 
# -> MPG = 1 / (fit$b * Displacement ^ -1/3 + fit$a + fit$b * -fit$xC)
1 / (fit$b * x0[[1]] ^ (-1/3) + fit$a + fit$b * -fit$xC)
############


