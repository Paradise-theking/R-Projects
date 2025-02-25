#lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach,character.only = T, unload = T, force = T)
rm(list = ls())
library(LearnEDAfunctions)
library(tidyverse)
library(reshape2)
library(aplpack)
library(vcd)
library(ggplot2)
library(twoway)
library(car)

salaries.data <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/Data Warehousing and Mining/PROJECTS/Salaries.csv")
head(salaries.data)
salaries.data <- na.omit(salaries.data)

for (rank in unique(salaries.data$rank)) {
  print(paste0("rank: ",rank))
  print(stem.leaf(salaries.data[salaries.data$rank == rank, ]$salary))
} 

for (rank in unique(salaries.data$rank)) {
  print(paste0("rank: ",rank))
  print(fivenum(salaries.data[salaries.data$rank == rank, ]$salary))
} 

for (rank in unique(salaries.data$rank)) {
  print(paste0("rank: ",rank))
  print(hist(salaries.data[salaries.data$rank == rank, ]$salary))
} 

ggplot(salaries.data, aes(x =  salary,fill =  rank))+
  geom_histogram()+facet_wrap(~rank)
#######################



new.salaries.data <- salaries.data[, c(2,4,5,7)]

new.salaries.data %>%
  group_by(rank) %>%
  summarise_all(sum) %>%
  data.frame() -> salaries.dataz

head(salaries.dataz)

(salariess <- data.frame(salaries.dataz[,-1], row.names = salaries.dataz[,1]))

# Median polish
(additive.fit <- medpolish(log(salariess)))

plot(twoway(salariess))
grid(col = "#bbbbbb", lwd = 1.4)

##############
# Additive fit plot
Row.Part <- with(additive.fit, row + overall)
Col.Part <- additive.fit$col
plot2way(Row.Part, Col.Part,
         dimnames(salariess)[[1]], dimnames(salariess)[[2]])

#################
###################



spread_level_plot(salaries.data,salary,rank)

(p <- 1-0.87)

framed.datas <- data.frame(rank = salaries.data$rank,
                           salary =salaries.data$salary)
#################################
ggplot(framed.datas, aes(rank,salary))+
  geom_boxplot()+
  labs(title = 'raw',y = "Salary" ,x = "Rank" )+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))


midds <- lval(framed.datas$salary)
midds$mids


lval_plus(framed.datas,salary,rank) %>%
  group_by(rank) %>%
  summarise(rank = unique(rank),
            Fence_LO = median(Fence_LO),
            Fence_HI = median (Fence_HI),
            Count_Outs = sum(OUT == TRUE),
            .groups = 'drop')



lval_plus(salaries.data,salary,rank)[-c(2:3)]%>%
  group_by(rank)%>%
  filter(OUT == TRUE)%>%
  arrange(salary, .by_group = T)%>%
  select(c('rank','discipline','yrs.since.phd','yrs.service','sex','salary'))


framed.data <- data.frame(rank = salaries.data$rank,
                           salary =log(salaries.data$salary))
#################################
ggplot(framed.data, aes(rank,salary))+
  geom_boxplot()+
  labs(title = 'raw',y = "Salary" ,x = "Rank" )+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))

################################
#######################################

########################


######################
sort(salaries.data$salary)[1:10]
max(salaries.data$salary)
min(salaries.data$salary)

h.bin <- hist(salaries.data$salary, plot = T)
bins <- h.bin$breaks
# (bins <- seq(1000, 80000, 1000))
(bin.mids <- (bins[-1] + bins[-length(bins)]) / 2)
(h <- with(salaries.data,hist(salary, breaks = bins, xlab = "Salary", main = "Histogram")))



data.frame(Mid=h$mids, Count=h$counts, Roots=sqrt(h$counts))###????

h$counts <- sqrt(h$counts)
plot(h, xlab = "TIME", ylab = "ROOT FREQUENCY", main = "standardized histogram")

# Gaussian Resistant measures
(fl.u <- fivenum(salaries.data$salary)[c(2,4)])

g.mean <- sum(fl.u)/2
g.sd <- diff(fl.u)/1.349

# Standard Resistant measures
median <- median(salaries.data$salary)
df <- IQR(salaries.data$salary)

s <- fit.gaussian(salaries.data$salary, bins, median, df)
s <- fit.gaussian(salaries.data$salary, bins, g.mean, g.sd)

options(digits=4)
(data <- data.frame(Mid=bin.mids, d=s$counts, sqrt.d=sqrt(s$counts),
                    Prob=s$probs, e=s$expected, sqrt.e=sqrt(s$expected),
                    Residual=s$residual))

plot(h, xlab = "TIME", ylab = "ROOT FREQUENCY", main = "")

lines(bin.mids, sqrt(s$expected))




rootogram(s$counts, s$expected)
rootogram(s$counts, s$expected, type = 'deviation')


data$drr <- with(data, sqrt(2 + 4*d) - sqrt(1 + 4*e))

# Alternative especially when bins have small counts
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e)))
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e), type = 'deviation'))
################
#######################
########################
salaries.data<- data.frame(rank = salaries.data$rank,discipline = salaries.data$discipline,
                           yrs.since.phd= salaries.data$yrs.since.phd,yrs.service = salaries.data$yrs.service,
                           sex = salaries.data$sex, salary = salaries.data$salary, log.salary = log(salaries.data$salary))

sort(salaries.data$log.salary)[1:10]
max(salaries.data$log.salary)
min(salaries.data$log.salary)

h.bin <- hist(salaries.data$log.salary, plot = T)
bins <- h.bin$breaks
# (bins <- seq(1000, 80000, 1000))
(bin.mids <- (bins[-1] + bins[-length(bins)]) / 2)
(h <- hist(salaries.data$log.salary, breaks = bins, xlab = "Salary", main = "Histogram"))



data.frame(Mid=h$mids, Count=h$counts, Roots=sqrt(h$counts))###????

h$counts <- sqrt(h$counts)
plot(h, xlab = "TIME", ylab = "ROOT FREQUENCY", main = "standardized histogram")

# Gaussian Resistant measures
(fl.u <- fivenum(salaries.data$log.salary)[c(2,4)])

g.mean <- sum(fl.u)/2
g.sd <- diff(fl.u)/1.349

# Standard Resistant measures
median <- median(salaries.data$log.salary)
df <- IQR(salaries.data$log.salary)

s <- fit.gaussian(salaries.data$log.salary, bins, median, df)
s <- fit.gaussian(salaries.data$log.salary, bins, g.mean, g.sd)

options(digits=3)
(data <- data.frame(Mid=bin.mids, d=s$counts, sqrt.d=sqrt(s$counts),
                    Prob=s$probs, e=s$expected, sqrt.e=sqrt(s$expected),
                    Residual=s$residual))

plot(h, xlab = "Salary", ylab = "ROOT FREQUENCY", main = "")
#lines(bin.mids, sqrt(s$expected))


rootogram(s$counts, s$expected)
rootogram(s$counts, s$expected, type = 'deviation')


data$drr <- with(data, sqrt(2 + 4*d) - sqrt(1 + 4*e))

# Alternative especially when bins have small counts
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e)))
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e), type = 'deviation'))


################
############################
head(salaries.data)
plot(salaries.data$salary,salaries.data$yrs.service)
symplot(salaries.data$salary)

new.framed <- data.frame(sqrt.yrs.service = sqrt(salaries.data$yrs.service),
                         sqrt.salary = sqrt(salaries.data$salary))
plot(new.framed$sqrt.salary,new.framed$sqrt.yrs.service)
(sorted.datas <- new.framed[order(new.framed$sqrt.yrs.service, decreasing = T),])

sorted.datas$group <- as.numeric(cut_number(sorted.datas$sqrt.yrs.service, 3)) #dividing data into three groups
length(sorted.datas$sqrt.salary)
# Counts per group
(n1 <- nrow(sorted.datas[which(sorted.datas$group == 1),]))# which means where
(n2 <- nrow(sorted.datas[which(sorted.datas$group == 2),]))
(n3 <- nrow(sorted.datas[which(sorted.datas$group == 3),]))

# Find divisions between groups
t1 <- first(sorted.datas[which(sorted.datas$group == 1),
                              'sqrt.yrs.service'])

t2 <- first(sorted.datas[which(sorted.datas$group == 2),
                              'sqrt.yrs.service'])

# find (median x, median y) and put to one data frame
# summary.points <- data.frame(x=c(1.79, 1.87, 2.03),
#                        y=c(2.08, 2.14, 2.31))
(summary.points <- as.data.frame(
  rbind(
    sapply(sorted.datas[which(sorted.datas$group == 1),
                       1:2], 2, FUN = median),
    sapply(sorted.datas[which(sorted.datas$group == 2),
                       1:2], 2, FUN = median),
    sapply(sorted.datas[which(sorted.datas$group == 3),
                       1:2], 2, FUN = median) #2 means column
  )
)
)

(slr.full <- lm(sqrt.salary ~ sqrt.yrs.service,
                data = new.framed))
(slr.resistant <- lm(sqrt.salary ~ sqrt.yrs.service,  summary.points))
#is rlm model from original data, will it be similar to resistant line
#r.resistant <-rlm(log.2000~log.1985, data = home.prices))




# View plot with 3rds and summary points
with(sorted.datas, plot(sqrt.yrs.service, sqrt.salary,
                       main="3 Groups, Summary Points\n and Lines of Best Fit"))
abline(v=t1)
abline(v=t2)
points(summary.points, cex=2, pch=19, col="red")

abline(slr.full, col = "blue")
abline(slr.resistant, col = "red")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Fitting a risistant line to data with rline
(myfit <- rline(sqrt.salary~sqrt.yrs.service, new.framed))
FIT <- with(myfit, a + b * (new.framed$sqrt.salary - xC))
RESIDUAL <- myfit$residual
options(digits=4) # round everything to 4 digits
salaries.data <- data.frame(rank = salaries.data$rank, 
           discipline = salaries.data$discipline, 
           yrs.since.phd =  salaries.data$yrs.since.phd,
          yrs.service = salaries.data$yrs.service,
          sex = salaries.data$sex,
          salary = salaries.data$salary,
          sqrt.yrs.service = new.framed$sqrt.yrs.service,
          sqrt.salary = new.framed$sqrt.salary,
           FIT, RESIDUAL)

head(salaries.data)
# Residual Analysis
plot(salaries.data$sqrt.yrs.service, RESIDUAL,
     main = 'Resistant SLR Residual vs yrs.service')
abline(h=0, col = "red", lwd=2)

# Finding convergence with iterations from 5 to 10
Results <- data.frame(Iteration=NULL, Slope=NULL, Intercept=NULL)
for(iterations in 1:10){
  fit <- rline(sqrt.yrs.service ~ sqrt.salary, salaries.data, iter=iterations)
  Results <- rbind(Results,
                   data.frame(Iteration=iterations,
                              Slope=fit$b, Intercept=fit$a))
}
Results

# Median x value
(xc <- summary.points[[2,1]])

# Build linear model
lm(sqrt.yrs.service ~ I(sqrt.salary - xc), data = salaries.data)

# Create an outlier to see difference
(salaries.data$yrs.service.a <- salaries.data$sqrt.yrs.service)
(salaries.data$yrs.service.a[331])
(salaries.data$yrs.service.a[331] <- 10) ##?????

r <- rline(yrs.service.a ~ sqrt.salary, salaries.data, 10)
r[c("a", "b", "xC")]

(slr.a <- lm(salaries.data$yrs.service.a ~ I(sqrt.salary - r$xC), salaries.data))
(slr.a <- lm(yrs.service.a ~ sqrt.salary, salaries.data))

with(salaries.data, plot(sqrt.salary, salaries.data$yrs.service.a))
abline(a = r$a + r$b * -r$xC,
       b = r$b, col = "red")
abline(slr.a, col = "blue")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Resistant v Full Residual Analysis
plot(salaries.data$sqrt.salary, r$residual,
     main = 'Resistant SLR Residual vs sqrt.salary')
abline(h=0, col = "red", lwd=2)
plot(salaries.data$sqrt.salary, slr.a$residual,
     main = 'Full SLR Residual vs sqrt.salary')
abline(h=0, col = "blue", lwd=2)

# Predict price of a home in 2000 when the 1985 price = 45
x0 <- data.frame(y1985 = 45)
# Using: log(house.price.2000) = 0.9071∗log(house.price.1985) + 1.006
exp(r$b * log(x0[[1]]) + r$a + r$b * -r$xC)
# Using: house.price.2000 = (house.price.1985) ^ 0.9071 ∗ e ^ 1.006 
x0[[1]] ^ r$b * exp(r$a + r$b * -r$xC)

######################
##########################

