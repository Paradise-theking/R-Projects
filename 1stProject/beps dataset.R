rm(list = ls())
library(LearnEDAfunctions)
library(tidyverse)
library(reshape2)
library(aplpack)
library(vcd)
library(ggplot2)
library(twoway)
library(car)
BEPS <- read.csv("C:/Users/user/Desktop/sgele/YEAR III/Data Warehousing and Mining/PROJECTS/BEPS.csv")
head(BEPS)



for (vote in unique(BEPS$vote)) {
  print(paste0("vote: ",vote))
  print(fivenum(BEPS[BEPS$vote == vote, ]$age))
} 

for (vote in unique(BEPS$vote)) {
  print(paste0("vote: ",vote))
  print(stem.leaf(BEPS[BEPS$vote == vote, ]$age))
}

for (vote in unique(BEPS$vote)) {
  print(paste0("vote: ",vote))
  print(hinkley(BEPS[BEPS$vote == vote, ]$age))
}


ggplot(BEPS, aes(x =  age,fill =  vote))+
  geom_histogram()+facet_wrap(~vote)


#####################################
#####################################
#################################
new.beps <- BEPS[, c(2,4,5,6,7,8,9,10)]

new.beps %>%
  group_by(vote) %>%
  summarise_all(sum) %>%
  data.frame() -> beps.changed

head(beps.changed)

(beps.new <- data.frame(beps.changed[,-1], row.names = beps.changed[,1]))

# Median polish
(additive.fit <- medpolish(log(beps.new)))

plot(twoway(beps.new))
grid(col = "#bbbbbb", lwd = 1.4)

##############
# Additive fit plot
Row.Part <- with(additive.fit, row + overall)
Col.Part <- additive.fit$col
plot2way(Row.Part, Col.Part,
         dimnames(salariess)[[1]], dimnames(beps.new)[[2]])

#########################
#########################
############################



df <- data.frame(vote = BEPS$vote,age = BEPS$age,economic.cond.national= BEPS$economic.cond.national,
                 economic.cond.household= BEPS$economic.cond.household,political.knowledge=BEPS$political.knowledge)
#################################
ggplot(df, aes(vote,age))+
  geom_boxplot()+
  labs(title = 'raw',y = "age" ,x = "vote" )+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))



symplot(BEPS$age)


midz <- lval(df$age)
midz$mids


lval_plus(df,age,vote) %>%
  group_by(vote) %>%
  summarise(vote = unique(vote),
            Fence_LO = median(Fence_LO),
            Fence_HI = median (Fence_HI),
            Count_Outs = sum(OUT == TRUE),
            .groups = 'drop')



lval_plus(BEPS,age,vote)[-c(2:3)]%>%
  group_by(vote)%>%
  filter(OUT == TRUE)%>%
  arrange(age, .by_group = T)%>%
  select(c('X','vote','age','economic.cond.national','economic.cond.household',
           'Blair','Hague','Kennedy','Europe','political.knowledge','gender'))


spread_level_plot(BEPS,age,vote)
(p <- 1-0.81)
reexpressed.df = data.frame(vote = BEPS$vote,age = BEPS$age, log.age = log(BEPS$age))

ggplot(reexpressed.df, aes(vote,log.age))+
  geom_boxplot()+
  labs(title = 'reexpressed',y = "log.age" ,x = "vote" )+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))





sort(BEPS$age)[1:10]
max(BEPS$age)
min(BEPS$age)

h.bin <- hist(BEPS$age, plot = F)
bins <- h.bin$breaks
# (bins <- seq(1000, 80000, 1000))
(bin.mids <- (bins[-1] + bins[-length(bins)]) / 2)
(h <- with(BEPS,hist(age, breaks = bins, xlab = "age", main = "Histogram")))



data.frame(Mid=h$mids, Count=h$counts, Roots=sqrt(h$counts))###????

h$counts <- sqrt(h$counts)
plot(h, xlab = "age", ylab = "ROOT FREQUENCY", main = "standardized histogram")

# Gaussian Resistant measures
(fl.u <- fivenum(BEPS$age)[c(2,4)])

g.mean <- sum(fl.u)/2
g.sd <- diff(fl.u)/1.349

# Standard Resistant measures
median <- median(BEPS$age)
df <- IQR(BEPS$age)

s <- fit.gaussian(BEPS$age, bins, median, df)
s <- fit.gaussian(BEPS$age, bins, g.mean, g.sd)

options(digits=4)
(data <- data.frame(Mid=bin.mids, d=s$counts, sqrt.d=sqrt(s$counts),
                    Prob=s$probs, e=s$expected, sqrt.e=sqrt(s$expected),
                    Residual=s$residual))

plot(h, xlab = "age", ylab = "ROOT FREQUENCY", main = "")

lines(bin.mids, sqrt(s$expected))




rootogram(s$counts, s$expected)
rootogram(s$counts, s$expected, type = 'deviation')


data$drr <- with(data, sqrt(2 + 4*d) - sqrt(1 + 4*e))

# Alternative especially when bins have small counts
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e)))
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e), type = 'deviation'))



BEPS$log.age <- log(BEPS$age)
BEPS$log.europe <- log(BEPS$Europe)
head(BEPS)

sort(BEPS$log.age)[1:10]
max(BEPS$log.age)
min(BEPS$log.age)

h.bin <- hist(BEPS$log.age, plot = F)
bins <- h.bin$breaks
# (bins <- seq(1000, 80000, 1000))
(bin.mids <- (bins[-1] + bins[-length(bins)]) / 2)
(h <- with(BEPS,hist(log.age, breaks = bins, xlab = "age", main = "Histogram")))



data.frame(Mid=h$mids, Count=h$counts, Roots=sqrt(h$counts))###????

h$counts <- sqrt(h$counts)
plot(h, xlab = "log.age", ylab = "ROOT FREQUENCY", main = "standardized histogram")

# Gaussian Resistant measures
(fl.u <- fivenum(BEPS$log.age)[c(2,4)])

g.mean <- sum(fl.u)/2
g.sd <- diff(fl.u)/1.349

# Standard Resistant measures
median <- median(BEPS$log.age)
df <- IQR(BEPS$log.age)

s <- fit.gaussian(BEPS$log.age, bins, median, df)
s <- fit.gaussian(BEPS$log.age, bins, g.mean, g.sd)

options(digits=4)
(data <- data.frame(Mid=bin.mids, d=s$counts, sqrt.d=sqrt(s$counts),
                    Prob=s$probs, e=s$expected, sqrt.e=sqrt(s$expected),
                    Residual=s$residual))

plot(h, xlab = "log.age", ylab = "ROOT FREQUENCY", main = "")

lines(bin.mids, sqrt(s$expected))




rootogram(s$counts, s$expected)
rootogram(s$counts, s$expected, type = 'deviation')


data$drr <- with(data, sqrt(2 + 4*d) - sqrt(1 + 4*e))

# Alternative especially when bins have small counts
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e)))
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e), type = 'deviation'))
#################################
################################
#####################################
plot(BEPS$age,BEPS$Europe)


plot(BEPS$log.age,BEPS$log.europe)
(sorted.datas <- BEPS[order(BEPS$log.age, decreasing = T),])

sorted.datas$group <- as.numeric(cut_number(sorted.datas$log.age, 3)) #dividing data into three groups
length(sorted.datas$log.age)
# Counts per group
(n1 <- nrow(sorted.datas[which(sorted.datas$group == 1),]))# which means where
(n2 <- nrow(sorted.datas[which(sorted.datas$group == 2),]))
(n3 <- nrow(sorted.datas[which(sorted.datas$group == 3),]))

# Find divisions between groups
t1 <- first(sorted.datas[which(sorted.datas$group == 1),
                         'log.age'])

t2 <- first(sorted.datas[which(sorted.datas$group == 2),
                         'log.age'])

# find (median x, median y) and put to one data frame
# summary.points <- data.frame(x=c(1.79, 1.87, 2.03),
#                        y=c(2.08, 2.14, 2.31))
head( BEPS)
(summary.points <- as.data.frame(
  rbind(
    sapply(sorted.datas[which(sorted.datas$group == 1),
                        c(12,13)], 2, FUN = median),
    sapply(sorted.datas[which(sorted.datas$group == 2),
                        c(12,13)], 2, FUN = median),
    sapply(sorted.datas[which(sorted.datas$group == 3),
                        c(12,13)], 2, FUN = median) #2 means column
  )
)
)

(slr.full <- lm(log.age ~ log.europe,
                data = sorted.datas))
(slr.resistant <- lm(log.age ~ log.europe,  summary.points))
#is rlm model from original data, will it be similar to resistant line
#r.resistant <-rlm(log.2000~log.1985, data = home.prices))




# View plot with 3rds and summary points
with(sorted.datas, plot(log.age,log.europe,
                        main="3 Groups, Summary Points\n and Lines of Best Fit"))
abline(v=t1)
abline(v=t2)
points(summary.points, cex=2, pch=19, col="red")

abline(slr.full, col = "blue")
abline(slr.resistant, col = "red")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Fitting a risistant line to data with rline
(myfit <- rline(log.age~log.europe, sorted.datas))
FIT <- with(myfit, a + b * (sorted.datas$log.age - xC))
RESIDUAL <- myfit$residual
options(digits=4) # round everything to 4 digits
BEPS <- data.frame(X = BEPS$X,vote = BEPS$vote,age =  BEPS$age,
                            economic.cond.national = BEPS$economic.cond.national,
                            economic.cond.household = BEPS$economic.cond.household,
                            Blair = BEPS$Blair,Hague = BEPS$Hague,Kennedy = BEPS$Kennedy,
                            Europe = BEPS$Europe, political.knowledge = BEPS$political.knowledge,
                            gender = BEPS$gender, log.age = BEPS$log.age,log.europe = BEPS$log.europe,
                            FIT, RESIDUAL)

# Residual Analysis
plot(BEPS$log.age, RESIDUAL,
     main = 'Resistant SLR Residual vs log.1985')
abline(h=0, col = "red", lwd=2)

# Finding convergence with iterations from 5 to 10
Results <- data.frame(Iteration=NULL, Slope=NULL, Intercept=NULL)
for(iterations in 1:10){
  fit <- rline(log.europe ~ log.age, BEPS, iter=iterations)
  Results <- rbind(Results,
                   data.frame(Iteration=iterations,
                              Slope=fit$b, Intercept=fit$a))
}
Results

# Median x value
(xc <- summary.points[[2,1]])

# Build linear model
lm(log.europe ~ I(log.age - xc), data = BEPS)

# Create an outlier to see difference
(BEPS$Europe.a <- BEPS$Europe)
(BEPS$Europe.a[331])
(BEPS$Europe.a[331] <- 10) ##?????

r <- rline(Europe.a ~ log.age, BEPS, 10)
r[c("a", "b", "xC")]

(slr.a <- lm(BEPS$Europe.a ~ I(log.age - r$xC), BEPS))
(slr.a <- lm(BEPS$Europe.a ~ log.age, BEPS))

with(BEPS, plot(age, Europe.a))
abline(a = r$a + r$b * -r$xC,
       b = r$b, col = "red")
abline(slr.a, col = "blue")
legend("topleft", legend = c("SLR Full", "SLR Resistant"), 
       col = c("blue", "red"), lwd=2)

# Resistant v Full Residual Analysis
plot(BEPS$log.age, r$residual,
     main = 'Resistant SLR Residual vs log.age')
abline(h=0, col = "red", lwd=2)
plot(BEPS$log.age, slr.a$residual,
     main = 'Full SLR Residual vs log.age')
abline(h=0, col = "blue", lwd=2)

# Predict price of a home in 2000 when the 1985 price = 45
x0 <- data.frame(age = 45)
# Using: log(house.price.2000) = 0.9071∗log(house.price.1985) + 1.006
exp(r$b * log(x0[[1]]) + r$a + r$b * -r$xC)
# Using: house.price.2000 = (house.price.1985) ^ 0.9071 ∗ e ^ 1.006 
x0[[1]] ^ r$b * exp(r$a + r$b * -r$xC)

