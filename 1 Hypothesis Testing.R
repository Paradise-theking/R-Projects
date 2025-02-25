# Reset and detach all packages
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), 
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(gginference) # ggttest()
library(dplyr)
library(ggpubr) #ggpaired
library(knitr) # kable()
# library(texreg) # screenreg(list(model1, model2,...))

# Set ggplot2 theme
theme_set(theme_minimal())

# Load data
path.full <- here::here() #Look at our directory tree

path <- paste0(dirname(path.full), "/datasets/birth_weight.csv")
birth_weight <- readr::read_csv(path)

birth_weight$Maternal.Smoker <- as.numeric(birth_weight$Maternal.Smoker)

##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Example 1: One-Sample Population Mean (Two-Sided) #
##### ##### ##### ##### ##### ##### ##### ##### ##### 
### Create water pressure dataset
RNGkind (sample.kind = "Rounding") 
set.seed(3) #try different seeds and see whether you REJECT or FAIL TO REJECT the null

n <- 30

mean <- 51.788
sd <- 3.389

## Use rnorm() to create dataset with approximately above parameters
(waterpressure <- rnorm(n, mean, sd))

# Check
(xbar <- mean(waterpressure))
(s <- sd(waterpressure))

### Hypothesis testing. Test null hypothesis, mu = 50
### at alpha = 0.05 and degrees of freedom = 29
mu <- 50
alpha <- 0.05
# Divided by 2 bc it's two-sided
(alpha_range <- c(alpha/2, 1-(alpha/2)))
standard_error <- s/sqrt(n)

## T-Statistic for waterpressure data
(t_statistic <- (xbar - mu)/standard_error)

## Critical value for TWO-TAILED t-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range, df = n-1))) 

## Respective p-value
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 2 * (1 - pt(abs(t_statistic), n-1))) 

### SAME AS ABOVE IN ONE FUNCTION t.test() ###
(t <- t.test(waterpressure, mu = mu))

### Visualize
gginference::ggttest(t)

## Same as above, read the code and understand the
## One-Sample Two-Sided Hypothesis Test process
ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm, geom = "area", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, xlim = c(-4, crit_value[1]),
                geom = "area", fill = "grey", alpha = 0.7) +
  annotate("text", x = crit_value[1], y = 0,
           label = signif(crit_value[1],4), vjust = 1.5) +
  stat_function(fun = dnorm, xlim = c(crit_value[2], 4),
                geom = "area", fill = "gray", alpha = 0.7) +
  annotate("text", x = crit_value[2], y = 0,
           label = signif(crit_value[2],4), vjust = 1.5) +
  geom_vline(xintercept = t_statistic, color = "darkblue") +
  geom_text(x = t_statistic, y = 0, angle = 90, hjust = -0.5, vjust = -0.5,
            label = paste("Test statistic = ", signif(t_statistic, 5)), color = "darkblue") +
  labs(title = "One-Sample Two-Sided Hypothesis Test", y = NULL,
       x = paste0("t-distribution with ", n-1, " degrees of freedom"))

##### ##### ##### ##### ##### ##### ##### ##### ##### 
# Example 2: One-Sample Population Mean (One-Sided) #
##### ##### ##### ##### ##### ##### ##### ##### ##### 
### Create MPA scores
RNGkind (sample.kind = "Rounding") 
set.seed(0) #try different seeds and see whether you REJECT or FAIL TO REJECT the null

n <- 18

mean <- 69
sd <- 21.14933

## Use rnorm() to create dataset with approximately above parameters
(mpa_scores <- rnorm(n, mean, sd))

# Check
(xbar <- mean(mpa_scores))
(s <- sd(mpa_scores))

### Hypothesis testing. Test null hypothesis, mu >= 80
### at alpha = 0.05 and degrees of freedom = 17
mu <- 80
alpha <- 0.05
# One sided so do not divide by 2
(alpha_range <- c(alpha, 1-alpha))
standard_error <- s/sqrt(n)

## T-Statistic for mpa_scores data
(t_statistic <- (xbar - mu)/standard_error)

## Critical value for ONE SIDED t-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range[1], df = n-1))) # ALT is < so use alpha_range[1]

## Respective p-value. ONE SIDED so do not multiply by 2.
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 1 - pt(abs(t_statistic), n-1))

### SAME AS ABOVE IN ONE FUNCTION t.test() ###
(t <- t.test(mpa_scores, mu = mu, alternative = "less"))

### Visualize
gginference::ggttest(t)

## Same as above, read the code and understand the
## One-Sample One-Sided Hypothesis Test process
ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm, geom = "area", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, xlim = c(-4, crit_value),
                geom = "area", fill = "gray", alpha = 0.7) +
  annotate("text", x = crit_value, y = 0,
           label = signif(crit_value,4), vjust = 1.5) +
  geom_vline(xintercept = t_statistic, color = "darkblue") +
  geom_text(x = t_statistic, y = 0, angle = 90, hjust = -0.5, vjust = -0.5,
            label = paste("Test statistic = ", signif(t_statistic, 5)), color = "darkblue") +
  labs(title = "One-Sample One-Sided Hypothesis Test", y = NULL,
       x = paste0("t-distribution with ", n-1, " degrees of freedom"))

##### ##### ##### ##### ##### #####  #####  #####
# Example 3: One-Sample Population Proportion #
##### ##### ##### ##### ##### #####  #####  ##### 
### Instagram data
RNGkind (sample.kind = "Rounding") 
set.seed(3) #try different seeds and see whether you REJECT or FAIL TO REJECT the null

n <- 1366

pbar <- 0.3097
p0 <- 0.333

### Hypothesis testing. Test null hypothesis, p0 = 0.33
alpha <- 0.05
# Divided by 2 bc it's two-sided
(alpha_range <- c(alpha/2, 1-(alpha/2)))
# Notice difference in standard error for population data
standard_error <- sqrt(p0*(1-p0)/n)

## Test Statistic for Instagram
(t_statistic <- (pbar-p0)/standard_error)

## Critical value for Population Proportion z-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range, df = n-1)))

## Respective p-value
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 2 * (1 - pt(abs(t_statistic), df = n-1))) 
2 * (1 - pnorm(abs(t_statistic)))

### SAME AS ABOVE IN ONE FUNCTION prop.test() ###
(pt <- prop.test(x = pbar * n, n = n, p = p0,
                 alternative = "two.sided", correct = F))

### Visualize
ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm, geom = "area", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, xlim = c(-4, crit_value[1]),
                geom = "area", fill = "grey", alpha = 0.7) +
  annotate("text", x = crit_value[1], y = 0,
           label = signif(crit_value[1],4), vjust = 1.5) +
  stat_function(fun = dnorm, xlim = c(crit_value[2], 4),
                geom = "area", fill = "gray", alpha = 0.7) +
  annotate("text", x = crit_value[2], y = 0,
           label = signif(crit_value[2],4), vjust = 1.5) +
  geom_vline(xintercept = t_statistic, color = "darkblue") +
  geom_text(x = t_statistic, y = 0, angle = 90, hjust = -0.5, vjust = -0.5,
            label = paste("Test statistic = ", signif(t_statistic, 5)), color = "darkblue") +
  labs(title = "One-Sample Population Proportion Z-Test", y = NULL,
       x = "Z-score")


##### ##### ##### ##### ##### ##### ##### 
# Example 4: Two-Sample Population Means #
##### ##### ##### ##### ##### ##### ##### 
### Create vectors of birthweights of smokers and nonsmokers
smoker <- birth_weight %>%
            filter(Maternal.Smoker == 1) %>%
            pull(Birth.Weight)

nonsmoker <- birth_weight %>%
                filter(Maternal.Smoker == 0) %>%
                pull(Birth.Weight)

# Test the assumption of equal variance by computing the ratio
# of the two variances, and if the ratio is in the range of
# 0.5 to 2.0, then the assumption is adequately met.
var(smoker) / var(nonsmoker) # therefore, we proceed w equal variance assumption

# See lecture slide Textbook Example
(meanD <- diff(c(mean(smoker), mean(nonsmoker))))

n_smoker <- length(smoker)
n_nonsmoker <- length(nonsmoker)
n <- n_smoker + n_nonsmoker

Sp <- sqrt(
  sum((n_smoker-1)*var(smoker),
      (n_nonsmoker-1)*var(nonsmoker))
  /(n-2))
  
### Hypothesis testing. Test null hypothesis, muD = 0
### at alpha = 0.05 and degrees of freedom = n - 2
muD <- 0 # in other words, mu_smoker = mu_nonsmoker
alpha <- 0.05
# TWO-TAILED so divide by 2
(alpha_range <- c(alpha/2, 1-alpha/2))
(standard_error <- Sp * sqrt(sum(1/n_smoker, 1/n_nonsmoker)))

## T-Statistic for birth weight data
(t_statistic <- (meanD - muD)/standard_error)

## Critical value for TWO-TAILED t-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range, df = n-2)))

## Respective p-value
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 2 * (1 - pt(abs(t_statistic), n-2))) 
2 * (1 - pnorm(abs(t_statistic)))

### SAME AS ABOVE IN ONE FUNCTION t.test() ###
(t <- t.test(smoker, nonsmoker, var.equal = TRUE))

## Visualize with ggttest
ggttest(t)

## Same as above, read the code and understand the
## Two-Sample Two-Sided Hypothesis Test process
ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm, geom = "area", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, xlim = c(-4, crit_value[1]),
                geom = "area", fill = "grey", alpha = 0.7) +
  annotate("text", x = crit_value[1], y = 0,
           label = signif(crit_value[1],4), vjust = 1.5) +
  stat_function(fun = dnorm, xlim = c(crit_value[2], 4),
                geom = "area", fill = "gray", alpha = 0.7) +
  annotate("text", x = crit_value[2], y = 0,
           label = signif(crit_value[2],4), vjust = 1.5) +
  geom_vline(xintercept = t_statistic, color = "darkblue") +
  geom_text(x = t_statistic, y = 0, angle = 90, hjust = -0.5, vjust = -0.5,
            label = paste("Test statistic = ", signif(t_statistic, 5)), color = "darkblue") +
  labs(title = "Two-Sample Population Means Hypothesis Test", y = NULL,
       x = paste0("t-distribution with ", n-2, " degrees of freedom"))

### Distributions visual
## Row bind
df <- bind_rows(data.frame(value = smoker, group = "smoker"),
                data.frame(value = nonsmoker, group = "nonsmoker"))

## Summarize
df_summary <- df %>%
  group_by(group) %>%
  summarize(mean = mean(value),
            sd = sd(value))

## Fit Normal Curve
ggplot(df, aes(x=value, color=group, fill=group)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.2) +
  stat_function(fun = dnorm, geom = "area", fill = "blue", color = "blue", alpha = 0.4,
                args = list(mean = df_summary$mean[1],sd = df_summary$sd[1])) +
  stat_function(fun = dnorm, geom = "area", fill = "red", color = "red", alpha = 0.4,
                args = list(mean = df_summary$mean[2], sd = df_summary$sd[2])) +
  geom_vline(data=df_summary, aes(xintercept=mean, color=group),
             linetype=4, linewidth = 1) +
  scale_color_manual(values=c("blue","red")) +
  scale_fill_manual(values=c("blue","red")) +
  theme(legend.position = "bottom") +
  labs(title="Two-Sample Population Means: Birth Weight", x=NULL, y=NULL)

## Fit density curve
ggplot(df, aes(x=value, color=group, fill=group)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.2)+
  geom_density(alpha=0.4)+
  geom_vline(data=df_summary, aes(xintercept=mean, color=group),
             linetype=4, linewidth = 1)+
  scale_color_manual(values=c("blue","red"))+
  scale_fill_manual(values=c("blue","red"))+
  theme(legend.position = "bottom") +
  labs(title="Two-Sample Population Means: Birth Weight", x=NULL, y = NULL)

## Individual distributions
ggplot(data.frame(x = smoker), aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = 'red', alpha = 0.3) +
  stat_function(fun = dnorm, args = list(mean = mean(smoker), sd = sd(smoker)),
                geom = "area", fill = "red", alpha = 0.4) +
  geom_vline(xintercept = mean(smoker), color = "red", lty = 2) +
  geom_text(x = mean(smoker), y = 0, vjust = -0.5,
            label = paste("Mean Birth Weight (smoker) = ", signif(mean(smoker), 5)), color = "black") +
  labs(title = 'Smoker with Normal Curve Fit',
       x = 'Weight',
       y = 'Density')

ggplot(data.frame(x = nonsmoker), aes(x)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = 'blue', alpha = 0.3) +
  stat_function(fun = dnorm, args = list(mean = mean(nonsmoker), sd = sd(nonsmoker)),
                geom = "area", fill = "blue", alpha = 0.4) +
  geom_vline(xintercept = mean(nonsmoker), color = "blue", lty = 2) +
  geom_text(x = mean(nonsmoker), y = 0, vjust = -0.5,
            label = paste("Mean Birth Weight (nonsmoker) = ", signif(mean(nonsmoker), 5)), color = "black") +
  labs(title = 'Nonsmoker with Normal Curve Fit',
       x = 'Weight',
       y = 'Density')

##### ##### ##### ##### ##### ##### 
## Example 5: Paired Difference ##
##### ##### ##### ##### ##### ##### 
### Create vectors of textsbook prices of online and bookstore
online = c(10.20,18.95,184.53,236.75,67.41)
bookstore = c(11.40,19,200.75,247.20,71.25)

D <- online - bookstore

(n <- length(online))

knitr::kable(data.frame(online, bookstore, D))

# See lecture slide Textbook Example
(sigmaD <- sum(D))
(Dbar <- mean(D))
(sD <- sd(D))

### Hypothesis testing. Test null hypothesis, muD = 0
### at alpha = 0.05 and degrees of freedom = n - 1
muD <- 0
alpha <- 0.05
# TWO-TAILED so divide by 2
(alpha_range <- c(alpha/2, 1-alpha/2))
(standard_error <- sD / sqrt(n))

## T-Statistic for mpa_scores data
(t_statistic <- (Dbar - muD)/standard_error)

## Critical value for TWO-TAILED t-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range, df = n-1)))

## Respective p-value
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 2 * (1 - pt(abs(t_statistic), n-1))) 

### SAME AS ABOVE IN ONE FUNCTION t.test() ###
(t <- t.test(online, bookstore, paired=TRUE))

### Visualize
ggttest(t)

## Row bind
df <- bind_rows(data.frame(value = online, group = "online"),
                data.frame(value = bookstore, group = "bookstore"))

# Create a box plot
ggplot(df, aes(x = group, y = value, group = group)) +
  stat_boxplot(geom = "errorbar", width = 0.1) + # adds whisker end vertical lines
  geom_boxplot(fill = "lightgray", width = 0.4, outlier.shape = 1) + #gray box and outlier circles
  labs(x = NULL, y = "Price") +
  ggtitle("Paired Difference: Textbooks")

ggpubr::ggpaired(df, x = "group", y = "value",
                 order = c("bookstore", "online"),
                 ylab = "Price", xlab = NULL)

##### ##### ##### ##### ##### #####  #####  ##### 
## Example 6: Two-Sample Population Proportions ##
##### ##### ##### ##### ##### #####  #####  ##### 
### Create vectors of textsbook prices of online and bookstore
yes = c(207, 231)
n_each = c(702, 565)

knitr::kable(data.frame(Sex = c("Female", "Male"),
                        Yes.Gun.Owner = yes,
                        No.Gun.Owner = n_each - yes))

n <- sum(n_each)

pD <- abs(diff(yes/n_each))
pbar <- sum(yes)/sum(n_each)

### Hypothesis testing. Test null hypothesis, etaD = 0
etaD <- 0 # Null being both proportions (eta) should be equal
alpha <- 0.05
# Divided by 2 bc it's two-sided
(alpha_range <- c(alpha/2, 1-(alpha/2)))
# Notice difference in standard error for population data
standard_error <- sqrt(sum(pbar*(1-pbar)/n_each))

## Test Statistic for gun ownership
(t_statistic <- (pD-etaD)/standard_error)

## Critical value for Population Proportion z-test
# Is abs(t_statistic) > crit_value? If yes, REJECT NULL
(crit_value <- (qt(alpha_range, df = n-2)))

## Respective p-value
# Is p-value < alpha (0.05)? If yes, REJECT NULL
(p_value <- 2 * (1 - pt(abs(t_statistic), n-2))) 

### SAME AS ABOVE IN ONE FUNCTION prop.test() ###
(pt <- prop.test(yes, n_each))

### Visualize
ggplot(data.frame(x=c(-4,4)), aes(x = x)) +
  stat_function(fun = dnorm, geom = "area", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, xlim = c(-4, crit_value[1]),
                geom = "area", fill = "grey", alpha = 0.7) +
  annotate("text", x = crit_value[1], y = 0,
           label = signif(crit_value[1],4), vjust = 1.5) +
  stat_function(fun = dnorm, xlim = c(crit_value[2], 4),
                geom = "area", fill = "gray", alpha = 0.7) +
  annotate("text", x = crit_value[2], y = 0,
           label = signif(crit_value[2],4), vjust = 1.5) +
  geom_vline(xintercept = t_statistic, color = "darkblue") +
  geom_text(x = t_statistic, y = 0, angle = 90, hjust = -0.5, vjust = -0.5,
            label = paste("Test statistic = ", signif(t_statistic, 5)), color = "darkblue") +
  labs(title = "Two-Proportion Z-Test", y = NULL,
       x = "Z-score")

