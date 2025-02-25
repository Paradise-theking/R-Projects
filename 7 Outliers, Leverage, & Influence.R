# Removes all prior variables and loaded packages (error if no loaded packages, don't worry)
suppressWarnings(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)),
                                  detach, character.only=TRUE, unload=TRUE, force=TRUE)))
rm(list = ls())

library(ggplot2)
library(ggpmisc)
library(dplyr)
library(yardstick)
library(broom)
library(car)
library(cowplot)

# Set ggplot2 theme
theme_set(theme_minimal())

## Original data
x <- c(0.1,	0.45401,	1.09765,	1.27936,	2.20611,	2.50064,	3.0403,
       3.23583,	4.45308,	4.1699,	5.28474,	5.59238,	5.92091,
       6.66066,	6.79953,	7.97943,	8.41536,	8.71607,	8.70156,	9.16463)
y <- c(-0.0716,	4.1673,	6.5703,	13.815,	11.4501,	12.9554,	20.1575,
       17.5633,	26.0317,	22.7573,	26.303,	30.6885,	33.9402,
       30.9228,	34.11,	44.4536,	46.5022,	50.0568,	46.5475,	45.7762)

data <- data.frame(x, y)
slr <- lm(y ~ x)
summary(slr)

response <- y
fit <- fitted(slr)

mae_original <- yardstick::mae_vec(response, fit)

p_original <- ggplot(data, aes(x, y)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
    stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~~~")),
                 label.x.npc = "left", label.y.npc = 1,
                 color = "blue", formula = y ~ x) +
    labs(title = "Original Data", x = NULL, y = NULL)

(p_original_cow <- p_original +
    geom_text(aes(label = paste0("MAE = ", signif(mae_original, 4))),
              x = -Inf, y = Inf, hjust = 0, vjust = 5, size = 4.5))

# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr)

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
(p1_original <- ggplot(df, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Linearity Assumption: Original",
       x = "Fitted Values",
       y = "Residuals"))

##### 2. Check for normality of random error #####
(p2_original <- ggplot(df, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = 'red', linetype = 2) +
  ggtitle("Residual Q-Q Plot: Original") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles"))

### 3. Check for zero mean and constant variance of random error ###
(p3_original <- ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr))))) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = mean(sqrt(abs(rstandard(slr)))), col = "red", lty = 2) +
  labs(title = "Scale-Location: Original",
       x = "Fitted Values",
       y = "sqrt(abs(Standardized Residuals)))"))

### 4. Check for independence of random error ###
# Transform to along dataframe to plot with ggplot
regress.vars <- names(coef(slr)[-1])

df2_org <- df[c(regress.vars, ".resid")] %>% 
  tidyr::pivot_longer(cols = regress.vars,
                      names_to = "statistic",
                      values_to = "xvalue") %>%
  arrange(-xvalue)

# scatter plot of residuals sorted
(p4_original <- ggplot(df2_org, aes(x = 1:nrow(df2_org), y = .resid)) +
  geom_point() +
  geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  labs(title = "Independence Assumption: Original",
       x = "Row Numbers",
       y = "Residuals"))

### ### ### ###
### Outliers ###
### ### ### ###
outlier_x <- 4.4
outlier_y <- 40

x_outlier <- c(x, outlier_x)
y_outlier <- c(y, outlier_y)


slr_outlier <- lm(y_outlier ~ x_outlier)
summary(slr_outlier)

response <- y_outlier
fit <- fitted(slr_outlier)

mae_outlier <- yardstick::mae_vec(response, fit)

## Detecting outliers
(p1 <- p_original +
    labs(title = "Identifying Outliers"))

(p2 <- p1 +
  geom_point(aes(outlier_x, outlier_y), size = 2.5, color = "red"))

(p_outier <- p2 +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5,
              data = data.frame(x = x_outlier, y = y_outlier)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~~~")),
               data = data.frame(x = x_outlier, y = y_outlier), vjust = 1.1,
               label.x.npc = "left", label.y.npc = "top",
               color = "red", formula = y ~ x) +
  geom_text(aes(label = paste0("MAE = ", signif(mae_outlier, 4))),
            x = -Inf, y = Inf, hjust = 0, vjust = 5, size = 4.5))

# Find Outlier Points
(out_plot <- car::outlierTest(slr_outlier))
(out_pnts <- as.integer(names(out_plot$bonf.p)))

# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr_outlier)

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
(p1_outlier <- ggplot(df, aes(x = .fitted, y = .resid)) +
   geom_point(col = ifelse(rownames(df) %in% out_pnts, "red", "black")) +
   geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
   geom_hline(yintercept = 0, col = "red", lty = 2) +
   labs(title = "Linearity Assumption: Outliers",
        x = "Fitted Values", y = "Residuals"))

##### 2. Check for normality of random error #####
# Transform to along dataframe to plot with ggplot
regress.vars <- names(coef(slr_outlier)[-1])

df2 <- df[c(regress.vars, ".resid")] %>% 
  tidyr::pivot_longer(cols = regress.vars,
                      names_to = "statistic",
                      values_to = "xvalue") %>%
  arrange(-xvalue)

(outlier_xorder <- match(df$.resid[out_pnts], df2$.resid))
(resid_outlier <- match(outlier_xorder, order(df2$.resid)))

(p2_outlier <- ggplot(df, aes(sample = .resid)) +
   stat_qq(col = ifelse(rownames(df) %in% resid_outlier, "red", "black")) +
   stat_qq_line(color = 'red', linetype = 2) +
   ggtitle("Residual Q-Q Plot: Outliers") +
   xlab("Theoretical Quantiles") +
   ylab("Sample Quantiles"))

### 3. Check for zero mean and constant variance of random error ###
(p3_outlier <- ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr_outlier))))) +
    geom_point(col = ifelse(rownames(df) %in% out_pnts, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = mean(sqrt(abs(rstandard(slr_outlier)))), col = "red", lty = 2) +
    labs(title = "Scale-Location: Outliers",
         x = "Fitted Values",
         y = "sqrt(abs(Standardized Residuals)))"))

### 4. Check for independence of random error ###
# scatter plot of residuals sorted
(p4_outlier <- ggplot(df2, aes(x = 1:nrow(df2), y = .resid)) +
    geom_point(col = ifelse(rownames(df2) %in% outlier_xorder, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = 0, col = "red", lty = 2) +
    labs(title = "Independence Assumption: Outliers",
         x = "Row Numbers",
         y = "Residuals"))

### ### ### ### ### ##
### Leverage Point ###
### ### ### ### ### ##
leverage_x <- 12
leverage_y <- 60

x_leverage <- c(x, leverage_x)
y_leverage <- c(y, leverage_y)

slr_leverage <- lm(y_leverage ~ x_leverage)
summary(slr_leverage)

response <- y_leverage
fit <- fitted(slr_leverage)

mae_leverage <- yardstick::mae_vec(response, fit)

# Detecting leverage point
(p1 <- p_original +
    xlim(0, 12) + 
    ylim(-0.1, 60) +
    labs(title = "Identifying Leverage Points"))

p2 <- p1 +
    geom_point(aes(leverage_x, leverage_y), size = 2.5, color = "red")

p_leverage <- p2 +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5,
              data = data.frame(x = x_leverage, y = y_leverage)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~~~")),
               data = data.frame(x = x_leverage, y = y_leverage), vjust = 1.1,
               label.x.npc = "left", label.y.npc = "top",
               color = "red", formula = y ~ x) +
  geom_text(aes(label = paste0("MAE = ", signif(mae_leverage, 4))),
            x = -Inf, y = Inf, hjust = 0, vjust = 5, size = 4.5)

### Tests for leverage points ###
## ** Take note: This is how you do the 6 tests below individually ** ## 
# Calculate the hat values
hv <- hatvalues(slr_leverage)
df <- data.frame(row_num = c(1:length(hv)),
                 hv = hv)

n <- nrow(df)
k <- length(slr_leverage$coefficients) - 1  # k usually refers to number of regressors

fence <- (2*(k+1)/n) #specific to hat values

(leverage <- which(hv > fence))

# Create the plot
ggplot(df, aes(x = row_num, y = hv)) +
  geom_point(color = ifelse(df$row_num %in% leverage, "red", "black")) +
  geom_hline(yintercept = fence, linetype = 2, color = "gray50") +
  labs(title = "Identification of high leverage points",
       x = "Row number",
       y = "Hat values")

# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr_leverage)

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
(p1_leverage <- ggplot(df, aes(x = .fitted, y = .resid)) +
   geom_point(col = ifelse(rownames(df) %in% leverage, "red", "black")) +
   geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
   geom_hline(yintercept = 0, col = "red", lty = 2) +
   labs(title = "Linearity Assumption: Leverages",
        x = "Fitted Values",
        y = "Residuals"))

##### 2. Check for normality of random error #####
# Calculate the order of the residuals in the sorted list
residual_order <- order(slr_leverage$residuals)

# Use the order to match the leverage points with the correct indices in the sorted list
# Transform to along dataframe to plot with ggplot
regress.vars <- names(coef(slr_leverage)[-1])

df2 <- df[c(regress.vars, ".resid")] %>% 
  tidyr::pivot_longer(cols = regress.vars,
                      names_to = "statistic",
                      values_to = "xvalue") %>%
  arrange(-xvalue)

leverage_xorder <- match(df$.resid[leverage], df2$.resid)
resid_leverage <- match(leverage_xorder, order(df2$.resid))

(p2_leverage <- ggplot(df, aes(sample = .resid)) +
   stat_qq(col = ifelse(rownames(df) %in% resid_leverage, "red", "black")) +
   stat_qq_line(color = 'red', linetype = 2) +
   ggtitle("Residual Q-Q Plot: Leverages") +
   xlab("Theoretical Quantiles") +
   ylab("Sample Quantiles"))

### 3. Check for zero mean and constant variance of random error ###
(p3_leverage <- ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr_leverage))))) +
    geom_point(col = ifelse(rownames(df) %in% leverage, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = mean(sqrt(abs(rstandard(slr_leverage)))), col = "red", lty = 2) +
    labs(title = "Scale-Location: Leverages",
         x = "Fitted Values",
         y = "sqrt(abs(Standardized Residuals)))"))

### 4. Check for independence of random error ###
# scatter plot of residuals sorted
(p4_leverage <- ggplot(df2, aes(x = 1:nrow(df2), y = .resid)) +
    geom_point(col = ifelse(rownames(df2) %in% leverage_xorder, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = 0, col = "red", lty = 2) +
    labs(title = "Independence Assumption: Leverages",
         x = "Row Numbers",
         y = "Residuals"))

### ### ### ### ### ### ##
### Influential Points ###
### ### ### ### ### ### ##
influential_x <- 11
influential_y <- 10

x_influential <- c(x, influential_x)
y_influential <- c(y, influential_y)

slr_influential <- lm(y_influential ~ x_influential)
summary(slr_influential)

response <- y_influential
fit <- fitted(slr_influential)

mae_influential <- yardstick::mae_vec(response, fit)

# Detecting influential point
(p1 <- p_original +
    xlim(0, 11) +
    labs(title = "Identifying Influential Points"))

(p2 <- p1 +
    geom_point(aes(influential_x, influential_y), size = 2.5, color = "red"))

(p_influential <- p2 +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5,
              data = data.frame(x = x_influential, y = y_influential)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~~~~")),
               data = data.frame(x = x_influential, y = y_influential), vjust = 1.1,
               label.x.npc = "left", label.y.npc = "top",
               color = "red", formula = y ~ x) +
  geom_text(aes(label = paste0("MAE = ", signif(mae_influential, 4))),
            x = -Inf, y = Inf, hjust = 0, vjust = 5, size = 4.5))

# Find Influential Points
(infl_plot <- car::influencePlot(slr_influential))#you dont want to see blue and big circle
infl_points <- as.integer(rownames(infl_plot)) #preferred method 
infl_points <- 21 # Above is more general so no need to use this line

# broom's augment() builds a dataframe convenient for ggplot
df <- broom::augment(slr_influential)

##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
(p1_influential <- ggplot(df, aes(x = .fitted, y = .resid)) +
   geom_point(col = ifelse(rownames(df) %in% infl_points, "red", "black")) +
   geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
   geom_hline(yintercept = 0, col = "red", lty = 2) +
   labs(title = "Linearity Assumption: Influentials",
        x = "Fitted Values",
        y = "Residuals"))

##### 2. Check for normality of random error #####
# Transform to along dataframe to plot with ggplot
regress.vars <- names(coef(slr_influential)[-1])

df2 <- df[c(regress.vars, ".resid")] %>% 
  tidyr::pivot_longer(cols = regress.vars,
                      names_to = "statistic",
                      values_to = "xvalue") %>%
  arrange(-xvalue)

influential_xorder <- match(df$.resid[infl_points], df2$.resid)
resid_influential <- match(influential_xorder, order(df2$.resid))

(p2_influential <- ggplot(df, aes(sample = .resid)) +
   stat_qq(col = ifelse(rownames(df) %in% resid_influential, "red", "black")) +
   stat_qq_line(color = 'red', linetype = 2) +
   ggtitle("Residual Q-Q Plot: Influentials") +
   xlab("Theoretical Quantiles") +
   ylab("Sample Quantiles"))

### 3. Check for zero mean and constant variance of random error ###
(p3_influential <- ggplot(df, aes(x = .fitted, y = sqrt(abs(rstandard(slr_influential))))) +
    geom_point(col = ifelse(rownames(df) %in% infl_points, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = mean(sqrt(abs(rstandard(slr_influential)))), col = "red", lty = 2) +
    labs(title = "Scale-Location: Influentials",
         x = "Fitted Values",
         y = "sqrt(abs(Standardized Residuals)))"))

### 4. Check for independence of random error ###
# scatter plot of residuals sorted
(p4_influential <- ggplot(df2, aes(x = 1:nrow(df2), y = .resid)) +
    geom_point(col = ifelse(rownames(df2) %in% leverage_xorder, "red", "black")) +
    geom_smooth(span = 1, se = F, color = "gray66", lty = 2) +
    geom_hline(yintercept = 0, col = "red", lty = 2) +
    labs(title = "Independence Assumption: Influentials",
         x = "Row Numbers",
         y = "Residuals"))

### ### ### ### ### ### ### ###
## Comparing different Tests ##
### ### ### ### ### ### ### ###
### Hat Values ###
## Calculate the hat values
hv_outlier <- hatvalues(slr_outlier)
hv_leverage <- hatvalues(slr_leverage)
hv_influential <- hatvalues(slr_influential)


df <- data.frame(row_num = c(1:length(hv_outlier)),
                 hv_outlier, hv_leverage, hv_influential)

n <- nrow(df)
k <- length(slr_leverage$coefficients) - 1

fence <- (2*(k+1)/n)

(lev_outlier <- which(hv_outlier > fence))
(lev_leverage <- which(hv_leverage > fence))
(lev_influential <- which(hv_influential > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "hatvals")

# Create the plot for hat values
ggplot(df3, aes(x = row_num, y = hatvals, color = type, shape = type)) +
  geom_point(size = ifelse(df3$type %in% c("hv_outlier", "hv_leverage", "hv_influential") &
                             df3$hatvals > fence, 3, 1.5)) +
  geom_hline(yintercept = c(fence), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (Hat Values)",
       x = "Row number", y = "Hat Values")

### RStandard ###
## Calculate the rstandard
rsta_outlier <- rstandard(slr_outlier)
rsta_leverage <- rstandard(slr_leverage)
rsta_influential <- rstandard(slr_influential)

df <- data.frame(row_num = c(1:length(rsta_outlier)),
                 rsta_outlier, rsta_leverage, rsta_influential)

n <- nrow(df)
k <- length(slr_leverage$coefficients) - 1

fence <- 3

(rstand_outlier <- which(abs(rsta_outlier) > fence))
(rstand_leverage <- which(abs(rsta_leverage) > fence))
(rstand_influential <- which(abs(rsta_influential) > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "rstandard")

# Create the plot for hat values
ggplot(df3, aes(x = row_num, y = rstandard, color = type, shape = type)) +
  geom_point(size = ifelse(df3$type %in% c("rsta_outlier", "rsta_leverage", "rsta_influential") &
                             abs(df3$rstandard) > fence, 3, 1.5)) +
  geom_hline(yintercept = c(fence, -fence), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (RStandard)",
       x = "Row number", y = "RStandard")

### RStudent ###
## Calculate the rstudent
rstu_outlier <- rstudent(slr_outlier)
rstu_leverage <- rstudent(slr_leverage)
rstu_influential <- rstudent(slr_influential)

df <- data.frame(row_num = c(1:length(rstu_outlier)),
                 rstu_outlier, rstu_leverage, rstu_influential)

n <- nrow(df)
k <- length(slr_leverage$coefficients) - 1

fence <- 3

(rstudent_outlier <- which(abs(rstu_outlier) > fence))
(rstudent_leverage <- which(abs(rstu_leverage) > fence))
(rstudent_influential <- which(abs(rstu_influential) > fence))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "rstudent")

# Create the plot for hat values
ggplot(df3, aes(x = row_num, y = rstudent, color = type, shape = type)) +
  geom_point(size = ifelse(df3$type %in% c("rstu_outlier", "rstu_leverage", "rstu_influential") &
                             abs(df3$rstudent) > fence, 3, 1.5)) +
  geom_hline(yintercept = c(fence, -fence), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (RStudent)",
       x = "Row number", y = "Rstudent")

### Difference in Fits ### 
# influential if abs(dffits) > 2*sqrt((k+2)/(n-k-2))
d_outlier <- dffits(slr_outlier)
d_leverage <- dffits(slr_leverage)
d_influential <- dffits(slr_influential)

df <- data.frame(row_num = c(1:length(d_influential)),
                 d_outlier, d_leverage, d_influential)

n <- nrow(df)
k <- length(slr_influential$coefficients) - 1

fences <- 2*sqrt((k+2)/(n-k-2))

(idx_outlier <- which(abs(d_outlier) > fences))
(idx_leverage <- which(abs(d_leverage) > fences))
(idx_influential <- which(abs(d_influential) > fences))

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "dffits")

# Create the plot for dffits
ggplot(df3, aes(x = row_num, y = dffits, color = type, shape = type)) +
  geom_point(size = ifelse((df3$type %in% c("d_outlier", "d_leverage", "d_influential")) &
                             (df3$dffits > fences | df3$dffits < -fences), 3, 1.5)) +
  geom_hline(yintercept = c(fences, -fences), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (dffits)",
       x = "Row number", y = "Difference in Fits")

### Cook's Distance ### 
## Calculate Cook's distance and identify influential points
cd_outlier <- cooks.distance(slr_outlier) 
cd_leverage <- cooks.distance(slr_leverage) 
cd_influential <- cooks.distance(slr_influential) 


cutoff <- 4/(nrow(df)-length(slr_influential$coefficients)-2) #alternative,more sensitive,avoid
cutoff <- 0.5

(cooks_outlier <- which(cd_outlier > cutoff))
(cooks_leverage <- which(cd_leverage > cutoff))
(cooks_influential <- which(cd_influential > cutoff))

# Create a data frame with Cook's distance and row numbers
df <- data.frame(row_num = c(1:length(cd_influential)),
                 cd_outlier, cd_leverage, cd_influential)

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "cooks")

# Create the plot for Cook's distance
ggplot(df3, aes(x = row_num, y = cooks, color = type, shape = type)) +
  geom_point(size = ifelse(df3$type %in% c("cd_outlier", "cd_leverage", "cd_influential") &
                             df3$cooks > cutoff, 3, 1.5)) +
  geom_hline(yintercept = cutoff, linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (Cook's distance)",
       x = "Row number", y = "Cook's distance")

### CovRatio ### 
## Identify influential points based on covratio
cv_outlier <- covratio(slr_outlier) 
cv_leverage <- covratio(slr_leverage) 
cv_influential <- covratio(slr_influential)

fence_high <- (1 + 3*(k+1)/n)
fence_low <- (1 - 3*(k+1)/n)

(covratio_outlier <- c(which(cv_outlier > fence_high),
                       which(cv_outlier < fence_low)))
(covratio_leverage <- c(which(cv_leverage > fence_high),
                        which(cv_leverage < fence_low)))
(covratio_influential <- c(which(cv_influential > fence_high),
                           which(cv_influential < fence_low)))

# Create a data frame with covratio and row numbers
df <- data.frame(row_num = c(1:length(cv_influential)),
                 cv_outlier, cv_leverage, cv_influential)

df3 <- df %>% 
  tidyr::pivot_longer(cols = -row_num,
                      names_to = "type",
                      values_to = "covratio")

# Create the plot for covratio
ggplot(df3, aes(x = row_num, y = covratio, color = type, shape = type)) +
  geom_point(size = ifelse((df3$type %in% c("cv_outlier", "cv_leverage", "cv_influential")) &
                             (df3$covratio > fence_high | df3$covratio < fence_low), 3, 1.5)) +
  geom_hline(yintercept = c(fence_high, fence_low), linetype = 2, color = "gray50") +
  theme(legend.position = "bottom") +
  labs(title = "Influential Points (covratio)",
       x = "Row number", y = "Covratio")

### ### ### ### ### ### ##
### Side-by-Side Plots ###
### ### ### ### ### ### ##
##### Linear Model Assumptions #####
### 1. Check for linearity between different variables ###
cowplot::plot_grid(p1_original, p1_outlier, p1_leverage, p1_influential)

### 2. Check for normality of random error ###
cowplot::plot_grid(p2_original, p2_outlier, p2_leverage, p2_influential)

### 3. Check for zero mean and constant variance of random error ###
cowplot::plot_grid(p3_original, p3_outlier, p3_leverage, p3_influential)

### 4. Check for independence of random error ###
cowplot::plot_grid(p4_original, p4_outlier, p4_leverage, p4_influential)

# Fits
cowplot::plot_grid(p_original_cow, p_outier, p_leverage, p_influential)

