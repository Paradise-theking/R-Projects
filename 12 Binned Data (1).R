#################
### Binning 1 ###
#################
library(LearnEDAfunctions)
head(grandma.19.40)
max(grandma.19.40)

(sort(grandma.19.40$time)[1:10])

(bins <- seq(8000, 23000, 1000))
(bin.mids <- (bins[-1] + bins[-length(bins)]) / 2)

(h <- with(grandma.19.40,
     hist(time, breaks = bins, xlab = "TIME", main = "Histogram")))

 data.frame(Mid=h$mids, Count=h$counts, Roots=sqrt(h$counts))

h$counts <- sqrt(h$counts)
plot(h, xlab = "TIME", ylab = "ROOT FREQUENCY", main = "standardized histogram")

# Gaussian Resistant measures
(fl.u <- fivenum(grandma.19.40$time)[c(2,4)])

g.mean <- sum(fl.u)/2
g.sd <- diff(fl.u)/1.349

# Standard Resistant measures
median <- median(grandma.19.40$time)
df <- IQR(grandma.19.40$time)

s <- fit.gaussian(grandma.19.40$time, bins, median, df)
s <- fit.gaussian(grandma.19.40$time, bins, g.mean, g.sd)

options(digits=3)
(data <- data.frame(Mid=bin.mids, d=s$counts, sqrt.d=sqrt(s$counts),
           Prob=s$probs, e=s$expected, sqrt.e=sqrt(s$expected),
           Residual=s$residual))

plot(h, xlab = "TIME", ylab = "ROOT FREQUENCY", main = "")
lines(bin.mids, sqrt(s$expected))






library(vcd)
rootogram(s$counts, s$expected)
rootogram(s$counts, s$expected, type = 'deviation')

# Double Root Residuals
data$drr <- with(data, sqrt(2 + 4*d) - sqrt(1 + 4*e))

# Alternative especially when bins have small counts
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e)))
with(data, rootogram(sqrt(2 + 4*d), sqrt(1 + 4*e), type = 'deviation'))

##################
### Binning 2 ###
#################
set.seed(1)
my.sim <- function() {
  x <- rnorm(8)
  return(mean(x) / sd(x))
}
R <- replicate(1000, my.sim())

bins <- seq(min(R), max(R), length.out = 15)
N <- length(bins)
bin.midpts <- (bins[1:(N - 1)] + bins[2:N]) / 2

(h <- hist(R, bins, plot = F))
options(digits=2)
(d <- data.frame(BIN.LO=bins[1:(N - 1)],
                BIN.HI=bins[2:N],
                BIN.MID=bin.midpts,
                COUNT=h$counts,
                ROOT=sqrt(h$counts)))

plot(d$BIN.MID, d$ROOT,
     xlab = "MIDPOINTS", ylab = "ROOTS",
     type = "l", lwd = 2)

d$Smooth.Root <- smooth(d$ROOT, twiceit=TRUE)

plot(d$BIN.MID, d$ROOT,
     xlab = "MIDPOINTS", ylab = "ROOTS",
     type = "l", lwd = 2)
lines(d$BIN.MID, d$Smooth.Root, col="red", lwd=1.5)

d$Shift.Sq <- round((bin.midpts - 0) ^ 2, 2)

with(d, plot(Shift.Sq, Smooth.Root, pch = 19))

par(mfrow=c(2, 2))
for (p in seq(0.5,-1,by=-0.5)) {
  x <- d$Shift.Sq
  y <- power.t(d$Smooth.Root, p)
  plot(x, y, main= paste0("Power = ",p), ylab="New Y", pch=19)
  abline(lm(y ~ x,), col="blue")
  qqnorm(y, pch = 19, cex = 0.7, col = 'red')
  qqline(y, lty = 2)
}

for (p in seq(0,-0.75,by=-0.25)) {
  x <- d$Shift.Sq
  y <- power.t(d$Smooth.Root, p)
  plot(x, y, main= paste0("Power = ",p), ylab="New Y", pch=19)
  abline(lm(y ~ x,), col="blue")
  qqnorm(y, pch = 19, cex = 0.7, col = 'red')
  qqline(y, lty = 2)
  print(hinkley(y))
}

par(mfrow=c(1, 2))
# For log transformation
fit.log <- lm(I(log(Smooth.Root)) ~ Shift.Sq, data=d)
b <- coef(fit.log)
Final.Smooth.log <- exp((b[1] + b[2] * d$Shift.Sq)) ^ 2
d$Final.S.Root.log <- sqrt(Final.Smooth.log)

plot(d$BIN.MID, d$ROOT, main = "Log Smooth",
     cex.main = 1, xlab = "MIDPOINTS", ylab = "ROOTS",
     type = "l", lwd = 2)
lines(d$BIN.MID, d$Final.S.Root.log, col = 'red', lwd = 1.5)

# For power transformation
p = -0.25

fit.p <- lm(I(Smooth.Root^p) ~ Shift.Sq, data=d)
b <- coef(fit.p)
Final.Smooth.p <- (b[1] + b[2] * d$Shift.Sq) ^ (2 / p)
d$Final.S.Root.p <- sqrt(Final.Smooth.p)

plot(d$BIN.MID, d$ROOT, main = paste0("Power = ",p, " smooth"),
     cex.main = 1, xlab = "MIDPOINTS", ylab = "ROOTS",
     type = "l", lwd = 2)
lines(d$BIN.MID, d$Final.S.Root.p, col = 'red', lwd = 1.5)

par(mfrow=c(1, 1))
plot(d$BIN.MID, d$ROOT, main = "All Smooths",
     xlab = "MIDPOINTS", ylab = "ROOTS",
     type = "l", lwd = 2)
lines(d$BIN.MID, d$Final.S.Root.log, col = 2, lwd = 1.5)
lines(d$BIN.MID, d$Final.S.Root.p, col = 3, lwd = 1.5)
lines(d$BIN.MID, d$Smooth.Root, col= 4, lwd=1.5)
legend('topright', col = c(2:4), lty = 1, cex = 1,
       legend = c("Log Smooth", "Power Smooth", "3RSSH, Twice"))

d$Residual = d$ROOT - d$Final.S.Root.log
plot(d$BIN.MID, d$Residual)
abline(h=0, col = 'red', cex = 1.5)

