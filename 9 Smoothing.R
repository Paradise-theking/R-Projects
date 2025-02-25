rm(list = ls())

##################
### Smoothing ###
#################
library(LearnEDAfunctions)
head(braves.attendance, 10)

# Plot
with(braves.attendance, plot(Game, Attendance))

# Smooth by hand
Smooth3 <- c(NA, 320, 332, 341, 341, 331, 331, 331, 288, NA)
cbind(braves.attendance[1:10, ], Smooth3)

# Smoothing 3R function
smooth.3R <- smooth(braves.attendance$Attendance, kind="3R")
smooth.3R[1:12]

with(braves.attendance,
     plot(Game, smooth.3R,type="l",col="red",lwd=2,
          xlab="GAME NUMBER",ylab="ATTENDANCE",
          main="3R SMOOTH", ylim=c(250,500)))

# Smoothing 3R Splitting Twice (3RSS)
smooth.3RSS <- smooth(braves.attendance$Attendance,
                      kind="3RSS")
with(braves.attendance,
     plot(Game, smooth.3R,type="l",col="blue",lwd=2,
          xlab="GAME NUMBER",ylab="ATTENDANCE",
          main="3RSS Smooth",
          ylim=c(250,500)))

# Smoothing Hanning 3RSS
smooth.3RSSH <- han(smooth.3RSS)
with(braves.attendance,
     plot(Game, smooth.3RSSH, type="l",col="green",lwd=2,
          xlab="GAME NUMBER", ylab="ATTENDANCE",
          main="3RSSH SMOOTH", ylim=c(250,500)))

# Rerough the Hanning (3RS3R) Twice
smooth.3RS3R.twice <- smooth(braves.attendance$Attendance,
                             kind="3RS3R", twiceit=TRUE)
with(braves.attendance,
     plot(Game, smooth.3RS3R.twice,
          col="purple", lwd=2, type="l",
          main="3RSSR, Twice Smooth"))

# Plotting all together
with(braves.attendance,
     plot(Game, Attendance, cex = 0.4, type = "b",
          xlab="GAME NUMBER", ylab="ATTENDANCE",
          main="3R, 3RSS, 3RSSH, & 3RSSR Twice Smooths",
          ylim=c(200,500)))


with(braves.attendance,
     lines(Game, smooth.3R, col="blue", lwd=2))
with(braves.attendance,
     lines(Game, smooth.3RSS, col="red", lwd=2))
with(braves.attendance,
     lines(Game, smooth.3RSSH, col="green", lwd=2))
with(braves.attendance,
     lines(Game, smooth.3RS3R.twice, col="purple", lwd=2))
legend("topleft",
       legend=c("3R", "3RSS", "3RSSH", "3RSSR 2*Smooth"),
       cex = 0.9, lty=1, col=c("blue", "red", "green", "purple"))

# In-Process Residual Analysis
Rough <- with(braves.attendance,
              Attendance - smooth.3RSSH)
with(braves.attendance,
     head(cbind(Attendance, smooth.3RSSH, Rough)))
plot(braves.attendance$Game, Rough,
     pch=19, cex=1, xlab = "GAME NUMBER", ylab = "ROUGH")
abline(h=0, lwd=3, col = "green")

# Final Residual Analysis
FinalRough <- braves.attendance$Attendance - smooth.3RS3R.twice
plot(braves.attendance$Game, FinalRough,
     pch = 19, xlab = "GAME NUMBER", ylab = "ROUGH")
abline(h=0, lwd=3, col = "purple")

####### EXAMPLE #########
#### batting.history ####
head(batting.history, 10)
min(batting.history$Year)

# (batting.history <- batting.history[order(batting.history$Year, decreasing = F), ])
# batting.history$tser <- seq(1, nrow(batting.history))

plot_against <- function(data, x) {
  # Get list of column names, excluding the x variable
  colnames <- names(data)
  colnames <- colnames[colnames != x]
  
  # Loop through the column names and create a scatter plot
  for (col in colnames) {
    plot(data[[x]], data[[col]], main=paste(col, "vs.", x), xlab=x, ylab=col)
  }
}

plot_against(pitching.history, "Year")

# Smoothing 3R function
smooth.3R <- smooth(batting.history$SLG, kind="3R")

with(batting.history,
     plot(Year, SLG, type = "b", cex = 0.4, xlab="Year",ylab="SLG",
          main="3R SMOOTH"))
with(batting.history,
     lines(Year, smooth.3R, col="red", lwd=2))

# Smoothing 3R Splitting Twice (3RSS)
smooth.3RSS <- smooth(batting.history$SLG,
                      kind="3RSS")
with(batting.history,
     plot(Year, smooth.3R,type="l",col="blue",lwd=2,
          xlab="Year",ylab="SLG",
          main="3RSS Smooth"))

# Smoothing Hanning 3RSS
smooth.3RSSH <- han(smooth.3RSS)
with(batting.history,
     plot(Year, smooth.3RSSH, type="l",col="green",lwd=2,
          xlab="Year", ylab="SLG"))

# Rerough the Hanning (3RS3R) Twice
smooth.3RS3R.twice <- smooth(batting.history$SLG,
                             kind="3RS3R", twiceit=TRUE)
with(batting.history,
     plot(Year, smooth.3RS3R.twice,
          col="purple", lwd=2, type="l",
          main="3RSSR, Twice Smooth"))

# Plotting all together
with(batting.history,
     plot(Year, SLG, cex = 0.4, type = "b",
          xlab="Year", ylab="SLG",
          main="3R, 3RSS, 3RSSH, & 3RSSR Twice Smooths"))
with(batting.history,
     lines(Year, smooth.3R, col="blue", lwd=2))
with(batting.history,
     lines(Year, smooth.3RSS, col="red", lwd=2))
with(batting.history,
     lines(Year, smooth.3RSSH, col="green", lwd=2))
with(batting.history,
     lines(Year, smooth.3RS3R.twice, col="purple", lwd=2))
legend("topleft",
       legend=c("3R", "3RSS", "3RSSH", "3RSSR 2*Smooth"),
       cex = 0.9, lty=1, col=c("blue", "red", "green", "purple"))

# Final Residual Analysis
FinalRough <- batting.history$SLG - smooth.3RS3R.twice
plot(batting.history$Year, FinalRough,
     pch = 19, xlab = "Year", ylab = "ROUGH")
abline(h=0, lwd=3, col = "purple")

