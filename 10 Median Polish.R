rm(list = ls())

library(LearnEDAfunctions)
temperatures

# Make City names into the index 
(temps <- data.frame(temperatures[,-1], row.names = temperatures[,1]))

# Row medians
REFF <- apply(temps, 1, median) # 1 for rows
cbind(temps, REFF)

# subtract REFF across rows
Residual <- sweep(temps, 1, REFF)
(RowSweep <- cbind(Residual, REFF))

# Column medians

CEFF <- apply(RowSweep, 2, median) # 2 for columns
rbind(RowSweep, CEFF = CEFF)

# subtract CEFF across rows
Residual <- sweep(RowSweep, 2, CEFF)
(ColSweep <- rbind(Residual, CEFF = CEFF))

# Row medians 2nd time
Resid <- ColSweep[, -5]
REFF <- ColSweep[, 5]
Rmed <- apply(Resid, 1, median)
cbind(Resid, Rmed, REFF)

# Updating REFF
REFF <- REFF + Rmed
Resid <- sweep(Resid, 1, Rmed)
(RowSweep <- cbind(Resid, REFF))

# Column medians 2nd time

Resid <- RowSweep[-6, ]
CEFF <- RowSweep[6, ]
ceff <- apply(Resid, 2, median)
rbind(Resid, ceff, CEFF)

# Updating CEFF
CEFF <- CEFF + ceff
Resid <- sweep(Resid, 2, ceff)
(ColSweep <- rbind(Resid, CEFF))

# medpolish, above process iterated up to 10 times
(additive.fit <- medpolish(temps))

# Interpret residuals
additive.fit$residuals


