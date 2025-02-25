# Install Packages / Libraries for Introduction
install.packages('knitr', dependencies = TRUE)
install.packages('remotes')

# Call library 'remotes' to be accessible during current R session
library(remotes)
install_github("bayesball/LearnEDAfunctions")

# Call library 'LearnEDAfunctions' and view head of dataset 'immigrants'
library(LearnEDAfunctions)
head(immigrants)

# Function to allow you to copy data.frame to excel
write.excel <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep = "\t" , row.names = row.names, col.names = col.names, ...)
}

write.excel(immigrants)

# Revelation of immigrant count for 2008
stripchart(immigrants$Count.2008,
           method="jitter", pch=1,
           xlab="2008 Immigrant Count")

# Another view of the raw counts as a histogram
hist(immigrants$Count.2008, breaks = 14)

# View top 5 immigrant countries. Do they correspond to the stripchart?
head(immigrants[order(-immigrants[,4]),])

# Reexpression because of a strong right-skew
log.Count <- log10(immigrants$Count.2008)
immigrants$log.Count <- log.Count

stripchart(log.Count,
           method="jitter", pch=1,
           xlab="Log 2008 Immigrant Count")

# Another view of the Log counts as a histogram
hist(log.Count)

# View top 5 immigrant countries. Do they correspond to the stripchart?
head(immigrants[order(-immigrants[,5]),])

# Measures of central tendency on reexpressed values
mean <- mean(log.Count)
median <- median(log.Count)

# Residual of logs
Residual <- log.Count - median

# Residual stripchart and histogram
stripchart(Residual,
           method="jitter", pch=1,
           xlab="Residual")

hist(Residual)

