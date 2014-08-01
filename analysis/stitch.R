# Author: Matthew Shane Loop

# Purpose: Stitch together the results of each iteration

# Packages
library(plyr)

# Import the conditions 
conditions <- read.table("conditions.txt", sep = "\t", header = TRUE)

# Number of replicates
B <- 5000

# Stitch results together into one file
results <- data.frame()
for(j in 1:nrow(conditions)){
  for(i in 1:B){
	data <- read.table(file = paste("results-", conditions$condition[j], "-", i, ".txt", sep = ""), sep = "\t", header = T)
  
results <- rbind.fill(results, data)
}
}
write.table(results, "results.txt", sep = "\t", row.names = FALSE)

# Delete all individual files
for(j in 1:nrow(conditions)){
    for(i in 1:B){
        file.remove(file = paste("results-", conditions$condition[j], "-", i, ".txt", sep = ""))
    }
}
sessionInfo()
