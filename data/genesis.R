# Author: Matthew Shane Loop

# Purpose: Generate datasets necessary to study the type 1 error rate of the K test when testing at many ranges

#LOADING ARGUMENTS, SETTING VARIABLES ############
if(TRUE){
    args=(commandArgs(TRUE))
    if(length(args)==0){
        print('no args')
        i=1; full=TRUE
    }else{
        print('eval agrs')
        for(i in 1:length(args)){
            eval(parse(text=args[[i]]))
        }
    }}

simulation <- read.table("conditions.txt", sep = "\t", header = TRUE)

# At the moment, we can only reasonably produce the data for areas the size of AL (the smaller value of variable A). The computation is a burden for producing data for the other 12 conditions.

alabama <- simulation[which(simulation$A == 52419), ]

# Number of datasets per condition
B <- 5000

# setting up preliminaries
set.seed(65324)
library(spatstat)

# each cluster consist of 5 points in a disc of radius sqrt(A)/10 (for AL, the cluster size is about 23 meters)
# this example comes directly from the documentation for the rNeymanScott function
nclust <- function(x0, y0, radius, n) {
return(runifdisc(n, radius, centre=c(x0, y0)))
}

# Generate the data
for(j in 1:nrow(alabama)){
dir.create(c(paste("datasets/c-",alabama$condition[j], sep = "")), recursive = T)
parameters <- alabama[j, ]

# set maximum cluster radius
rmax <- sqrt(parameters$A)/10

# set the area
win <- as.owin(c(0, sqrt(parameters$A), 0, sqrt(parameters$A)))

seeds <- rnorm(B, 89781, 10000)

# Create B datasets

    set.seed(seeds[i])
    data <- rNeymanScott(parameters$kappa, rmax, nclust, win, radius = rmax, n=5) # choice of n needs to be thought about
    case <- rbinom(data[["n"]], 1, parameters$p)
    data <- data.frame(data, case)
    write.table(data, paste("datasets/c-", parameters$condition, "/", i, ".txt", sep = ""), sep = "\t", row.names = F)
}

sessionInfo()