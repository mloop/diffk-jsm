# Author: Matthew Shane Loop

# Purpose: To estimate type 1 error rate of K test under 3 different conditions for JSM abstract

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

# Packages
library(splancs)
library(plyr)

# Import the conditions 
conditions <- read.table("conditions.txt", sep = "\t", header = TRUE)

# Loop through each condition and perform the test on each of the 5000 datasets per condition. Write each iteration output to a file.
for(j in 1:nrow(conditions)){
        data <- read.table(file = paste("datasets/c-", conditions$condition[j], "/", i, ".txt", sep = ""), sep = "\t", header = T)
        data$status<-ifelse(data$case == 1,c("case"),c("control"))
        # Prepare data set for use in the "splancs" package
        cases<-data[which(data$status=="case"),c("x","y")]
        controls<-data[which(data$status=="control"),c("x","y")]
        cases<-data.matrix(cases)
        controls <- data.matrix(controls)
        m<-matrix(c(0, 0, sqrt(conditions$A[j]), sqrt(conditions$A[j])), ncol=2, dimnames = list(NULL, c("min", "max")))
        poly<-bboxx(m)
        
        # Calculate the distances at which to evaluate the test
        max <- sqrt(conditions$A[j])/2
        int <- max/conditions$tests[j]
        h <- seq(0+int, max, int) # Thus, the number of tests actually done is one less than stated in the condition
        
        # Calculate observed difference K functions for a set of distances
        cases.k <- khat(cases,poly,h)
        controls.k <- khat(controls,poly,h)
        diff.k <- cases.k-controls.k
        
        # Perform the random labeling 500 times and create the 2.5 and 97.5 quantile envelopes
        r <- 500
        env <- Kenv.label(cases, controls, poly, nsim = r, s = h, quiet = T)
        reject <- rep(0, times = length(h))
        condition <- rep(conditions$condition[j], times = length(h))
        kappa <- rep(conditions$kappa[j], times = length(h))
        A <- rep(conditions$A[j], times = length(h))
        p <- rep(conditions$p[j], times = length(h))
        tests <- rep(conditions$tests[j], times = length(h))
        iteration <- rep(i, times = length(h))
        range <- seq(1, length(h), 1)
        data <- data.frame(condition, iteration, kappa, A, p, tests, range, h, diff.k, env$q.2.5, env$q.97.5, reject)
        data$reject[which(data$diff.k < data$env.q.2.5|data$diff.k > data$env.q.97.5)] <- 1
        write.table(data, paste("results-", conditions$condition[j], "-", i, ".txt", sep = ""), sep = "\t", row.names = FALSE)

}
sessionInfo()
