## We systematically reread the data form scratch
source("readActivity.R")

message("Before replacing missing values")
print(summary(Activ$steps))

## First split the data per day
Activday <- split(Activ,Activ$wday)
## loop through each day
## Set a seed
set.seed(42)
for(Activloc in Activday){
    ## Check for NA
    nbNA <- sum(is.na(Activloc$step))
    if(nbNA>0){
        # Compute the 68% confidence region
        l68      <- tapply(Activloc$steps,Activloc$interval,quantile,probs=0.15,na.rm=TRUE)
        h68      <- tapply(Activloc$steps,Activloc$interval,quantile,probs=0.85,na.rm=TRUE)
        # make a list of row with na
        listna   <- row.names(Activloc[is.na(Activloc),])
        # A print to pass the time
        print(paste(
            "Missing day:",
            length(unique(Activloc[listna,"date"])), 
            unique(Activloc$wday)
        ))
        for(i in listna){
            # find the time
            time  <- as.character(Activloc[i,"interval"])
            # generate a random nb of step
            # following the corresponding mean and standard deviation
            step_gen <- round(runif(n=1,min=l68[time],h68[time]))
            # replace the NA with the generated point.
            Activ[i,"steps"] <- step_gen
        }
    }
}
message("After replacing missing values")
print(summary(Activ$steps))