## This script read the input "activity.csv" csv file into dataframe Activ
## Both script and input should be sotred in the working directory

file_Activity <- "activity.csv"
if(!file.exists(file_Activity)){
    error(paste("Missing file",file_Activity))
}
message(paste("Reading file",file_Activity))
Activ <- read.csv(file_Activity)

library(lubridate)
## Edit the entries
## First convert the date comlumn into date object
## Note: this entry is in the default ISO 8601 format (%Y-%m-%d")
## so no need to specify one
Activ$date <- as.Date(Activ$date)
## Convert interval form integers to character (with 0 fill)
Activ$interval <- formatC(Activ$interval,format="d",width=4,flag="0")
## Add ':' in the middle so that it is usable for lubridate
Activ$interval <- paste(
    substring(Activ$interval,1,2),
    substring(Activ$interval,3,4),
    sep=":"
)
## Convert interval into time
Activ$time <- hm(Activ$interval)
## Add weekday factor
Activ$wday <- as.factor(weekdays(Activ$date))
## Add formated date without year
Activ$md   <- as.character(Activ$date,"%m/%d")