## Launching reading routine if data is not loaded
if(!exists("Activ")){
    source("readActivity.R")
}

message("Plotting mean evolution of nb steps over days")

#library(lubridate)

stepbyt <- data.frame(
    mean = tapply(Activ$step,as.factor(Activ$interval),mean,na.rm=TRUE),
    sd   = tapply(Activ$step,as.factor(Activ$interval),sd,na.rm=TRUE)
)
stepbyt$upsh  <- stepbyt$mean+stepbyt$sd
stepbyt$lowsh <- stepbyt$mean-stepbyt$sd
stepbyt       <- rbind(stepbyt,"24:00"=stepbyt[1,])
stepbyt$tday  <- 1:nrow(stepbyt)

print(head(stepbyt,n = 30))
print(tail(stepbyt,n = 30))

fhour   <- as.factor(substring(as.character.POSIXt(Activ$interval),1,2)) 
stepbyh <- data.frame(
    mean = tapply(Activ$step,fhour,mean,na.rm=TRUE),
    sd   = tapply(Activ$step,fhour,sd,na.rm=TRUE)
)
stepbyh$tday <- grep("30$",row.names(stepbyt))
print(head(stepbyh,n =24))

## Load plotting library 
library(Hmisc)
with(stepbyh,
     errbar(tday,mean,mean+sd,mean-sd,
            xaxt="n",
            xlab= "time",
            ylab= "<nb of steps>",
            xlim=c(1,nrow(stepbyt)),
            ylim=c(0.,500.),
            col="#0000c0f0",
            errbar.col="#0000c0f0",
            lwd=2
     )
)
#par(new=T)
with(stepbyt,lines(tday,mean,lwd=1.5))
## Add saded region 
with(stepbyt,
     polygon(c(tday, rev(tday)), c(upsh, rev(lowsh)),
             col = "#90909040", border = NA)
)
## reset the axis
axis(1,
     at=grep(":00",row.names(stepbyt)),
     label=grep(":00",row.names(stepbyt),value=TRUE)
)
title(main= "Number of steps accross all days",
    sub = "Data averaged from October to November 2012"
)
