## Launching reading routine if data is not loaded
if(!exists("Activ")){
    source("readActivity.R")
}

message("Plotting mean evolution of nb steps over days, weekdays vs weekends")

## Split data between weekend and weekdays
Activ$period <- "weekday"
Activ$period[grep("^S",Activ$wday)] <- "weekend"
Activ$period <- as.factor(Activ$period)
Activsplit <- split(Activ,Activ$period)

## Load plotting library 
library(Hmisc)
par(mfrow=c(2,1))

for(period in names(Activsplit)){
    Activloc <- Activsplit[[period]]
    ## This is the same as graphActivitPattern.R
    stepbyt <- data.frame(
        mean = tapply(Activloc$step,as.factor(Activloc$interval),mean,na.rm=TRUE),
        sd   = tapply(Activloc$step,as.factor(Activloc$interval),sd,na.rm=TRUE)
    )
    stepbyt$upsh  <- stepbyt$mean+stepbyt$sd
    stepbyt$lowsh <- stepbyt$mean-stepbyt$sd
    stepbyt       <- rbind(stepbyt,"24:00"=stepbyt[1,])
    stepbyt$tday  <- 1:nrow(stepbyt)
    
    print(head(stepbyt,n = 30))
    print(tail(stepbyt,n = 30))
    
    fhour   <- as.factor(substring(as.character.POSIXt(Activloc$interval),1,2)) 
    stepbyh <- data.frame(
        mean = tapply(Activloc$step,fhour,mean,na.rm=TRUE),
        sd   = tapply(Activloc$step,fhour,sd,na.rm=TRUE)
    )
    stepbyh$tday <- grep("30$",row.names(stepbyt))
    print(head(stepbyh,n =24)) 
    ## Change the borders to remove space between pannels
    if(period == names(Activsplit)[1]){par(mar=c(0,4,4,2))}
    if(period == names(Activsplit)[2]){par(mar=c(3,4,0,2))}
    with(stepbyh,
         errbar(tday,mean,mean+sd,mean-sd,
                xaxt="n",
                xlab = "time", ## lable doesn't display, need to fix that
                ylab= "<nb of steps>",
                xlim=c(1,nrow(stepbyt)),
                ylim=c(0.,550.),
                col="#0000c0f0",
                errbar.col="#0000c0f0",
                lwd=2
         )
    )
    ## label the pannel
    text(median(stepbyh$tday),500,period)
    with(stepbyt,lines(tday,mean,lwd=1.5))
    ## Add saded region 
    with(stepbyt,
         polygon(c(tday, rev(tday)), c(upsh, rev(lowsh)),
                 col = "#90909040", border = NA)
    )
    ## set the axis top tick mark, always displayed woithout labels
    axis(3,
         at=grep(":00",row.names(stepbyt)),
         labels=FALSE
    )
    if(period == names(Activsplit)[1]){
        ## top pannel, do not show labels at the bottom
        ## Add a title
        axis(1,
             at=grep(":00",row.names(stepbyt)),
             labels=FALSE
        )
        title(main="Number of steps accross all days")
    }
    if(period == names(Activsplit)[2]){
        ## Create axis with the labels the way we like
        axis(1,
             at=grep(":00",row.names(stepbyt)),
             label=grep(":00",row.names(stepbyt),value=TRUE)
        )
    }
}

