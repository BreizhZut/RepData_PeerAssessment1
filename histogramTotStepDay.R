## Launching reading routine if data is not loaded
if(!exists("Activ")){
    source("readActivity.R")
}

message("Computing total number of steps per days")
## Counting the total number of steps per day after removing NAs.
noNA       <- !is.na(Activ$steps)
dayActnoNA <- tapply(Activ$steps[noNA],as.factor(Activ$md[noNA]),sum)
## Mean and standard deviation
meantot    <- mean(dayActnoNA)
sdtot      <- sd(dayActnoNA)
## Median and 68%
tot68      <- quantile(dayActnoNA,probs=c(0.16,0.5,0.84))
## Counting the total number of steps per day
## dates with only NA will give 0
dayAct <- tapply(Activ$steps,as.factor(Activ$md),sum,na.rm=TRUE)

message("Plotting total number of steps per days")
## Plotting the histogram
mybar <- barplot(dayAct,xaxt="n",
                 main = "Total Activity per day in October November 2012",
                 xlab = "date [mm/dd]",
                 ylab = "nb of steps",
                 col = "#0000d0"
)
## set colors here
colmean <- "#008000"
colmed  <- "#a000a0"
## draw mean and standart deviation as horizontal lines
abline(h=meantot,lwd=2,col=colmean)
abline(h=meantot-sdtot,lwd=1.5, lty = "longdash",col=colmean)
abline(h=meantot+sdtot,lwd=1.5, lty = "longdash",col=colmean)
## draw mean and standart deviation as horizontal lines
abline(h=tot68[2],lwd=2,col=colmed)
abline(h=tot68[1],lwd=1.5, lty = "longdash",col=colmed)
abline(h=tot68[3],lwd=1.5, lty = "longdash",col=colmed)
## a legend
legend("topleft",
       bty="n",
       legend=c("mean","standard deviation","median","68%"),
       lty=c(1,5,1,5),
       lwd=c(3,1.5,3,1.5),
       col=c(colmean,colmean,colmed,colmed)
)
## draw and label the axis the way I want, (the barplot was saved as mybar to obtain a matching axis)
axis(1,at=mybar[grep("5$",row.names(dayAct))],labels=grep("5$",row.names(dayAct),value=TRUE))