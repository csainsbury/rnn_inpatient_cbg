library(data.table)

# load data
# CHIsetCombined <- read.csv("../_workingSourceData/CHIsetCombined_09-15a.csv",header=TRUE,row.names=NULL)
CHIsetCombined <- read.csv("~/R/GlCoSy/source/CHIsetCombined_CORE_allAdult_09-16.csv",header=TRUE,row.names=NULL)
interestSet<-CHIsetCombined
interestSet<- interestSet

## DM
summaryOutputName <- paste("~/R/GlCoSy/source/DTwithPerIDdata.csv",sep="")
DT<-read.csv(summaryOutputName, header=TRUE , sep="," , row.names=NULL)
DT<-data.table(DT)
# firstRowAdmissions<-DT[CBGinSequencePerAdmission==1]

# cut to admissions with ==20 CBG values
cut_DT <- DT[admissionDurationDays >= 7 & admissionDurationDays < 100]

# take off last time period for calculation of y
  timePeriodDays <- 1
  timePeriodSeconds <- timePeriodDays * (60*60*24)
  
  cut_DT[, c("flagWithinLastTime") := (ifelse(dateplustime1 > (min(dateplustime1) + (admissionDuration - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]
  
# scale admissions to n points
