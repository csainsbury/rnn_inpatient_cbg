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

# take off last day for calculation of y
# scale admissions to n points



