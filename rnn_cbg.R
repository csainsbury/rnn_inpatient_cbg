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
cut_DT <- DT[admissionDurationDays >= 50 & admissionDurationDays < 100]

# take off last time period for calculation of y
  timePeriodDays <- 1
  timePeriodSeconds <- timePeriodDays * (60*60*24)
  
  cut_DT[, c("flagWithinLastTime") := (ifelse(dateplustime1 >= (min(dateplustime1) + (admissionDuration[1] - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]
  cut_DT[, c("lessThan4_withinLastTime") := (ifelse(flagWithinLastTime == 1 & min(yyyy)<4, 1, 0)), by=.(ID, admissionNumberFlag)]
  cut_DT[, c("lessThan3_withinLastTime") := (ifelse(flagWithinLastTime == 1 & min(yyyy)<3, 1, 0)), by=.(ID, admissionNumberFlag)]
  
  cut_DT[, c("flagLastCBG") := (ifelse(CBGinSequencePerAdmission == max(CBGinSequencePerAdmission), 1, 0)), by=.(ID, admissionNumberFlag)]
  
  
  report_y <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan4_withinLastTime)
  colnames(report_y) <- c("ID", "hypo_4")
  
# scale admissions to n points
  cut_DT[, c("flag_for_processing") := (ifelse(dateplustime1 < (min(dateplustime1) + (admissionDuration[1] - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]
  
  process_DT <- data.table(cut_DT$ID, cut_DT$admissionNumberFlag, cut_DT$dateplustime1, cut_DT$yyyy); colnames(process_DT) <- c("ID", "admissionNumberFlag", "dateplustime1", "yyyy")
  process_DT[, c("scaled_dateplustime1") := (dateplustime1 - min(dateplustime1)) / (max(dateplustime1) - min(dateplustime1)) , by=.(ID, admissionNumberFlag)]
  
  n_points = 1000
  process_X <- data.frame(matrix(nrow = 0, ncol = n_points + 2))
  
  idVector <- unique(process_DT$ID)

  for (i in seq(1, length(idList), 1)) {
    
    if (i%%100 == 0) {print(i)}
    
    id_sub <- process_DT[ID == idList[i]]
    
      admissionVector <- as.numeric(levels(as.data.frame(table(id_sub$admissionNumberFlag))$Var1))[as.data.frame(table(id_sub$admissionNumberFlag))$Var1]
      
      for (j in seq(1, length(admissionVector), 1)) {
        
        id_admission_sub <- id_sub[admissionNumberFlag == admissionVector[j]]
        
        output_X <- approx(id_admission_sub$scaled_dateplustime1, id_admission_sub$yyyy, n = n_points)
        
        # first col = ID, second col = admissionFlag, rest is cbg values
        concat_X <- c(idList[i], admissionVector[j], output_X[[2]])
        
        process_X <- rbind(process_X, concat_X)
        
      }
      
  }
  # plot 1000 points per admission
  # generate for each admission, and write into a row
  # y = approx(x$scaled_dateplustime1, x$yyyy, n = 1000)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
