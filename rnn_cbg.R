library(data.table)


returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}


## DM
summaryOutputName <- paste("~/R/GlCoSy/source/DTwithPerIDdata.csv",sep="")
DT<-read.csv(summaryOutputName, header=TRUE , sep="," , row.names=NULL)
DT<-data.table(DT)

# diagnosisDataset<-read.csv("../GlCoSy/SDsource/diagnosisDateDeathDate.txt")
diagnosisDataset<-read.csv("~/R/GlCoSy/SDsource/demogALL.txt", quote = "", 
                           row.names = NULL, 
                           stringsAsFactors = FALSE)
diagnosisDataset$deathDateUnix <- returnUnixDateTime(diagnosisDataset$DeathDate)
deathFrame <- data.frame(diagnosisDataset$PatId, diagnosisDataset$deathDateUnix)
colnames(deathFrame) <- c("ID", "deathDateUnix")

deathFrame$deathDateUnix[is.na(deathFrame$deathDateUnix)] <- 0
deathFrame$isDead <- ifelse(deathFrame$deathDateUnix > 0, 1, 0)
deathFrame$ID<-as.numeric(levels(deathFrame$ID))[deathFrame$ID]; deathFrame$ID <- round(deathFrame$ID, 0)
deathFrame$ID[is.na(deathFrame$ID)] <- 0

deathFrameDT <- data.table(deathFrame)
# firstRowAdmissions<-DT[CBGinSequencePerAdmission==1]

# cut to admissions with ==20 CBG values
# cut_DT <- DT[admissionDurationDays >= 5 & admissionDurationDays < 30 & nCBGperAdmission > 20]

      ## need to be able to take all admissions greater than n days and look at hypo probability on day n
      dayN_ofInterest = 5
      dayN_ofInterestSeconds <- dayN_ofInterest * (60*60*24)
      
      # limit dataset to admissions that will have the required data ie at least 5 days of data
      cut_DT <- DT[admissionDurationDays > dayN_ofInterest]
      
      # flag values within the window of interest - here 5 days (dayN_ofInterest ==  5)
      cut_DT[, c("flagWithinNdays") := (ifelse(dateplustime1 < (min(dateplustime1) + dayN_ofInterestSeconds), 1, 0)) , by=.(ID, admissionNumberFlag)]
      
      # function and execution to ensure that there are more than 1 CBG values in the runin period (ie in days 0-4) - to make sure the code won't crash due to not being able to allocate a scaled time point.
      numberOfCBGsBeforeDayN <- function(dateplustime1) {
         x <- dateplustime1[dateplustime1 < (min(dateplustime1) + (dayN_ofInterestSeconds - (60*60*24)))]
         return(length(x))
      }
      
      cut_DT[, c("numberOfCBGsBeforeDayN") := numberOfCBGsBeforeDayN(dateplustime1) , by=.(ID, admissionNumberFlag)]
      
      # cut the data to values within 5 days, and with enough runin CBGs
      cut_DT <- cut_DT[flagWithinNdays == 1 & numberOfCBGsBeforeDayN > 1]


# optional cut for type of DM
# cut_DT <- cut_DT[DiabetesMellitusType_Mapped == "Type 1 Diabetes Mellitus"]

# take off last time period for calculation of y
  timePeriodDays <- 1 # time of interest is last 24 hours in dataset - that corresponds to day 5 of admission
  timePeriodSeconds <- timePeriodDays * (60*60*24)
  
  flag_CBGbelow_n_inLastTime <- function(flagWithinLastTime, yyyy, n) {
    
    lastimeValues <- yyyy[flagWithinLastTime == 1]
    returnVals <- ifelse(lastimeValues < n, 1, 0)
    
    flagPositiveResult <- ifelse(sum(returnVals) > 0, 1, 0)
    
    return(rep(flagPositiveResult, length(yyyy)))
  }
  
  isDead_n_years <- function(cut_DT_ID, dateplustime1, n) {
    
    if (nrow(deathFrameDT[ID == cut_DT_ID]) > 0) {
    isDead <- deathFrameDT[ID == cut_DT_ID]$isDead
    deathDate <- deathFrameDT[ID == cut_DT_ID]$deathDateUnix
    
    returnValue <- ifelse(deathDate > 0 & deathDate > (min(dateplustime1) + n * (60*60*24*365.25)), 1, 0)
    
    returnValue <- rep(returnValue, length(dateplustime1))
    }
    
    if (nrow(deathFrameDT[ID == cut_DT_ID]) == 0) { returnValue <- rep(0, length(dateplustime1)) }
    
    return(returnValue)
    
  }
  
  cut_DT[, c("flagWithinLastTime") := (ifelse(dateplustime1 >= (min(dateplustime1) + ((max(dateplustime1) - min(dateplustime1)) - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]
  cut_DT[, c("lessThan4_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 4), by=.(ID, admissionNumberFlag)]
  cut_DT[, c("lessThan3_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 3), by=.(ID, admissionNumberFlag)]
  cut_DT[, c("lessThan2p88_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 2.88), by=.(ID, admissionNumberFlag)]
  
  cut_DT[, c("isDead_1y") := isDead_n_years(ID, dateplustime1, 1), by=.(ID, admissionNumberFlag)]
  
  cut_DT[, c("flagLastCBG") := (ifelse(CBGinSequencePerAdmission == length(yyyy), 1, 0)), by=.(ID, admissionNumberFlag)]
  
  report_y_hypo4 <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan4_withinLastTime)
  colnames(report_y_hypo4) <- c("ID", "hypo_4")
  report_y_hypo3 <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan3_withinLastTime)
  colnames(report_y_hypo3) <- c("ID", "hypo_3")
  
# scale admissions to n points
#  cut_DT[, c("flag_for_processing") := (ifelse(dateplustime1 < (min(dateplustime1) + (admissionDuration[1] - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]
  
  cut_DT_processSegment <- cut_DT[flagWithinLastTime == 0]
  process_DT <- data.table(cut_DT_processSegment$ID, cut_DT_processSegment$admissionNumberFlag, cut_DT_processSegment$dateplustime1, cut_DT_processSegment$yyyy); colnames(process_DT) <- c("ID", "admissionNumberFlag", "dateplustime1", "yyyy")
  
  # remove the unusual admissions where all CBGs occur in last day/time period
  # ensure that at least 5 cbgs to work with
  # process_DT[, c("nrows_post_lasttimeRemoval") := .N , by=.(ID, admissionNumberFlag)]
  # process_DT <- process_DT[nrows_post_lasttimeRemoval > 4]
  
  process_DT[, c("scaled_dateplustime1") := (dateplustime1 - min(dateplustime1)) / (max(dateplustime1) - min(dateplustime1)) , by=.(ID, admissionNumberFlag)]
  
  n_points = 500
  process_X <- data.frame(matrix(nrow = 0, ncol = n_points + 2))
  
  idVector <- unique(process_DT$ID)

  for (i in seq(1, length(idVector), 1)) {
    
    if (i%%100 == 0) {print(i)}
    
    id_sub <- process_DT[ID == idVector[i]]
    
      admissionVector <- as.numeric(levels(as.data.frame(table(id_sub$admissionNumberFlag))$Var1))[as.data.frame(table(id_sub$admissionNumberFlag))$Var1]
      
      for (j in seq(1, length(admissionVector), 1)) {
        
        id_admission_sub <- id_sub[admissionNumberFlag == admissionVector[j]]
        
        output_X <- approx(id_admission_sub$scaled_dateplustime1, id_admission_sub$yyyy, n = n_points)
        
        # first col = ID, second col = admissionFlag, rest is cbg values
        concat_X <- c(idVector[i], admissionVector[j], output_X[[2]])
        
        process_X <- rbind(process_X, concat_X)
        
      }
      
  }
  # plot 1000 points per admission
  # generate for each admission, and write into a row
  # y = approx(x$scaled_dateplustime1, x$yyyy, n = 1000)

  # ensure that report_y in same order as process_X
  id_diff <- process_X[, 1] - report_y_hypo4[, 1]
  ifelse(sum(id_diff) > 0, sum(id_diff), print("id match"))
  
  # save out input data (X) and (y)
  save_X <- process_X[, -1]
  save_X <- save_X[, -1]
  
  save_X <- round(save_X, 2)
  
  save_y_hypo4 <- report_y_hypo4$hypo_4
  save_y_hypo3 <- report_y_hypo3$hypo_3
  
  
  # randomisingSequence <- runif(nrow(report_y_hypo4), 0, 1)
  
  ## writeout files for tensorflow
  # write out sequence for analysis
  write.table(save_X, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/5thday_processX.csv", sep=",", row.names = FALSE)
  
  
  # write out dep variable (y)
  write.table(save_y_hypo4, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/5thday_report_y_hypo4.csv", sep = ",", row.names = FALSE)
  write.table(save_y_hypo3, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/5thday_report_y_hypo3.csv", sep = ",", row.names = FALSE)
  
  
  
  
  
  
  
  
  
  
  
  
