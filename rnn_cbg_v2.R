library(data.table)


returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}


## DM
summaryOutputName <- paste("~/R/GlCoSy/source/DTwithPerIDdata.csv",sep="")
DT<-read.csv(summaryOutputName, header=TRUE , sep="," , row.names=NULL)
DT<-data.table(DT)

#list primes - to generate lookup table
prime = function(n) {
  n = as.integer(n)
  if(n > 1e8) stop("n too large")
  primes = rep(TRUE, n)
  primes[1] = FALSE
  last.prime = 2L
  fsqr = floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] = FALSE
    sel = which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime = last.prime + min(sel)
    }else last.prime = fsqr+1
  }
  which(primes)
}

prime(10000)

# take each day of admission
# calcalate median and IQR of day
# encode as single number using godel coding - plot

# T1 only
T1_DT <- DT[DiabetesMellitusType_Mapped == "Type 1 Diabetes Mellitus"]

# interested in the 15th day of admission
cut_DT <- T1_DT[admissionDurationDays >= 15]
# mark dateTimeInDays
cut_DT[, c("dateTimeDays") := (dateplustime1 - min(dateplustime1)) / (60*60*24) , by=.(ID, admissionNumberFlag)]
cut_DT$dateTimeDays_floor <- floor(cut_DT$dateTimeDays)

cut_DT[, c("day_median") := median(yyyy) , by=.(ID, admissionNumberFlag, dateTimeDays_floor)]
cut_DT[, c("day_IQR") := quantile(yyyy)[4] - quantile(yyyy)[2], by=.(ID, admissionNumberFlag, dateTimeDays_floor)]




# cut_DT <- DT[admissionDurationDays >= 5 & admissionDurationDays < 30 & nCBGperAdmission > 20]

## need to be able to take all admissions greater than n days and look at hypo probability on day n
dayN_ofInterest = 10
daySeconds = (60*60*24)
dayN_ofInterestSeconds <- dayN_ofInterest * daySeconds

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

# find the odd case where there are only duplicate values in the analysis section
check_all_dateplustime1_different <- function(dateplustime1) {
  x <- dateplustime1[dateplustime1 < (min(dateplustime1) + (dayN_ofInterestSeconds - (60*60*24)))]
  y <- ifelse(sum(diff(x)) == 0, 0, 1)
  return(y)
}
cut_DT[, c("check_all_dateplustime1_different") := check_all_dateplustime1_different(dateplustime1) , by=.(ID, admissionNumberFlag)]

# cut the data to values within 5 days, and with enough runin CBGs
cut_DT <- cut_DT[flagWithinNdays == 1 & numberOfCBGsBeforeDayN > 1 & check_all_dateplustime1_different == 1]


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

# flag_CBGbelow_n_in24h_before_LastTime <- function(flagWithin_24h_prior_LastTime, yyyy, n) {
#   
#   lastimeValues <- yyyy[flagWithin_24h_prior_LastTime == 1]
#   returnVals <- ifelse(lastimeValues < n, 1, 0)
#   
#   flagPositiveResult <- ifelse(sum(returnVals) > 0, 1, 0)
#   
#   return(rep(flagPositiveResult, length(yyyy)))
# }
# 
# isDead_n_years <- function(cut_DT_ID, dateplustime1, n) {
#   
#   if (nrow(deathFrameDT[ID == cut_DT_ID]) > 0) {
#   isDead <- deathFrameDT[ID == cut_DT_ID]$isDead
#   deathDate <- deathFrameDT[ID == cut_DT_ID]$deathDateUnix
#   
#   returnValue <- ifelse(deathDate > 0 & deathDate <= (min(dateplustime1) + n * (60*60*24*365.25)), 1, 0)
#   
#   returnValue <- rep(returnValue, length(dateplustime1))
#   }
#   
#   if (nrow(deathFrameDT[ID == cut_DT_ID]) == 0) { returnValue <- rep(0, length(dateplustime1)) }
#   
#   return(returnValue)
#   
# }

cut_DT[, c("flagWithinLastTime") := (ifelse( dateplustime1 >= (min(dateplustime1) + ((dayN_ofInterest - 1) * daySeconds)) & dateplustime1 < (min(dateplustime1) + ((dayN_ofInterest) * daySeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]

cut_DT[, c("lessThan4_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 4), by=.(ID, admissionNumberFlag)]
cut_DT[, c("lessThan3_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 3), by=.(ID, admissionNumberFlag)]
cut_DT[, c("lessThan2p88_withinLastTime") := flag_CBGbelow_n_inLastTime(flagWithinLastTime, yyyy, 2.88), by=.(ID, admissionNumberFlag)]

# cut_DT[, c("flagWithin_24h_prior_LastTime") := (ifelse((dateplustime1 >= (min(dateplustime1) + ((max(dateplustime1) - min(dateplustime1)) - 2*timePeriodSeconds))) & (dateplustime1 < (min(dateplustime1) + ((max(dateplustime1) - min(dateplustime1)) - timePeriodSeconds))), 1, 0)) , by=.(ID, admissionNumberFlag)]
# cut_DT[, c("lessThan4_within_24h_prior_LastTime") := flag_CBGbelow_n_in24h_before_LastTime(flagWithin_24h_prior_LastTime, yyyy, 4), by=.(ID, admissionNumberFlag)]


# for death analysis run this
##****##  # cut_DT[, c("isDead_3y") := isDead_n_years(ID, dateplustime1, 3), by=.(ID, admissionNumberFlag)]
##****##  # cut_DT[, c("firstCBGperID") := (dateplustime1 == min(dateplustime1)), by=.(ID)]
##****##  # cut_DT[, c("flagPartOfFirstAdmission") := ifelse(max(firstCBGperID) == 1, 1, 0), by=.(ID, admissionNumberFlag)]

cut_DT[, c("flagLastCBG") := (ifelse(CBGinSequencePerAdmission == length(yyyy), 1, 0)), by=.(ID, admissionNumberFlag)]

report_y_hypo4 <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan4_withinLastTime)
colnames(report_y_hypo4) <- c("ID", "hypo_4")
report_y_hypo3 <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan3_withinLastTime)
colnames(report_y_hypo3) <- c("ID", "hypo_3")

# report_y_hypo4_24hPrior <- data.frame(cut_DT[flagLastCBG == 1]$ID, cut_DT[flagLastCBG == 1]$lessThan4_within_24h_prior_LastTime)
# colnames(report_y_hypo4_24hPrior) <- c("ID", "hypo_4")


## code to test using prior hypo as a predictor
# test <- report_y_hypo4[39502:49346, ]; test_24hprior <- report_y_hypo4_24hPrior[39502:49346, ]
# library(pROC); auc(test$hypo_4, test_24hprior$hypo_4)

##****## report_y_death_3y <- data.frame(cut_DT[firstCBGperID == 1]$ID, cut_DT[firstCBGperID == 1]$isDead_3y)
##****## colnames(report_y_death_3y) <- c("ID", "dead_3y")

# scale admissions to n points
#  cut_DT[, c("flag_for_processing") := (ifelse(dateplustime1 < (min(dateplustime1) + (admissionDuration[1] - timePeriodSeconds)), 1, 0)) , by=.(ID, admissionNumberFlag)]

##****##  # for death analysis - only analyse a single CBG dataset per ID - taking the first admission in the dataset
##****##  # cut_DT_processSegment <- cut_DT[flagPartOfFirstAdmission == 1]

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
# save_y_dead3 <- report_y_death_3y$isDead_3y


# randomisingSequence <- runif(nrow(report_y_hypo4), 0, 1)

## writeout files for tensorflow
# write out sequence for analysis
write.table(save_X, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/4thday_processX.csv", sep=",", row.names = FALSE)


# write out dep variable (y)
write.table(save_y_hypo4, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/4thday_report_y_hypo4.csv", sep = ",", row.names = FALSE)
write.table(save_y_hypo3, file = "~/R/_workingDirectory/rnn_inpatient_cbg/data/4thday_report_y_hypo3.csv", sep = ",", row.names = FALSE)












