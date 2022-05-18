Cleandata<- function (filename){
  data<-read.csv(filename, header = TRUE)
  
  aligned<-data[data$rot =="baseline",2:ncol(data)]
  rot1<-data[data$rot =="rotation",2:ncol(data)]
  counter<-data[data$rot =="counter",2:ncol(data)]
  errorclamp<-data[data$rot =="errorclamp",2:ncol(data)]
  
  
  
  aligned[aligned > 40] <- NA
  aligned[aligned < -40] <- NA
  
  rot1[rot1 < -75]<- NA
  rot1[rot1 >  15]<- NA
  
  counter[counter < -30]<- NA
  counter[counter >  75]<- NA
  
  errorclamp[errorclamp > 40] <- NA
  errorclamp[errorclamp < -40] <- NA
  
  
  alldata<- rbind(aligned,rot1,counter,errorclamp)
  return(alldata)
}



loaddata<- function() {
  
  
  schedule<- c(rep(0, times = 20), rep(45, times = 100), rep(-45, times = 8), rep(NA, times = 16))
  continuous_reaches<<- cbind(schedule,Cleandata('ana/condition 1 trialtype 1_cuttoff0.7.csv'))
  #continuous_reaches<<- continuous_reaches[,-c(4)]
  terminal_reaches<<- cbind(schedule,Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv'))
  terminal_reaches<<- terminal_reaches[,-c(1)]
  cursorjump_reaches<<- cbind(schedule,Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv'))
  cursorjump_reaches<<- cursorjump_reaches[,-c(6,8,12,13)]
  
  continuous_nocursors<<- cbind(schedule,Cleandata('ana/condition 1 trialtype 0_cuttoff0.7.csv'))
  #continuous_nocursors<<- continuous_nocursors[,-c(4)]
  terminal_nocursors<<- cbind(schedule,Cleandata('ana/condition 3 trialtype 0_cuttoff0.7.csv'))
  terminal_nocursors<<- terminal_nocursors[,-c(1)]
  cursorjump_nocursors<<- cbind(schedule,Cleandata('ana/condition 4 trialtype 0_cuttoff0.7.csv'))
  cursorjump_nocursors<<- cursorjump_nocursors[,-c(6,8,12,13)]
  
}


getslowprocesses<- function() {
  schedule<- c(rep(0, times = 20), rep(45, times = 100), rep(-45, times = 8), rep(NA, times = 16))
pars<-  fitTwoRateReachModel(reaches = as.numeric(unlist(rowMeans(continuous_reaches[2:ncol(continuous_reaches)], na.rm = TRUE))), schedule = schedule)
csp<-twoRateReachModel(pars, schedule)$slow
write.csv(csp, file = "ana/continuous slow process.csv", quote = FALSE, row.names = FALSE)

pars<-  fitTwoRateReachModel(reaches = as.numeric(unlist(rowMeans(terminal_reaches[2:ncol(terminal_reaches)], na.rm = TRUE))), schedule = schedule)
tsp<-twoRateReachModel(pars, schedule)$slow
write.csv(tsp, file = "ana/terminal slow process.csv", quote = FALSE, row.names = FALSE)

pars<-  fitTwoRateReachModel(reaches = as.numeric(unlist(rowMeans(cursorjump_reaches[2:ncol(cursorjump_reaches)], na.rm = TRUE))), schedule = schedule)
cjsp<-twoRateReachModel(pars, schedule)$slow
write.csv(cjsp, file = "ana/cursorjump slow process.csv", quote = FALSE, row.names = FALSE)
}
