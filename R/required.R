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
  #continuous_reaches<<- subset(continuous_reaches, select = -p4)
  terminal_reaches<<- cbind(schedule,Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv'))
  terminal_reaches<<- subset(terminal_reaches, select = -p1)
  cursorjump_reaches<<- cbind(schedule,Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv'))
  cursorjump_reaches<<- subset(cursorjump_reaches, select = -c(p6,p12))
  
  continuous_nocursors<<- cbind(schedule,Cleandata('ana/condition 1 trialtype 0_cuttoff0.7.csv'))
  #continuous_nocursors<<- subset(continuous_nocursors, select = -p4)
  terminal_nocursors<<- cbind(schedule,Cleandata('ana/condition 3 trialtype 0_cuttoff0.7.csv'))
  terminal_nocursors<<- subset(terminal_nocursors, select = -p1)
  cursorjump_nocursors<<- cbind(schedule,Cleandata('ana/condition 4 trialtype 0_cuttoff0.7.csv'))
  cursorjump_nocursors<<- subset(cursorjump_nocursors, select = -c(p6,p12))
  
}


getslowprocesses<- function() {
  schedule<- c(rep(0, times = 20), rep(45, times = 100), rep(-45, times = 8), rep(NA, times = 16))
  
##need slow process for each person
csp<-c()  
for (p in 2:ncol (continuous_reaches)) {
  reaches<- continuous_reaches[,p]
pars<-  fitTwoRateReachModel(reaches = reaches, schedule = schedule)
csp<-cbind(csp,twoRateReachModel(pars, schedule)$slow)
}
csp<- data.frame(csp)
cnames<- names(continuous_reaches[2:ncol(continuous_reaches)])
names(csp)<- cnames
write.csv(csp, file = "ana/continuous_slowprocess.csv", quote = FALSE, row.names = FALSE)



tsp<-c()  
for (p in 2:ncol (terminal_reaches)) {
  reaches<- terminal_reaches[,p]
  pars<-  fitTwoRateReachModel(reaches = reaches, schedule = schedule)
  tsp<-cbind(tsp,twoRateReachModel(pars, schedule)$slow)
}
tsp<- data.frame(tsp)
cnames<- names(terminal_reaches[2:ncol(terminal_reaches)])
names(tsp)<- cnames
write.csv(tsp, file = "ana/terminal_slowprocess.csv", quote = FALSE, row.names = FALSE)


cjsp<-c()  
for (p in 2:ncol (cursorjump_reaches)) {
  reaches<- cursorjump_reaches[,p]
  pars<-  fitTwoRateReachModel(reaches = reaches, schedule = schedule)
  cjsp<-cbind(cjsp,twoRateReachModel(pars, schedule)$slow)
}
cjsp<- data.frame(cjsp)
cnames<- names(cursorjump_reaches[2:ncol(cursorjump_reaches)])
names(cjsp)<- cnames
write.csv(cjsp, file = "ana/cursorjump_slowprocess.csv", quote = FALSE, row.names = FALSE)
}


 Baselinedata<- function(data) {
  
  for (i in 1:ncol(data)){
    average<- mean(data[1:20,i], na.rm = TRUE)
    data[,i]<- data[,i]-average
  }
   return(data) 
   
 }
 
 
 Getasymptotes<- function(){
 
 cd<- read.csv('forwardana/continuous_reaches.csv', header = TRUE)
 td<- read.csv('forwardana/terminal_reaches.csv', header = TRUE)
 jd<- read.csv('forwardana/cursorjump_reaches.csv', header = TRUE)
 
 cdRMR<-ANOVAcombine(cd)
 ContR<-cdRMR$Reaches[cdRMR$Time == "R1_late"]
 tdRMR<-ANOVAcombine(td)
 TermR<-tdRMR$Reaches[tdRMR$Time == "R1_late"]
 jdRMR<-ANOVAcombine(jd)
 JumpR<-jdRMR$Reaches[jdRMR$Time == "R1_late"]

 allRM<- c(ContR , TermR, JumpR)
write.csv(allRM, "learning asymptote.csv", quote = FALSE, row.names = FALSE)
}