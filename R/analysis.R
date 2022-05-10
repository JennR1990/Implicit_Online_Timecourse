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


#here i am trying to find the locations to do the analysis at for Sebastian Data (may need to set working directory of whever the data is. )
createstartpoints<- function(data) {
 # dat<-read.csv("condition 1 trialtype 0.csv", header = TRUE)
  reaches <- apply(data[,2:ncol(data)], 1, median, na.rm=T)
  plot(reaches, type = 'l', xlab = "Trials", ylab = "Reach Deviations", main = "Reach Trials")
  phases<-c("practice", "baseline", "rotation", "counter", "errorclamp")
  rotations<- c(0,0,45,-45,NA)
  Perturbation<-c()
  
  
  for (i in 1:length(data$rot)) {
    loc<- which(data$rot[i] == phases)
    Perturbation[i]<-rotations[loc]
  }
  
  
  
  counter<- 1
  stop<- c()
  for (rot in rotations[1:4]){
    all<-which(Perturbation == rot)
    stop[counter]<- all[unlist(length(all))]
    #start<- length(all)-block
    counter<- counter + 1
  }
  
  # alignedend<-as.numeric(stop[1])
  # Rotationbeginning<-as.numeric(stop[2]+1)
  # Rotationend<-as.numeric(stop[3])
  # counterend<-as.numeric(stop[4])
  # errorclampend<-as.numeric(length(Perturbation))
  return(start<- (c(as.numeric(stop[1]),as.numeric(stop[2]+8),as.numeric(stop[3]),as.numeric(stop[4]),as.numeric(length(Perturbation)) ))-7)
}




#THis combines the data to make it ready for an ANOVA already updated for Sebastian data
ANOVAcombine<- function(data) {
  ParticipantARM<- data.frame()
  participants <- names(data)[1:dim(data)[2]]
  epochs <- list('R1_early'=c(21,4), 'R1_late'=c(117,4), 'R2D2'=c(125,4), 'EC'=c(141,4))
  Reaches<- c()
  Time<- c()
  ID<- c()
  
  for (participant in participants){
    
    participant_reaches <- unlist(data[,participant])
    
    for (epoch in names(epochs)) {
      
      start <- epochs[[epoch]][1]
      finish <- start -1 + epochs[[epoch]][2]
      Reaches <- c(Reaches, mean(participant_reaches[start:finish], na.rm=TRUE))
      Time <- c(Time, epoch)
      ID <- c(ID, participant)
      ANOVARM<- data.frame(Reaches, Time, ID)
    }
  }
  #b<- !is.nan(ANOVARM$Reaches)
  # ANOVARM<- ANOVARM[c(b),]
  return(ANOVARM)
}

#This combines the data into the format needed to do t-tests in R (Needs to be updated for Sebastian Data)
TCombine<- function(data) {
  ParticipantRM<- data.frame()
  participants <- c(1:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[17:20,participant]), na.rm = TRUE)
    R1_Early<- mean(unlist(data[21:24,participant]), na.rm = TRUE)
    R1_Late<- mean(unlist(data[117:120,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[125:128,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[129:132,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[141:144,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned,R1_Early, R1_Late, R2, EC, EC_Late)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}




#these codes below will run the combine functions above on every condition and put it together for analysis. 

PrepdataforANOVA <- function(conditions = c(1,3,4), type = 0) {
  AllRM<- data.frame()

  for (cond in conditions){
    
  filename<- sprintf("ana/condition %d trialtype %d_cuttoff0.7.csv", cond, type)
  data<- Cleandata(filename)
  RM<-ANOVAcombine(data)
  RM$Experiment <- rep(sprintf("%d", cond), nrow(RM))  
  
  
  if (nrow(AllRM) == 0) {
  AllRM<- RM
  } else {
    AllRM<- rbind(AllRM, RM)
    
  }
  }
  
  

  return(AllRM)
  
}

PrepdataforT <- function() {
  continuousR_RM<-TCombine(continuous_reaches)
  continuousR_RM$Experiment<- rep("Continuous", times = nrow(continuousR_RM))
  terminalR_RM<-TCombine(terminal_reaches)
  terminalR_RM$Experiment<- rep("Terminal", times = nrow(terminalR_RM))
  cursorJumpR_RM<-TCombine(cursorJump_reaches)
  cursorJumpR_RM$Experiment<- rep("CursorJump", times = nrow(cursorJumpR_RM))
  
  
  continuousNC_RM<-TCombine(continuous_nocursors)
  continuousNC_RM$Experiment<- rep("Continuous", times = nrow(continuousNC_RM))
  terminalNC_RM<-TCombine(terminal_nocursors)
  terminalNC_RM$Experiment<- rep("Terminal", times = nrow(terminalNC_RM))
  cursorJumpNC_RM<-TCombine(cursorJump_nocursors)
  cursorJumpNC_RM$Experiment<- rep("CursorJump", times = nrow(cursorJumpNC_RM))
  
  AllData<- rbind(continuousR_RM,terminalR_RM,cursorJumpR_RM,continuousNC_RM,terminalNC_RM,cursorJumpNC_RM)
  
  AllData$Task <- c(rep("Reaches", times = sum(nrow(continuousR_RM),nrow(terminalR_RM),nrow(cursorJumpR_RM) )),
                    rep("No-Cursors", times = sum(nrow(continuousNC_RM),nrow(terminalNC_RM),nrow(cursorJumpNC_RM) )))
  
  
  
  return(AllData)
  
}



#This code actually runs the ANOVA once we run other functions first. To run this code you need to run prepdataforANOVA and assign that to a variable "data" and run ANOVAanalysis(data) 
ANOVAanalysis<- function(AllDataANOVA){
  AllDataANOVA$Time<- as.factor(AllDataANOVA$Time)
  AllDataANOVA$ID<- as.factor(AllDataANOVA$ID)
  AllDataANOVA$Experiment<- as.factor(AllDataANOVA$Experiment)
  fullmodel <- ezANOVA(data=AllDataANOVA,
                       dv=Reaches,
                       wid=ID,
                       within=Time,
                       between = Experiment,
                       type=3,
                       return_aov=TRUE)
  return(fullmodel)
}





# to run this code you have to run Prepdata for T and assign it to a variable named RM then run this line of code IndependentT(RM, 1,1,0)
IndependentT<- function(data, cond1, cond2, type) {
  library(effsize)
  library(SMCL)
  print(sprintf('this is the between subjects comparison of condition %s to %s %s Data', cond1, cond2, type))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == cond1 & data$Task == type],data$Aligned[data$Experiment == cond2 & data$Task == type])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == cond1 & data$Task == type],data$Aligned[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == cond1 & data$Task == type],data$Aligned[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == cond1 & data$Task == type],data$R1_Early[data$Experiment == cond2 & data$Task == type])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == cond1 & data$Task == type],data$R1_Early[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == cond1 & data$Task == type],data$R1_Early[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == cond1 & data$Task == type],data$R1_Late[data$Experiment == cond2 & data$Task == type])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == cond1 & data$Task == type],data$R1_Late[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == cond1 & data$Task == type],data$R1_Late[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == cond1 & data$Task == type],data$R2[data$Experiment == cond2 & data$Task == type])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == cond1 & data$Task == type],data$R2[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == cond1 & data$Task == type],data$R2[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == cond1 & data$Task == type],data$EC[data$Experiment == cond2 & data$Task == type])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == cond1 & data$Task == type],data$EC[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == cond1 & data$Task == type],data$EC[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == cond1 & data$Task == type],data$EC_Late[data$Experiment == cond2 & data$Task == type]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == cond1 & data$Task == type],data$EC_Late[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == cond1 & data$Task == type],data$EC_Late[data$Experiment == cond2 & data$Task == type], na.rm = TRUE))
  
}

makep <- function(){
  data <- read.csv("condition 0 trialtype 1.csv", header = TRUE)
phases<-c("practice", "baseline", "rotation", "counter", "errorclamp")
rotations<- c(0,0,45,-45,NA)
Perturbation<-c()


for (i in 1:length(data$rot)) {
  loc<- which(data$rot[i] == phases)
  Perturbation[i]<-rotations[loc]
}

return(Perturbation)
}