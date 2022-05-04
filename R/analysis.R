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
  participants <- names(data)[2:dim(data)[2]]
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
  participants <- c(2:ncol(data))
  for (participant in participants){
    Aligned<- mean(unlist(data[17:24,participant]), na.rm = TRUE)
    r1<- unlist(data[25,participant])
    r2<- unlist(data[26,participant])
    r3<- unlist(data[27,participant])
    r4<- unlist(data[28,participant])
    R1_Early<- mean(unlist(data[21:24,participant]), na.rm = TRUE)
    R1_second<-mean(unlist(data[33:40,participant]), na.rm = TRUE) 
    R1_third<- mean(unlist(data[41:48,participant]), na.rm = TRUE)
    R1_forth<-mean(unlist(data[49:56,participant]), na.rm = TRUE) 
    R1_fifth<- mean(unlist(data[57:64,participant]), na.rm = TRUE)
    R1_sixth<-mean(unlist(data[65:72,participant]), na.rm = TRUE) 
    R1_Late<- mean(unlist(data[117:124,participant]), na.rm = TRUE)
    R2<- mean(unlist(data[125:132,participant]), na.rm = TRUE)
    EC<- mean(unlist(data[133:140,participant]), na.rm = TRUE)
    EC1<- mean(unlist(data[141,participant]), na.rm = TRUE)
    EC2<- mean(unlist(data[142,participant]), na.rm = TRUE)
    EC3<- mean(unlist(data[143,participant]), na.rm = TRUE)
    EC4<- mean(unlist(data[144,participant]), na.rm = TRUE)
    EC_Late<- mean(unlist(data[141:148,participant]), na.rm = TRUE)
    RM<- data.frame(Aligned, r1, r2,r3,r4, R1_Early, R1_second, R1_third, R1_forth, R1_fifth, R1_sixth, R1_Late, R2, EC, EC_Late, EC1, EC2, EC3, EC4)
    if (prod(dim(ParticipantRM)) == 0) {
      ParticipantRM <- RM
    } else {
      ParticipantRM <- rbind(ParticipantRM, RM)
    }
  }
  return(ParticipantRM)
}




#these codes below will run the combine functions above on every condition and put it together for analysis. 

PrepdataforANOVA <- function(conditions = c(0,1,3,4,5), type = 0) {
  AllRM<- data.frame()

  for (cond in conditions){
    
  filename<- sprintf("condition %d trialtype %d.csv", cond, type)
  data<- read.csv(filename, header = TRUE)
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

PrepdataforT <- function(conditions = c(0,1,3,4,5), type = 0) {
  AllRM<- data.frame()
  
  for (cond in conditions){
    
    filename<- sprintf("condition %d trialtype %d.csv", cond, type)
    data<- read.csv(filename, header = TRUE)
    RM<-TCombine(data)
    RM$Experiment <- rep(sprintf("%d", cond), nrow(RM))  
    
    
    if (nrow(AllRM) == 0) {
      AllRM<- RM
    } else {
      AllRM<- rbind(AllRM, RM)
      
    }
  }
  
  return(AllRM)
  
}



#This code actually runs the ANOVA once we run other functions first. To run this code you need to run prepdataforANOVA and assign that to a variable "data" and run ANOVAanalysis(data) 
ANOVAanalysis<- function(AllDataANOVA){
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
  print(sprintf('this is the between subjects comparison of condition %d to %d type %d Data', cond1, cond2, type))
  print('Aligned')
  print(t.test(data$Aligned[data$Experiment == cond1],data$Aligned[data$Experiment == cond2])) #not sig A vs. NC
  print(cohen.d(data$Aligned[data$Experiment == cond1],data$Aligned[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$Aligned[data$Experiment == cond1],data$Aligned[data$Experiment == cond2], na.rm = TRUE))
  print('Beginning of 1st rotation')
  print(t.test(data$R1_Early[data$Experiment == cond1],data$R1_Early[data$Experiment == cond2])) # p-value = 0.04535 A vs. NC
  print(cohen.d(data$R1_Early[data$Experiment == cond1],data$R1_Early[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Early[data$Experiment == cond1],data$R1_Early[data$Experiment == cond2], na.rm = TRUE))
  print('End of 1st rotation')
  print(t.test(data$R1_Late[data$Experiment == cond1],data$R1_Late[data$Experiment == cond2])) # not sig A vs. NC
  print(cohen.d(data$R1_Late[data$Experiment == cond1],data$R1_Late[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$R1_Late[data$Experiment == cond1],data$R1_Late[data$Experiment == cond2], na.rm = TRUE))
  print('Beginning of 2nd rotation')
  print(t.test(data$R2[data$Experiment == cond1],data$R2[data$Experiment == cond2])) # not sig  A vs. NC
  print(cohen.d(data$R2[data$Experiment == cond1],data$R2[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$R2[data$Experiment == cond1],data$R2[data$Experiment == cond2], na.rm = TRUE))
  print('Beginning of Error Clamp')
  print(t.test(data$EC[data$Experiment == cond1],data$EC[data$Experiment == cond2])) # p-value = 0.005945  A vs. NC
  print(cohen.d(data$EC[data$Experiment == cond1],data$EC[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC[data$Experiment == cond1],data$EC[data$Experiment == cond2], na.rm = TRUE))
  print('End of Error Clamp (32 trials)')
  print(t.test(data$EC_Late[data$Experiment == cond1],data$EC_Late[data$Experiment == cond2]))  #p-value = 1.36e-07  A vs. NC
  print(cohen.d(data$EC_Late[data$Experiment == cond1],data$EC_Late[data$Experiment == cond2], na.rm = TRUE))
  print(etaSquaredTtest(data$EC_Late[data$Experiment == cond1],data$EC_Late[data$Experiment == cond2], na.rm = TRUE))
  
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