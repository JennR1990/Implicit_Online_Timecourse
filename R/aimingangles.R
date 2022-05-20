

calculateaimingangles<- function() {
source('../R/preprocessing.R')
pinfo<-getpinfo()
setwd("E:/Jenn/Documents/Implicit_Online_Timecourse/data")

 groups<- unique(pinfo$conditions)
 for (group in groups){
   ab_diff<- c()
   aiming_directions<- c()
   aiming_targets<-c()
   files<- pinfo$files[pinfo$conditions == group]
   
   for (file in files) {
     
     df<- read.csv(file, header = TRUE)
     aiming_targets<- rbind(aiming_targets,df$aiming_target[df$aiming_type == 1])
     aiming_directions<- rbind(aiming_directions,df$aiming_direction[df$aiming_type == 1])

     
     finalaiming<-data.frame(matrix(NA, nrow = length(files),ncol = 4))
     names(finalaiming)<- c("45", "90", "90", "135")
     targetlocs<- c(45,90,90,135)
    

   
   
   }
   for (i in 1:length(files)){
     for (target in c(1,4)){
       indx<-which(aiming_targets[i,] == targetlocs[target])
       finalaiming[i,target]<-aiming_directions[i,indx] 
     }
     
   }
   
   
   for (i in 1:length(files)){
     indx<-which(aiming_targets[i,] == targetlocs[3])
     finalaiming[i,2:3]<-aiming_directions[i,indx] 
   }
   
   output<- sprintf("../AimingAngle/%d_aimingangle.csv", group)
write.csv(finalaiming, output, quote = FALSE, row.names = FALSE)
   
 }

}
 
getmedianaims<- function() {
df<-read.csv("AimingAngle/aims.csv", header = TRUE)

df[,1:4]<- abs(df[,1:4])


medianaims<-apply(df[,1:4],MARGIN = 1, FUN = median)


aims<- data.frame(medianaims, df$experiment)
write.csv(aims, "AimingAngle/Participant Median Aims.csv", quote = FALSE, row.names = FALSE)
}

aimingANOVA<- function(){
data <- read.csv("AimingAngle/Participant Median Aims.csv", header=TRUE,
                 col_types = cols(experiment = col_factor(levels = c("continuous", 
                 "terminal", "cursorjump"))))
data$Participant<- as.factor(data$Participant)
fullmodel <- ezANOVA(data=data,
                     dv=medianaims,
                     wid=Participant,
                     between = experiment,
                     type=3,
                     return_aov=TRUE)
}

