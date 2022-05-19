

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

     
     finalaiming<-data.frame(matrix(NA, nrow = 15,ncol = 4))
     names(finalaiming)<- c("45", "90", "90", "135")
     targetlocs<- c(45,90,90,135)
    
     for (i in 1:15){
       for (target in c(1,4)){
         indx<-which(aiming_targets[i,] == targetlocs[target])
         finalaiming[i,target]<-aiming_directions[i,indx] 
       }
       
     }
     
     
     for (i in 1:15){
       indx<-which(aiming_targets[i,] == targetlocs[3])
       finalaiming[i,2:3]<-aiming_directions[i,indx] 
     }
     
     
     
     
   }

   
 }
 
return(finalaiming)
}
 


 
 