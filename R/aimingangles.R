

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
write.csv(aims, "AimingAngle/Participant_Median_Aims.csv", quote = FALSE, row.names = FALSE)
return(aims)
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




plotaimingangles<- function() {
  medianaims<- getmedianaims()
ca<- medianaims[medianaims$df.experiment == "continuous",]
ta<- medianaims[medianaims$df.experiment == "terminal",]
ja<- medianaims[medianaims$df.experiment == "cursorjump",]

plot(100,100,xlim = c(0.5,3.5), ylim = c(0,75), xlab = "Feedback Group", ylab = "Aim Deviation [°]", axes = FALSE, main = "median aiming angle")
axis(1, at=c(1,2,3), labels = c("continuous", 'terminal', 'cursorjump'))
axis(2, at = c(0,10,20,30,40,50,60,70), las = 2)
points(x = rep(1, times = nrow(ca)),y=ca$medianaims, col = "dodgerblue")
points(x = rep(2, times = nrow(ta)),y=ta$medianaims, col = "sienna2")
points(x = rep(3, times = nrow(ja)),y=ja$medianaims, col = "mediumseagreen")
abline(h=45, lty= 3, col= "grey")
abline(h=0, lty= 3, col= "grey")

points(x = 1,y=mean(ca$medianaims), col = "dodgerblue", pch = 15)
lower<-t.interval(ja$medianaims)[1]
upper<-t.interval(ja$medianaims)[2]
x<- c(.95,1.05,1.05,.95)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.04, 0.3, .5, 0.2), border = NA)

points(x = 2,y=mean(ta$medianaims), col = "sienna2", pch = 15)
lower<-t.interval(ta$medianaims)[1]
upper<-t.interval(ta$medianaims)[2]
x<- c(1.95,2.05,2.05,1.95)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.93, 0.47, .26, 0.2), border = NA)

points(x = 3,y=mean(ja$medianaims), col = "mediumseagreen", pch = 15)
lower<-t.interval(ja$medianaims)[1]
upper<-t.interval(ja$medianaims)[2]
x<- c(2.95,3.05,3.05,2.95)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.23, 0.70, .44, 0.2), border = NA)

text(x = 3.25, y = 45.6, labels = "Fully Explicit")
text(x = 3.25, y = 0.6, labels = "Fully Implicit")


}



