

calculateaimingangles<- function() {
source('R/preprocessing.R')
pinfo<-getpinfo()
setwd("E:/Jenn/Documents/Implicit_Online_Timecourse/summerdata")

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

     
     finalaiming<-data.frame(matrix(NA, nrow = length(files),ncol = 8))
     names(finalaiming)<- c("45", "75", "105", "135","45", "75", "105", "135")
     targetlocs<- c(45,75,105,135)
   
   }
   
   
   
   for (i in 1:length(files)){
     for (target in c(1:4)){
       indx<-which(aiming_targets[i,] == targetlocs[target])
       inds<-which(names(finalaiming) == as.character(targetlocs[target]))
       finalaiming[i,inds]<-aiming_directions[i,indx] 
     }
     
   }
   
names(finalaiming)<- c("45", "75", "105", "135","45*", "75*", "105*", "135*")
finalaiming$'45'[finalaiming$'45' == 60]<- NA
finalaiming$'45*'[finalaiming$'45*' == 60]<- NA
finalaiming$'75'[finalaiming$'75' == 90]<- NA
finalaiming$'75*'[finalaiming$'75*' == 90]<- NA
finalaiming$'105'[finalaiming$'105' == 120]<- NA
finalaiming$'105*'[finalaiming$'105*' == 120]<- NA
finalaiming$'135'[finalaiming$'135' == 150]<- NA
finalaiming$'135*'[finalaiming$'135*' == 150]<- NA

finalaiming$'45'[finalaiming$'45' < 15]<- NA
finalaiming$'45*'[finalaiming$'45*' < 15]<- NA
finalaiming$'75'[finalaiming$'75' < 15]<- NA
finalaiming$'75*'[finalaiming$'75*' < 15]<- NA
finalaiming$'105'[finalaiming$'105' < 15]<- NA
finalaiming$'105*'[finalaiming$'105*' < 15]<- NA
finalaiming$'135'[finalaiming$'135' < 15]<- NA
finalaiming$'135*'[finalaiming$'135*' < 15]<- NA



   
   
   output<- sprintf("../AimingAngle/Summer%d_aimingangle.csv", group)
write.csv(finalaiming, output, quote = FALSE, row.names = FALSE)
   
 }

}
 






getmedianaims<- function() {
df<-read.csv("AimingAngle/Forward_Aims.csv", header = TRUE)

df[,1:4]<- abs(df[,1:4])


medianaims<-apply(df[,1:4],MARGIN = 1, FUN = median, na.rm = TRUE)


aims<- data.frame(medianaims, df$Experiment)
write.csv(aims, "AimingAngle/Forward_Participant_Median_Aims.csv", quote = FALSE, row.names = FALSE)
return(aims)
}

aimingAnalysis<- function(){
data <- read.csv("AimingAngle/Forward_Participant_Median_Aims_analysis.csv", header=TRUE)
data$Participant<- as.factor(data$Participant)
data$df.Experiment<- as.factor(data$df.Experiment)
fullmodel <- ezANOVA(data=data,
                     dv=medianaims,
                     wid=Participant,
                     between = df.Experiment,
                     type=3,
                     return_aov=TRUE)

t.test(data$medianaims[data$df.Experiment == 'Continuous'],data$medianaims[data$df.Experiment == 'Terminal'])
t.test(data$medianaims[data$df.Experiment == 'Continuous'],data$medianaims[data$df.Experiment == 'Cursor-Jump'])
t.test(data$medianaims[data$df.Experiment == 'Cursor-Jump'],data$medianaims[data$df.Experiment == 'Terminal'])


}




plotaimingangles<- function() {
  medianaims<- read.csv('AimingAngle/Forward_Participant_Median_Aims-simplified.csv', header = TRUE)
ca<- medianaims[medianaims$df.Experiment == "Continuous",]
ta<- medianaims[medianaims$df.Experiment == "Terminal",]
ja<- medianaims[medianaims$df.Experiment == "Cursor-Jump",]

plot(100,100,xlim = c(.75,2.25), ylim = c(0,55), xlab = "Feedback Group", ylab = "Aim Deviation [°]", axes = FALSE, main = "median aiming angle", cex = 1.25, cex.lab = 1.25)
axis(1, at=c(1,1.5,2), labels = c("continuous", 'terminal', 'cursorjump'), cex.axis = 1.25)
axis(2, at = c(0,10,20,30,40,50), las = 2, cex.axis = 1.25)
points(x = rep(1, times = nrow(ca)),y=ca$medianaims, col = "dodgerblue")
points(x = rep(1.5, times = nrow(ta)),y=ta$medianaims, col = "sienna2")
points(x = rep(2, times = nrow(ja)),y=ja$medianaims, col = "mediumseagreen")
abline(h=45, lty= 3, col= "grey")
abline(h=0, lty= 3, col= "grey")

lines(x = c(.95,1.05),y=c(mean(ca$medianaims, na.rm=TRUE),mean(ca$medianaims, na.rm=TRUE)), col = "dodgerblue")
lower<-t.interval(ca$medianaims)[1]
upper<-t.interval(ca$medianaims)[2]
x<- c(.95,1.05,1.05,.95)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.04, 0.3, .5, 0.2), border = NA)
d<- density(ca$medianaims, n=24,from = 0.5, to = 35, bw = 1)$y
dX <- (d / sum(d)) 
dY <- seq(0.5,35,length.out = 24)
polygon(x=c(0,dX,0)+1.07, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.04, 0.3, .5, 0.2))


lines(x = c(1.45,1.55),y=c(mean(ta$medianaims),mean(ta$medianaims)), col = "sienna2")
lower<-t.interval(ta$medianaims)[1]
upper<-t.interval(ta$medianaims)[2]
x<- c(1.45,1.55,1.55,1.45)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.93, 0.47, .26, 0.2), border = NA)
d<- density(ta$medianaims, n=nrow(ta),from = min(ta$medianaims), to = max(ta$medianaims), bw = 1)$y
dX <- (d / sum(d)) 
dY <- seq(min(ta$medianaims),max(ta$medianaims),length.out = nrow(ta))
polygon(x=c(0,dX,0)+1.57, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.93, 0.47, .26, 0.2))


lines(x = c(1.95,2.05),y=c(mean(ja$medianaims),mean(ja$medianaims)), col = "mediumseagreen")
lower<-t.interval(ja$medianaims)[1]
upper<-t.interval(ja$medianaims)[2]
x<- c(1.95,2.05,2.05,1.95)
y<- c(lower,lower,upper,upper)
polygon(x,y,col = rgb(0.23, 0.70, .44, 0.2), border = NA)
d<- density(ja$medianaims, n=nrow(ja),from = min(ja$medianaims), to = max(ja$medianaims), bw = 1)$y
dX <- (d / sum(d)) 
dY <- seq(min(ja$medianaims),max(ja$medianaims),length.out = nrow(ja))
polygon(x=c(0,dX,0)+2.07, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.23, 0.70, .44, 0.2))


text(x = 2.2, y = 45.6, labels = "Fully Explicit")
text(x = 2.2, y = 0.6, labels = "Fully Implicit")


}


