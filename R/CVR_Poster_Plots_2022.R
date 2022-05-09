
plotreachestogether<- function() {
cd<- read.csv('ana/condition 1 trialtype 1_cuttoff0.7.csv', header = TRUE)
cd1<- Cleandata('ana/condition 1 trialtype 1_cuttoff0.7.csv')
cd<-data.frame(cd$rot[17:160],cd1)
rm(cd1)
td<- read.csv('ana/condition 3 trialtype 1_cuttoff0.7.csv', header = TRUE)
td1<- Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv')
td<-data.frame(td$rot[17:160],td1)
rm(td1)
jd<- read.csv('ana/condition 4 trialtype 1_cuttoff0.7.csv', header = TRUE)
jd1<- Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv')
jd<-data.frame(jd$rot[17:160],jd1)
jd<- jd[,-c(6,8,13)]
rm(jd1)

plot(rowMeans(cd[,3:16]*-1, na.rm=TRUE), type = "l", col = "dodgerblue4", ylim = c(-50,50), main = "Reaches", xlab = "trials", ylab = "Hand Direction [°]", axes = FALSE, cex.lab = 1.25)
data<- cd[,3:16]
dataCIs <- trialCI(data = data)
dataCIs <- dataCIs*-1
x <-  c(c(1:144), rev(c(1:144)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = rgb(0.04, 0.3, .5, 0.2), border = NA)

lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))

lines(rowMeans(td[,3:16]*-1, na.rm=TRUE), type = "l", col = "sienna2")
data<- td[,3:16]
dataCIs <- trialCI(data = data)
dataCIs <- dataCIs*-1
x <-  c(c(1:144), rev(c(1:144)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = rgb(0.93, 0.47, .26, 0.2), border = NA)

#plot(rowMeans(jd[17:160,2:18]*-1, na.rm=TRUE), type = "l", col = "mediumseagreen")
lines(rowMeans(jd[,2:15]*-1, na.rm=TRUE), type = "l", col = "mediumseagreen")
data<- jd[,2:15]
dataCIs <- trialCI(data = data)
dataCIs <- dataCIs*-1
x <-  c(c(1:144), rev(c(1:144)))
y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
polygon(x, y, col = rgb(0.23, 0.70, .44, 0.2), border = NA)

lines(rowMeans(cd[,3:16]*-1, na.rm=TRUE), type = "l", col = "dodgerblue4")
lines(rowMeans(td[,3:16]*-1, na.rm=TRUE), type = "l", col = "sienna2")

legend(
  5,
  -15,
  legend = c("Continuous", "Terminal", "Cursor Jump"),
  col = c("Dodgerblue4","sienna2", "mediumseagreen"),
  lty = c(1,1,1),
  lwd = c(2,2,2),
  bty = 'n',
  cex = 1.25
)
axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
     las = 2)
axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)

}

plotnocursorstogether<- function() {
  cd<- read.csv('ana/condition 1 trialtype 0_cuttoff0.7.csv', header = TRUE)
  cd1<- Cleandata('ana/condition 1 trialtype 0_cuttoff0.7.csv')
  cd<-data.frame(cd$rot[17:160],cd1)
  td<- read.csv('ana/condition 3 trialtype 0_cuttoff0.7.csv', header = TRUE)
  td1<- Cleandata('ana/condition 3 trialtype 0_cuttoff0.7.csv')
  td<-data.frame(td$rot[17:160],td1)
  jd<- read.csv('ana/condition 4 trialtype 0_cuttoff0.7.csv', header = TRUE)
  jd1<- Cleandata('ana/condition 4 trialtype 0_cuttoff0.7.csv')
  jd<-data.frame(jd$rot[17:160],jd1)
  jd<- jd[,-c(6,8,13)]
  
  plot(rowMeans(cd[,3:16]*-1, na.rm=TRUE), type = "l", col = "dodgerblue4", ylim = c(-50,50), main = "No-Cursors", xlab = "trials", ylab = "Hand Direction [°]", axes = FALSE, cex.lab = 1.25)
  data<- cd[,3:16]
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(0.04, 0.3, .5, 0.2), border = NA)
  
  lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
  lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
  
  lines(rowMeans(td[,3:16]*-1, na.rm=TRUE), type = "l", col = "sienna2")
  data<- td[,3:16]
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(0.93, 0.47, .26, 0.2), border = NA)
  
  #plot(rowMeans(jd[17:160,2:18]*-1, na.rm=TRUE), type = "l", col = "mediumseagreen")
  lines(rowMeans(jd[,2:15]*-1, na.rm=TRUE), type = "l", col = "mediumseagreen")
  data<- jd[,2:15]
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(0.23, 0.70, .44, 0.2), border = NA)
  
  lines(rowMeans(cd[,3:16]*-1, na.rm=TRUE), type = "l", col = "dodgerblue4")
  lines(rowMeans(td[,3:16]*-1, na.rm=TRUE), type = "l", col = "sienna2")
  
  legend(
    5,
    -15,
    legend = c("Continuous", "Terminal", "Cursor Jump"),
    col = c("Dodgerblue4","sienna2", "mediumseagreen"),
    lty = c(1,1,1),
    lwd = c(2,2,2),
    bty = 'n',
    cex = 1.25
  )
  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
  
}



PlotzoomedINNocursors<- function() {
  
  cd<- read.csv('ana/condition 1 trialtype 0_cuttoff0.7.csv', header = TRUE)
  cd1<- Cleandata('ana/condition 1 trialtype 0_cuttoff0.7.csv')
  cd<-data.frame(cd$rot[17:160],cd1)
  rm(cd1)
  td<- read.csv('ana/condition 3 trialtype 0_cuttoff0.7.csv', header = TRUE)
  td1<- Cleandata('ana/condition 3 trialtype 0_cuttoff0.7.csv')
  td<-data.frame(td$rot[17:160],td1)
  rm(td1)
  jd<- read.csv('ana/condition 4 trialtype 0_cuttoff0.7.csv', header = TRUE)
  jd1<- Cleandata('ana/condition 4 trialtype 0_cuttoff0.7.csv')
  jd<-data.frame(jd$rot[17:160],jd1)
  jd<- jd[,-c(6,8,13)]
  rm(jd1)
  
  cdRM<-ANOVAcombine(cd)
  tdRM<-ANOVAcombine(td)
  jdRM<-ANOVAcombine(jd)
  
  means<- (c(mean(cdRM$Reaches[cdRM$Time == "R1_early"], na.rm = TRUE),mean(tdRM$Reaches[tdRM$Time == "R1_early"], na.rm = TRUE),mean(jdRM$Reaches[jdRM$Time == "R1_early"], na.rm = TRUE)))*-1
  plot(y=means, x = c(.8,1,1.2), col = c("dodgerblue4","sienna2", 'mediumseagreen'), ylim = c(-60,60), pch = 15, cex = 1.5, xlim = c(0.5,4), xlab = "Trial Numbers", ylab = "Hand Deviation [°]", main = "No-Cursors", axes = FALSE, cex.lab = 1.25)
  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 1.9, 2.8, 3.7), labels = c("21-24", "117-120", "125-129", "141-144"), cex.axis = 1.25)
  
  lines(c(.5, .75,.75, 2.15, 2.15, 3.05, 3.05), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
  lines(c(3.05, 4), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
  
  
  
  SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R1_early"], na.rm = TRUE)/sqrt(length(cdRM$Reaches[cdRM$Time == "R1_early"])),sd(tdRM$Reaches[tdRM$Time == "R1_early"], na.rm = TRUE)/sqrt(length(tdRM$Reaches[tdRM$Time == "R1_early"])),sd(jdRM$Reaches[jdRM$Time == "R1_early"], na.rm = TRUE)/sqrt(length(jdRM$Reaches[jdRM$Time == "R1_early"])))
  segments(x0= .8, y0=means[1]+SEs[1], x1 = .8, y1 = means[1]-SEs[1], col = "dodgerblue4")
  segments(x0= 1, y0=means[2]+SEs[2], x1 = 1, y1 = means[2]-SEs[2], col = "sienna2")
  segments(x0= 1.2, y0=means[3]+SEs[3], x1 = 1.2, y1 = means[3]-SEs[3], col = 'mediumseagreen')
  
  points(x = c(rep(.8, times = length(cdRM$Reaches[cdRM$Time == "R1_early"]))),y= (cdRM$Reaches[cdRM$Time == "R1_early"])*-1, col = "dodgerblue4")
  points(x = c(rep(1, times = length(tdRM$Reaches[tdRM$Time == "R1_early"]))),y= (tdRM$Reaches[tdRM$Time == "R1_early"])*-1, col = "sienna2")
  points(x = c(rep(1.2, times = length(jdRM$Reaches[jdRM$Time == "R1_early"]))),y= (jdRM$Reaches[jdRM$Time == "R1_early"])*-1, col = "mediumseagreen")
  
  
  means<- (c(mean(cdRM$Reaches[cdRM$Time == "R1_late"], na.rm = TRUE),mean(tdRM$Reaches[tdRM$Time == "R1_late"], na.rm = TRUE),mean(jdRM$Reaches[jdRM$Time == "R1_late"], na.rm = TRUE)))*-1
  SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R1_late"], na.rm = TRUE)/sqrt(length(cdRM$Reaches[cdRM$Time == "R1_late"])),sd(tdRM$Reaches[tdRM$Time == "R1_late"], na.rm = TRUE)/sqrt(length(tdRM$Reaches[tdRM$Time == "R1_late"])),sd(jdRM$Reaches[jdRM$Time == "R1_late"])/sqrt(length(jdRM$Reaches[jdRM$Time == "R1_late"])))
  segments(x0= 1.7, y0=means[1]+SEs[1], x1 = 1.7, y1 = means[1]-SEs[1], col = "dodgerblue4")
  segments(x0= 1.9, y0=means[2]+SEs[2], x1 = 1.9, y1 = means[2]-SEs[2], col = "sienna2")
  segments(x0= 2.1, y0=means[3]+SEs[3], x1 = 2.1, y1 = means[3]-SEs[3], col = 'mediumseagreen')
  points(y=means, x = c(1.7,1.9,2.1), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
  points(x = c(rep(1.7, times = length(cdRM$Reaches[cdRM$Time == "R1_late"]))),y= (cdRM$Reaches[cdRM$Time == "R1_late"])*-1, col = "dodgerblue4")
  points(x = c(rep(1.9, times = length(tdRM$Reaches[tdRM$Time == "R1_late"]))),y= (tdRM$Reaches[tdRM$Time == "R1_late"])*-1, col = "sienna2")
  points(x = c(rep(2.1, times = length(jdRM$Reaches[jdRM$Time == "R1_late"]))),y= (jdRM$Reaches[jdRM$Time == "R1_late"])*-1, col = "mediumseagreen")
  
  
  means<- (c(mean(cdRM$Reaches[cdRM$Time == "R2D2"], na.rm = TRUE),mean(tdRM$Reaches[tdRM$Time == "R2D2"], na.rm = TRUE),mean(jdRM$Reaches[jdRM$Time == "R2D2"], na.rm = TRUE)))*-1
  SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R2D2"], na.rm = TRUE)/sqrt(length(cdRM$Reaches[cdRM$Time == "R2D2"])),sd(tdRM$Reaches[tdRM$Time == "R2D2"], na.rm = TRUE)/sqrt(length(tdRM$Reaches[tdRM$Time == "R2D2"])),sd(jdRM$Reaches[jdRM$Time == "R2D2"], na.rm = TRUE)/sqrt(length(jdRM$Reaches[jdRM$Time == "R2D2"])))
  segments(x0= 2.6, y0=means[1]+SEs[1], x1 = 2.6, y1 = means[1]-SEs[1], col = "dodgerblue4")
  segments(x0= 2.8, y0=means[2]+SEs[2], x1 = 2.8, y1 = means[2]-SEs[2], col = "sienna2")
  segments(x0= 3, y0=means[3]+SEs[3], x1 = 3, y1 = means[3]-SEs[3], col = 'mediumseagreen')
  points(y=means, x = c(2.6,2.8,3), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
  points(x = c(rep(2.6, times = length(cdRM$Reaches[cdRM$Time == "R2D2"]))),y= (cdRM$Reaches[cdRM$Time == "R2D2"])*-1, col = "dodgerblue4")
  points(x = c(rep(2.8, times = length(tdRM$Reaches[tdRM$Time == "R2D2"]))),y= (tdRM$Reaches[tdRM$Time == "R2D2"])*-1, col = "sienna2")
  points(x = c(rep(3, times = length(jdRM$Reaches[jdRM$Time == "R2D2"]))),y= (jdRM$Reaches[jdRM$Time == "R2D2"])*-1, col = "mediumseagreen")
  
  
  means<- c(mean(cdRM$Reaches[cdRM$Time == "EC"], na.rm = TRUE),mean(tdRM$Reaches[tdRM$Time == "EC"], na.rm = TRUE),mean(jdRM$Reaches[jdRM$Time == "EC"], na.rm = TRUE))*-1
  SEs<- c(sd(cdRM$Reaches[cdRM$Time == "EC"], na.rm = TRUE)/sqrt(length(cdRM$Reaches[cdRM$Time == "EC"])),sd(tdRM$Reaches[tdRM$Time == "EC"], na.rm = TRUE)/sqrt(length(tdRM$Reaches[tdRM$Time == "EC"])),sd(jdRM$Reaches[jdRM$Time == "EC"], na.rm = TRUE)/sqrt(length(jdRM$Reaches[jdRM$Time == "EC"])))
  points(y=means, x = c(3.5,3.7,3.9), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
  segments(x0= 3.5, y0=means[1]+SEs[1], x1 = 3.5, y1 = means[1]-SEs[1], col = "dodgerblue4")
  segments(x0= 3.7, y0=means[2]+SEs[2], x1 = 3.7, y1 = means[2]-SEs[2], col = "sienna2")
  segments(x0= 3.9, y0=means[3]+SEs[3], x1 = 3.9, y1 = means[3]-SEs[3], col = 'mediumseagreen')
  points(x = c(rep(3.5, times = length(cdRM$Reaches[cdRM$Time == "EC"]))),y= (cdRM$Reaches[cdRM$Time == "EC"])*-1, col = "dodgerblue4")
  points(x = c(rep(3.7, times = length(tdRM$Reaches[tdRM$Time == "EC"]))),y= (tdRM$Reaches[tdRM$Time == "EC"])*-1, col = "sienna2")
  points(x = c(rep(3.9, times = length(jdRM$Reaches[jdRM$Time == "EC"]))),y= (jdRM$Reaches[jdRM$Time == "EC"])*-1, col = "mediumseagreen")
  
  legend(
    .5,
    -10,
    legend = c("Continuous", "Terminal", "Cursor Jump", "Rotation"),
    col = c("dodgerblue4", "sienna2","mediumseagreen", "Black"),
    lty = c(1),
    lwd = c(1),
    bty = 'n',
    cex = 1.25
  )
  
  
  
} 


PlotzoomedINreaches<- function() {

cd<- read.csv('ana/condition 1 trialtype 1_cuttoff0.7.csv', header = TRUE)
cd1<- Cleandata('ana/condition 1 trialtype 1_cuttoff0.7.csv')
cd<-data.frame(cd$rot[17:160],cd1)
rm(cd1)
td<- read.csv('ana/condition 3 trialtype 1_cuttoff0.7.csv', header = TRUE)
td1<- Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv')
td<-data.frame(td$rot[17:160],td1)
rm(td1)
jd<- read.csv('ana/condition 4 trialtype 1_cuttoff0.7.csv', header = TRUE)
jd1<- Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv')
jd<-data.frame(jd$rot[17:160],jd1)
jd<- jd[,-c(6,8,13)]
rm(jd1)

cdRM<-ANOVAcombine(cd)
tdRM<-ANOVAcombine(td)
jdRM<-ANOVAcombine(jd)

means<- (c(mean(cdRM$Reaches[cdRM$Time == "R1_early"]),mean(tdRM$Reaches[tdRM$Time == "R1_early"]),mean(jdRM$Reaches[jdRM$Time == "R1_early"])))*-1
plot(y=means, x = c(.8,1,1.2), col = c("dodgerblue4","sienna2", 'mediumseagreen'), ylim = c(-60,60), pch = 15, cex = 1.5, xlim = c(0.5,4), xlab = "Trial Numbers", ylab = "Hand Deviation [°]", main = "Reaches", axes = FALSE, cex.lab = 1.25)
axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
     las = 2)
axis(1, at = c(1, 1.9, 2.8, 3.7), labels = c("21-24", "117-120", "125-129", "141-144"), cex.axis = 1.25)

lines(c(.5, .75,.75, 2.15, 2.15, 3.05, 3.05), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
lines(c(3.05, 4), c(0, 0), lty = 2, col = rgb(0., 0., 0.))



SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R1_early"])/sqrt(length(cdRM$Reaches[cdRM$Time == "R1_early"])),sd(tdRM$Reaches[tdRM$Time == "R1_early"])/sqrt(length(tdRM$Reaches[tdRM$Time == "R1_early"])),sd(jdRM$Reaches[jdRM$Time == "R1_early"])/sqrt(length(jdRM$Reaches[jdRM$Time == "R1_early"])))
segments(x0= .8, y0=means[1]+SEs[1], x1 = .8, y1 = means[1]-SEs[1], col = "dodgerblue4")
segments(x0= 1, y0=means[2]+SEs[2], x1 = 1, y1 = means[2]-SEs[2], col = "sienna2")
segments(x0= 1.2, y0=means[3]+SEs[3], x1 = 1.2, y1 = means[3]-SEs[3], col = 'mediumseagreen')

points(x = c(rep(.8, times = length(cdRM$Reaches[cdRM$Time == "R1_early"]))),y= (cdRM$Reaches[cdRM$Time == "R1_early"])*-1, col = "dodgerblue4")
points(x = c(rep(1, times = length(tdRM$Reaches[tdRM$Time == "R1_early"]))),y= (tdRM$Reaches[tdRM$Time == "R1_early"])*-1, col = "sienna2")
points(x = c(rep(1.2, times = length(jdRM$Reaches[jdRM$Time == "R1_early"]))),y= (jdRM$Reaches[jdRM$Time == "R1_early"])*-1, col = "mediumseagreen")


means<- (c(mean(cdRM$Reaches[cdRM$Time == "R1_late"]),mean(tdRM$Reaches[tdRM$Time == "R1_late"]),mean(jdRM$Reaches[jdRM$Time == "R1_late"])))*-1
SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R1_late"])/sqrt(length(cdRM$Reaches[cdRM$Time == "R1_late"])),sd(tdRM$Reaches[tdRM$Time == "R1_late"])/sqrt(length(tdRM$Reaches[tdRM$Time == "R1_late"])),sd(jdRM$Reaches[jdRM$Time == "R1_late"])/sqrt(length(jdRM$Reaches[jdRM$Time == "R1_late"])))
segments(x0= 1.7, y0=means[1]+SEs[1], x1 = 1.7, y1 = means[1]-SEs[1], col = "dodgerblue4")
segments(x0= 1.9, y0=means[2]+SEs[2], x1 = 1.9, y1 = means[2]-SEs[2], col = "sienna2")
segments(x0= 2.1, y0=means[3]+SEs[3], x1 = 2.1, y1 = means[3]-SEs[3], col = 'mediumseagreen')
points(y=means, x = c(1.7,1.9,2.1), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
points(x = c(rep(1.7, times = length(cdRM$Reaches[cdRM$Time == "R1_late"]))),y= (cdRM$Reaches[cdRM$Time == "R1_late"])*-1, col = "dodgerblue4")
points(x = c(rep(1.9, times = length(tdRM$Reaches[tdRM$Time == "R1_late"]))),y= (tdRM$Reaches[tdRM$Time == "R1_late"])*-1, col = "sienna2")
points(x = c(rep(2.1, times = length(jdRM$Reaches[jdRM$Time == "R1_late"]))),y= (jdRM$Reaches[jdRM$Time == "R1_late"])*-1, col = "mediumseagreen")


means<- (c(mean(cdRM$Reaches[cdRM$Time == "R2D2"]),mean(tdRM$Reaches[tdRM$Time == "R2D2"], na.rm = TRUE),mean(jdRM$Reaches[jdRM$Time == "R2D2"])))*-1
SEs<- c(sd(cdRM$Reaches[cdRM$Time == "R2D2"])/sqrt(length(cdRM$Reaches[cdRM$Time == "R2D2"])),sd(tdRM$Reaches[tdRM$Time == "R2D2"], na.rm = TRUE)/sqrt(length(tdRM$Reaches[tdRM$Time == "R2D2"])),sd(jdRM$Reaches[jdRM$Time == "R2D2"])/sqrt(length(jdRM$Reaches[jdRM$Time == "R2D2"])))
segments(x0= 2.6, y0=means[1]+SEs[1], x1 = 2.6, y1 = means[1]-SEs[1], col = "dodgerblue4")
segments(x0= 2.8, y0=means[2]+SEs[2], x1 = 2.8, y1 = means[2]-SEs[2], col = "sienna2")
segments(x0= 3, y0=means[3]+SEs[3], x1 = 3, y1 = means[3]-SEs[3], col = 'mediumseagreen')
points(y=means, x = c(2.6,2.8,3), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
points(x = c(rep(2.6, times = length(cdRM$Reaches[cdRM$Time == "R2D2"]))),y= (cdRM$Reaches[cdRM$Time == "R2D2"])*-1, col = "dodgerblue4")
points(x = c(rep(2.8, times = length(tdRM$Reaches[tdRM$Time == "R2D2"]))),y= (tdRM$Reaches[tdRM$Time == "R2D2"])*-1, col = "sienna2")
points(x = c(rep(3, times = length(jdRM$Reaches[jdRM$Time == "R2D2"]))),y= (jdRM$Reaches[jdRM$Time == "R2D2"])*-1, col = "mediumseagreen")


means<- c(mean(cdRM$Reaches[cdRM$Time == "EC"]),mean(tdRM$Reaches[tdRM$Time == "EC"]),mean(jdRM$Reaches[jdRM$Time == "EC"]))*-1
SEs<- c(sd(cdRM$Reaches[cdRM$Time == "EC"])/sqrt(length(cdRM$Reaches[cdRM$Time == "EC"])),sd(tdRM$Reaches[tdRM$Time == "EC"])/sqrt(length(tdRM$Reaches[tdRM$Time == "EC"])),sd(jdRM$Reaches[jdRM$Time == "EC"])/sqrt(length(jdRM$Reaches[jdRM$Time == "EC"])))
points(y=means, x = c(3.5,3.7,3.9), col = c("dodgerblue4","sienna2", 'mediumseagreen'), pch = 15, cex = 1.5)
segments(x0= 3.5, y0=means[1]+SEs[1], x1 = 3.5, y1 = means[1]-SEs[1], col = "dodgerblue4")
segments(x0= 3.7, y0=means[2]+SEs[2], x1 = 3.7, y1 = means[2]-SEs[2], col = "sienna2")
segments(x0= 3.9, y0=means[3]+SEs[3], x1 = 3.9, y1 = means[3]-SEs[3], col = 'mediumseagreen')
points(x = c(rep(3.5, times = length(cdRM$Reaches[cdRM$Time == "EC"]))),y= (cdRM$Reaches[cdRM$Time == "EC"])*-1, col = "dodgerblue4")
points(x = c(rep(3.7, times = length(tdRM$Reaches[tdRM$Time == "EC"]))),y= (tdRM$Reaches[tdRM$Time == "EC"])*-1, col = "sienna2")
points(x = c(rep(3.9, times = length(jdRM$Reaches[jdRM$Time == "EC"]))),y= (jdRM$Reaches[jdRM$Time == "EC"])*-1, col = "mediumseagreen")

legend(
  .5,
  -10,
  legend = c("Continuous", "Terminal", "Cursor Jump", "Rotation"),
  col = c("dodgerblue4", "sienna2","mediumseagreen", "Black"),
  lty = c(1),
  lwd = c(1),
  bty = 'n',
  cex = 1.25
)



}     







RegressionPLot <- function() {

  ##Get no-cursor data
  cdnc<- read.csv('ana/condition 1 trialtype 0_cuttoff0.7.csv', header = TRUE)
  cdncleaned<- Cleandata('ana/condition 1 trialtype 0_cuttoff0.7.csv')
  cdnc<-data.frame(cdnc$rot[17:160],cdncleaned)
  td<- read.csv('ana/condition 3 trialtype 0_cuttoff0.7.csv', header = TRUE)
  td1<- Cleandata('ana/condition 3 trialtype 0_cuttoff0.7.csv')
  tdnc<-data.frame(td$rot[17:160],td1)
  rm(td1)
  jd<- read.csv('ana/condition 4 trialtype 0_cuttoff0.7.csv', header = TRUE)
  jd1<- Cleandata('ana/condition 4 trialtype 0_cuttoff0.7.csv')
  jdnc<-data.frame(jd$rot[17:160],jd1)
  jdnc<- jdnc[,-c(6,8,13)]
  rm(jd1)
  
  cdRM<-ANOVAcombine(cdnc)
  Contnc<-cdRM$Reaches[cdRM$Time == "EC"]
  tdRM<-ANOVAcombine(tdnc)
  Termnc<-tdRM$Reaches[tdRM$Time == "EC"]
  jdRM<-ANOVAcombine(jdnc)
  Jumpnc<-jdRM$Reaches[jdRM$Time == "EC"]
  
  ##get reach data
  
  cd<- read.csv('ana/condition 1 trialtype 1_cuttoff0.7.csv', header = TRUE)
  cd1<- Cleandata('ana/condition 1 trialtype 1_cuttoff0.7.csv')
  cd<-data.frame(cd$rot[17:160],cd1)
  rm(cd1)
  td<- read.csv('ana/condition 3 trialtype 1_cuttoff0.7.csv', header = TRUE)
  td1<- Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv')
  td<-data.frame(td$rot[17:160],td1)
  rm(td1)
  jd<- read.csv('ana/condition 4 trialtype 1_cuttoff0.7.csv', header = TRUE)
  jd1<- Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv')
  jd<-data.frame(jd$rot[17:160],jd1)
  jd<- jd[,-c(6,8,13)]
  rm(jd1)
  
  cdRMR<-ANOVAcombine(cd)
  ContR<-cdRMR$Reaches[cdRMR$Time == "EC"]
  tdRMR<-ANOVAcombine(td)
  TermR<-tdRMR$Reaches[tdRMR$Time == "EC"]
  jdRMR<-ANOVAcombine(jd)
  JumpR<-jdRMR$Reaches[jdRMR$Time == "EC"]
  
  
  
  
  
  plot(
    ContR ~ Contnc,
    col = "dodgerblue4",
    xlab = 'No-Cursors',
    ylab = 'Reaches',
    main = 'Regression During Error Clamp',
    xlim = c(-45, 45),
    ylim = c(-45, 45),
    axes = FALSE, asp = 1, cex.lab = 1.25
  )
  axis(2,
       at = c( -45,-25, 0, 25,45),
       cex.axis = 1.2, las = 2)
  axis(1,
       at = c( -45,-25, 0, 25 ,45),
       cex.axis = 1.2)
  lines(x = c(-45:45), y = rep(0, times = length(-45:45)), lty = 3)
  abline(v = c(0), lty = 3)
  lm<-plotRegressionWithCI(ContR, Contnc, colors = c(rgb(0.04, 0.3, .5, 0.2), 'dodgerblue4'))
  pr<-summary(lm)$r.squared
  print(summary(lm)$coefficients[2,4])

  
  points(JumpR ~ Jumpnc, col = "mediumseagreen" )
  em<-plotRegressionWithCI(JumpR, Jumpnc, colors = c(rgb(0.23, 0.70, .44, 0.2), "mediumseagreen"))
  er<-summary(em)$r.squared
  print(summary(em)$coefficients[2,4])
  
  
  points(TermR ~ Termnc, col = "Sienna2")
  tm<-plotRegressionWithCI(TermR, Termnc, colors = c(rgb(0.93, 0.47, .26, 0.2), "sienna2"))
  tr<-summary(tm)$r.squared
  print(summary(tm)$coefficients[2,4])


  
  label1<- sprintf("Continuous, r2=%.2f", pr)
  label2<- sprintf("Terminal, r2=%.2f", tr)
  label3<- sprintf("Cursor Jump, r2=%.2f", er)
  legend(
    -50,
    40,
    legend = c(
      label1,
      label2,
      label3
    ),
    col = c("dodgerblue4", "sienna2", 'mediumseagreen'),
    lty = c(1, 1, 1),
    lwd = c(2, 2, 2),
    bty = 'n'
  )
  
  
}




















plotRegressionWithCI <-
  function(X, Y, colors = c('#99999999', 'black')) {
    # fit regression model
    this.lm <- lm(Y ~ X)
    
    # where is the interesting data
    pointlocs <- seq(min(X, na.rm = TRUE), max(X, na.rm = TRUE), .1)
    
    # get the confidence interval
    y1 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "upr"]
    y2 = predict(this.lm,
                 newdata = data.frame(X = pointlocs),
                 interval =
                   "confidence")[, "lwr"]
    
    # show the confidence interval
    polygon(c(pointlocs, rev(pointlocs)),
            c(y1, rev(y2)),
            col = colors[1],
            border = NA)
    
    # and show a regression line:
    lines(
      range(X, na.rm = TRUE),
      predict(this.lm, newdata = data.frame(X = range(X, na.rm = TRUE))),
      col = colors[2],
      lwd = 2
    )
    return(this.lm)
  }

