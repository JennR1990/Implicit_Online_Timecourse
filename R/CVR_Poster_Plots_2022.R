
plotreachestogether<- function() {
cd<- read.csv('ana/condition 1 trialtype 1_cuttoff0.7.csv', header = TRUE)
cd1<- Cleandata('ana/condition 1 trialtype 1_cuttoff0.7.csv')
cd<-data.frame(cd$rot[17:160],cd1)
td<- read.csv('ana/condition 3 trialtype 1_cuttoff0.7.csv', header = TRUE)
td1<- Cleandata('ana/condition 3 trialtype 1_cuttoff0.7.csv')
td<-data.frame(td$rot[17:160],td1)
jd<- read.csv('ana/condition 4 trialtype 1_cuttoff0.7.csv', header = TRUE)
jd1<- Cleandata('ana/condition 4 trialtype 1_cuttoff0.7.csv')
jd<-data.frame(jd$rot[17:160],jd1)
jd<- jd[,-c(6,8,13)]


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





