

plotindividualcurves<- function() {
  
  filenames<- c("ana/condition 1 trialtype 1_cuttoff0.3.csv","ana/condition 3 trialtype 1_cuttoff0.3.csv","ana/condition 4 trialtype 1_cuttoff0.3.csv","ana/condition 1 trialtype 1_cuttoff0.7.csv","ana/condition 3 trialtype 1_cuttoff0.7.csv","ana/condition 4 trialtype 1_cuttoff0.7.csv","ana/condition 1 trialtype 0_cuttoff0.7.csv","ana/condition 3 trialtype 0_cuttoff0.7.csv","ana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s_individual_curves.jpeg", names[i], task[i])
    jpeg(outputname, width = 15, height = 15, units = "in", res = 100)
    filename<- filenames[i]
    #data<-Cleandata(filename)
    data<-read.csv(filename, header = TRUE)
    
    
    if ( i %% 3 == 0){
    layout(matrix(c(1:(ncol(data))), nrow=(ncol(data))/3, byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    } else {
    layout(matrix(c(1:(ncol(data)-1)), nrow=(ncol(data))/3, byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    }  
    for (k in 2:ncol(data)){
    plot(data[17:160,k], type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
    lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
    lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
    # legend(
    #   5,
    #   40,
    #   legend = task[i],
    #   col = "Blue",
    #   lty = c(1),
    #   lwd = c(2),
    #   bty = 'n',
    #   cex = 1.25
    # )
    axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
         las = 2)
    axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)

    }
    dev.off()    
    
  }
  
  
}
















plotcleandataseparately<- function() {
  
  filenames<- c("ana/condition 1 trialtype 1_cuttoff0.3.csv","ana/condition 3 trialtype 1_cuttoff0.3.csv","ana/condition 4 trialtype 1_cuttoff0.3.csv","ana/condition 1 trialtype 1_cuttoff0.7.csv","ana/condition 3 trialtype 1_cuttoff0.7.csv","ana/condition 4 trialtype 1_cuttoff0.7.csv","ana/condition 1 trialtype 0_cuttoff0.7.csv","ana/condition 3 trialtype 0_cuttoff0.7.csv","ana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s.jpeg", names[i], task[i])
    jpeg(outputname)
    filename<- filenames[i]
    data<-Cleandata(filename)
    
    plot(apply(data, 1, median, na.rm=T), type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-45,45), col = "Blue", axes = FALSE, cex.lab = 1.25)
    lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
    lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
    dataCIs <- trialCI(data = data)
    dataCIs <- dataCIs
    x <-  c(c(1:144), rev(c(1:144)))
    y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
    polygon(x, y, col = rgb(0, 0, .5, 0.2), border = NA)
    
    
    legend(
      5,
      40,
      legend = task[i],
      col = "Blue",
      lty = c(1),
      lwd = c(2),
      bty = 'n',
      cex = 1.25
    )
    axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
         las = 2)
    axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
    dev.off()
  }
}


plotcleandatatogether<- function() {
  
  filenames<- c("ana/condition 1 trialtype 1_cuttoff0.3.csv","ana/condition 1 trialtype 1_cuttoff0.7.csv","ana/condition 1 trialtype 0_cuttoff0.7.csv","ana/condition 3 trialtype 1_cuttoff0.3.csv","ana/condition 3 trialtype 1_cuttoff0.7.csv","ana/condition 3 trialtype 0_cuttoff0.7.csv","ana/condition 4 trialtype 1_cuttoff0.3.csv","ana/condition 4 trialtype 1_cuttoff0.7.csv","ana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous" ,"Continuous" ,"Continuous" , "Terminal","Terminal","Terminal", "Cursor-Jump")
  
  points<- c(1,4,7)
  for (i in points) {
    
    outputname<- sprintf("figs/%s_mean.jpeg", names[i])
    jpeg(outputname)
    filename<- filenames[i]
    data<-Cleandata(filename)
    
    plot(apply(data, 1, mean, na.rm=T), type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-45,45), col = "Cyan", axes = FALSE, cex.lab = 1.25)
    
    dataCIs <- trialCI(data = data)
    dataCIs <- dataCIs
    x <-  c(c(1:144), rev(c(1:144)))
    y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
    polygon(x, y, col = rgb(0, 1, 1, 0.2), border = NA)
    
    
    
    
    lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
    lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
    filename<- filenames[i+1]
    data<-Cleandata(filename)
    lines(apply(data, 1, median, na.rm=T), type = "l", col = "Dark Blue", lty = 2 )
    
    dataCIs <- trialCI(data = data)
    dataCIs <- dataCIs
    x <-  c(c(1:144), rev(c(1:144)))
    y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
    polygon(x, y, col = rgb(0, 0, .5, 0.2), border = NA)
    
    
    filename<- filenames[i+2]
    data<-Cleandata(filename)
    lines(apply(data, 1, median, na.rm=T), type = "l", col = "red")
    
    dataCIs <- trialCI(data = data)
    dataCIs <- dataCIs
    x <-  c(c(1:144), rev(c(1:144)))
    y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
    polygon(x, y, col = rgb(1, 0, 0, 0.2), border = NA)
    
    legend(
      5,
      40,
      legend = c("Reaches (.3)", "Reaches (.7)", "No-Cursor (.7)"),
      col = c("cyan", "Dark Blue", "Red"),
      lty = c(1,2,1),
      lwd = c(2,2,2),
      bty = 'n',
      cex = 1.25
    )
    axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
         las = 2)
    axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
    dev.off()
  }
}


trialCI <- function(data) {
  AllCIs <- data.frame()
  for (trial in 1:nrow(data)) {
    y <- unlist(data[trial,])
    CItrial <- t.interval(unlist(y))
    if (prod(dim(AllCIs)) == 0) {
      AllCIs <- CItrial
    } else {
      AllCIs <- rbind(AllCIs, CItrial)
    }
  }
  return(AllCIs)
}


t.interval = function(data, variance = var(data, na.rm = TRUE), conf.level = 0.95) {
  z = qt((1 - conf.level) / 2,
         df = length(data) - 1,
         lower.tail = FALSE)
  
  xbar = mean(data, na.rm = TRUE)
  sdx = sqrt(variance / length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}


Cleandata<- function (filename){
data<-read.csv(filename, header = TRUE)

aligned<-data[data$rot =="baseline",2:16]
rot1<-data[data$rot =="rotation",2:16]
counter<-data[data$rot =="counter",2:16]
errorclamp<-data[data$rot =="errorclamp",2:16]



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