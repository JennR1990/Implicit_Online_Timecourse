plotschedule<- function(){
  
  
  plot(NULL, xlab = "Trials", ylab = "Perturbation", main = "Rotation Schedule", ylim = c(-45,45), xlim = c(0,144), axes = FALSE, cex.lab = 1.25)
  lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
  lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
}


plotdata <- function(filename, cond, task, fun){
  data <- read.csv(filename, header = TRUE)
  data <- removeReachOutliers(data)
  method <- as.character(fun)
  title <- sprintf("%s Deviations for Condition %d Using %s", task, cond, method)
  plot(apply(data[,2:ncol(data)], 1, fun, na.rm=T), type = 'l', xlab = "Trials", ylab = "Deviations", main = title, ylim = c(-45,45), col = "Blue", axes = FALSE, cex.lab = 1.25)
  points(x = rep(25, times = ncol(data) - 1), y = data[25, 2:ncol(data)])
  points(x = rep(123, times = ncol(data) - 1), y = data[123, 2:ncol(data)])
  lines(c(1, 25, 25, 125, 125, 133, 133), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
  lines(c(133, 148), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
  
  legend(
    5,
    40,
    legend = task,
    col = "Blue",
    lty = c(1),
    lwd = c(2),
    bty = 'n',
    cex = 1.25
  )
  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 25, 125, 133, 148), cex.axis = 1.25, las = 2)
}


plotindividualcurves<- function() {
  
  filenames<- c("summerana/condition 1 trialtype 1_cuttoff0.7.csv","summerana/condition 3 trialtype 1_cuttoff0.7.csv","summerana/condition 4 trialtype 1_cuttoff0.7.csv","summerana/condition 1 trialtype 0_cuttoff0.7.csv","summerana/condition 3 trialtype 0_cuttoff0.7.csv","summerana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s_Summer_individual_curves_updated.jpeg", names[i], task[i])
    jpeg(outputname, width = 15, height = 15, units = "in", res = 100)
    filename<- filenames[i]
    #data<-Cleandata(filename)
    data<-read.csv(filename, header = TRUE)
    
    
    if ( i %% 3 == 0){
    layout(matrix(c(1:(ncol(data))), nrow=(ceiling(ncol(data))/3), byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    } else {
    layout(matrix(c(1:(ncol(data))), nrow=(ceiling(ncol(data))/3), byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    #layout(matrix(c(1:(ncol(data)-1)), nrow=(ceiling(ncol(data))/3), byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    }  
    for (k in 2:ncol(data)){
    plot(data[17:160,k]*-1, type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
    lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
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



plotindividualcurvescleaned<- function() {
  
  filenames<- c("ana/condition 1 trialtype 1_cuttoff0.3.csv","ana/condition 3 trialtype 1_cuttoff0.3.csv","ana/condition 4 trialtype 1_cuttoff0.3.csv","ana/condition 1 trialtype 1_cuttoff0.7.csv","ana/condition 3 trialtype 1_cuttoff0.7.csv","ana/condition 4 trialtype 1_cuttoff0.7.csv","ana/condition 1 trialtype 0_cuttoff0.7.csv","ana/condition 3 trialtype 0_cuttoff0.7.csv","ana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s_individual_curves_cleaned.jpeg", names[i], task[i])
    jpeg(outputname, width = 15, height = 15, units = "in", res = 100)
    filename<- filenames[i]
    data<-Cleandata(filename)
    
    
    
    if ( i %% 3 == 0){
      layout(matrix(c(1:(ncol(data)+1)), nrow=6, byrow=TRUE), heights=c(2,2,2,2,2,2))
    } else {
      layout(matrix(c(1:(ncol(data))), nrow=(ncol(data))/3, byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    }  
    for (k in 1:ncol(data)){
      

      plot(data[,k], type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
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



plotblockscleaned<- function() {
  
  filenames<- c("ana/condition 1 trialtype 1_cuttoff0.3.csv","ana/condition 3 trialtype 1_cuttoff0.3.csv","ana/condition 4 trialtype 1_cuttoff0.3.csv","ana/condition 1 trialtype 1_cuttoff0.7.csv","ana/condition 3 trialtype 1_cuttoff0.7.csv","ana/condition 4 trialtype 1_cuttoff0.7.csv","ana/condition 1 trialtype 0_cuttoff0.7.csv","ana/condition 3 trialtype 0_cuttoff0.7.csv","ana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s_blocks_cleaned.jpeg", names[i], task[i])
    jpeg(outputname, width = 15, height = 15, units = "in", res = 100)
    filename<- filenames[i]
    data<-Cleandata(filename)
    
    

    if ( i %% 3 == 0){
      layout(matrix(c(1:(ncol(data)+1)), nrow=6, byrow=TRUE), heights=c(2,2,2,2,2,2))
    } else {
      layout(matrix(c(1:(ncol(data))), nrow=(ncol(data))/3, byrow=TRUE), heights=c(rep(2,times = (ncol(data))/3)))
    }  
    for (k in 1:ncol(data)){
      
      
      blocks<- c()     
      starts<- seq(from = 1, to = 144, by =4)
      stops<- seq(from = 4, to = 144, by =4)
      for (j in 1:36){
        start<- starts[j]
        stop<- stops[j]
      blocks<-  c(blocks,mean(data[start:stop,k], na.rm = TRUE))
        
      }
      
      
      
      
      plot(blocks, type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
      lines(c(1, 5, 5, 30, 30, 32, 32), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
      lines(c(32, 36), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
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
      axis(1, at = c(1, 5, 30, 32, 36), cex.axis = 1.25, las = 2)
      
    }
    dev.off()    
    
  }
  
  
}


plotcleandataseparately<- function() {
  
  filenames<- c("summerana/condition 1 trialtype 1_cuttoff0.7.csv","summerana/condition 3 trialtype 1_cuttoff0.7.csv","summerana/condition 4 trialtype 1_cuttoff0.7.csv","summerana/condition 1 trialtype 0_cuttoff0.7.csv","summerana/condition 3 trialtype 0_cuttoff0.7.csv","summerana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)","Continuous (.7)", "Terminal (.7)", "Cursor-Jump (.7)")
  task<- c("reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s Summer_forwardTarget.jpeg", names[i], task[i])
    jpeg(outputname)
    filename<- filenames[i]
    data<-Cleandata(filename)
    
    plot(apply(data, 1, median, na.rm=T)*-1, type = 'l', xlab = "Trials", ylab = "Deviations", main = names[i], ylim = c(-45,45), col = "Blue", axes = FALSE, cex.lab = 1.25)
    lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
    lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
    dataCIs <- trialCI(data = data)
    dataCIs <- dataCIs*-1
    x <-  c(c(1:144), rev(c(1:144)))
    y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
    polygon(x, y, col = rgb(0, 0, .5, 0.2), border = NA)
    
    
    legend(
      5,
      -40,
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






for (k in 2:ncol(data)){
  plot(as.numeric(unlist(data[17:160,k])), type = 'l', xlab = "Trials", ylab = "Deviations", main = "continuous reaches", ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
  lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
  lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))

  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
  
}
