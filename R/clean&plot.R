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
  
  filenames<- c("forwardana/condition 1 trialtype 1_cuttoff0.3.csv","forwardana/condition 3 trialtype 1_cuttoff0.3.csv"
                ,"forwardana/condition 4 trialtype 1_cuttoff0.3.csv","forwardana/condition 1 trialtype 0_cuttoff0.3.csv"
                ,"forwardana/condition 3 trialtype 0_cuttoff0.3.csv","forwardana/condition 4 trialtype 0_cuttoff0.3.csv")
  names<- c("Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)","Continuous (.3)", "Terminal (.3)", "Cursor-Jump (.3)")
  task<- c("reaches","reaches","reaches","No-Cursors","No-Cursors","No-Cursors")
  
  for (i in 1:length(filenames)) {
    
    outputname<- sprintf("figs/%s %s_Forward_individual_curves_cuttoff3.svg", names[i], task[i])
    svg(outputname, width = 4, height = 80)
    filename<- filenames[i]
    data<-read.csv(filename, header = TRUE)
    data1<- Cleandata(filename)
    data<- data[,-1]
    
    layout(matrix(1:ncol(data), nrow=ncol(data), byrow=TRUE))
    
    
    TR<- c()
    
    for (k in 1:ncol(data)){
      plot(data[17:136,k]*-1, type = 'l', xlab = "Trials", ylab = "Deviations", main = as.character(k), ylim = c(-60,60), col = "red", axes = FALSE, cex.lab = 1.25)
      sum(is.na(data[17:136,k]))
      Removed<-sum(is.na(data1[1:120,k]))
      lines(data1[1:120,k]*-1, type = 'l', col = "blue")
      lines(c(1, 20, 20, 120, 120), c(0, 0, 45, 45, 45), col = rgb(0., 0., 0.))
      text(x=105, y = -10, labels = sprintf("trials removed %s", Removed))
      
      TR<- c(TR,Removed)
      
      
      #lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
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
      axis(2, at = c(-45, -35,-25,-15,-5, 0,5,15, 25,35, 45), cex.axis = 1.25,
           las = 2)
      axis(1, at = c(1, 20, 120), cex.axis = 1.25, las = 2)
      
    }
    outputname<- sprintf("figs/%s %s_Forward_individual_curves_trials_removed.csv", names[i], task[i])
    write.csv(TR, file = outputname, quote = FALSE, row.names = FALSE )
    dev.off()    
  }
  
  
}

plottrialsremoveddensity<- function() {
  TR<-read.csv(file = 'figs/All Trials Removed.csv', header = TRUE)
  svg("figs/All TRials Removed Density.svg", width = 4, height = 4)
  d<- density(TR$Trials.Removed, bw = 1)
  plot(d, main = "reaches", xlab = "Trials Removed")
  dev.off()
  
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
  
  filenames<- c("summerana/condition 1 trialtype 1_cuttoff0.7.csv","summerana/condition 1 trialtype 0_cuttoff0.7.csv"
                ,"summerana/condition 3 trialtype 1_cuttoff0.7.csv","summerana/condition 3 trialtype 0_cuttoff0.7.csv"
                ,"summerana/condition 4 trialtype 1_cuttoff0.7.csv","summerana/condition 4 trialtype 0_cuttoff0.7.csv")
  names<- c("Continuous" ,"Continuous" ,"Terminal","Terminal", "Cursor-Jump", "Cursor-Jump")
  
  
  
  outputname<- "figs/Forward_No-Cursors.jpeg"
  jpeg(outputname)
  filename<- filenames[2]
  data<-Cleandata(filename)
  
  plot((apply(data, 1, mean, na.rm=T))*-1, type = 'l', xlab = "Trials", ylab = "Deviations", main = "No-Cursors", ylim = c(-45,45), col = "dodgerblue", axes = FALSE, cex.lab = 1.25)
  
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(.11, .56, .99, 0.2), border = NA)
  
  
  
  
  lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
  lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
  filename<- filenames[4]
  data<-Cleandata(filename)
  lines((apply(data, 1, median, na.rm=T))*-1, type = "l", col = "sienna2" )
  
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(.93, .47, .28, 0.2), border = NA)
  
  
  filename<- filenames[6]
  data<-Cleandata(filename)
  lines((apply(data, 1, median, na.rm=T))*-1, type = "l", col = "mediumseagreen")
  
  dataCIs <- trialCI(data = data)
  dataCIs <- dataCIs*-1
  x <-  c(c(1:144), rev(c(1:144)))
  y <- c(dataCIs[, 1], rev(dataCIs[, 2]))
  polygon(x, y, col = rgb(.23, .69, 0.44, 0.2), border = NA)
  
  legend(
    30,
    -5,
    legend = c("Continuous", "Terminal", "Cursor-Jump"),
    col = c( "dodgerblue", "sienna2", "mediumseagreen"),
    lty = c(1),
    lwd = c(2,2),
    bty = 'n',
    cex = 1.25
  )
  axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
       las = 2)
  axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
  dev.off()
  
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



# 
# 
# 
# for (k in 2:ncol(data)){
#   plot(as.numeric(unlist(data[17:160,k])), type = 'l', xlab = "Trials", ylab = "Deviations", main = "continuous reaches", ylim = c(-60,60), col = "Blue", axes = FALSE, cex.lab = 1.25)
#   lines(c(1, 20, 20, 120, 120, 128, 128), c(0, 0, -45, -45, 45, 45, 0), col = rgb(0., 0., 0.))
#   lines(c(128, 144), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
# 
#   axis(2, at = c(-45, -25, 0, 25, 45), cex.axis = 1.25,
#        las = 2)
#   axis(1, at = c(1, 20, 120, 128, 144), cex.axis = 1.25, las = 2)
#   
# }





plotlearningasymptotes<- function() {
  
  #filenames<- c("summerana/condition 1 trialtype 1_cuttoff0.7.csv","summerana/condition 1 trialtype 0_cuttoff0.7.csv","summerana/condition 3 trialtype 1_cuttoff0.7.csv","summerana/condition 3 trialtype 0_cuttoff0.7.csv","summerana/condition 4 trialtype 1_cuttoff0.7.csv","summerana/condition 4 trialtype 0_cuttoff0.7.csv")
  filenames<- c("forwardana/continuous_reaches.csv","forwardana/continuous_nocursors.csv","forwardana/terminal_reaches.csv"
                , "forwardana/terminal_nocursors.csv", "forwardana/cursorjump_reaches.csv", "forwardana/cursorJump_nocursors.csv")
  
                                                                                              
  
  df<- read.csv(filenames[2])
  newdf<-df
  contmedians<-apply(newdf, 2, median, na.rm=TRUE)
  
  df<- read.csv(filenames[4])
  newdf<-df
  termmedians<-apply(newdf, 2, median, na.rm=TRUE)
  
  
  df<- read.csv(filenames[6])
  newdf<-df
  Jumpmedians<-apply(newdf, 2, median, na.rm=TRUE)
  
  
  plot(100,100,xlim = c(.75,2.25), ylim = c(-10,50), xlab = "Feedback Group", ylab = "Hand Direction [°]", axes = FALSE, main = "Median Reach Aftereffects", cex = 1.25, cex.lab = 1.25)
  axis(1, at=c(1,1.5,2), labels = c("Continuous", 'Terminal', 'Cursor Jump'), cex.axis = 1.25)
  axis(2, at = c(-10,0,10,20,30,40,50), las = 2, cex.axis = 1.25)
  points(x = rep(1, times = length(contmedians)),y=contmedians*-1, col = "dodgerblue")
  points(x = rep(1.5, times = length(termmedians)),y=termmedians*-1, col = "sienna2")
  points(x = rep(2, times = length(Jumpmedians)),y=Jumpmedians*-1, col = "mediumseagreen")
  abline(h=45, lty= 3, col= "grey")
  abline(h=0, lty= 3, col= "grey")
  
  lines(x = c(.95,1.05),y=c(mean(contmedians*-1, na.rm=TRUE),mean(contmedians*-1, na.rm=TRUE)), col = "dodgerblue")
  lower<-t.interval(contmedians*-1)[1]
  upper<-t.interval(contmedians*-1)[2]
  x<- c(.95,1.05,1.05,.95)
  y<- c(lower,lower,upper,upper)
  polygon(x,y,col = rgb(0.04, 0.3, .5, 0.2), border = NA)
  d<- density(contmedians*-1, n=24,from = -10, to = 60, bw = 2)$y
  dX <- (d / sum(d)) 
  dY <- seq(-10,60,length.out = 24)
  polygon(x=c(0,dX,0)+1.07, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.04, 0.3, .5, 0.2))
  
  
  lines(x = c(1.45,1.55),y=c(mean(termmedians*-1),mean(termmedians*-1)), col = "sienna2")
  lower<-t.interval(termmedians*-1)[1]
  upper<-t.interval(termmedians*-1)[2]
  x<- c(1.45,1.55,1.55,1.45)
  y<- c(lower,lower,upper,upper)
  polygon(x,y,col = rgb(0.93, 0.47, .26, 0.2), border = NA)
  d<- density(termmedians*-1, n=length(termmedians),from = -10, to = 60, bw = 2)$y
  dX <- (d / sum(d)) 
  dY <- seq(-10,60,length.out = length(termmedians*-1))
  polygon(x=c(0,dX,0)+1.57, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.93, 0.47, .26, 0.2))
  
  
  lines(x = c(1.95,2.05),y=c(mean(Jumpmedians*-1),mean(Jumpmedians*-1)), col = "mediumseagreen")
  lower<-t.interval(Jumpmedians*-1)[1]
  upper<-t.interval(Jumpmedians*-1)[2]
  x<- c(1.95,2.05,2.05,1.95)
  y<- c(lower,lower,upper,upper)
  polygon(x,y,col = rgb(0.23, 0.70, .44, 0.2), border = NA)
  d<- density(Jumpmedians*-1, n=length(Jumpmedians*-1),from = -10, to = 60, bw = 2)$y
  dX <- (d / sum(d)) 
  dY <- seq(-10,60,length.out = length(Jumpmedians*-1))
  polygon(x=c(0,dX,0)+2.07, y=c(dY[1],dY,dY[length(dY)]), border=NA, col=rgb(0.23, 0.70, .44, 0.2))
  
  
  
}

