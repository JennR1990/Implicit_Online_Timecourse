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
  