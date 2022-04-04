#Used these codes to find problematic subjects
#write.csv(reachsamples, file = "226744 Reach Movements.csv", quote = FALSE, row.names = FALSE)
#df <- read.csv("226396_implicit_time_2021-05-28_13h58.11.897.csv", header = TRUE)
#cond0 <- newdata$participants[newdata$conditions == 0]
#cond0df <- read.csv("condition 3 trialtype 1.csv", header = TRUE)
#cond0df[124,2:24]


#Codes to plot individual participants full reaches
#pnum <- c(222334, 226420, 226444, 226939, 226264, 226351, 226744, 226876)
#pnum <- c(222031, 226267, 226414, 226759, 226135, 226465, 226156, 226369)
plotsubjects <- function(cond){
  library(gtools)
pnum <- getgoodp(cond)
exp <- c("Continious","2","Terminal","Cursor-Jump","Target-Jump")
setwd("~/Desktop/grad/ImplicitProcesses/Good participants")

#pnum <- participants[exp == cond]
filenames <- c()
tnum <- 241:248

for (i in pnum){
  filenames <- c(filenames, sprintf("%d Reach Movements.csv", i))}
output <- sprintf("../Figures/Individual %s Reaches.svg", exp[cond])
svglite(file = output, width = 8, height = 40)
par(mfcol = c(length(pnum), 2))
#layout(matrix(c(1:length(pnum)), ncol = 2, byrow = FALSE))
for (task in c("training", "no-cursor")){
  counter <- 1
  participant <- 1
  for (file in filenames){
    print(file)
    if (task == "training"){
      title <- sprintf("Last 4 Trials of Training for p%d", pnum[participant])
      trials <- tnum[odd(tnum)]
    } else{
      title <- sprintf("Last 4 Trials of No-Cursors for p%d", pnum[participant])
      trials <- tnum[even(tnum)]
    }
    
   #if (task == "training"){
   #   trials <- tnum[odd(tnum)]
   # } else {
    #  trials <- tnum[even(tnum)]
   # }
    df <- read.csv(file, header = TRUE)
    
    plot(df$allx[df$alltrials == trials[4] & df$alls == 1], df$ally[df$alltrials == trials[4] & df$alls == 1], type = "l", ylim = c(-1.25,1.25), xlim = c(-1.25,1.25), col = "blue", xlab = "X Distance", ylab = "Y Distance", main = title)
    for (step in c(1,2)){
      if (step == 1){
        color <- "blue"
      } else {
        color <- "red"
      }
      lines(df$allx[df$alltrials == trials[4] & df$alls == step], df$ally[df$alltrials == trials[4] & df$alls == step], type = "l", col = color)
      lines(df$allx[df$alltrials == trials[3] & df$alls == step], df$ally[df$alltrials == trials[3] & df$alls == step], type = "l", col = color)
      lines(df$allx[df$alltrials == trials[2] & df$alls == step], df$ally[df$alltrials == trials[2] & df$alls == step], type = "l", col = color)
      lines(df$allx[df$alltrials == trials[1] & df$alls == step], df$ally[df$alltrials == trials[1] & df$alls == step], type = "l", col = color)
      legend(-1, 1.3, legend = c("Step 1", "Step 2"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 2)
      
    }
    participant <- participant+1
  }
  counter <- counter+1
}
dev.off()
}

plotPausesubjects <- function(){
  library(gtools)
  pnum <- getgoodp(0)
  exp <- "Pause"
  setwd("~/Desktop/grad/ImplicitProcesses/Good participants")
  
#  pnum <- participants[exp == "Pause"]
  filenames <- c()
  trials <- 122:125
  
  for (i in pnum){
    filenames <- c(filenames, sprintf("%d Reach Movements.csv", i))}
  output <- sprintf("../Figures/Individual Pause Reaches.svg")
  svglite(file = output, width = 8, height = 40)
  par(mfrow = c(10, 2))
  participant = 1
    for (file in filenames){
        title <- sprintf("Last 4 Trials of Training for p%d", pnum[participant])
        
      
      #if (task == "training"){
      #   trials <- tnum[odd(tnum)]
      # } else {
      #  trials <- tnum[even(tnum)]
      # }
      df <- read.csv(file, header = TRUE)
      
      plot(df$allx[df$alltrials == trials[4] & df$alls == 1], df$ally[df$alltrials == trials[4] & df$alls == 1], type = "l", ylim = c(-1.25,1.25), xlim = c(-1.25,1.25), col = "blue", xlab = "X Distance", ylab = "Y Distance", main = title)
      for (step in c(1,2)){
        if (step == 1){
          color <- "blue"
        } else {
          color <- "red"
        }
        lines(df$allx[df$alltrials == trials[4] & df$alls == step], df$ally[df$alltrials == trials[4] & df$alls == step], type = "l", col = color)
        lines(df$allx[df$alltrials == trials[3] & df$alls == step], df$ally[df$alltrials == trials[3] & df$alls == step], type = "l", col = color)
        lines(df$allx[df$alltrials == trials[2] & df$alls == step], df$ally[df$alltrials == trials[2] & df$alls == step], type = "l", col = color)
        lines(df$allx[df$alltrials == trials[1] & df$alls == step], df$ally[df$alltrials == trials[1] & df$alls == step], type = "l", col = color)
        legend(-1, 1.3, legend = c("Step 1", "Step 2"), col = c("blue", "red"), bty = "n", lty = 1, lwd = 2)
        
      }
      participant = participant+1
    }
  
  dev.off()
}

df <- read.csv("../condition 3 trialtype 1.csv", header = TRUE)
#df <- cbind(rot,df)
df <- df[,2:ncol(df)]*-1
output <- sprintf("../Figures/Terminal Reaches Learning Curves.svg")
svglite(file = output, width = 8, height = 40)
par(mfrow = c(14, 2))
for (i in 2:ncol(df)){
  title <- sprintf("participant %d", i-1)
  
plot(df[,i], type = "l", axes = FALSE, xlab = "Trials", ylab = "Deviations", main = title, ylim = c(-150,150))
axis(1, at = c(1, 25, 125, 133, 148), cex.axis = 1.25, las = 2)
axis(2, at = c(150, 100, 50, 0, -50, -100, -150), cex.axis = 1.25, las=2)
lines(c(133, 148), c(0, 0), lty = 2, col = rgb(0., 0., 0.))
lines(c(1, 25, 25, 125, 125, 133, 133), c(0, 0, 45, 45, -45, -45, 0), col = rgb(0., 0., 0.))
}
dev.off()



plot(apply(data[,2:ncol(data)], 1, fun, na.rm=T), type = 'l', xlab = "Trials", ylab = "Deviations", main = title, ylim = c(-45,45), col = "Blue", axes = FALSE, cex.lab = 1.25)

