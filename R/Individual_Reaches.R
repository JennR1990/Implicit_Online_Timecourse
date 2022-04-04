newdata <- getpinfo()
cond1p <- newdata$participants[newdata$conditions == 1]
cond3p <- newdata$participants[newdata$conditions == 3]
cond4p <- newdata$participants[newdata$conditions == 4]

cond1f <- newdata$files[newdata$conditions == 1]
cond3f <- newdata$files[newdata$conditions == 3]
cond4f <- newdata$files[newdata$conditions == 4]
setwd("~/Desktop/grad/ImplicitProcesses/itcwithW22/itc3tabs/data")

for (i in 1:length(cond1f)) {
  reachsamp <- getreachsamples(cond1f[i])
  outputname <- sprintf("reaches/%s Reach Movements.csv", cond1p[i])
  write.csv(reachsamp, file = outputname, quote = FALSE, row.names = FALSE)
}
for (i in 1:length(cond3f)) {
  reachsamp <- getreachsamples(cond3f[i])
  outputname <- sprintf("reaches/%s Reach Movements.csv", cond3p[i])
  write.csv(reachsamp, file = outputname, quote = FALSE, row.names = FALSE)
}
for (i in 1:length(cond4f)) {
  reachsamp <- getreachsamples(cond4f[i])
  outputname <- sprintf("reaches/%s Reach Movements.csv", cond4p[i])
  write.csv(reachsamp, file = outputname, quote = FALSE, row.names = FALSE)
}


getreachsamples <- function(filename){
  cutoff = 0.7
# if the file can't be read, return empty list for now
df <- NULL
try(df <- read.csv(filename, stringsAsFactors = F), silent = TRUE)
if (is.null(df)) {
  return(list())
}

# set up vectors for relevant data:
trialno <- c()            #trialNum
target <- c()
trialtype <-c()              #trialsType
reachdeviation_deg <- c()
participant <- c()
phase <- c()

# remove empty lines:
#df <- df[which(!is.na(df$trialsNum)),]
#df <- df[which(df$trialsNum == 2),]

# loop through all trials
#plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
allx <- c()
ally <- c()
alls <- c()
alltrials <- c()
cutdate <- "2021-10-13_01h00.00.000"
#cutdate <- "2021-11-13_01h00.00.000"
if (cutdate < unique(df$date)) {
  
  for (trialnum in c(1:dim(df)[1])) {
    #print(trialnum)
    x <- convertCellToNumVector1(df$mousex_rel[trialnum])
    allx <- c(allx,x)
    y <- convertCellToNumVector1(df$mousey_rel[trialnum])
    ally <- c(ally,y)
    #plot(x[s == 2],y[s == 2],type="l")
    s <- convertCellToNumVector1(df$step[trialnum])
    alls <- c(alls,s)
    alltrials <- c(alltrials, rep(trialnum, times = length(x)))
    m <- df$trialtype[trialnum]
    a <- df$target[trialnum]
    p <- df$participant[trialnum]
    ph <- df$phase[trialnum]
    
    # remove stuff that is not step==2
    step2idx = which(s == 1)
    x <- x[step2idx]
    y <- y[step2idx]
    
    #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    #lines(c(0,1),c(0,0),col='black')
    #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
    
    # get first point beyond some distance (home-target is 40% of height of participant's screen)
    # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
    # sebastian data ranges from 0-1 so 0.7 is 70%
    d <- sqrt(x^2 + y^2)
    #print(max(d))
    idx <- which(d > cutoff)[1]
    x <- x[idx]
    y <- y[idx]
    
    #points(x,y,col='red')
    
    # get angular deviation of reach from target angle:
    rotcoords <- rotateTrajectory(x,y,-a)
    x <- rotcoords[1]
    y <- rotcoords[2]
    
    rd <- (atan2(y, x) / pi) * 180
    
    
    #text(0,-0.1,sprintf('%0.3f',rd))
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    target <- c(target, a)
    trialtype <-c(trialtype, m)
    reachdeviation_deg <- c(reachdeviation_deg, rd)
    participant <- c(participant, p)
    phase <- c(phase, ph)
    
  }
} else{
  for (trialnum in c(1:dim(df)[1])) {
    #print(trialnum)
    x <- convertCellToNumVector1(df$mousex_rel[trialnum])
    allx <- c(allx,x)
    y <- convertCellToNumVector1(df$mousey_rel[trialnum])
    ally <- c(ally,y)
    #plot(x[s == 2],y[s == 2],type="l")
    s <- convertCellToNumVector1(df$step[trialnum])
    alls <- c(alls,s)
    alltrials <- c(alltrials, rep(trialnum, times = length(x)))
    m <- df$trialtype[trialnum]
    a <- df$target[trialnum]
    p <- df$participant[trialnum]
    ph <- df$phase[trialnum]
    
    # remove stuff that is not step==2
    step2idx = which(s == 1)
    x <- x[step2idx]
    y <- y[step2idx]
    
    #plot(x,y,type='l',col='blue',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2))
    #lines(c(0,1),c(0,0),col='black')
    #points(c(0,cos((a/180)*pi)),c(0,sin((a/180)*pi)),col='black')
    
    # get first point beyond some distance (home-target is 40% of height of participant's screen)
    # we can set a cutoff at 30% of home-target distance (30% of .4 = .12)
    # sebastian data ranges from 0-1 so 0.7 is 70%
    d <- sqrt(x^2 + y^2)
    #print(max(d))
    idx <- which(d > cutoff)[1]
    x <- x[idx]
    y <- y[idx]
    
    #points(x,y,col='red')
    
    # get angular deviation of reach from target angle:
    rotcoords <- rotateTrajectory(x,y,-a)
    x <- rotcoords[1]
    y <- rotcoords[2]
    
    rd <- (atan2(y, x) / pi) * 180
    
    
    #text(0,-0.1,sprintf('%0.3f',rd))
    
    # store in vectors:
    trialno <- c(trialno, trialnum)
    target <- c(target, a)
    trialtype <-c(trialtype, m)
    reachdeviation_deg <- c(reachdeviation_deg, rd)
    participant <- c(participant, p)
    phase <- c(phase, ph)
    
  }
}

reachsamples <<- data.frame(allx, ally, alls, alltrials)
}


plotreaches <- function(data, condition){
pnum = data
filenames <- c()
tnum <- 241:248
cond = condition
library(gtools)

for (i in pnum){
  filenames <- c(filenames, sprintf("%s Reach Movements.csv", i))}
setwd("~/Desktop/grad/ImplicitProcesses/itcwithW22/itc3tabs/data/reaches")
output <- sprintf("Figures/Individual %s Reaches.svg", cond)
svglite(file = output, width = 8, height = 40)
par(mfcol = c(length(pnum), 2))
#layout(matrix(c(1:length(pnum)), ncol = 2, byrow = FALSE))
for (task in c("training", "no-cursor")){
  counter <- 1
  participant <- 1
  for (file in filenames){
    print(file)
    if (task == "training"){
      title <- sprintf("Last 4 Trials of Training for p%s", pnum[participant])
      trials <- tnum[odd(tnum)]
    } else{
      title <- sprintf("Last 4 Trials of No-Cursors for p%s", pnum[participant])
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
