createmovementfiles <- function(){
  
for (cond in c(0,1,3,4,5)){
goodps <- getgoodp(cond)

for (i in goodps){
  setwd("~/Desktop/grad/ImplicitProcesses/itc/data")
num <- as.character(i)
filenames <- list.files(pattern = num)
df <- read.csv(filenames[1], header = TRUE)

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

for (trialnum in c(1:dim(df)[1])) {
  print(trialnum)
  x <- convertCellToNumVector(df$mousex_rel[trialnum])
  allx <- c(allx,x)
  y <- convertCellToNumVector(df$mousey_rel[trialnum])
  ally <- c(ally,y)
  #plot(x[s == 2],y[s == 2],type="l")
  s <- convertCellToNumVector(df$step[trialnum])
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
  print(max(d))
  idx <- which(d > 0.8)[1]
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

reachsamples <<- data.frame(allx, ally, alls, alltrials)
output <- sprintf("%s Reach Movements.csv", num)
setwd("~/Desktop/grad/ImplicitProcesses/Good participants")
write.csv(reachsamples, file = output, quote = FALSE, row.names = FALSE)
}
}
}

getgoodp <- function(condition = 1){
newdata <- read.csv("~/Desktop/grad/ImplicitProcesses/Participant Info.csv", header = TRUE)

pnums <- newdata$participants[newdata$conditions == condition]
data <- read.csv(sprintf("~/Desktop/grad/ImplicitProcesses/condition %d trialtype 1.csv", condition), header = TRUE)
goodps <- pnums[which((colMeans(data[121:124, 2:ncol(data)]) >= -67.5) & (colMeans(data[121:124, 2:ncol(data)]) <= -22.5))]
return(goodps)
}




