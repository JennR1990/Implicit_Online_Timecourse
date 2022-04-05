getpinfo <- function() {
setwd("/Jenn/Documents/Implicit_Online_Timecourse/data")
files <- list.files(pattern = "csv")
participants <- c()
conditions <- c()
dates <- c()
for (file in 1:length(files)) {
filename <- files[file]
df <- read.csv(filename,header = TRUE)
participants[file] <- unique(df$participant)
conditions[file] <- unique(df$condition)
dates[file] <- unique(df$date)
}
write.csv(data.frame(participants,conditions,dates,files), file = "../ana/Participant Info.csv", quote = FALSE, row.names = FALSE)
return(data.frame(participants,conditions,dates,files))
}


#.7 means 70% of the movement out from the target so thats good for endpoints, .3 for the training, 30%
# anything -15 to 75 for rotation phases
#-40 to 40 for aligned and clamp
#quick view, absolute 60.
#start looking at individual data
#find a way to look at it per target
getpangles <- function(cond = 1,type = 1, cutoff = 0.3) {
  newdata <- getpinfo()
filenames <- newdata$files[newdata$conditions == cond]
DF <- handleOneFile(filenames[1])
#pnums <- newdata$participants[newdata$conditions == 1]

if (cond == 0) {
  rd <- data.frame(matrix(NA,nrow(DF),length(filenames)))
} else {
rd <- data.frame(matrix(NA,nrow(DF)/2,length(filenames)))
}
rot <- c()
setwd("/Jenn/Documents/Implicit_Online_Timecourse/data")
for (file in 1:length(filenames)) {
  filename <- filenames[file]
  #pnum <- pnums[1]
  df <- handleOneFile(filename, cutoff)
  rd[ ,file] <- df$reachdeviation_deg[df$trialtype == type]
  rot <- df$phase[df$trialtype == type]
}
newnames <- c()
for (num in 1:length(filenames)) {
  newnames[num] <- sprintf("p%d",num)

}
setwd("/Jenn/Documents/Implicit_Online_Timecourse/ana")
colnames(rd) <- newnames
rd <- cbind(rot,rd)
rd <- data.frame(rd)

rd[is.na(rd)]<- 110
#if (type == 1){
#  trials<- 1:(length(rd[rd$rot == "rotation",1])/3)+(length(rd[rd$rot == "practice"| rd$rot == 'baseline',1])+67)
#  for (j in 2:ncol(rd)){
    
#    for (i in trials) {
#     if (rd[i,j]> -15){
 #       rd[i,j]<- NA
 #     } else if (rd[i,j]< -75) {
 #       rd[i,j]<- NA
  #    }else {
  #      print(j)
  #    }
  #  }
 # }
 # 
  #
#} else {
  
  #rd[rd > 15]<- NA
 # rd[rd < -105]<- NA
 # rd$rot <- rot
  
#}

rd[rd == 110] <- NA
outputname <- sprintf("condition %d trialtype %d_cuttoff%.1f.csv",cond,type, cutoff)
write.csv(rd,file=outputname,row.names = F,quote = F)
return(rd)

}

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\["', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub('"]', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, '","')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}

convertCellToNumVector1 <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}

handleOneFile <- function(filename, cutoff = .7) {
  
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
  
  # vectors as data frame columns:
  dfrd <- data.frame(trialno, target, trialtype, reachdeviation_deg, participant, phase)
  
  
  
  
  
  # tasklist <- list()
  # 
  # for (taskno in c(1,2,3)) {
  #   
  #   taskdf <- dfrd[which(dfrd$taskno == taskno),]
  #   if (dim(taskdf)[1] != 160) { next }
  #   
  #   numrots <- length(unique(taskdf$mirror))
  #   condition <- list('4'='abrupt','7'='stepped','51'='gradual')[[sprintf('%d',numrots)]]
  #   modifier <- sign(taskdf$mirror[120])
  #   
  #   taskdf$mirror <- taskdf$mirror * modifier
  #   taskdf$reachdeviation_deg <- taskdf$reachdeviation_deg * modifier
  #   
  #   taskdf$reachdeviation_deg[which(abs(taskdf$reachdeviation_deg) > 60)] <- NA
  #   taskdf$reachdeviation_deg <- taskdf$reachdeviation_deg - mean(taskdf$reachdeviation_deg[17:32], na.rm=T)
  #   
  #   tasklist[[condition]] <- taskdf
  #   
  # }
  # 
  # # output:
  # return(tasklist)
  return(dfrd)
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates by angle degrees
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


combinetrials <- function() {
for (i in c(1,3,4)){
  newdata <- getpangles(cond = i,type = 1, cutoff = 0.3)
}

#for (i in c(1,3,4)){
#  newdata <- getpangles(cond = i,type = 0, cutoff = 0.7)
#}
  return(print("done"))
}


removeSDoutliers <- function(values, sds=3) {
  
  avg <- mean(values, na.rm=TRUE)
  std <- sd(values, na.rm=TRUE) * sds
  
  values[values > avg + std] <- NA
  values[values < avg - std] <- NA
  
  return(values)
  
}

removeIQRoutliers <- function(values, range=3) {
  
  bp <- boxplot(values, range=3, plot=FALSE)
  
  values[values %in% bp$out] <- NA
  
  return(values)
  
}


removeReachOutliers <- function(data) {
  
  ntrials <- nrow(data)
  
  for (trialn in c(1:ntrials)) {
    
    data[trialn,2:ncol(data)] <- removeSDoutliers(as.numeric(unlist(data[trialn,2:ncol(data)])))
    
  }
  
  return(data)
  
}
