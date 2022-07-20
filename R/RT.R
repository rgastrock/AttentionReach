source('R/shared.R')

#uncorrected----
#plot RT across blocks, separate target only (TO) and target + distractor (TD) trials
# blocks 0, 6, 12, 18 are for presses
getTrialuncRT <- function(id, response, block){
  #read in data (press): one participant for now
  data <- read.csv(file=sprintf('data/p0%2s/p0%2s-%s-b%2s.csv', id, id, response, block))
  
  #for every trial, determine RT, determine if TO or TD
  trials <- unique(data$trial)
  
  responsetime <- c()
  condition <- c()
  trial <- c()
  
  for(trialno in trials){
    
    subdat <- data[which(data$trial == trialno),]
    distangle <- unique(subdat$distractorangle_deg)
    if(is.na(distangle)){
      cond <- 'to'
    } else{
      cond <- 'td'
    }
    
    gosig <- subdat[which(subdat$step == 2),]
    gotime <- gosig$time_ms[length(gosig$time_ms)]
    
    keysig <- subdat[which(subdat$step == 4),]
    presstime <- keysig$time_ms[length(keysig$time_ms)]
    
    rt = presstime - gotime
    
    trialno <- trialno + 1 #account for python starting from 0
    trial <- c(trial, trialno)
    responsetime <- c(responsetime, rt)
    condition <- c(condition, cond)
  }
  
  df <- data.frame(trial, responsetime, condition)
  return(df)
}

#for this one person, we could probably plot TO and TD means per block
getTargetOnlyuncRTCI <- function(id = '00', response = 'press', blocks = c('00', '06', '12', '18')){
  
  targetCI <- data.frame()
  
  for(block in blocks){
    dat <- getTrialRT(id = id, response = response, block = block)
    to <- dat[which(dat$condition == 'to'),]
    to_CI <- getConfidenceInterval(to$responsetime)
    to_CI <- data.frame(t(to_CI))
    
    
    targetCI <- rbind(targetCI, to_CI)
    
  }
  return(targetCI)
}

getTargetDistuncRTCI <- function(id = '00', response = 'press', blocks = c('00', '06', '12', '18')){
  
  targdistCI <- data.frame()
  
  for(block in blocks){
    dat <- getTrialRT(id = id, response = response, block = block)
    td <- dat[which(dat$condition == 'td'),]
    td_CI <- getConfidenceInterval(td$responsetime)
    td_CI <- data.frame(t(td_CI))
    
    
    targdistCI <- rbind(targdistCI, td_CI)
    
  }
  return(targdistCI)
}

plotuncRT <- function(trialtype = c('td', 'to'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='doc/fig/Fig1_UncorrectedKeyPressRT.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,5), ylim = c(350,750), 
       xlab = "block", ylab = "reaction time (ms)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Key press trials", xaxt = 'n', yaxt = 'n')
  
  axis(side=1, at=c(1, 2, 3, 4))
  axis(2, at = c(400, 420, 440, 460, 480, 500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700), las = 2)
  
  
  
  for (type in trialtype) {
    blockno <- 0
    
    if(type == 'to'){
      blocked <- getTargetOnlyuncRTCI()
    } else {
      blocked <- getTargetDistuncRTCI()
    }
    
    colourscheme <- getColourScheme(trialtype=type)
    #get bootstrapped 2.5, 50, 97.5% CIs of percentages
    #meandist <- getConfidenceInterval(data=blocked, method='b')
    #meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    for(blk in 1:nrow(blocked)){
      if(type == 'to'){
        blockno <- (blockno + 1) - 0.10 #counter for block, so that we can refer to it in x coordinates
        subdat <- blocked[blk,]
        col <- colourscheme[[type]][['S']]
        lines(x=rep(blockno,2),y=c(subdat[[1]], subdat[[3]]),col=col) #lower and upper CI
        points(x=blockno,y=subdat[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
        blockno <- blockno + 0.10
      } else {
        blockno <- (blockno + 1) + 0.10 #counter for block, so that we can refer to it in x coordinates
        subdat <- blocked[blk,]
        col <- colourscheme[[type]][['S']]
        lines(x=rep(blockno,2),y=c(subdat[[1]], subdat[[3]]),col=col) #lower and upper CI
        points(x=blockno,y=subdat[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
        blockno <- blockno - 0.10
      }

    }
    
    
  }
  
  #add legend
  legend(3,700,legend=c('target only','target + distractor'),
         col=c(colourscheme[['to']][['S']],colourscheme[['td']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#corrected----
#account for errors: remove wrong responses
#remove trials faster or slower than 2 or 3 stdev in each block (block mean and stdev are considered for each block)

getTrialRT <- function(id, response, block){
  #read in data (press): one participant for now
  data <- read.csv(file=sprintf('data/p0%2s/p0%2s-%s-b%2s.csv', id, id, response, block))
  
  #for every trial, determine RT, determine if TO or TD
  trials <- unique(data$trial)
  
  responsetime <- c()
  condition <- c()
  trial <- c()
  trial_error <- c()
  
  for(trialno in trials){
    
    subdat <- data[which(data$trial == trialno),]
    error <- unique(subdat$trial_error)[2] #first will always be NA
    distangle <- unique(subdat$distractorangle_deg)
    if(is.na(distangle)){
      cond <- 'to'
    } else{
      cond <- 'td'
    }
    
    gosig <- subdat[which(subdat$step == 2),]
    gotime <- gosig$time_ms[length(gosig$time_ms)]
    
    keysig <- subdat[which(subdat$step == 4),]
    presstime <- keysig$time_ms[length(keysig$time_ms)]
    
    rt = presstime - gotime
    
    trialno <- trialno + 1 #account for python starting from 0
    trial <- c(trial, trialno)
    responsetime <- c(responsetime, rt)
    condition <- c(condition, cond)
    trial_error <- c(trial_error, error)
  }
  
  df <- data.frame(trial, responsetime, condition, trial_error)
  return(df)
}

getBlockRT <- function(id = '01', response = 'press', blocks = c('00', '05', '10', '15', '20')){
  
  blockdat <- data.frame()
  
  for(block in blocks){
    dat <- getTrialRT(id = id, response = response, block = block)
    #remove trial error = 1 (trials with errors)
    ndat <- dat[which(dat$trial_error == 0),]

    blockmu <- mean(ndat$responsetime, na.rm = T)
    blocksigma <- sd(ndat$responsetime, na.rm = T)
    upperblockclip <- abs(blockmu) + (blocksigma * 3)
    lowerblockclip <- abs(blockmu) - (blocksigma * 3)
    
    ndat$responsetime[which(abs(ndat$responsetime) > upperblockclip)] <- NA
    ndat$responsetime[which(abs(ndat$responsetime) < lowerblockclip)] <- NA
    ndat$blockno <- rep(block, nrow(ndat))
    blockdat<- rbind(blockdat, ndat)
    
  }
  return(blockdat)
}

getTargetOnlyRTCI <- function(blocks = c('00', '05', '10', '15', '20')){
  
  dat <- getBlockRT()
  targetCI <- data.frame()
  
  for(block in blocks){
    subdat <- dat[which(dat$blockno == block),]
    to <- subdat[which(subdat$condition == 'to'),]
    to <- na.omit(to)
    to_CI <- getConfidenceInterval(to$responsetime)
    to_CI <- data.frame(t(to_CI))
    
    
    targetCI <- rbind(targetCI, to_CI)
    
  }
  return(targetCI)
}

getTargetDistRTCI <- function(blocks = c('00', '05', '10', '15', '20')){
  
  dat <- getBlockRT()
  targdistCI <- data.frame()
  
  for(block in blocks){
    subdat <- dat[which(dat$blockno == block),]
    td <- subdat[which(subdat$condition == 'td'),]
    td <- na.omit(td)
    td_CI <- getConfidenceInterval(td$responsetime)
    td_CI <- data.frame(t(td_CI))
    
    
    targdistCI <- rbind(targdistCI, td_CI)
    
  }
  return(targdistCI)
}

plotRT <- function(trialtype = c('td', 'to'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg'){
    svglite(file='doc/fig/Fig1_KeyPressRT.svg', width=10, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(0,6), ylim = c(450,850), 
       xlab = "block", ylab = "reaction time (ms)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Key press trials", xaxt = 'n', yaxt = 'n')
  
  axis(side=1, at=c(1, 2, 3, 4, 5))
  axis(2, at = c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720, 740, 760, 780, 800), las = 2)
  
  
  
  for (type in trialtype) {
    blockno <- 0
    
    if(type == 'to'){
      blocked <- getTargetOnlyRTCI()
    } else {
      blocked <- getTargetDistRTCI()
    }
    
    colourscheme <- getColourScheme(trialtype=type)
    #get bootstrapped 2.5, 50, 97.5% CIs of percentages
    #meandist <- getConfidenceInterval(data=blocked, method='b')
    #meandist <- getAngularReachDevsCI(data = blocked, group = group)
    
    for(blk in 1:nrow(blocked)){
      if(type == 'to'){
        blockno <- (blockno + 1) - 0.10 #counter for block, so that we can refer to it in x coordinates
        subdat <- blocked[blk,]
        col <- colourscheme[[type]][['S']]
        lines(x=rep(blockno,2),y=c(subdat[[1]], subdat[[3]]),col=col) #lower and upper CI
        points(x=blockno,y=subdat[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
        blockno <- blockno + 0.10
      } else {
        blockno <- (blockno + 1) + 0.10 #counter for block, so that we can refer to it in x coordinates
        subdat <- blocked[blk,]
        col <- colourscheme[[type]][['S']]
        lines(x=rep(blockno,2),y=c(subdat[[1]], subdat[[3]]),col=col) #lower and upper CI
        points(x=blockno,y=subdat[[2]],pch=16,cex=1.5,col=col) #50% plotted as a point
        blockno <- blockno - 0.10
      }
      
    }
    
    
  }
  
  #add legend
  legend(3.5,800,legend=c('target only','target + distractor'),
         col=c(colourscheme[['to']][['S']],colourscheme[['td']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}























