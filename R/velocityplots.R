data <- read.csv(file='data/p001-reach-b00.csv')

trials <- unique(data$trial)

for(trialno in trials){
  
  subdat <- data[which(data$trial == trialno),]
  subdat_step1 <- subdat[which(subdat$step == 1),]
  subdat_step2 <- subdat[which(subdat$step == 2),]
  subdat_step3 <- subdat[which(subdat$step == 3),]
  subdat_step4 <- subdat[which(subdat$step == 4),]
  
  subdat_steps <- rbind(subdat_step1, subdat_step2, subdat_step3, subdat_step4)
  
  x <- subdat_steps$mousex_px
  y <- subdat_steps$mousey_px
  t <- subdat_steps$time_ms
  
  ab <- sqrt(diff(x)^2 + diff(y)^2) / diff(t)
  
}

sqrt(diff(x)^2 + diff(y)^2) / diff(t)
