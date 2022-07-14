library(tidyr)
library(lattice) #for density plot tests
library(circular)
library(RColorBrewer)
library(svglite)
library(scales)
library(ez)
library(vioplot)
library(phia) #used for interactionMeans
library(emmeans)
library(afex) #used for aov_ez


getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar,  xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    mid <- .50
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,mid,hi)))
    }
    
  }
  
}

getColourScheme <- function(trialtype = c('td','to')){
  #create a list containing the colourscheme per type
  for (type in trialtype){
    colourscheme <- list()
    
    colourscheme[['td']] <- list('S'='#e51636ff', #vivid/york red
                                            'T'='#e516362f')
    
    colourscheme[['to']] <-   list('S'='#50C878ff', #dark grey
                                   'T'='#50C8782f')
    
    #colourscheme[['to']] <-   list('S'='#A9A9A9ff', #dark grey
     #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}