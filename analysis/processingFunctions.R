if (!require("zoo",character.only = TRUE,quietly=TRUE)){
  install.packages("zoo")
  message("'zoo' dependency installed. Ignore the warning message below.")
}
source("helpers.R")

removeExtraneousColumns <- function(datafile){
  datafile$DataID <- NULL
  datafile$lp_pic <- NULL
  datafile$lp_type <- NULL
  datafile$rp_pic <- NULL
  datafile$Left_X <- NULL
  datafile$Left_Y <- NULL
  datafile$Right_X <- NULL
  datafile$Right_Y <- NULL
  datafile$Left_Type <- NULL
  datafile$Right_Type <- NULL
  datafile$Left_LookStart <- NULL
  datafile$Left_EventStart <- NULL
  datafile$Right_LookStart <- NULL
  datafile$Right_EventStart <- NULL
  datafile$SessionNumber <- NULL
  datafile$TrialID <- NULL
  datafile$Time_rel_r_clickedon <- NULL
  datafile$Left_LookStart_rel_r_clickedon <- NULL
  datafile$Right_LookStart_rel_r_clickedon <- NULL
  datafile$Left_EventStart_rel_r_clickedon <- NULL
  datafile$Right_EventStart_rel_r_clickedon <- NULL
  datafile$Time_rel_r_condition <- NULL
  datafile$Left_LookStart_rel_r_condition <- NULL
  datafile$Right_LookStart_rel_r_condition <- NULL
  datafile$Left_EventStart_rel_r_condition <- NULL
  datafile$Right_EventStart_rel_r_condition <- NULL
  datafile$Time_rel_stim_soundfile <- NULL
  datafile$Left_LookStart_rel_stim_soundfile <- NULL
  datafile$Right_LookStart_rel_stim_soundfile <- NULL
  datafile$Left_EventStart_rel_stim_soundfile <- NULL
  datafile$Right_EventStart_rel_stim_soundfile <- NULL
  datafile$Left_LookStart_rel_stim_adjonset <- NULL
  datafile$Left_EventStart_rel_stim_adjonset <- NULL
  datafile$Right_EventStart_rel_stim_adjonset <- NULL
  datafile$Left_LookStart_rel_stim_nounonset <- NULL
  datafile$Left_EventStart_rel_stim_nounonset <- NULL
  datafile$Right_EventStart_rel_stim_nounonset <- NULL
  datafile$Time_rel_stim_nounoffset <- NULL
  datafile$Left_LookStart_rel_stim_nounoffset <- NULL
  datafile$Left_EventStart_rel_stim_nounoffset <- NULL
  datafile$Right_EventStart_rel_stim_nounoffset <- NULL
  datafile$time_r_clickedon <- NULL
  datafile$time_r_condition <- NULL
  datafile$stim_adjonset <- NULL
  datafile$stim_nounonset <- NULL
  datafile$stim_nounoffset <- NULL
  datafile$time_stim_soundfile <- NULL
  datafile$time_stim_adjonset <- NULL
  datafile$time_stim_nounonset <- NULL
  datafile$time_stim_nounoffset <- NULL
  datafile$rp_pic <- NULL
  datafile$Right_LookStart_rel_stim_adjonset <- NULL
  datafile$Right_LookStart_rel_stim_nounonset <- NULL
  datafile$Right_LookStart_rel_stim_nounoffset <- NULL
  
  return(datafile)
}
isolateCriticalRows <- function(datafile){
  datafile <- datafile[datafile$SubjectName!="4AW",]              # whoops
  datafile <- datafile[datafile$SubjectName!="10MW",]             # remove unusable data
  datafile <- datafile[which(datafile$r_condition=="Critical"),]  # critical trials
  datafile <- datafile[which(datafile$r_clickedon=="target"),]    # correct trials
  datafile <- datafile[datafile$Time_rel_stim_adjonset >= 0,]     # after adjective utterance
  
  datafile$r_condition <- NULL
  datafile$r_clickedon <- NULL
  
  return(datafile)
}
extractSubjectNumber <- function(SubjectName){
  subjectNumber <- as.vector(SubjectName)
  # standardize irregular nomenclature
  subjectNumber[subjectNumber=="1Bethany Schmidt"] <- "01BS"
  subjectNumber[subjectNumber=="2MohamedK"] <- "02MK"
  subjectNumber[subjectNumber=="3RFP"] <- "03RP"
  return(as.integer(substring(subjectNumber,1,2)))
}

proportionalFixation <- function(trialList){
  absFreq <- c()      # absolute frequency of target fixations
  numTrials <- c()    # number of trials with data
  stdError <- c()     # estimated error of fixation proportions
  
  # compute fixation proportions
  longestTrialLength <- 0; timeStamp <- c()
  for(trial in trialList){
    if (length(trial$subjectNumber) > longestTrialLength){
      longestTrialLength <- length(trial$subjectNumber)
      timeStamp <- trial$Time_rel_stim_adjonset
    }
  }
  absFreq[1:longestTrialLength] <- 0
  numTrials[1:longestTrialLength] <- 0
  stdError[1:longestTrialLength] <- 0
  for(trial in trialList){
    absFreq = absFreq + as.vector(extend(ifelse(trial$rp_type == "target", 1, 0), longestTrialLength))
    numTrials[1:length(trial$subjectNumber)] = numTrials[1:length(trial$subjectNumber)] + 1
  }
  fixationProportion <- absFreq/numTrials
  
  # estimate standard error
  for(trial in trialList){
    stdError <- stdError + (as.vector(extend(ifelse(trial$rp_type == "target", 1, 0), longestTrialLength)) - fixationProportion)^2
  }
  stdError <- sqrt(stdError/(numTrials - 1))/sqrt(numTrials)
  
  return(data.frame(timeStamp, fixationProportion, stdError))
}
plotProp <- function(fp, title){
  smooth <- rollmean(fp$fixationProportion,10,na.pad = TRUE, align = "right")
  smoothE <- rollmean(fp$stdError,10,na.pad = TRUE, align = "right")
  par(mar=c(4, 4, 4, 4) + 0.1)
  plot(fp$timeStamp,smooth,type="h", col="green",xlab="Time (ms)",ylab="Proportion of trials",main=title)
  par(new=TRUE)
  plot(fp$timeStamp,smoothE,type="h", col="red",ylab="",xlab="",yaxt="n")
  axis(4, at=c(0,1,2,3), las=2, cex.axis=0.7, tck=-.01)
  mtext("Standard error", 4, line=2)
}