# Plotting and analysis code for BCS 206 class project
# Project Members: Bethany Gardner, Anaclare Sullivan, Tyler Trine
# Project Member Email Addresses: {bethany.gardner@,asulli22@u.,ttrine@u.}rochester.edu
# Code contributors: Tyler Trine (Dec.7, 2015), Chigusa Kurumada (Nov.23, 2015)

# Experiment: A conceptual replication of Groner & Sedivy (2011)
# Predictors: reliability of speaker, presence / absence of contrast item (isContrastTrial)
# 8 target items with contrast item, 8 target items without contrast item
# 16 fillers (not included in the analysis)

source("helpers.R")                                # boiler plate code
source("processingFunctions.R")                    # workhorse functions
source("http://peterhaschke.com/Code/multiplot.R") # plotting
library(ggplot2); library(plyr)                    # plotting
library(ez)                                        # ANOVA

#1: load data
if(file.exists("data/batch3raw.csv")){ # if the raw data is present, preprocess it from scratch
  message("Found raw file, trimming it down...")
  df <- read.csv("data/batch3raw.csv")
  message("ok")
  df <- removeExtraneousColumns(df)
  df <- isolateCriticalRows(df)
  df$subjectNumber <- extractSubjectNumber(df$SubjectName) # extract subjectNumber from subjectName
  df$hasContrast <- hasContrast[apply(as.matrix(df),1,subjectToCondition)]  # list of contrast trials for condition
  df$isContrastTrial <- apply(as.matrix(df),1,isIn)         # true iff trial has contrast
  df$SubjectName <- NULL                                    # de-identify the data
  df$hasContrast <- NULL                                    # can't write this column to file
  df$targetFixation <- ifelse(df$rp_type == "target", 1, 0) # is subject looking at target?
  df$competitorFixation <- ifelse(df$rp_type == "competitor", 1, 0) # is subject looking at competitor?
  df$isContrastTrial = as.factor(df$isContrastTrial)
  df$subjectNumber = as.factor(df$subjectNumber)
  message("Writing preprocessing output...")
  writeToFile(df,"data/batch3preprocess.csv")               # write trimmed data to .csv file
  df$speakerReliability <- addSpeakerReliability(df)           # so we can explicitly reference this
  message("ok")
}
else if(file.exists("data/batch3preprocess.csv")){ # otherwise use preprocessed data
  message("Found preprocessed file, loading it...")
  df <- read.csv("data/batch3preprocess.csv")
  df$speakerReliability <- addSpeakerReliability(df)           # so we can explicitly reference this
  message("ok")
}
else{
  stop("Whoops! I couldn't find the raw data or the preprocessed data.")
}

#2: collapse data over conditions (contrast vs. no contrast)
df2 <- ddply(df, c("subjectNumber","isContrastTrial","Time_rel_stim_adjonset","speakerReliability"),
             summarise,
             prop.target = sum(targetFixation)/length(targetFixation),
             prop.competitor = sum(competitorFixation)/length(competitorFixation)
)
df2$Time_rel_stim_adjonset_bin = floor(df2$Time_rel_stim_adjonset/20)*20 # 20ms bins for plots

#3: take within-subject means
subj.means <- ddply(df2, c("subjectNumber","isContrastTrial","Time_rel_stim_adjonset_bin","speakerReliability"),
                    summarise,
                    N = sum(!is.na(prop.target)),
                    target.mean = mean(prop.target, na.rm=TRUE),
                    target.sd   = sd(prop.target, na.rm=TRUE),
                    target.se   = target.sd / sqrt(N),
                    competitor.mean = mean(prop.competitor, na.rm=TRUE),
                    competitor.sd   = sd(prop.competitor, na.rm=TRUE),
                    competitor.se   = competitor.sd / sqrt(N)
)

#4: take between-subject mean
overall.mean <- ddply(subj.means, c("isContrastTrial","Time_rel_stim_adjonset_bin","speakerReliability"),
                      summarise,
                      N = sum(!is.na(target.mean)),
                      target.mean = mean(target.mean, na.rm=TRUE),
                      target.sd   = sd(target.mean, na.rm=TRUE),
                      target.se   = target.sd / sqrt(N),
                      competitor.mean = mean(competitor.mean, na.rm=TRUE),
                      competitor.sd   = sd(competitor.mean, na.rm=TRUE),
                      competitor.se   = competitor.sd / sqrt(N)
)

#5: plot fixation proportions over time
reliableNoContrast = subset(overall.mean,
                            overall.mean$speakerReliability == "reliable"
                            & overall.mean$isContrastTrial == FALSE)

reliableContrast = subset(overall.mean,
                            overall.mean$speakerReliability == "reliable"
                            & overall.mean$isContrastTrial == TRUE)

unreliableNoContrast = subset(overall.mean,
                            overall.mean$speakerReliability == "unreliable"
                            & overall.mean$isContrastTrial == FALSE)

unreliableContrast = subset(overall.mean,
                            overall.mean$speakerReliability == "unreliable"
                            & overall.mean$isContrastTrial == TRUE)



reliableNoContrastPlot = ggplot(reliableNoContrast) +
  scale_x_continuous(limits = c(0, 1700),name="") +
  scale_y_continuous(limits = c(0, .8),name="Proportion of Trials") +
  geom_line(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue") +
  geom_point(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue",pch=22,fill="white") +
  aes(x=Time_rel_stim_adjonset_bin, y=competitor.mean) +
  geom_line(color="red") +
  geom_point(color="red",pch=18) +
  geom_vline(xintercept = 362) + # noun onset
  geom_vline(xintercept = 855) + # noun offset
  annotate("text", x = 170, y = 0.6, label = "Adjective") +
  annotate("text", x = 600, y = 0.6, label = "Noun") +
  theme(axis.title.y = element_text(size=16),
        axis.text.y  = element_text(vjust=0.5, size=14),
        axis.text.x  = element_text(vjust=0.5, size=16))+
  xlab("Time from Adjective Onset") +
  theme_bw() +
  ggtitle("Reliable Speaker - No Contrast")

reliableContrastPlot = ggplot(reliableContrast) +
  scale_x_continuous(limits = c(0, 1700),name="") +
  scale_y_continuous(limits = c(0, .8),name="Proportion of Trials") +
  geom_line(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue") +
  geom_point(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue",pch=22,fill="white") +
  aes(x=Time_rel_stim_adjonset_bin, y=competitor.mean) +
  geom_line(color="red") +
  geom_point(color="red",pch=18) +
  geom_vline(xintercept = 362) + # noun onset
  geom_vline(xintercept = 855) + # noun offset
  annotate("text", x = 170, y = 0.6, label = "Adjective") +
  annotate("text", x = 600, y = 0.6, label = "Noun") +
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y  = element_text(vjust=0.5, size=14),
    axis.text.x  = element_text(vjust=0.5, size=16))+
  xlab("Time from Adjective Onset") +
  theme_bw() +
  ggtitle("Reliable Speaker - Contrast")

unreliableNoContrastPlot = ggplot(unreliableNoContrast) +
  scale_x_continuous(limits = c(0, 1700),name="") +
  scale_y_continuous(limits = c(0, .8),name="Proportion of Trials") +
  geom_line(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue") +
  geom_point(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue",pch=22,fill="white") +
  aes(x=Time_rel_stim_adjonset_bin, y=competitor.mean) +
  geom_line(color="red") +
  geom_point(color="red",pch=18) +
  geom_vline(xintercept = 362) + # noun onset
  geom_vline(xintercept = 855) + # noun offset
  annotate("text", x = 170, y = 0.6, label = "Adjective") +
  annotate("text", x = 600, y = 0.6, label = "Noun") +
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y  = element_text(vjust=0.5, size=14),
    axis.text.x  = element_text(vjust=0.5, size=16))+
  xlab("Time from Adjective Onset") +
  theme_bw() +
  ggtitle("Unreliable Speaker - No Contrast")

unreliableContrastPlot = ggplot(unreliableContrast) +
  scale_x_continuous(limits = c(0, 1700),name="") +
  scale_y_continuous(limits = c(0, .8),name="Proportion of Trials") +
  geom_line(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue") +
  geom_point(aes(x=Time_rel_stim_adjonset_bin, y=target.mean),color="blue",pch=22,fill="white") +
  aes(x=Time_rel_stim_adjonset_bin, y=competitor.mean) +
  geom_line(color="red") +
  geom_point(color="red",pch=18) +
  geom_vline(xintercept = 362) + # noun onset
  geom_vline(xintercept = 855) + # noun offset
  annotate("text", x = 170, y = 0.6, label = "Adjective") +
  annotate("text", x = 600, y = 0.6, label = "Noun") +
  theme(
    axis.title.y = element_text(size=16),
    axis.text.y  = element_text(vjust=0.5, size=14),
    axis.text.x  = element_text(vjust=0.5, size=16)) +
  xlab("Time from Adjective Onset") +
  theme_bw() +
  ggtitle("Unreliable Speaker - Contrast")

#6: plot target advantage within critical window
targetRegion <- subset(df, df$Time_rel_stim_nounonset >= 200 &
                         df$Time_rel_stim_nounonset <= 700)

targetRegion$isContrastTrial = as.factor(targetRegion$isContrastTrial)

targetRegion$subjectNumber = as.factor(targetRegion$subjectNumber)

targetRegion2 <- ddply(targetRegion, c("subjectNumber","isContrastTrial","Time_rel_stim_adjonset","speakerReliability"),
                       summarise,
                       target_prop.trial = sum(targetFixation)/length(targetFixation),
                       competitor_prop.trial = sum(competitorFixation)/length(competitorFixation),
                       targetAdvantage = target_prop.trial - competitor_prop.trial
)
targetRegion3 <-ddply(targetRegion2, c("isContrastTrial","speakerReliability"),
                       summarise,
                       target.N = sum(!is.na(targetAdvantage)),
                       target.mean = mean(targetAdvantage, na.rm=TRUE),
                       target.sd   = sd(targetAdvantage, na.rm=TRUE),
                       target.se   = target.sd / sqrt(target.N)
)

##Rename the "isContrast" factor levels
levels(targetRegion3$isContrastTrial) <- c("b", "a")
targetRegion3$isContrastTrial <- as.factor(as.character(targetRegion3$isContrastTrial))

##Target advantage plot
ggplot(data=targetRegion3, aes(x=speakerReliability, y=target.mean, fill=isContrastTrial)) +
  scale_x_discrete(labels=c("Reliable","Unreliable")) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  geom_errorbar(aes(ymin=target.mean-target.se, ymax=target.mean+target.se),
                width=.2,position=position_dodge(.9)) +
  ylab("Fixations on Target - Competitor") +
  xlab("Speaker Condition") +
  guides(fill=FALSE) +
  scale_fill_manual(values=c("white","blue")) +
  theme(legend.position=c(.2,.8),
  axis.title.y = element_text(size=18),
  axis.text.x  = element_text(vjust=0.5, size=16))+
  theme_bw()

#7: statistical hypothesis tests
ezANOVA(data=targetRegion2, # did we replicate?
        dv=targetAdvantage,
        wid=subjectNumber,
        within=isContrastTrial,
        between=speakerReliability
)

ezANOVA(data=targetRegion2[targetRegion2$isContrastTrial=="TRUE",], # does contrast predict fp for reliable sp?
        dv=targetAdvantage,
        wid=subjectNumber,
        between=speakerReliability
)

ezANOVA(data=targetRegion2[targetRegion2$isContrastTrial=="FALSE",], # for unreliable sp?
        dv=targetAdvantage,
        wid=subjectNumber,
        between=speakerReliability
)
