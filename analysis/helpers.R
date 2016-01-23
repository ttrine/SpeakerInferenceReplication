spl <- function(bin){ # split arbitrary data subset into lists of trials
  l <- list()
  index <- 1
  for (trial in unique(bin$TrialDataID)){
    l[[index]] <- bin[bin$TrialDataID == trial,]
    index <- index + 1
  }
  return(l)
}
extend <- function(x, n){ # add 0s to the end of a vector
  att <- attributes(x)
  length(x) <- n
  attributes(x) <- att
  x[is.na(x)] <- 0
  x
}
hasContrast <- list(
  c(4, 7, 12, 18, 23, 26, 29, 33),
  c(4, 8, 11, 14, 19, 25, 30, 33),
  c(5, 10, 16, 20, 22, 24, 27, 30),
  c(7, 10, 13, 16, 17, 21, 27, 32),
  c(4, 7, 12, 18, 23, 26, 29, 33),
  c(4, 8, 11, 14, 19, 25, 30, 33),
  c(5, 10, 16, 20, 22, 24, 27, 30),
  c(7, 10, 13, 16, 17, 22, 28, 33)
)
subjectToCondition <- function(row){
  num <- as.integer(row[9])
  if ((num %% 8) == 1) return(1)
  if ((num %% 8) == 2) return(5)
  if ((num %% 8) == 3) return(2)
  if ((num %% 8) == 4) return(6)
  if ((num %% 8) == 5) return(3)
  if ((num %% 8) == 6) return(7)
  if ((num %% 8) == 7) return(4)
  if ((num %% 8) == 0) return(8)
  return(NULL)
}
isIn <- function(row) row$TrialNumber %in% row$hasContrast
removeExtension <- function(row) strsplit(toString(row[5]),'.',fixed=TRUE)[[1]][1]
writeToFile <- function(data,filename){
  cat(names(data), sep=",", file=filename)
  cat("\n", file=filename,append=TRUE)
  apply(df,1,writeRow,filename=filename)
}
writeRow <- function(row,filename){
  cat(gsub(" ", "", toString(row), fixed=TRUE), sep=",", file=filename, append=TRUE)
  cat("\n", file=filename,append=TRUE)
}
addSpeakerReliability <- function(df){
  a <- ifelse(df$subjectNumber == 1, "reliable",
              ifelse(df$subjectNumber == 2, "unreliable",
                     ifelse(df$subjectNumber == 3, "reliable",
                            ifelse(df$subjectNumber == 4, "unreliable",
                                   ifelse(df$subjectNumber == 5, "reliable",
                                          ifelse(df$subjectNumber == 6, "unreliable",
                                                 ifelse(df$subjectNumber == 7, "reliable",
                                                        ifelse(df$subjectNumber == 8, "unreliable",
                                                               ifelse(df$subjectNumber == 9, "reliable",
                                                                      ifelse(df$subjectNumber == 11, "unreliable",
                                                                             ifelse(df$subjectNumber == 12, "reliable",
                                                                                    ifelse(df$subjectNumber == 13, "unreliable",
                                                                                           ifelse(df$subjectNumber == 14, "reliable",
                                                                                                  ifelse(df$subjectNumber == 15, "unreliable",
                                                                                                         ifelse(df$subjectNumber == 16, "reliable",
                                                                                                                ifelse(df$subjectNumber == 17, "unreliable",
                                                                                                                       ifelse(df$subjectNumber == 18, "reliable",
                                                                                                                              ifelse(df$subjectNumber == 19, "unreliable",
                                                                                                                                     ifelse(df$subjectNumber == 20, "reliable",
                                                                                                                                            ifelse(df$subjectNumber == 21, "unreliable",
                                                                                                                                                   ifelse(df$subjectNumber == 22, "reliable",
                                                                                                                                                          ifelse(df$subjectNumber == 23, "unreliable",
                                                                                                                                                                 ifelse(df$subjectNumber == 24, "reliable",
                                                                                                                                                                        ifelse(df$subjectNumber == 25, "unreliable",
                                                                                                                                                                               ifelse(df$subjectNumber == 26, "reliable",
                                                                                                                                                                                      ifelse(df$subjectNumber == 27, "unreliable",
                                                                                                                                                                                             ifelse(df$subjectNumber == 28, "reliable",
                                                                                                                                                                                                    ifelse(df$subjectNumber == 29, "unreliable",
                                                                                                                                                                                                           ifelse(df$subjectNumber == 30, "unreliable",
                                                                                                                                                                                                                  ifelse(df$subjectNumber == 31, "reliable",
                                                                                                                                                                                                                         ifelse(df$subjectNumber == 32, "unreliable",
                                                                                                                                                                                                                                ifelse(df$subjectNumber == 33, "reliable",                             
                                                                                                                                                                                                                                       ifelse(df$subjectNumber == 34, "unreliable",                             
                                                                                                                                                                                                                                              ifelse(df$subjectNumber == 35, "reliable",                             
                                                                                                                                                                                                                                                     ifelse(df$subjectNumber == 36, "unreliable",                             
                                                                                                                                                                                                                                                            ifelse(df$subjectNumber == 37, "reliable",                             
                                                                                                                                                                                                                                                                   ifelse(df$subjectNumber == 38, "unreliable",                             
                                                                                                                                                                                                                                                                          ifelse(df$subjectNumber == 39, "reliable",                             
                                                                                                                                                                                                                                                                                 ifelse(df$subjectNumber == 40, "unreliable",                                   
                                                                                                                                                                                                                                                                                        "error")))))))))))))))))))))))))))))))))))))))
  return(as.factor(a))
}