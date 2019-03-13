#!/usr/bin/env Rscript

survey <- read.csv(file="inf-students(2).csv", header=TRUE, sep=",")
numSurvey <- read.csv(file="numerics-data(2).csv", header=TRUE, sep=",")

cleanNum <- numSurvey[numSurvey$sebisLikert.sebisCheck. == 5, ]
cleanNum <- cleanNum[cleanNum$hackersLikert.folkHCheck. == 2, ]

cleanSurvey <- survey[survey$sebisLikert.sebisCheck. == "Always", ]
cleanSurvey <- cleanSurvey[cleanSurvey$hackersLikert.folkHCheck. == "Disagree", ]

westinNum <- cleanNum[, grepl("westin", names(cleanNum))]

westinCat <- ("W")

for(i in 1:nrow(westinNum)) {
    row <- westinNum[i, ]
    if(row[1] >= 3 && row[2] <= 2 && row[3] <= 2) {
        westinCat[i] <- "Fundamentalist"
    }
    else if(row[1] <= 2 && row[2] >= 3 && row[3] >= 3) {
        westinCat[i] <- "Unconcerned"
    }
    else {
        westinCat[i] <- "Pragmatist"
    }
}

cleanSurvey["westin"] <- westinCat
cleanNum["westin"] <- westinCat

f7inv <- integer()
f8inv <- integer()
f11inv <- integer()
f12inv <- integer()
f14inv <- integer()
f16inv <- integer()

f7 <- cleanNum[, grepl("F07", names(cleanNum))]
f8 <- cleanNum[, grepl("F08", names(cleanNum))]
f11 <- cleanNum[, grepl("F11", names(cleanNum))]
f12 <- cleanNum[, grepl("F12", names(cleanNum))]
f14 <- cleanNum[, grepl("F14", names(cleanNum))]
f16 <- cleanNum[, grepl("F16", names(cleanNum))]

for(i in 1:nrow(cleanNum)) {
  f7inv[i] <- 6 - f7[i]
  f8inv[i] <- 6 - f8[i]
  f11inv[i] <- 6 - f11[i]
  f12inv[i] <- 6 - f12[i]
  f14inv[i] <- 6 - f14[i]
  f16inv[i] <- 6 - f16[i]
}

cleanNum["sebisLikert.sebisF07.-inverted"] <- f7inv
cleanNum["sebisLikert.sebisF08.-inverted"] <- f8inv
cleanNum["sebisLikert.sebisF11.-inverted"] <- f11inv
cleanNum["sebisLikert.sebisF12.-inverted"] <- f12inv
cleanNum["sebisLikert.sebisF14.-inverted"] <- f14inv
cleanNum["sebisLikert.sebisF16.-inverted"] <- f16inv

cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF16.$", colnames(cleanNum)), grep("^sebisLikert.sebisF16.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF16.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]
cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF14.$", colnames(cleanNum)), grep("^sebisLikert.sebisF14.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF14.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]
cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF12.$", colnames(cleanNum)), grep("^sebisLikert.sebisF12.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF12.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]
cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF11.$", colnames(cleanNum)), grep("^sebisLikert.sebisF11.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF11.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]
cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF08.$", colnames(cleanNum)), grep("^sebisLikert.sebisF08.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF08.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]
cleanNum <- cleanNum[, c(1:grep("^sebisLikert.sebisF07.$", colnames(cleanNum)), grep("^sebisLikert.sebisF07.-inverted$", colnames(cleanNum)), (grep("^sebisLikert.sebisF07.$", colnames(cleanNum)) + 1):(ncol(cleanNum) - 1))]

#Device: F3, F4, F5, F6
deviceCols <- cleanNum[, c(grep("^sebisLikert.sebisF03.$|^sebisLikert.sebisF04.$|^sebisLikert.sebisF05.$|^sebisLikert.sebisF06.$", colnames(cleanNum)))]
deviceMean = rowMeans(deviceCols)

cleanNum["device"] <- deviceMean
cleanSurvey["device"] <- deviceMean


#Password: F12-inv, F13, F14-inv, F15
passCols <- cleanNum[, c(grep("^sebisLikert.sebisF12.-inverted$|^sebisLikert.sebisF13.$|^sebisLikert.sebisF14.-inverted$|^sebisLikert.sebisF15.$", colnames(cleanNum)))]
passMean = rowMeans(passCols)

cleanNum["password"] <- passMean
cleanSurvey["password"] <- passMean


#Awareness: F7-inv, F8-inv, F10, F11-inv, F16-inv
awCols <- cleanNum[, c(grep("^sebisLikert.sebisF07.-inverted$|^sebisLikert.sebisF08.-inverted$|^sebisLikert.sebisF10.$|^sebisLikert.sebisF11.-inverted$|^sebisLikert.sebisF16.-inverted$", colnames(cleanNum)))]
awMean = rowMeans(awCols)

cleanNum["awareness"] <- awMean
cleanSurvey["awareness"] <- awMean


#Updating: F1, F2, F9
upCols <- cleanNum[, c(grep("^sebisLikert.sebisF01.$|^sebisLikert.sebisF02.$|^sebisLikert.sebisF09.$", colnames(cleanNum)))]
upMean = rowMeans(upCols)

cleanNum["updating"] <- upMean
cleanSurvey["updating"] <- upMean

write.csv(cleanNum, file="processed-numeric.csv")
write.csv(cleanSurvey, file="processed-raw.csv")

