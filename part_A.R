#!/usr/bin/env Rscript

library(tibble)

survey <- read.csv(file="inf-students(2).csv", header=TRUE, sep=",")
numSurvey <- read.csv(file="numerics-data(2).csv", header=TRUE, sep=",")

cleanNum <- numSurvey[numSurvey$sebisLikert.sebisCheck. == 5, ]
cleanNum <- cleanNum[cleanNum$hackersLikert.folkHCheck. == 2, ]

cleanSurvey <- survey[survey$sebisLikert.sebisCheck. == "Always", ]
cleanSurvey <- cleanSurvey[cleanSurvey$hackersLikert.folkHCheck. == "Disagree", ]

westinNum <- cleanNum[, grepl("westin", names(cleanNum))]

# print(cleanNum$sebisLikert.sebisCheck.)
# print(cleanNum$hackersLikert.folkHCheck.)

# print(cleanSurvey[, grepl("Check", names(cleanSurvey))])

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

# print(cleanSurvey[, grepl("Westin Category", names(cleanSurvey))])



