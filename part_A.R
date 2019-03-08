#!/usr/bin/env Rscript

survey <- read.csv(file="inf-students(2).csv", header=TRUE, sep=",")
numSurvey <- read.csv(file="numerics-data(2).csv", header=TRUE, sep=",")

cleanNum <- numSurvey[numSurvey$sebisLikert.sebisCheck. == 5, ]
cleanNum <- cleanNum[cleanNum$hackersLikert.folkHCheck. == 2, ]

print(cleanNum$sebisLikert.sebisCheck.)
print(cleanNum$hackersLikert.folkHCheck.)