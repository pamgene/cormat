library(bnutil)
library(cormat)
library(dplyr)
library(reshape2)
library(ggplot2)
library(epiR)
library(pgscales)

getdata = function() AnnotatedData$new(data=df, metadata=mdf)

getRunfolder = function() file.path(getwd(), 'run')

setResult = function(annotatedResult){

  print(annotatedResult)

}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getRunFolderHandler = getRunfolder
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult


bnshiny::startBNTestShiny('cormat', sessionType='show', bnMessageHandler=bnMessageHandler)
