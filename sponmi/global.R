library(XML)
library(shiny)
library(ggplot2)
library(scales)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Additional functions needed for this app
#Function needed in server.R to coerce user input to numeric data.frame
#Convert xml objects to data.frame
xml2dfNAMED <- function(xmlRooters,value,name,char=F) {
  QQ <- vector("list",xmlSize(xmlRooters))
  for(i in 1:xmlSize(xmlRooters)){
    QQ[[ i ]] <- xmlAttrs(xmlRooters[[i]])[[value]]
    names(QQ[[ i ]]) <- xmlAttrs(xmlRooters[[i]])[[name]]
  }
  a <- unlist(QQ)
  b <- data.frame(as.list(a))
  if (char==F) {
    c <- data.frame(lapply(b[1,], function(x) as.numeric(levels(x))[x]))
    return(c)
  } else {
    return(b)
  }
}

checkandconvert <- function(x) {
  if (betaType[names(x)]=="continuous") {return (as.numeric(x))} 
  else if (betaType[names(x)]=="categorical") {
      if (x==mappingType[names(x)]) {return (as.numeric(1))} 
      else {return (as.numeric(0))}
  } 
}
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Establish a connection with the database
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Let's put together a xml object so that we can use certain functions to parse
  allFiles <- list.files()
  pmmlFiles <- allFiles[grep('*.pmml',allFiles)]
  xmlfile <- xmlParse(pmmlFiles[[1]])
  xmltop <- xmlRoot(xmlfile) #gives content of root
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Get data elements in R friendly formats (i.e. data.frames / numeric vectors)
  dict <- xmltop[["DataDictionary"]]
  referencePoints <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["ParameterList"]],value="referencePoint",name="label",char=F)
  mapping <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["PPMatrix"]], value="predictorName", name="parameterName",char=T)
  mappingType <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["PPMatrix"]], value="value", name="predictorName",char=T)
  betaType <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["DataDictionary"]]), value="optype", name="name",char=T)
  betaType <- betaType[which(colnames(betaType) %in% colnames(mappingType))]
  betas <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["ParamMatrix"]], value="beta", name="parameterName",char=F)
  betacomb2<- betas[1,]
  colnames(betacomb2) <- colnames(referencePoints)
  time <- as.numeric(unname(sapply(xmlChildren(xmlChildren(xmltop[["GeneralRegressionModel"]])[["BaseCumHazardTables"]]), xmlGetAttr, "time")))
  cumHazard <- as.numeric(unname(sapply(xmlChildren(xmlChildren(xmltop[["GeneralRegressionModel"]])[["BaseCumHazardTables"]]), xmlGetAttr, "cumHazard")))
  #Transformations
  title     <- xmlGetAttr(xmltop[["Header"]],name='description')
  transforms <- getNodeSet(xmlfile,"//PMML:DerivedField",c(PMML="http://www.dmg.org/PMML-4_2"))


