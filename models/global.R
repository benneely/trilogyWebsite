library(XML)
library(shiny)
library(ggplot2)
library(scales)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Additional functions needed for this app
#Function needed in server.R to coerce user input to numeric data.frame
checkandconvert <- function(x) {
  if (as.character(x) %in% c('No Angiography','No Beta Blocker','Unstable Angina','Male','No History')) {
    return(as.numeric(0))
  } else if (as.character(x) %in% c('Yes Angiography','Yes Beta Blocker','NSTEMI','Female','Yes History')) {
    return(as.numeric(1))
  } else {
    as.numeric(as.character(x))
  }
}
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
#Assuming one new variable has to be created
generateTransform <-function(x) {
  return(max(0,x-130))
}

  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Establish a connection with the database
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Let's put together a xml object so that we can use certain functions to parse
  xmlfile <- xmlParse("trilogyGUSTO_Manual.pmml")
  xmltop <- xmlRoot(xmlfile) #gives content of root
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Get data elements in R friendly formats (i.e. data.frames / numeric vectors)
  dict <- xmltop[["DataDictionary"]]
  referencePoints <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["ParameterList"]],value="referencePoint",name="label",char=F)
  mapping <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["PPMatrix"]], value="predictorName", name="parameterName",char=T)
  betas <- xml2dfNAMED(xmlRooters=xmlChildren(xmltop[["GeneralRegressionModel"]])[["ParamMatrix"]], value="beta", name="parameterName",char=F)
  betacomb2<- betas[1,]
  colnames(betacomb2) <- sapply(mapping[1,],as.character)
  time <- as.numeric(unname(sapply(xmlChildren(xmlChildren(xmltop[["GeneralRegressionModel"]])[["BaseCumHazardTables"]]), xmlGetAttr, "time")))
  cumHazard <- as.numeric(unname(sapply(xmlChildren(xmlChildren(xmltop[["GeneralRegressionModel"]])[["BaseCumHazardTables"]]), xmlGetAttr, "cumHazard")))
  #Transformations
  transforms <- getNodeSet(xmlfile,"//PMML:DerivedField",c(PMML="http://www.dmg.org/PMML-4_2"))


