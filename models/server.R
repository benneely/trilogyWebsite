#################################################################################
# Developer: Benjamin Neely
# Date:      5/10/2015
# Name:      server.R
# Desc:      This is the server.R file for the prediction matrix website.
#################################################################################
#Beging that shiny stuff-----------------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  #==========================================
  #Check User Input, if exceeds limits scale 
  #up / back
  #==========================================
  observe({
    if (is.null(input$CREATCN_IMP)) {
      return(NULL)
    } else {
    for(i in 1:xmlSize(dict)){
      if  (!xmlAttrs(dict[[i]])[["name"]] %in% c("event","TIMETO","EVENTINDICATOR")) {
        if ( (xmlAttrs(dict[[i]])[["optype"]]=="continuous") & 
               (!is.na(eval(parse(text = paste("input$", xmlAttrs(dict[[i]])[["name"]], sep=""))))) ){
          if (  eval(parse(text = paste("input$", xmlAttrs(dict[[i]])[["name"]], sep="")))
                >
                as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["rightMargin"]]) ) {
            eval(parse(text = paste0("updateNumericInput(session,\"",xmlAttrs(dict[[i]])[["name"]],"\", value = ", 
                                     as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["rightMargin"]]),")")))
          } else if (
                eval(parse(text = paste("input$", xmlAttrs(dict[[i]])[["name"]], sep="")))
                <
                as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["leftMargin"]])
            ) {
            eval(parse(text = paste0("updateNumericInput(session,\"",xmlAttrs(dict[[i]])[["name"]],"\", value = ", 
                                     as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["leftMargin"]]),")")))
          }
        }
      }
    }
    }
  })
  #==========================================
  #Create a dynamic data.frame from usr input
  #==========================================
  QQ <- reactive({
    if (is.null(input$CREATCN_IMP)) {
      return(NULL)
    } else {
      QQ <- vector("list",xmlSize(dict)-3)
      for(i in 1:xmlSize(dict)){
        if  (!xmlAttrs(dict[[i]])[["name"]] %in% c("event","TIMETO","EVENTINDICATOR")) {
          QQ[[ i ]] <- eval(parse(text = paste("input$", xmlAttrs(dict[[i]])[["name"]], sep="")))
          names(QQ[[ i ]]) <- xmlAttrs(dict[[i]])[["name"]]
      } }
      a <- unlist(QQ) 
      b <- data.frame(as.list(a))
      c <- b
      indx <- sapply(b, is.factor)
      c[indx] <- lapply(b[indx], checkandconvert )
      print(c)     
      return(c)
    }
  })
  #==========================================
  #Statistical magic to get risk from coxph
  #==========================================
  x2 <- reactive({
    if (is.null(input$CREATCN_IMP)) {
      return(NULL)
    } else {
      if (input$model=="GUSTO Moderate, Severe, Life-threatening (non-CABG)"){
    c                 <- QQ()
    c$SBP_IMP130      <- generateTransform(c$SBP_IMP)
    same              <- intersect(names(c),names(betacomb2))
    same2             <- intersect(names(c),names(referencePoints))
    
  # this calculation is done in accordance with the PMML Specification
    r                 <- as.matrix(c[same])                 %*% t(as.matrix(betacomb2[same]))
    s                 <- as.matrix(referencePoints[same2])  %*% t(as.matrix(betacomb2[same2]))
    H_t               <- cumHazard*exp(r-s)
    S_t               <- exp(-H_t)
    C_t               <- 1-S_t
    final             <- data.frame("Time"=time,
                                    "Risk"=C_t)
    #return the predicted risk of the event
    return(final)
    } else {
      #---------------------------------
      xmlfile <- xmlParse("trilogyTIMI_Manual.pmml")
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
      #---------------------------------
      c                 <- QQ()
      same              <- intersect(names(c),names(betacomb2))
      same2             <- intersect(names(c),names(referencePoints))
      
      # this calculation is done in accordance with the PMML Specification
      r                 <- as.matrix(c[same])                 %*% t(as.matrix(betacomb2[same]))
      s                 <- as.matrix(referencePoints[same2])  %*% t(as.matrix(betacomb2[same2]))
      H_t               <- cumHazard*exp(r-s)
      S_t               <- exp(-H_t)
      C_t               <- 1-S_t
      final             <- data.frame("Time"=time,
                                      "Risk"=C_t)
      #return the predicted risk of the event
      return(final)
      
    }
    }
  })
  #=====================================================================================================================================================
  #=====================================================================================================================================================
  #RENDERING OUTPUT FOR UI
  #=====================================================================================================================================================
  #=====================================================================================================================================================
  #==========================================
  #Dynamic Risk Factors Printed
  #==========================================
  output$Dynamic <- renderUI({
    LL <- vector("list",xmlSize(dict)-3)
    for(i in 1:xmlSize(dict)){
      if  (!xmlAttrs(dict[[i]])[["name"]] %in% c("event","TIMETO","EVENTINDICATOR")) {
        if (xmlAttrs(dict[[i]])[["optype"]]=="continuous"){
          #Matt and Joakim didn't like the sliderInput so, well give them open field instead
          LL[[i]] <- numericInput(inputId=xmlAttrs(dict[[i]])[["name"]], 
                                  label=xmlAttrs(dict[[i]])[["displayName"]], 
                                  step=as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["size"]]),
                                  min=as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["leftMargin"]]), 
                                  max=as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["rightMargin"]]), 
                                  value=as.numeric(xmlAttrs(xmlChildren(dict[[i]])$Interval)[["leftMargin"]]))
        }
        if (xmlAttrs(dict[[i]])[["optype"]]=="categorical") {
          LL[[i]] <- selectInput(inputId=xmlAttrs(dict[[i]])[["name"]], 
                                 label=xmlAttrs(dict[[i]])[["displayName"]],
                                 choices=unname(sapply(xmlChildren(xmltop[["DataDictionary"]][[i]]), xmlGetAttr, "value")),
                                 selected=sapply(xmlChildren(xmltop[["DataDictionary"]][[i]]), xmlGetAttr, "value")[[1]])}}}
    return(LL) 
    print(LL)
  })
  #==========================================
  #Graphics to help with user interpretation
  #==========================================
  output$plot <- renderPlot({
    if (is.null(input$CREATCN_IMP)) {
      return(NULL)
    } else {
      return(
    ggplot(data=x2()) + 
      geom_step(aes(x=Time,y=Risk), direction="hv",size=1,colour="#e41a1c")+
      scale_y_continuous( breaks = seq(0, 1, by = 0.1), limits = c(0,1), labels = percent) +
      scale_x_continuous( breaks = seq(0, 900, by = 100), limits = c(0,920)) +
      theme(panel.grid.major=element_blank(),
            legend.title=element_blank(),
            legend.position="top")+
      theme_bw()+
      ylab("Probability of an Event Occuring")+
      xlab("Days")
      )
    }
  })
  #==========================================
  #Text to help with user interpretation
  #==========================================
  output$text2 <- renderText({
    paste("A person with the constellation of risk factors given at the left has a ", 
          paste0(formatC(x2()$Risk[tail(which(x2()$Time<=900),n=1)]*100,format="f",digits=2),"%"),
          " chance of experiencing a ",input$model," bleeding event at 900 days of follow-up.",
          sep="")
    })
})