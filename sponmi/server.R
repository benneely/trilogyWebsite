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
    if (is.null(input$AGEYR)) {
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
  ## FOR QC PURPOSES ONLY:
  # input <- list(HEMOGLBN_IMP=5.7,AGEYR=26,char_CANGIO_IMP="No Angiography",CREATCN_IMP=0.10,SBP_IMP=75,char_BETABLCK="No Beta Blocker",char_NSTEMI="NSTEMI",
  #              char_female="Male", char_HXPUD_INP="No History", WGTKGBL_IMP=26)
  QQ <- reactive({

    if (is.null(input$AGEYR)) {
      return(NULL)
    } else {
      QQ <- vector("list",xmlSize(dict)-3)
      for(i in 1:xmlSize(dict)){
        if  (!xmlAttrs(dict[[i]])[["name"]] %in% c("event","TIMETO","EVENTINDICATOR")) {
          QQ[[ i ]] <- eval(parse(text = paste("input$", xmlAttrs(dict[[i]])[["name"]], sep="")))
          names(QQ[[ i ]]) <- xmlAttrs(dict[[i]])[["name"]]
      } }
      cnames <- names(unlist(QQ))
      indx <- which(sapply(QQ, is.character))
      QQ[indx] <- unlist(lapply(QQ[indx], checkandconvert ))
      a <- unlist(QQ) 
      b <- data.frame(as.list(a),stringsAsFactors=F)
      c <- b
      colnames(c) <- cnames   
      ## FOR QC PURPOSES ONLY:
      # QQ <- function() { return(c)}
      return(c)
    }
  })
  #==========================================
  #Statistical magic to get risk from coxph
  #==========================================
  x2 <- reactive({

    if (is.null(input$AGEYR)) {
      return(NULL)
    } else {
    c                 <- QQ()
    #######################################################################
    #MANUALLY CUREATE TRANSFORMASTIONS HERE (DON"T KNOW HOW TO DO THIS YET)
    #######################################################################
    c$creatcn_imp85      <- max(0,c$CREATCN_IMP-0.85)
    print(c) 
    print('***********************')
    same              <- intersect(names(c),names(betacomb2))
    same2             <- intersect(names(c),names(referencePoints))
    
  # this calculation is done in accordance with the PMML Specification
    r                 <- as.matrix(c[same])                 %*% t(as.matrix(betacomb2[same]))
    s                 <- as.matrix(referencePoints[same2])  %*% t(as.matrix(betacomb2[same2]))
    H_t               <- cumHazard*exp(r-s)
    S_t               <- exp(-H_t)
    C_t               <- 1-S_t
    final             <- data.frame("Time"=time,
                                    "char_Time"=as.character(time),
                                    "Risk"=C_t)
   ## FOR QC PURPOSES ONLY:
   # x2 <- function() { return(final)}
    #return the predicted risk of the event
    return(final)
    }
  })
  
#   interactiveClickDF <- reactive({
#     if (is.null(input$AGEYR)) {
#       return(NULL)
#     } else {
#       if (is.null(input$plot_click$x)) {
#         mydf <- data.frame(x=max(x2()$Time),y=max(x2()$Risk))
#       }
#       else {
#         mydf <- data.frame(x=input$plot_click$x,y=input$plot_click$y)
#       }
#       print(mydf)
#       return(mydf)
#     }
#     # interactiveClickDF <- function() { return(mydf)}
#     
#   })
  
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
  })
  #==========================================
  #Graphics to help with user interpretation
  #==========================================
  output$plot <- renderPlot({
    if (is.null(input$AGEYR)) {
      return(NULL)
    } else {
      return(
    ggplot(data=x2()) + 
      geom_step(aes(x=Time,y=Risk), direction="hv",size=1, colour="#e41a1c")+
      scale_y_continuous( breaks = seq(0, 1, by = 0.1), limits = c(0,1), labels = percent) +
      scale_x_continuous( breaks = seq(0, max(x2()$Time), by = round(max(x2()$Time)/9,digits=1)), limits = c(0,max(x2()$Time))) +
      theme(panel.grid.major=element_blank(),
            legend.title=element_blank(),
            legend.position="top")+
      theme_bw()+
#       geom_vline(xintercept=isolate(interactiveClickDF()$x),colour="blue",linetype="longdash")+
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
          paste0(formatC(tail(x2()[which(x2()$Time<=900),]$Risk*100,n=1),format="f",digits=2),"%"),
          " chance of experiencing a ",input$model," spontaneous mi  event at ", paste0(formatC(tail(900,n=1),format="f",digits=0)),
          " days of follow-up.",
         # "To determine the risk at different points in time, use the mouse and hover over the risk line.",
          sep="")
    })
})
