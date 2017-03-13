#################################################################################
# Developer: Benjamin Neely
# Date:      8/11/2014
# Name:      ui.R
# Desc:      This is the user interface file for the prediction model website.
#################################################################################
shinyUI(fluidPage(
  #   theme="bootstrap.min.css",
  list(tags$head(
    HTML('<link rel="icon", href="Icon-Small.png", type="image/png" />'),
                 includeScript("google-analytics.js"))),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(title="", windowTitle="TRILOGY ACS")
      ),
  headerPanel(
    title=div(img(src="Icon-40.png"), "TRILOGY ACS Bleeding Risk", align="left")
  ),
  tabsetPanel(
    id = 'model',
    tabPanel("GUSTO Moderate, Severe, Life-threatening (non-CABG)"),
    tabPanel("TIMI Minor, Major (non-CABG)")
  ),
    sidebarPanel(
        h4("Risk Factors"),
        uiOutput("Dynamic")
    ),
    mainPanel(
      h4("Risk Plot Over Time"),
      tags$a(href="https://github.com/benneely/trilogyWebsite","Source Code available on GitHub."),
      plotOutput('plot'),
      h4("Explanation of Bleeding Risk"),
      textOutput("text2")
      
    )
  )
)
