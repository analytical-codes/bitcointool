# This is a Shiny web app is the supplement of the DSS journal paper.
# Rpub hosts this web app:

# Authors:
# Autho1
# Author2
# Author3
# Author4
# Author5

#_________________________________________________________________________
############ Load Packages and User-Defined Functions into R#############

source("app_bitcoin_functions.R") # A File Contains Non Standard/packaged Functions
require(pacman)
pacman::p_load(shinydashboard,shiny,htmltools,dplyr, shinycssloaders,rmarkdown)

# library(shinydashboard)
# library(shiny)
# library(htmltools)
# library(dplyr)
# library(shinycssloaders)
# library(rmarkdown)
#________________________________________________________________________

################ User Interface of the App #################
ui <- dashboardPage(skin="green",
  
  # App header and its Contents Including the Help Menu
  dashboardHeader(title = "Bitcoin Prediction",
                  titleWidth = 500),
  
  # App Pages as shown on the SideBar
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem(HTML("<font size = \"5px\">  Home </font>"), tabName = "home", icon = icon("home")),
                     menuItem(HTML("<font size = \"5px\">  Manual Entry </font>"), tabName = "manual", icon = icon("hand-paper")),
                     menuItem(HTML("<font size = \"5px\">  Table Entry </font>"), tabName = "table", icon = icon("send")),
                     menuItem(HTML("<font size = \"5px\">  Source Code </font>"), tabName = "codes", icon = icon("laptop")),
                     menuItem(HTML("<font size = \"5px\">  About Us </font>"), tabName = "about_us", icon = icon("vcard")))
  ),
  
  # Contents of Each Page
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      {
      tabItem(tabName = "home",
              
              HTML("<h2> <b>Introduction </b></h2>"), 
              HTML("<p class = \"app\">  This interactive app enables short term traders to have a data driven
                        insight on the Bitcoin Proce movement in the next day. The prediction is based on a parsimonious 
                        and transparent decision trees model. Data of disparate sources are investigated for developing
                        the model. The details of model development is explained in our 
                        <i> Decision Support Systems </i> manuscript.
                        The app delivers two modules for performing the analysis:
                              <br> <b> (1) Manual Entry</b>, where 
                              users should calculate the technical indicators manually and
                              enter them into the system.
                             <br> <b>  (2) Table Entry</b>, where users can upload a tabular file that includes  the last 6 days of the market's data with a comma seperated variable (CSV) format.
                             <br> These modules are accessible through the left side tabs.  
                             Besides, informations about the research team and the source codes are provided in the last two tabs. 
                             <br> </p>
                             <button class=\"button\" onClick=\"window.open('http://www.youtube.com');\">
                             <h4> <b> Click here for a tutorial video!! </b>  </h4> </button>
                   
                               <p> <br> <br> <br> </p> 
                             "),
              
              fluidRow(column(4, panel_div(class_type = "primary", 
                                           panel_title = "About the App", 
                                           content = HTML("<b> Version: </b> 1.0.0.
                                                   <br> <b> Last Updated at </b> Janury 20, 2021</br>
                                                   <b> Author: </b> unanimous
                                                   <br> <b> Status: </b> Running"))),
                       column(4, panel_div(class_type = "primary", 
                                           panel_title = "Contact Info:",
                                           content = HTML("This application is maintained by: 
                                         <br> <a href='mailto:unanimous@noname.edu?Subject=Bitcoin%Prediction%20Help' target='_top'>unanimous </a></br>
                                         <br></br>"))),
                       column(4, panel_div(class_type = "primary", 
                                           panel_title = "Copyrights", 
                                           content = HTML("<p> <b> Data: </b> is available from Yahoo Finance
                                                   <a href=\"https://finance.yahoo.com/\">here</a>. </p>
                                                   <p> <img height = \" 28\", src=\" http://i.creativecommons.org/p/zero/1.0/88x31.png\"> </img>
                                                                 <style=\"text-align:justify\"> 
                                                   <b> All Codes: </b> CC0 - 'No Rights Reserved' .
                                                   </p></br>")))))},
      
      
      # Manual Entry Page
      tabItem(tabName = "manual",
              HTML("<p class = \"app\">   
                   You must provide all the values to see the results! Check 
                   <a href=\"https://finance.yahoo.com\" target=\"_blank\">Yahoo Finance!</a>
                   </p>"),
              fluidRow(
                column(2,
                       sidebarLayout(
                         sidebarPanel(width = 70, id="sidebar",
                                      HTML("<p.h> <b> <font size=\"4px\"> Numeric Inputs  </font> </b> </p>"),
                                      numericInput(inputId="BTC_USD_close_diff", 
                                                   label=HTML("Difference of Consecutive Bitcoin Close Price"), 
                                                   value = "", min = -5000, max = 5000),
                                      numericInput(inputId="dollar2yuan_Open", 
                                                   label=HTML("Dollar/Yuan Exchange Rate Open Price"), 
                                                   value = "", min = 0.01, max = 100),
                                      numericInput(inputId="stock_Adjusted", 
                                                   label="Adjusted Stock Price of Apple ", value = "", min = 0, max = 30000),
                                      numericInput(inputId="dji_close_sma3_diff", 
                                                   label="Difference of consecutive 3-Day Moving Average of Dow Jones Close Price", 
                                                   value = "", min = -10000, max = 10000),
                                      numericInput(inputId="BTC_USD_Open_ema5", 
                                                   label="5-Day Exponential Moving Average of Bitcoin Open Price", value = "", min = 0, max = 200000)),
                         mainPanel( width = 0)
                       )
                )
              )
              # end of fluidRow
              ,
              # two conditions for showing the Run button: inputing file & not clicking on the Run button
              
              tags$head(
                tags$style(HTML('#runit_m{background-color:#4682b4}'))
              ),
              actionButton("runit_m",h2("Run"), style='padding:24px; font-size:80%'),
              conditionalPanel(
                condition = "input.runit_m",
                verbatimTextOutput("result_m")
              ),

              br(),
              conditionalPanel(
                condition = "output.button && !input.runit_m",
                h1("Loading...")
              ),
              conditionalPanel(
                condition = "input.runit_m",
                textOutput("explain"),
                br(),
                textOutput("note_m")%>% withSpinner(color="#0dc5c1"),
                br()
              ),
              
              br(),
              
              # the download button runs after calculation of survival ends
              conditionalPanel(condition = "input.runit_m",
                 br(),
                 br(),
                 br()
                               
              )
              
              ),# end of manual
      
      tabItem(tabName = "table",
              HTML("<h4> To find a template file that contains the data required for the prediction,  
    click <a href=\"https://github.com/analytical-codes/bitcointool/blob/main/datasample.csv\"> here. </a> 
    As you can see, you need to enter data of 6 days transactions in different markets. If you do not
   have some of the transactions data leave them empty, the tool will extrapolate those dates.</h4>"),
              
              fluidRow(
                column(12,sidebarLayout(
                  sidebarPanel(width = 12, id="sidebar_csv",
                               HTML("<p.h> <b> <font size=\"4px\">  Upload a CSV file: </font> </b> </p>"),
                               fileInput("file","The bitcoin open price for all the days required!" , 
                                         accept=c('text/csv', '.csv')
                               )
                  ),
                  mainPanel( width = 0)
                ) )
                #
              ),
              # two conditions for showing the Run button: inputing file & not clicking on the Run button
              conditionalPanel(
                condition = "output.button && !input.runit",
                tags$head(
                  tags$style(HTML('#runit{background-color:#4682b4}'))
                ),
                actionButton("runit",h2("Run"), style='padding:24px; font-size:80%')
                
                
              ),

              br(),
              conditionalPanel(
                condition = "input.runit",
                textOutput("note")%>% withSpinner(color="#0dc5c1")
              ),

              conditionalPanel(
                condition = "output.button",
                verbatimTextOutput("result"),
                textOutput("error_emptycsv"),
                br()
              )
              ), # For automated (sidebar_csv) Tab Items
      
      # Code
      tabItem(tabName = "codes",
              HTML("<h2> <b>Repository of the Source Codes</b></h2>"), 
              HTML("<p class = \"app\">  We have created an open repository that contains all the 
      codes. We invite the community to review and contribute into it: 
      <a href=\"https://github.com/analytical-codes/bitcointool/\" target=\"_blank\">Repository</a>. 
                   </p>")
      ),
      
      
      # About Us Page
      tabItem(tabName = "about_us",
              div(),
              div(style="clear: left;",
                  p(style="float: left;",
                    img(src="https://static.wixstatic.com/media/0f788f_182d11209baa4a9ea10321564e9d5a4e~mv2.jpg", 
                        height="120", width="120", border="0px", hspace="20 ")),
                  p(class = "app","The Authors are unknown during the peer review period.")
              ),
              div(),
              div(style="clear: left;",
                  p(style="float: left;",
                    img(src="https://static.wixstatic.com/media/0f788f_182d11209baa4a9ea10321564e9d5a4e~mv2.jpg", 
                        height="120", width="120", border="0px", hspace="20 ")),
                  p(class = "app","The Authors are unknown during the peer review period.")
              ),
              div(),
              div(style="clear: left;",
                  p(style="float: left;",
                    img(src="https://static.wixstatic.com/media/0f788f_182d11209baa4a9ea10321564e9d5a4e~mv2.jpg", 
                        height="120", width="120", border="0px", hspace="20 ")),
                  p(class = "app","The Authors are unknown during the peer review period.")
              ),
              div(),
              div(style="clear: left;",
                  p(style="float: left;",
                    img(src="https://static.wixstatic.com/media/0f788f_182d11209baa4a9ea10321564e9d5a4e~mv2.jpg", 
                        height="120", width="120", border="0px", hspace="20 ")),
                  p(class = "app","The Authors are unknown during the peer review period.")
              ),
              div(),
              div(style="clear: left;",
                  p(style="float: left;",
                    img(src="https://static.wixstatic.com/media/0f788f_182d11209baa4a9ea10321564e9d5a4e~mv2.jpg", 
                        height="120", width="120", border="0px", hspace="20 ")),
                  p(class = "app","The Authors are unknown during the peer review period.")
              )
      )
      
              ) # Tab items
      ) # For Dashboard Body
              ) # For Dashboard Page


####################################################
server <- function(input, output,session) {
  
  output$button <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, "button", suspendWhenHidden = FALSE)
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  
  parameters<-reactiveValues()
  
  observe({
    file1 <-input$file

    if(!is.null(file1)){parameters$data_base<-read.csv(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = FALSE)}
    
    # manual entry
    {
      parameters$dollar2yuan_Open<-input$dollar2yuan_Open
      parameters$stock_Adjusted<-input$stock_Adjusted
      parameters$dji_close_sma3_diff<- input$dji_close_sma3_diff
      parameters$BTC_USD_Open_ema5<- input$BTC_USD_Open_ema5
      parameters$BTC_USD_close_diff<- input$BTC_USD_close_diff
    }
    #   
  })
  
  observeEvent(input$runit, {
    observe({
      
      inFile <- input$file
      
      req(inFile)
      # outcomes <- prob_cal(parameters$data_base)
      outcomes <- prob_cal(parameters$data_base,style="automated")
      if(outcomes$err_check=="NO"){
        output$result<-renderPrint({outcomes$prob_cal})
      }else{output$result<-renderPrint(outcomes$error_message)}
      
      
      output$error_emptycsv<-renderText({outcomes$threshold})
      output$note<-renderText(c("Refresh the tool for new calculations"))
      
    })
    
  })
  #outcomes$survival_Probability output$error_no  output$error_vars output$error_range output$error_cat
  #output$note
  observeEvent(input$runit_m, {
    
    outcomes_m <- prob_cal(manual2df(parameters),style="manual")
    
    if(outcomes_m$err_check=="NO"){
      output$result_m<-renderPrint({outcomes_m$prob_cal})
    }else{output$result_m<-renderPrint(outcomes_m$error_message)}
    
    output$explain<-renderText({outcomes_m$threshold})
    output$note_m<-renderText(c("After multiple runs you may need to refresh the tool."))
    
    
  })
  

  

  
}# end of server

# Run the application
shinyApp(ui, server)