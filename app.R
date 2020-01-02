# This script produces a Shiny app that allows the user to manipulate and analyze the PSP data by choosing from a list of parameters (BETA)
# Note: Some of the HTML functionality of this app requires that it be run in a browser (in dropdown menu of "Run App" button, select 'Run External')
# Written by Colin Donald

TS <- Sys.time()

# Packages ----------------------------------------------------------------

library(plyr)
library(dplyr)
library(data.table)
library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(reshape2)
library(tidyr)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(httr)
library(XML)
library(xml2)
library(stringr)
library(webshot)
library(htmltools)
library(htmlwidgets)
library(shinyalert)
library(compiler)

source("appData/Credentials.R")

#### Bring in combined LASAR and ELEMENT Data ####

source("appData/AllDataforApp.R")

#### Bring in variables to be used across app ####

source("appData/BasinMapforApp.R")

LandUse <- fread("appData/PSPStationLandUse.csv") #Upstream land use by station
bsnLandUse <- fread("appData/PSPBasinLandUse.csv") #Land use within the PSP basin
bsnCrops <- fread("appData/cdlBasinStats2017.csv") #2017 cdl crop breakdown within each PSP basin
Terms <- fread('appData/Glossary of Terms.csv') #Reference material
IngredientDescriptions <- fread('appData/Ingredient Descriptions.csv') #Reference material
symbolCodes <- fread("appData/plotlyShapes.csv") #Symbols to be used with Plotly package
symbolCodes <- symbolCodes$symbol #Symbol codes as list
toImage2 <- list(list(name = "Download Plot (Large)", #Additional button for Plotly modebar that downloads
                      icon = list(width = 1000, ascent = 800,descent = -50,
                                  path = "m500 450c-83 0-150-67-150-150 0-83 67-150 150-150 83 0 150 67 150 150 0 83-67 150-150 150z m400 150h-120c-16 0-34 13-39 29l-31 93c-6 15-23 28-40 28h-340c-16 0-34-13-39-28l-31-94c-6-15-23-28-40-28h-120c-55 0-100-45-100-100v-450c0-55 45-100 100-100h800c55 0 100 45 100 100v450c0 55-45 100-100 100z m-400-550c-138 0-250 112-250 250 0 138 112 250 250 250 138 0 250-112 250-250 0-138-112-250-250-250z m365 380c-19 0-35 16-35 35 0 19 16 35 35 35 19 0 35-16 35-35 0-19-16-35-35-35z"),
                      click=JS("function(gd) {
                               Plotly.downloadImage(gd, {
                               format: 'jpeg',
                               filename: 'custom_plot',
                               width: 1400,
                               height: 900
                               })
                               }")
))
# configs <- c(displayModeBar='hover', editable=TRUE, showTips=TRUE, showAxisDragHandles=TRUE, showAxisRangeEntryBoxes=TRUE)
BasinNames <- as.character(unique(AllData_NoVoid$Project)) #Names of each PSP Basin
PollutantNames <- as.character(unique(AllData_NoVoid$Analyte)) #All analyte names
StationIDs <- unique(AllData_SumBy_Station$StationDescription) #All station descriptions
AllData_NoVoid$AboveBench <- ifelse(AllData_NoVoid$AQL_Ratio > 1, 1, ifelse(is.na(AllData_NoVoid$AQL_Ratio), NA, 0)) #Add column indicating whether the result is above benchmark
Vars <- as.character(colnames(AllData_NoVoid)) #Store all variable names in the main dataset
AllData_NoVoid$Sampling_Date <- as.Date(AllData_NoVoid$Sampling_Date, "%Y-%m-%d") #Change format of date in main dataset

easyPrintPlugin <- htmlDependency(name = "leaflet-easyprint",
                                  version = "2.2.1",
                                  src = c(file = "www/Leaflet.EasyPrint/dist"),
                                  script = "bundle.js")

# Source functions for app

source("appData/app_functions.R")

#### Create User Interface with tabs, panels, inputs, etc) ####

ui1 <- function(){
  fluidPage(
    theme = shinytheme("flatly"),
    tags$head(HTML('<link rel="icon" href = "deqlogo.ico" >')), #Add DEQ logo to browser tab (must be ran in HTML)
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle= 'Pesticide Stewardship Partnership' #Add PSP title to browser tab
        )
    ),
    # conditionalPanel("!output.gogo",
      # "input.Password != output.passw0rd || !output.gogo",
      passwordInput('Password','Password'),
      fluidRow(column(1,actionButton('go', "Enter")), column(3, textOutput('tryAgain'))),
      h3("Please input password to continue"),
    br(),
    h5(HTML("<i>If you experience performance issues while using Internet Explorer, try using an alternate browser</i>"))
    # )
  )
}

ui2 <- function(){
fluidPage(
    theme = shinytheme("flatly"),
    tags$head(HTML('<link rel="icon" href = "deqlogo.ico" >')), #Add DEQ logo to browser tab (must be ran in HTML)
    div(style="padding: 1px 0px; width: '100%';",
        titlePanel(
          title="", windowTitle= 'Pesticide Stewardship Partnership' #Add PSP title to browser tab
        )
    ),
    
  #Add DEQ logo and PSP title to navbar
  navbarPageWithText(title = 
               div(img(src = "deqlogo.ico"),
                   a("Pesticide Stewardship Partnership", 
                     href="http://www.oregon.gov/ODA/programs/Pesticides/Water/Pages/PesticideStewardship.aspx", target="_blank")
               ), id = "navTab",
             tabPanel(icon=icon("bar-chart"), "Data Summaries", value = "dataSum",
                      # Popup to suggest user guide and references
                      modalDialog(strong("Welcome to the Pesticide Stewardship Partnerships Data Viewer! If you are new to this tool, check out the User Guide by clicking on the ",
                                         img(src = "UserGuide.PNG"), " link in the Navigation Bar at the top of the window.",
                                         br(),br(),
                                         "You can find definitions, analyte descriptions and more in the ", img(src = "References.PNG"), " tab.",
                                         br(),br(),
                                         em(HTML("<center>"),"Please Note",HTML("<br>"),"This data viewer was developed to support Oregon's Pesticide Stewardship Partnership program, 
                                            a voluntary program aimed to identify water quality issues related to pesticides, share monitoring results, 
                                            implement solutions, and measure success. This data viewer provides access to information supporting this program and 
                                            is not intended to be a comprehensive source of information about pesticide distribution in state waters.",HTML("</center>"))),
                                  footer = modalButton("Dismiss"), fade = TRUE, easyClose = TRUE),
                      #### Sidebar code for parameter selection for all tabs ####
                      sidebarLayout(
                        div(id = "sidebarPanel", sidebarPanel(
                          div(tags$style(type='text/css', "#sideBarHide {float: right;}"), 
                              actionLink('sideBarHide', div(icon('chevron-left'),icon('chevron-left'),""))),
                          sliderInput('Year', "Years", 1999, 2019, c(1999:2019),
                                      value = c(2012,2018),
                                      dragRange = TRUE,
                                      step = 1,
                                      sep = '',
                                      animate = animationOptions(
                                        loop = TRUE,
                                        interval = 1500)),
                          div(HTML("<em>Note: Most recent year's data may not be complete.</em>")),
                          br(),
                          radioButtons("Basin", "Basins",
                                       choices = c("All", BasinNames)),
                          leafletOutput('pspMap', width = '100%', height = 300),
                          width = 2
                        )),
                        #Main panel holds the meat of the data visualization
                        div(id = "mainPanel1",
                            mainPanel(width = 10,
                                      # fluidRow(div(style = "height: 10px;",checkboxInput('sidebar', "Show Sidebar", value = TRUE))),
                                      div(div(type = 'text/css', style = "height: 10px; float: left;", hidden(actionLink('sideBarShow',div(icon('chevron-right'),icon('chevron-right'),"")))),
                                              h2(strong(textOutput('tab1Title')))
                                          # ,
                                          # tags$body()
                                          ),
                                      tabsetPanel(id="Tab", type = 'pills',
                                                  
                                                  #### Tab1 - UI for tab with summaries for all analytes within the selected area ####
                                                  
                                                  tabPanel("All Analytes", value = "allAnalytes",
                                                           mainPanel(width = 12,
                                                                     tabsetPanel(
                                                                       # width = 12,
                                                                            tabPanel("Detection Frequency", value='all1', plotlyOutput("plot1", width = "100%", height = "500px")),
                                                                            tabPanel("Frequency Over 50%", value='all2', plotlyOutput("plot2", width = "100%", height = "500px")),
                                                                            tabPanel("Aquatic Life Ratio", value='all3', plotlyOutput("plotALR", width = "100%", height = "500px"))
                                                                     ),
                                                                     checkboxInput("OnlyDetects", "Only Detected Analytes", TRUE),
                                                                     br(),
                                                                     dataTableOutput("results") #dataTableOutput allows user to manipulate the table (vs tableOutput)
                                                           )
                                                  ),
                                                  
                                                  #### Tab2 - UI for tab that provides summary statistics for individual analytes ####
                                                  
                                                  tabPanel("Analyte Per Year", value = "analytePerYear",
                                                           mainPanel(
                                                             width = 12,
                                                             fluidRow(column(1, tags$body(h3('Inputs: '))),
                                                                      column(5, selectizeInput('Station2', "StationID",
                                                                                               choices = c('All', StationIDs), selected = 'All',
                                                                                               options = list(placeholder = "Choose Stations"),
                                                                                               multiple = TRUE, width = '100%', size = 4)),
                                                                      column(5, selectInput('Analyte', "Analyte", choices = PollutantNames, width = '100%'))
                                                             ),
                                                             tags$body(h3(textOutput('tab2Title2'))),
                                                             tabsetPanel(
                                                               tabPanel("Average/Max", value='apr1',
                                                                        selectInput('Variable', 'Variable',
                                                                                    choices = c('Average', 'Median'), width = '100%'),
                                                                        plotlyOutput("AvgMaxTab2", width = "100%", height = "100%")),
                                                               tabPanel("Detection Frequency", value='apr2',
                                                                        plotlyOutput("DetFreqTab2", width = "100%", height = "100%"),
                                                               br(), br(),
                                                               dataTableOutput("tab2results")),
                                                               tabPanel("Detections by Station", value='apr3',
                                                                        plotlyOutput('detectsStationPlot', width = '100%', height = '100%'))
                                                             )
                                                             # plotlyOutput("resultPlot", width = "100%", height = "100%"),
                                                           )
                                                  ),
                                                  
                                                  #### Tab3 - UI for tab that provides summary statistics for specific station and analyte selections ####
                                                  
                                                  tabPanel("Station Summary", value = "stationSum",
                                                           mainPanel(width = 12,
                                                                     fluidRow(
                                                                       column(1, tags$body(h3("Inputs: "))),
                                                                       column(5, selectizeInput('Station', "StationID",
                                                                                                choices = c('All', StationIDs),
                                                                                                options = list(placeholder = "Choose Stations"),
                                                                                                multiple = TRUE, width = '100%')),
                                                                       column(5, selectizeInput('Analyte2', "Analyte",
                                                                                                choices = c('All', PollutantNames),
                                                                                                options = list(placeholder = 'Choose Analytes'),
                                                                                                multiple = TRUE, width = '100%'))
                                                                     ),
                                                                     textOutput("helpText"),
                                                                     tableOutput("StationDescription"),
                                                                     
                                                                     tabsetPanel(
                                                                       tabPanel("Detection Frequency", value='ss1',
                                                                                plotlyOutput("tab3plot1", width = "100%", height = "100%"),
                                                                                br(), br(),
                                                                                dataTableOutput("tab3results")
                                                                       ),
                                                                       tabPanel("AQL Ratio", value='ss2',
                                                                                plotlyOutput("tab3plot2", width = "100%", height = "100%")
                                                                       )
                                                                       # tabPanel("Historical Avg/Max",
                                                                       #          plotlyOutput("tab3plot3", width = "155%", height = "100%")
                                                                       # )
                                                                     )
                                                           )
                                                  ),
                                                  
                                                  #### Tab4 - UI for tab that produces a spatial representation of detections for the selected analytes ####
                                                  
                                                  tabPanel(icon=icon("map"), "Detection Map", value = "detectionMap",
                                                           mainPanel(width = '100%',
                                                                     fluidRow(
                                                                       column(3,
                                                                              div(style="z-index: 1001; position: relative;", selectInput('Analyte4', "Analyte", choices=PollutantNames,
                                                                                          selected = 'diuron', multiple=FALSE))
                                                                       ),
                                                                       column(3,
                                                                              selectInput('ALR', 'Aquatic Life Ratio',
                                                                                          choices = c('above 0'=0, 'above 0.1'=0.1, 'above 0.5'=0.5, 'above 1.0'=1)
                                                                                          # , inline = TRUE
                                                                              )),
                                                                       column(1, checkboxInput('opaqueBasins', 'Opaque Basins?', value = FALSE)),
                                                                       column(1, div(type='text/css', style= "line-height: 65px; vertical-align: middle; align: center;", 
                                                                                     a("NLCD Legend", href="https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend", target="_blank"))
                                                                       ),
                                                                       column(1,div(type='text/css', style= "line-height: 65px; vertical-align: middle; align: center;", 
                                                                                    a("Crop Legend", href="https://www.nass.usda.gov/Research_and_Science/Cropland/docs/US_2017_CDL_legend.jpg", target="_blank"))
                                                                       ),
                                                                       column(1, offset=1, div(type='text/css', style="float: right; line-height: 65px; vertical-align: bottom;",
                                                                                               actionButton('expand', "Expand"), hidden(actionButton('collapse', "Collapse")))
                                                                       )
                                                                     ),
                                                                     leafletOutput('detectionMap', width = '100%'), br(),
                                                                     HTML("<i>click on an individual station, basin, or detection to view</i>"),
                                                                     tabsetPanel(
                                                                       tabPanel("Detection Plot",
                                                                                plotlyOutput('subPlot'),
                                                                                dataTableOutput("subPlotData")
                                                                       ),
                                                                       tabPanel("Upstream Land Use",
                                                                                fluidRow(column(9,plotlyOutput("cdlPlot", height = '500px')),
                                                                                         column(3, plotlyOutput('landUsePlot', height = '500px'))),
                                                                                br(),
                                                                                HTML("Crop data provided by the US Department of Agriculture 2017 <a href='https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php' target='_blank'> Crop Data Layer</a>"),
                                                                                br(),
                                                                                HTML("Land use data provided by the USGS 2011 <a href='https://www.mrlc.gov/nlcd2011.php' target='_blank'> National Land Cover Dataset</a>"),
                                                                                br()
                                                                       )
                                                                     )
                                                           )
                                                  )
                                      )
                            )
                        )
                      )
             ),
             
             #### Navbar Panel 2 - UI for reference data to help users navigate the application ####
             
             tabPanel(icon=icon("book"), "Reference", value = "e",
                      mainPanel(width = 20,
                                tabsetPanel(
                                  tabPanel("Glossary of Terms",
                                           dataTableOutput("terms")
                                  ),
                                  tabPanel("Analyte Descriptions",
                                           dataTableOutput("ingDescriptions")),
                                  tabPanel("Station Reference",
                                           dataTableOutput("stationRef")
                                  )
                                  # ,
                                  # tabPanel("Metadata",
                                  #          tableOutput("metadata")
                                  # )
                                  # tabPanel("Get New Data",
                                  #          passwordInput('passcode', "Enter Passcode", placeholder = "Passcode"),
                                  #          actionButton('go', "Enter"),
                                  #          textOutput("Message1"),
                                  #          textOutput("Message2")
                                  # )
                                )
                      )
             ),
             
             #### User Guide link on Navbar Panel ####
             
             text = shiny::HTML("<a href='PSPAppTutorial.html' target='_blank'>User Guide</a>")
  )
)
}

#### ui input ####
ui <- tagList(useShinyjs(),
              useShinyalert(),
              uiOutput("page"),
              uiOutput("modals"),
              # uiOutput("color"),
              uiOutput("panels"),
              uiOutput("popup"))
              
#### Where all of the computation happens (in relation to inputs) ####

server <- function(input, output, session) {
  
  #### Code for password authentication ####
  output$page <- renderUI(ui1())
  output$tryAgain <- renderText("")
  passcode <- reactive({
    if(is.null(input$Password)){
      'pass'
    } else {input$Password}
  })
  observeEvent(input$go, {
    if(input$Password == passw0rd){
      output$page <- renderUI(ui2())
    } else{output$tryAgain <- renderText("Incorrect Password")}
  })
  
  # Show or hide sidebar on button click
  observeEvent(input$sideBarHide, {
    hideElement(id='sidebarPanel')
    showElement(id='sideBarShow')
    output$panels <- renderUI(tags$style(type = "text/css", "#mainPanel1 {width: 120% !important;}"))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$sideBarShow, {
    showElement(id='sidebarPanel')
    hideElement(id='sideBarShow')
    output$panels <- renderUI(tags$style(type = "text/css", "#mainPanel1 {width: 100% !important;}"))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  #### Reactive variables to be used throughout app ####
  
  output$pspMap <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% addPolygons(data=bsnSelect(),
                                                                                          label = bsnSelect()@data$PSP_Name,
                                                                                          stroke = TRUE,
                                                                                          weight = 1, fillOpacity = 0.4, smoothFactor = 0.5,
                                                                                          fillColor = topo.colors(length(bsnSelect()@data$PSP_Name), alpha = NULL),
                                                                                          highlight = highlightOptions(
                                                                                            weight = 5,
                                                                                            color = "#666",
                                                                                            # dashArray = "",
                                                                                            fillOpacity = 0.7,
                                                                                            bringToFront = FALSE),
                                                                                          labelOptions = labelOptions(
                                                                                            style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                            textsize = "15px",
                                                                                            direction = "auto")
    )
  })
  
  # unique((AllData_NoVoid %>%
  # filter(Project %in% Basin1()))$Station_ID
  # )
  
  Basin1 <- reactive({
    if(input$Basin == "All"){BasinNames} else {input$Basin}
  })
  
  bsnSelect <- reactive({bsns[bsns@data$PSP_Name %in% Basin1(),]})
  
  StationIDs <- reactive(unique(filter(AllData_NoVoid, Project %in% Basin1())$Station_Description))
  stnSelect <- reactive({stns[stns@data$Station_De %in% StationIDs(),]})
  # %>% unite(ID_Des, c(Station_ID, Station_Description), remove=TRUE, sep = " - "))
  
  BasinName <- reactive(
    if(input$Basin == "All"){
      "All PSP"
    }
    else{
      input$Basin
    }
  )
  
  #Filter the station choices by the basin selected in the sidebar
  observeEvent(input$Basin, ({
    updateSelectInput(session, inputId = 'Station', label = "StationID", choices = c('All', sort(StationIDs())))
    updateSelectInput(session, inputId = 'Station2', label = 'Station ID', choices = c('All', sort(StationIDs())))
  }), ignoreInit = TRUE)
  
  #### All Analytes tab ####
  
  output$tab1Title <- renderText(paste0(BasinName(), " ", input$Year[1], "-", input$Year[2]))
  
  ### Format data ###
  tab1Data <- reactive({
    t <- AllData_SumBy_Basin %>% 
      dplyr::group_by(Analyte) %>%
      dplyr::filter(Project %in% Basin1(),
                    Year >= input$Year[1],
                    Year <= input$Year[2]
                    
      ) %>%
      dplyr::summarise(NSamples = sum(N_Samples),
                       NDetects = sum(N_Detects),
                       DetectionFreq = round((NDetects/NSamples)*100, 2),
                       Bench = min(AQL_Value),
                       Per_10 = (sum(AQL_0_10)/sum(N_Detects))*DetectionFreq,
                       Per10_50 = (sum(AQL_10_50)/sum(N_Detects))*DetectionFreq,
                       Per50_100 = round((sum(AQL_50_100)/sum(N_Detects))*DetectionFreq, 2),
                       Per100_ = round((sum(Over_AQL)/sum(N_Detects))*DetectionFreq, 2),
                       NoBench = ifelse(is.na(sum(AQL_Value)), DetectionFreq, 0),
                       Per50_ = Per50_100 + Per100_,
                       AQL_Ratio = round(max(AQL.Ratio, na.rm = TRUE), 2),
                       ALR_1 = 1
      )
    t
  })
  
  tab1Data1 <- reactive({
    if(input$OnlyDetects){
      print("Detects on")
      return(dplyr::filter(tab1Data(),
                    NDetects > 0))
    } else {
      print("Detects off")
      return(tab1Data())
    }
  })
  
  ### Create plots for 'All Analytes' ###
  
  output$plot1 <- renderPlotly({
    shiny::validate(
      need(max(tab1Data1()$DetectionFreq) > 0, "No Detections")
    )
    p <- FreqPlot(tab1Data1(), tab1Data1()$Analyte, -tab1Data1()$DetectionFreq) %>% 
      layout(title = paste("<b>Detection Frequency: ", input$Basin, " ", input$Year[1], "-", input$Year[2], "</b>"),
             xaxis = list(title="<b>Analyte</b>",
                          tickangle = -45),
             yaxis = list(side = "left", title = "<b>Detection Frequency (%)</b>", showline = TRUE,
                          ticks = "outside"),
             margin = list(r = 80,
                           l = 130,
                           t = 60
                           # , b = 180
             )) %>%  plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  output$plot2 <- renderPlotly({
    validate(
      need(max(tab1Data1()$Per50_, na.rm = TRUE) > 0, "No detections above 50% of benchmark")
    )
    tab1Data2 <- tab1Data1()[order(tab1Data1()$Per50_, decreasing = TRUE),] %>% filter(Per50_ > 0)
    p <- plot_ly(tab1Data2) %>% 
      add_bars(x = ~Analyte,
               y = ~Per50_100,
               name = "50-100%",
               width = 0.75,
               color = I('#FF9900')) %>% 
      add_trace(type = 'bar',
                x = ~Analyte,
                y = ~Per100_,
                name = "Over Benchmark",
                width = 0.75,
                color = I('#b30000')) %>% 
      layout(title = paste0("<b>Detection Frequency over 50% of Benchmark: ", input$Basin, " ", input$Year[1], "-", input$Year[2], "</b>"),
             barmode = 'stack',
             hovermode = 'x',
             yaxis = list(title = '<b>Detection Frequency</b>',
                          range = c(0, ifelse(max(tab1Data2$Per50_, na.rm = TRUE) > 50, 100, max(tab1Data2$Per50_, na.rm = TRUE)*2))),
             xaxis = list(title = '',
                          tickvals= ~Analyte,
                          ticktext= ~Analyte,
                          tickangle= -45),
             margin = list(t = 100,
                           b = 180,
                           l = 130)) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  output$plotALR <- renderPlotly({
    p <- plot_ly(tab1Data1()) %>% 
      add_trace(type = "bar", 
                x= ~Analyte, 
                y= ~AQL_Ratio, 
                name = "<b>ALR</b>",
                marker = list(color= "#990000")
      ) %>% 
      add_trace(type = "scatter",
                mode = "lines",
                line = list(color = "#000000"),
                x= ~Analyte, 
                y= ~ALR_1, 
                name = "<b>ALR = 1</b>"
      ) %>% 
      add_trace(type = "scatter", 
                mode = "markers",
                marker = list(color= "#0000CC", symbol= "diamond"),
                x= ~Analyte, 
                y= ~DetectionFreq, 
                name = "<b>Detection Frequency</b>", 
                yaxis= "y2") %>% 
      layout(
        title = paste("<b>Aquatic Life Ratio Detection Frequency: ", input$Basin, "Basin <br>", input$Year[1], '-', input$Year[2], "</b>"),
        xaxis = list(title="<b>Analyte</b>",
                     categoryorder = "array",
                     categoryarray = tab1Data1()[order(tab1Data1()$AQL_Ratio, decreasing = TRUE),]$Analyte,
                     tickangle = -45),
        yaxis = list(side = "left", title = "<b>Aquatic Life Ratio</b>", showline = TRUE,
                     ticks = "outside"),
        yaxis2 = list(
          position = 1,
          range = c(0, 105),
          side = "right", 
          title = "<b>Detection Frequency</b>", 
          overlaying = 'y',
          showgrid = FALSE,
          mirror = TRUE,
          showline = TRUE,
          ticks = "outside"),
        hovermode = 'x',
        legend = list(x = 0.25, y = 1.1, orientation = "h", tracegroupgap = 10),
        margin = list(r = 80,
                      l = 80,
                      t = 100,
                      b = 180,
                      pad = 0)
      ) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  #### Analyte per Year tab ####
  
  ### Filter Data ###
  tab2DetFreqData <- reactive({
    AllData_NoVoid %>% filter(Project %in% Basin1(), Station_Description %in% Station3(),
                              Year >= input$Year[1], Year <= input$Year[2], Analyte %in% input$Analyte) %>% 
      group_by(Analyte, Year) %>%
      dplyr:::summarise(NSamples = length(Analyte),
                        NDetects = sum(!is.na(Result.ug.l)),
                        DetectionFreq = round((sum(!is.na(Result.ug.l))/NSamples)*100,2),
                        Bench = round(ifelse(is.infinite(min(na.omit(min.AQL.value))), NA, min(na.omit(min.AQL.value))), 2),
                        Per100_ = round((sum(na.omit(AQL_Ratio) > 1.0)/NSamples)*100, 2),
                        Per50_100 = round((sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0)/NSamples)*100, 2),
                        Per10_50 = round((sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5)/NSamples)*100, 2),
                        Per_10 = round(sum(na.omit(AQL_Ratio) <= 0.1)/NSamples*100, 2),
                        NoBench = round(ifelse(is.na(sum(min.AQL.value)), DetectionFreq, 0), 2),
                        Per50_ = round(Per50_100 + Per100_, 2)
      )
  })
  
  tab2Data2 <- reactive({
    AllData_NoVoid %>% 
      filter(Year >= input$Year[1],
             Year <= input$Year[2],
             Project %in% Basin1(),
             Analyte %in% input$Analyte,
             Station_Description %in% Station3())
  })
  
  Station3 <- reactive({
    if('All' %in% input$Station2){
      StationIDs()
    }
    else{input$Station2}
  })
  
  station3name <- reactive({
    if(input$Station2 == 'All'){
      if(input$Basin == "All"){"All PSP Stations"}else{paste0("All Stations in the ", input$Basin, " Basin")}
    } else if(length(unique(tab2Data3()$Station))>1){"Multiple Stations"} else{unique(tab2Data3()$Station)}
  })
  
  output$tab2Title2 <- renderText({
    validate(
      need(!is.null(input$Station2), "No Stations Selected")
    )
    paste0(station3name(), ": ", toupper(substr(input$Analyte, 1, 1)), substr(input$Analyte,2,nchar(input$Analyte)), " Summary")
  })
  
  tab2Data3 <- reactive({
    AllData_NoVoid %>% 
      filter(Analyte %in% input$Analyte,
             Year >= input$Year[1],
             Year <= input$Year[2],
             Project %in% Basin1()
             ,
             # Result.ug.l > 0,
             # Mon >= startSeason,
             # Mon <= endSeason,
             Station_Description %in% Station3()
      ) %>% 
      dplyr::group_by(Analyte, Year) %>%
      dplyr::summarise(
        Station = first(Station_Description),
        Average = mean(ResultNA_0, na.rm = TRUE),
        Average_Det = ifelse(!is.na(mean(Result.ug.l, na.rm = TRUE)), mean(Result.ug.l, na.rm = TRUE), 0),
        Maximum = max(Result.ug.l, na.rm = TRUE),
        Median = median(Result.ug.l, na.rm = TRUE),
        Error_Y = Maximum - Average,
        Error_Y_Med = Maximum - Median,
        Criteria = ifelse(!is.na(min(min.AQL.value)), min(min.AQL.value, na.rm = TRUE), NA),
        Per_Over_Bench = ((sum(AQL_Ratio > 1, na.rm = TRUE))/(sum(Result.ug.l > 0, na.rm = TRUE)))*100,
        DetectionFreq = ((sum(Result.ug.l > 0, na.rm = TRUE))/(length(Analyte)))*100
        # ,
        # Chronic = 0.041,
        # Acute = 0.083
      )
  })
  
  maxY <- reactive(if(input$Variable=='Median'){max(tab2Data3()[,c("Median","Criteria")])
  }else{max(tab2Data3()[,c("Average", "Criteria")], na.rm = TRUE)})
  rangeMax <- reactive(signif(1.2*maxY(), 2))
  annotationX <- reactive({
    if(TRUE %in% (tab2Data3()$Maximum > rangeMax())){
      tab2Data3()[tab2Data3()$Maximum > rangeMax(), c("Year", "Maximum")]
    } else {NULL}
  })
  
  #### Station Summary tab ####
  
  Analyte2 <- reactive({
    if('All' %in% input$Analyte2){PollutantNames}
    else{input$Analyte2}
  })
  
  Station2 <- reactive({
    if('All' %in% input$Station){StationIDs()}
    else{input$Station}
  })
  
  tab3Station <- reactive(filter(AllData_SumBy_Station, StationDescription %in% input$Station)[,c(2,4)])
  
  output$helpText <- renderText("Press 'Backspace' to remove selections")
  
  output$StationDescription <- renderTable({
    t <- as.data.frame(unique(tab3Station()))
    colnames(t) <- c("ID","Description")
    t
  },
  striped = TRUE)
  
  tab3Data <- reactive({
    tab3Data <- AllData_SumBy_Station %>% 
      group_by(Analyte) %>%
      filter(Year >= input$Year[1],
             Year <= input$Year[2],
             StationDescription %in% Station2(),
             Analyte %in% Analyte2()
      ) %>%
      dplyr:::summarise(NSamples = sum(N_Samples),
                        NDetects = sum(N_Detects),
                        DetectionFreq = (NDetects/NSamples)*100,
                        Bench = min(AQL_Value),
                        Per_10 = (sum(AQL_0_10)/sum(N_Detects))*DetectionFreq,
                        Per10_50 = (sum(AQL_10_50)/sum(N_Detects))*DetectionFreq,
                        Per50_100 = (sum(AQL_50_100)/sum(N_Detects))*DetectionFreq,
                        Per100_ = (sum(Over_AQL)/sum(N_Detects))*DetectionFreq,
                        NoBench = ifelse(is.na(sum(AQL_Value)), DetectionFreq, 0),
                        Per50_ = Per50_100 + Per100_)
    tab3Data <- if(input$OnlyDetects == TRUE){
      filter(tab3Data,
             DetectionFreq > 0)
    }
    else{tab3Data}
  })
  
  tab3Data2 <- reactive({
    AllData_NoVoid %>% 
      filter(Year >= input$Year[1],
             Year <= input$Year[2],
             Project %in% Basin1(),
             Station_Description %in% Station2(),
             Analyte %in% Analyte2(),
             if(input$OnlyDetects == TRUE){Result.ug.l > 0}
      )
  })
  
  tab3Data3 <- reactive({
    AllData_SumBy_Station %>% 
      filter(Year >= input$Year[1],
             Year <= input$Year[2],
             StationDescription %in% Station2(),
             Analyte %in% Analyte2(),
             if(input$OnlyDetects == TRUE){Max_Result > 0}
      )
  })
  
  #### Detection Map ####
  
  MapAnalyte <- reactive(
    if(input$Analyte4 == "All"){
      MapAnalyte <- unique(AllData_NoVoid$Analyte)
    }
    else{
      MapAnalyte <- input$Analyte4
    }
  )
  
  # Format data for 'Detection Map' and plot
  
  mapData <- reactive({
    AllData_NoVoid %>%
      filter(Year >= input$Year[1],
             Year <= input$Year[2],
             !is.na(Result.ug.l),
             AQL_Ratio > input$ALR | is.na(AQL_Ratio),
             Project %in% Basin1(),
             Analyte %in% MapAnalyte())
  })
  
  # Create detection map using function from BasinMapForApp.R
  
  observeEvent(input$opaqueBasins, {
    if(input$opaqueBasins){
      output$detectionMap <- renderLeaflet({
        m <- detectMap(inputData=mapData(), bsnData=bsnSelect(), stnSelect()) %>% 
          addPolygons(data = bsnSelect(),
                      group = 'Basins',
                      stroke = TRUE,
                      weight = 1,
                      opacity = 1,
                      fillOpacity = 1,
                      smoothFactor = 0.5,
                      fillColor = topo.colors(13, alpha = NULL),
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        # dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = FALSE),
                      layerId = bsnSelect()@data$PSP_Name,
                      label = bsnSelect()@data$PSP_Name,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>% registerPlugin(easyPrintPlugin) %>%
          onRender(jsCode = "function(el, x) {
              L.easyPrint({
                  title: 'Download Map (CDL layer not available for download)',
                  position: 'topleft',
                  sizeModes: ['Current'],
                  exportOnly: true,
                  filename: 'map',
              }).addTo(this);}")
      }) 
    } else {
      output$detectionMap <- renderLeaflet({
        m <- detectMap(inputData=mapData(), bsnData=bsnSelect(), stnSelect()) %>% registerPlugin(easyPrintPlugin) %>% onRender(
          jsCode = "function(el, x) {
              L.easyPrint({
                  title: 'Download Map (CDL layer not available for download)',
                  position: 'topleft',
                  sizeModes: ['Current'],
                  exportOnly: true,
                  filename: 'map',
              }).addTo(this);}")
      })
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Resize map on button click
  
  observeEvent(input$expand, {
    output$modals <- renderUI({
      tags$script(
        '$(window).on("resize", function() {
$("#detectionMap").height($(window).height()-20);
}).trigger("resize");')
    })
    hideElement(id = 'expand')
    showElement(id = 'collapse')
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  observeEvent(input$collapse, {
    output$modals <- renderUI({
      tags$script(
        '$(window).on("resize", function() {
$("#detectionMap").height(400);
}).trigger("resize");')
    })
    showElement(id = 'expand')
    hideElement(id = 'collapse')
  })
  
  # Filter data and display plots based on the user clicking different markers/basins/detections ####
  
  observeEvent(input$detectionMap_marker_click,{
    bsnID <- reactive({NULL})
    clickData <- reactive({
      input$detectionMap_marker_click$id
    })
    subPlotData <- reactive({
      mapData() %>% filter(Station_Description %in% clickData())
    })
    landUseData <- reactive({
      filter(LandUse, Station_De %in% clickData(), Year == 2011) %>%
        melt(id.vars=c('Station_ID', 'Station_De', 'Basin', 'Year'),
             measure.vars=c('PerUrbanWs', 'PerForestWs', 'PerAgWs', 'PerOtherWs'),
             variable.name="variable",
             value.name="value")
    })
    # output$clickText <- renderPrint({
    #   clickData()
    # })
    
    output$subPlot <- renderPlotly({
      validate(need(clickData(), "Click on a station or basin to view results"))
      plot_ly(subPlotData()) %>%
        add_trace(type = 'scatter',
                  mode = 'markers',
                  marker = list(color = BenchColor(subPlotData()$AQL_Ratio)),
                  x = ~Sampling_Date,
                  y = ~AQL_Ratio) %>%
        layout(title = paste("<b>", as.character(clickData())),
               xaxis = list(title = "<b>Sampling Date"),
               yaxis = list(title = "<b>Aquatic Life Ratio"),
               shapes=list(type="line", line=list(dash='dash',color = "red", width=1),
                           x0= min(subPlotData()$Sampling_Date),
                           x1= max(subPlotData()$Sampling_Date),
                           y0=1, y1=1),
               margin = list(t=80)
               # xaxis = list(type = 'date', range  = c(min(subPlotData()$Sampling_Date),
               #                                        max(subPlotData()$Sampling_Date)))
        ) %>%
        hide_colorbar()
    })
    
    output$landUsePlot <- renderPlotly({
      plot_ly(landUseData(), values = ~value, labels = c('<b>Urban', '<b>Forest', '<b>Agriculture', '<b>Other'),
              type = "pie", hole = 0.7, textinfo = "label+percent", hoverinfo = "label+percent", textposition = "outside",
              marker = list(colors = (c('#C82E0D', '#52994C', '#D4A506', '#75919A'))),
              pull = 0.05
      ) %>%
        layout(title = "<b>NLCD 2011 Land Use",
               margin=list(t=70, b=50)) %>% 
        plotly::config(displayModeBar='hover', editable=TRUE, collaborate=FALSE, displaylogo=FALSE)
    })
    output$cdlPlot <- renderPlotly({
      validate(need(!is.null(bsnID()), "Click on a basin to view crop breakdown"))
      plot_ly(cdlData()) %>% add_bars(x=~Crop, y=~Percent) %>% 
        layout(title = paste("<b>USDA Crop Data Layer 2017<br>", clickData()),
               margin = list(b = 100))
    })
    output$subPlotData <- renderDataTable({
      validate(need(clickData(), "Click on a station or basin to view results"))
      subPlotData()[,c("Analyte", "AQL_Ratio", "Sampling_Date", "Station_Description", "Project")]
    })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$detectionMap_shape_click,{
    bsnID <- reactive({input$detectionMap_shape_click$id})
    clickData <- reactive({
      input$detectionMap_shape_click$id
    })
    
    subPlotData <- reactive({
      mapData() %>% filter(Project %in% clickData())
    })
    landUseData <- reactive({
      filter(bsnLandUse, Basin %in% clickData(), Year == 2011) %>%
        melt(id.vars=c('Basin', 'Year'),
             measure.vars=c('PerUrbanCat', 'PerForestCat', 'PerAgCat', 'PerOtherCat'),
             variable.name="variable",
             value.name="value")
    })
    cdlData <- reactive({
      bsnCrops %>% select(-starts_with("total")) %>% 
        melt(id.vars = "ID", variable.name = "Crop", value.name = "Percent", variable.factor=FALSE) %>% 
        filter(ID %in% clickData(), !is.na(Percent))
    })
    
    output$subPlot <- renderPlotly({
      validate(need(clickData(), "Click on a station or basin to view results"))
      plot_ly(subPlotData()) %>%
        add_trace(type = 'scatter',
                  mode = 'markers',
                  marker = list(color = BenchColor(subPlotData()$AQL_Ratio)),
                  x = ~Sampling_Date,
                  y = ~AQL_Ratio) %>%
        layout(title = paste0("<b>", as.character(clickData())),
               yaxis = list(title = "<b>Aquatic Life Ratio"),
               xaxis = list(title = "<b>Sampling Date"),
               shapes=list(type="line", line=list(dash='dash',color = "red", width=1),
                           x0= min(subPlotData()$Sampling_Date),
                           x1= max(subPlotData()$Sampling_Date),
                           y0=1, y1=1),
               margin = list(t=80)
               # xaxis = list(type = 'date', range  = c(min(subPlotData()$Sampling_Date),
               #                                        max(subPlotData()$Sampling_Date)))
        ) %>%
        hide_colorbar()
    })
    
    output$landUsePlot <- renderPlotly({
      validate(need(clickData(), "Click on a station or basin to view results"))
      plot_ly(landUseData(), values = ~value, labels = c('<b>Urban', '<b>Forest', '<b>Agriculture', '<b>Other'),
              type = "pie", hole = 0.7, textinfo = "label+percent", hoverinfo = "label+percent", textposition = "outside",
              marker = list(colors = (c('#C82E0D', '#52994C', '#D4A506', '#75919A'))),
              pull = 0.05
      ) %>%
        layout(title = "<b>NLCD 2011 Land Use", margin=list(t=70, b=50)) %>% 
        plotly::config(displayModeBar='hover', editable=TRUE, collaborate=FALSE, displaylogo=FALSE)
    })
    
    output$cdlPlot <- renderPlotly({
      validate(need(!is.null(bsnID()), "Click on a basin to view crop breakdown"))
      plot_ly(cdlData(), x=~Crop, y=~Percent, type="bar") %>% 
        layout(title = paste("<b>USDA Crop Data Layer 2017<br>", clickData()),
               xaxis = list(title="<b>Crop Category",
                            tickangle= -45),
               yaxis = list(title="<b>Percent of Area"),
               margin = list(t=100, b = 120))
    })
    
    output$subPlotData <- renderDataTable({
      validate(need(!is.null(input$detectionMap_shape_click$id), "Click on a station or basin to view results"))
      subPlotData()[,c("Analyte", "AQL_Ratio", "Sampling_Date", "Station_Description", "Project")]
    })
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Crop Data Layer info popup ####
  
  observeEvent({
    input$Tab
    input$navTab
  }, 
  {if(input$Tab != "detectionMap" | input$navTab != "dataSum") ({
    hideElement("popup")
  }) else{
    showElement("popup")
    removeModal()
    shinyalert(title = 'Important info about the Detection Map Crop Data Layer',
               text = "This layer shows estimated crop distribution information on a national scale and is 
               not intended to show information specific to individual plots. The mapped information will 
               improve over time as additional surveys become available. This map is not intended for 
               use in identifying specific crops or land uses at this time.",
               type = 'info', closeOnEsc = TRUE, showConfirmButton = TRUE, html = TRUE)
  }
  })
  
  output$popup <- renderUI(absolutePanel(top = 300, right = 70, width = 250, height = 100, draggable = TRUE, uiOutput("cdlPop")))
  
  httr::set_config(httr::config( ssl_verifypeer = 0L ) )
  
  # Add functions for showing detection data on click for various layers
  observeEvent(input$detectionMap_click,{
    showElement(id="cdlTable")
    pnt <- reactive(LongLatToUTM(input$detectionMap_click$lng,input$detectionMap_click$lat))
    cdlData <- reactive(as.character(xmlTreeParse(content(GET("http://nassgeodata.gmu.edu/axis2/services/CDLService/GetCDLValue?",
                                                              query=list(year=2017,
                                                                         x=pnt()$X,
                                                                         y=pnt()$Y))))$doc$children$GetCDLValueResponse[1]$Result[1]$text)[6])
    cdlCat <- reactive(gsub('"',"",str_extract(strsplit(cdlData(), ", ")[[1]][4],'".*"'))[1])
    cdlCol <- reactive(gsub('"',"",str_extract(strsplit(cdlData(), ", ")[[1]][5],'".*"'))[1])
    output$cdlTable <- renderTable(data.frame(CDL.Category = cdlCat(),
                                              Color.Value = cdlCol(),
                                              Map.Layer = "Crops (CDL 2017)"))
    output$cdlPanel <- renderText(paste('<span style="font-weight: normal;">', cdlCat(), '</span>'))
    # output$color <- renderUI(tags$style(type = "text/css",
    #                                     paste("#cdlTable {border-style: solid;
    #                                           border-color: ", cdlCol(), ";
    #                                           border-width: 8px; width: 600px; height: 80px;}"))
    #                          )
    output$cdlPop <- renderUI(div(style=paste("border-style: solid; border-color:", cdlCol(), "; text-align: center; font-weight: bold;
                                                         border-width: 8px; background-color: #FFFFFF; border-radius: 25px; z-index: 1001; position: relative;"),
                                  "CDL 2017 Crop Category",
                                  htmlOutput("cdlPanel")))
    
  })
  
  #### Data for 'Reference' tab ####
  
  output$stationRef <- renderDataTable(
    StationReference
    # ,
    # width = '100%',
    # striped = TRUE,
    # bordered = TRUE
  )
  
  output$terms <- renderDataTable(
    Terms
  )
  
  output$ingDescriptions <- renderDataTable(
    IngredientDescriptions
  )
  
  #### Create plots for 'Analyte per Year' ####
  
  AvgMaxTab2 <- reactive({
    data <- tab2Data3()
    if(input$Variable == "Median"){
      yVar <- data$Median
      errY <- data$Error_Y_Med
    } else {yVar <- data$Average_Det
    errY <- data$Error_Y}
    
    p <- plot_ly(data) %>% 
      add_trace(type = "bar",
                x = ~Year,
                y = ~yVar,
                name = input$Variable,
                marker = list(color = "steelblue"),
                hoverinfo = "text",
                hovertext = ~paste0(input$Variable, " = ", format(round(yVar, 2), nsmall = 2),"<br>",
                                   "Maximum = ", format(round(Maximum, 2), nsmall = 2)),
                error_y = list(type = "data",
                               symmetric = FALSE,
                               array = ~errY,
                               arrayminus = 0)
      ) %>% 
      add_trace(type = "scatter",
                mode = "markers",
                marker = list(symbol = "diamond",
                              size = 10,
                              color = "black"),
                x = ~Year,
                y = ~DetectionFreq,
                name = "Detection Frequency",
                yaxis = "y2"
      ) %>%
      add_trace(type = "scatter",
                mode = "lines",
                x = ~Year,
                y = ~Criteria,
                name = paste("WQ Criteria = ", min(data$Criteria)),
                line = list(color = '#CC0000',
                            dash = 10)
      ) %>%
      layout(
        # title = paste0("<b>", station3name(), "<br> Average and Maximum Concentration of Detections<br> ",
        # toupper(substr(input$Analyte, 1, 1)), substr(input$Analyte,2,nchar(input$Analyte)), " 2009-2016</b>"),
        yaxis = list(title = "<b>Concentration (ug/L)</b>",
                     rangemode = 'tozero'
                     , range = c(0, rangeMax())
                     , mirror = TRUE
        ),
        yaxis2 = list(overlaying = "y",
                      side = "right",
                      title = "<b>Detection Frequency</b>",
                      mirror = TRUE,
                      showgrid = FALSE,
                      rangemode = 'tozero',
                      range = c(0, 105)),
        xaxis = list(title = "",
                     tickvals= ~Year,
                     ticktext= ~Year),
        hovermode = 'x',
        margin = list(t = 120),
        legend = list(y = 1.1,
                      x = 1.05)
      ) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  output$AvgMaxTab2 <- renderPlotly({
    validate(
      need(nrow(tab2Data3()) > 0, "No data for these parameters")
    )
    if(!is.null(annotationX())){
      AvgMaxTab2() %>% add_annotations(data = annotationX(),
                                       x = ~Year,
                                       y = rangeMax(),
                                       font = list(color = "steelblue"),
                                       text = paste0("<b>", annotationX()$Maximum),
                                       yshift = 6,
                                       showarrow = FALSE,
                                       legendtitle = FALSE)
    } else {AvgMaxTab2()}
  })
  
  output$detectsStationPlot <- renderPlotly({
    p <- plot_ly(tab2Data2()) %>% 
      add_trace(type='scatter',
                mode='markers',
                x=~Sampling_Date,
                y=~Result.ug.l,
                symbol=~Station_Description,
                symbols=~symbolCodes,
                marker=list(size=9)) %>% 
      add_lines(x=~Sampling_Date,
                y=~min.AQL.value,
                name=paste0("Benchmark: ", min(tab2Data2()$min.AQL.value), " ug/L"),
                line=list(color='red',
                          dash='dash')) %>% 
      layout(margin = list(t=100),
             xaxis=list(title="Sampling Date"),
             yaxis=list(title="Concentration (ug/L)")
      ) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  output$DetFreqTab2 <- renderPlotly({
    validate(
      need(max(tab2DetFreqData()$DetectionFreq, na.rm = TRUE) > 0, "No Detections of this Analyte")
    )
    p <- plot_ly(tab2DetFreqData()) %>% 
      add_bars(
        # type = 'bar',
        x = ~Year,
        y = ~Per_10,
        name = '<10% of benchmark',
        width = 0.75,
        color = I('#99CCCC')) %>% 
      add_trace(type = 'bar',
                x = ~Year,
                y = ~Per10_50,
                name = '10-50%',
                width = 0.75,
                color = I('#336699')) %>%
      add_trace(type = 'bar',
                x = ~Year,
                y = ~Per50_100,
                name = '50-100%',
                width = 0.75,
                color = I('#FF9900')) %>%
      add_trace(type = 'bar',
                x = ~Year,
                y = ~Per100_,
                name = 'Over benchmark',
                width = 0.75,
                color = I('#b30000')) %>%
      add_trace(type = 'bar',
                x = ~Year,
                y = ~NoBench,
                name = 'No benchmark',
                width = 0.75,
                color = I('grey')) %>% 
      layout(
        margin = list(t=100),
        barmode = 'stack',
        hovermode = 'x',
        yaxis = list(title = '<b>Detection Frequency</b>',
                     range = c(0,100)),
        xaxis = list(title = '',
                     tickvals= ~Year,
                     ticktext= ~Year)
      ) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  #### Create plots for 'Station Summary' ####
  
  output$tab3plot1 <- renderPlotly({
    validate(
      need(max(tab3Data()$DetectionFreq, na.rm = TRUE) > 0, "No Detections at this Station")
    )
    p <- FreqPlot(tab3Data(), tab3Data()$Analyte, -tab3Data()$DetectionFreq) %>% plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  output$tab3plot2 <- renderPlotly({
    validate(
      need(max(tab3Data2()$AQL_Ratio, na.rm = TRUE) > 0, "No detections at this station, or there is no Aquatic Life Benchmark for the selected analytes")
    )
    p <- plot_ly(tab3Data2()) %>% 
      add_markers(x=~Sampling_Date, y=~AQL_Ratio, symbol=~Analyte, symbols=~symbolCodes, color=~Station_Description,
                  marker=list(size=9)) %>% 
      layout(shapes=list(type='line', x0= min(tab3Data2()$Sampling_Date), x1= max(tab3Data2()$Sampling_Date), y0=1, y1=1,
                         line=list(dash='dash', width=1, color='red'), showlegend = TRUE),
             xaxis = list(title = "<b>Sampling Date"),
             yaxis = list(title = "<b>Aquatic Life Ratio"),
             margin = list(t = 60,
                           b =120)
      ) %>% 
      plotConfig()
    p$x$config$modeBarButtonsToAdd[1] <- toImage2
    p
  })
  
  # output$tab3plot3 <- renderPlotly({
  #   validate(
  #     need(max(tab3Data3()$Avg_Result) > 0, "No detections at this station")
  #   )
  #   ggplot(tab3Data3(), aes(x=Year, y=Avg_Result, ymin=Avg_Result, ymax=Max_Result, fill=Station_ID, group=Station_ID, color='black')) +
  #     geom_errorbar(position = 'dodge') +
  #     geom_bar(position = 'dodge', stat = 'identity') +
  #     scale_color_identity() +
  #     scale_fill_identity()
  # })
  
  #### Create table for tab1 ####
  
  output$results <- renderDataTable({
    tab1DataTable <- tab1Data1()[order(-tab1Data1()$DetectionFreq), c(1:12)]
    plyr:::rename(tab1DataTable, c("DetectionFreq" = "Detection Frequency",
                              "Bench" = "Aquatic Life Benchmark (ug/L)",
                              "Per100_" = "Over benchmark (%)",
                              "Per50_100" = "50-100% (%)",
                              "Per10_50" = "10-50% (%)",
                              "Per_10" = "<10% of benchmark (%)", 
                              "NoBench" = "No benchmark (%)",
                              "Per50_" = ">50% of benchmark (%)",
                              "AQL_Ratio" = "ALR"))
  })
  
  #### Create table for tab2 ####
  
  output$tab2results <- renderDataTable({
    tab2DetFreqData()
    plyr:::rename(tab2DetFreqData(), c("DetectionFreq" = "Detection Frequency",
                                       "Bench" = "Aquatic Life Benchmark (ug/L)",
                                       "Per100_" = "Over benchmark (%)",
                                       "Per50_100" = "50-100% (%)",
                                       "Per10_50" = "10-50% (%)",
                                       "Per_10" = "<10% of benchmark (%)", 
                                       "NoBench" = "No benchmark (%)",
                                       "Per50_" = ">50% of benchmark (%)"))
  }
  # , striped = TRUE
  )
  
  #### Create table for tab3 ####
  
  output$tab3results <- renderDataTable({
    tab3Data <- tab3Data()[order(-tab3Data()$DetectionFreq),]
    plyr:::rename(tab3Data(), c("DetectionFreq" = "Detection Frequency",
                                "Bench" = "Aquatic Life Benchmark (ug/L)",
                                "Per100_" = "Over benchmark (%)",
                                "Per50_100" = "50-100% (%)",
                                "Per10_50" = "10-50% (%)",
                                "Per_10" = "<10% of benchmark (%)", 
                                "NoBench" = "No benchmark (%)",
                                "Per50_" = ">50% of benchmark (%)"))
  }
  # , striped = TRUE
  )
  
  
}

print(Sys.time()-TS)

#### Function that creates the app from one script ####

shinyApp(ui = ui, server = server)