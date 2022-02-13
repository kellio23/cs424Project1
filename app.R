# CS 424 Project 1 - Kevin Elliott
# This project processes csv files of CTA stops from the chicago data portal
# and displays graphs and tables from that data
# Link to site: https://kellio23.shinyapps.io/CS424Project1kellio23/

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(readr)

# Code below is used with slight modifications to create new csv files from the database

# Checkpoint 1 - break file into 5MB chunks

# Import all the data in the csv
#rawData <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv", sep = ',', header = TRUE, quote = "")
#uicHalstedRawData <- read.table(file = "uicHalsted.csv", sep = ',', header = TRUE)

#View(rawData)
#View(uicHalstedRawData)
# Store in frame
#allDataFrame <- data.frame(rawData)
#uicHalstedFrame <- data.frame(uicHalstedRawData)

# old but keep for ohare
#allDataFrame$rides <- as.numeric(as.character(allDataFrame$rides))
#

# old but keep for ohare
#allDataFrame$yearNumber <- format(as.Date(allDataFrame$date, format="%m/%d/%Y"),"%Y")

# Get just the uic halsted data old keep for ohare
#ohareFrame = split(allDataFrame, allDataFrame$station_id)[['40890']]
#

# Write frame to csv so its under the 5MB limit old keep for ohare
#write.csv(ohareFrame,"C:/Users/guagu/OneDrive/Documents/CS424Project1kellio23/ohare.csv", row.names = TRUE)
#

#View(uicHalstedFrame)
#View(uicHalstedFrame2) has x column when 1st doesnt, shouldnt matter


# code above is left for adding more stops at a later point, only works locally
# since the file limit for shiny is 5mb


uicHalstedRawData <- read.table(file = "uicHalsted.csv", sep = ',', header = TRUE)
uicHalstedFrame <- data.frame(uicHalstedRawData)

ohareRawData <- read.table(file = "ohare.csv", sep = ',', header = TRUE)
ohareFrame <- data.frame(ohareRawData)


# Checkpoint 2 - use lubridate to convert date info to more useable form
# date - 12/22/2017
uicHalstedFrame$date <- mdy(uicHalstedFrame$date)
uicHalstedFrame$monthNumber <- month(uicHalstedFrame$date)
uicHalstedFrame$dayNumber <- day(uicHalstedFrame$date)
uicHalstedFrame$dayOfWeek <- weekdays(uicHalstedFrame$date)

ohareFrame$date <- mdy(ohareFrame$date)
ohareFrame$monthNumber <- month(ohareFrame$date)
ohareFrame$dayNumber <- day(ohareFrame$date)
ohareFrame$dayOfWeek <- weekdays(ohareFrame$date)

dayOrder <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
years<-c(2001:2021)

yearFrequencyTemp <- uicHalstedFrame
yearFreqList <- c("yearNumber","rides")
yearFrequency = yearFrequencyTemp[yearFreqList]
yearFrequency <- tapply(yearFrequency$rides, yearFrequency$yearNumber, FUN=sum)
yearFrequencyFrame <- data.frame(yearFrequency)
yearFrequencyFrame <- data.frame(yearNumber = years, yearFrequencyFrame)

yearFrequencyTempOhare <- ohareFrame
yearFrequencyOhare = yearFrequencyTempOhare[yearFreqList]
yearFrequencyOhare <- tapply(yearFrequencyOhare$rides, yearFrequencyOhare$yearNumber, FUN=sum)
yearFrequencyFrameOhare <- data.frame(yearFrequencyOhare)
yearFrequencyFrameOhare <- data.frame(yearNumber = years, yearFrequencyFrameOhare)

# Checkpoint 3 - Create interactive visualization in R

stationNames <- c("UIC Halsted", "Ohare")
totalAcrossYearsOptions <- c("Off", "Graph","Table","Both")
totalYearOptions <- c("Off", "Graph Only", "Table Only","Both")
dailyYearOptions <- c("Off","Graph Only", "Table Only","Both")
monthlyYearOptions <- c("Off","Graph Only", "Table Only","Both")


ui <- dashboardPage(
  dashboardHeader(title = "CTA Daily Rider Data"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("Dashboard", tabName = "dashboard", icon = NULL),
                     menuItem("About this project", tabName = "projInfo", icon = NULL)),
                     
                   
                   selectInput("Year", "Select the year to visualize L", years, selected = 2021),
                   selectInput("Stop", "Select the Stop", stationNames, selected = "UIC Halsted"),
                   selectInput("totalAcrossYears", "Total across Years", totalAcrossYearsOptions, selected = "Graph"),
                   selectInput("totalYear", "Total in Year", totalYearOptions, selected = "Off"),
                   selectInput("dailyYear", "Daily in Year", dailyYearOptions, selected = "Off"),
                   selectInput("monthlyYear", "Monthly in Year", monthlyYearOptions, selected = "Off"),
                   
                   selectInput("OhareYear", "Select the year to visualize R", years, selected = 2021),
                   selectInput("OhareStop", "Select the Stop", stationNames, selected = "Ohare"),
                   selectInput("OharetotalAcrossYears", "Total across Years", totalAcrossYearsOptions, selected = "Graph"),
                   selectInput("OharetotalYear", "Total in Year", totalYearOptions, selected = "Off"),
                   selectInput("OharedailyYear", "Daily in Year", dailyYearOptions, selected = "Off"),
                   selectInput("OharemonthlyYear", "Monthly in Year", monthlyYearOptions, selected = "Off")
                   
  ),
  dashboardBody(
    # fluidRow(
    #   
    # ),
    
    tabItems(
      
      tabItem(tabName = "dashboard",
             
              #Halsted Col
              column(6,
                     
                     uiOutput("startGraphRender"),
                     uiOutput("monthly"),
                     uiOutput("dailyYear"),
                     uiOutput("weekdays")
              ),
              # Ohare Col
              column(6,
                
                     uiOutput("startGraphRenderOhare"),
                     uiOutput("monthlyOhare"),
                     uiOutput("dailyYearOhare"),
                     uiOutput("weekdaysOhare")
                     
                     
              ),
              # Third Stop
              # column(6,
              #          
              #          uiOutput("startGraphRenderThird"),
              #          uiOutput("monthlyThird"),
              #          uiOutput("dailyYearThird"),
              #          uiOutput("weekdaysthird")
              #          
              #          
              # )
              
      ),
      
      # Second tab content
      tabItem(tabName = "projInfo",
              h2("Welcome to my CS 424 Project 1"),
              h2("Created by Kevin Elliott, Spring 2022"),
              h2(""),
              h2("The data for this project was downloaded from the Chicago data portal, under the name CTA - Ridership 'L' Staion Entries - Daily Totals"),
              h2("Project is based of the starter code provided in week 2 by Professor Johnson"),
              h2("Each dataset can be displayed as a graph, table, or both via the side panel."),
              h2("L corresponds to the left column of data and R to the right")
              
              
      )
    )
  )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  theme_set(theme_grey(base_size = 14)) 
  
  justOneYearReactive <- reactive({subset(uicHalstedFrame, year(uicHalstedFrame$date) == input$Year)})
  justOneYearReactiveOhare <- reactive({subset(ohareFrame, year(ohareFrame$date) == input$OhareYear)})
  
  ## menu graph and table option reactives
  totalAcrossYearsOptionsReactive <- reactive({input$totalAcrossYears})

  monthlyReactive <- reactive({input$monthlyYear})
  dailyYearReactive <- reactive({input$dailyYear})
  weekdaysReactive <- reactive({input$totalYear})
  startGraphReactive <- reactive({input$totalAcrossYears})
  
  monthlyReactiveOhare <- reactive({input$OharemonthlyYear})
  dailyYearReactiveOhare <- reactive({input$OharedailyYear})
  weekdaysReactiveOhare <- reactive({input$OharetotalYear})
  startGraphReactiveOhare <- reactive({input$OharetotalAcrossYears})
  
  staticSub <- subset(uicHalstedFrame, year(uicHalstedFrame$date) == 2021)
  
  ##############################MONTHLY OUTPUT #####################################
  output$monthly <- renderUI( {
    
    monthlyInput <- monthlyReactive()
    
    if(monthlyInput == "Both"){
    
    column(10,
      fluidRow(
        box( title = "Total Rides per Month", solidHeader = TRUE, status = "primary", width = 15,
             plotOutput("histMonthlyRiders", height = 250)
        ),
      ),
      fluidRow(
        box(title = "Monthly Riders", solidHeader = TRUE, status = "primary", width = 9,
            dataTableOutput("monthlyRiderTable", height = 220)
        )
      )
    )
    }
    else if(monthlyInput == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Total Rides per Month", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("histMonthlyRiders", height = 250)
               ),
             )
      )
    }
    else if(monthlyInput == "Table Only"){
      column(10,
             
             fluidRow(
               box(title = "Monthly Riders", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("monthlyRiderTable", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  output$monthlyOhare <- renderUI( {
    
    monthlyInputOhare <- monthlyReactiveOhare()
    
    if(monthlyInputOhare == "Both"){
      
      column(10,
             fluidRow(
               box( title = "Total Rides per Month", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("histMonthlyRidersOhare", height = 250)
               ),
             ),
             fluidRow(
               box(title = "Monthly Riders", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("monthlyRiderTableOhare", height = 220)
               )
             )
      )
    }
    else if(monthlyInputOhare == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Total Rides per Month", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("histMonthlyRidersOhare", height = 250)
               ),
             )
      )
    }
    else if(monthlyInputOhare == "Table Only"){
      column(10,
             
             fluidRow(
               box(title = "Monthly Riders", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("monthlyRiderTableOhare", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  ##############################END MONTHLY OUTPUT ####################################
  
  
  ##############################DAILY YEAR OUTPUT #####################################
  output$dailyYear <- renderUI( {
    
    dailyYearInput <- dailyYearReactive()
    
    if(dailyYearInput == "Both"){
      
      column(10,
             fluidRow(
               box( title = "Daily Riders in a Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("eachDayOfYear", height = 250)
               )
             ),
             box(title = "Rides per Day", solidHeader = TRUE, status = "primary", width = 9,
                 dataTableOutput("dailyYearlyRiderTable", height = 220)
             )
      )
    }
    else if(dailyYearInput == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Daily Riders in a Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("eachDayOfYear", height = 250)
               )
             )
      )
    }
    else if(dailyYearInput == "Table Only"){
      column(10,
             
             fluidRow(
               box(title = "Rides per Day", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyYearlyRiderTable", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  output$dailyYearOhare <- renderUI( {
    
    dailyYearInputOhare <- dailyYearReactiveOhare()
    
    if(dailyYearInputOhare == "Both"){
      
      column(10,
             fluidRow(
               box( title = "Daily Riders in a Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("eachDayOfYearOhare", height = 250)
               )
             ),
             box(title = "Rides per Day", solidHeader = TRUE, status = "primary", width = 9,
                 dataTableOutput("dailyYearlyRiderTableOhare", height = 220)
             )
      )
    }
    else if(dailyYearInputOhare == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Daily Riders in a Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("eachDayOfYearOhare", height = 250)
               )
             )
      )
    }
    else if(dailyYearInputOhare == "Table Only"){
      column(10,
             
             fluidRow(
               box(title = "Rides per Day", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyYearlyRiderTableOhare", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  ##############################END DAILY YEAR OUTPUT ###############################
  
  ##############################WEEKDAYS OUTPUT #####################################
  output$weekdays <- renderUI( {
    
    weekdaysInput <- weekdaysReactive()
    
    if(weekdaysInput == "Both"){
      
      column(10,
             fluidRow(
               box( title = "Total Entries per Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("dayOfWeek", height = 250)
               )
             ),
             fluidRow(
               box(title = "Weekly Rides", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyRiderTable", height = 220)
               )
             )
      )
    }
    else if(weekdaysInput == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Total Entries per Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("dayOfWeek", height = 250)
               )
             )
      )
    }
    else if(weekdaysInput == "Table Only"){
      column(10,
             fluidRow(
               box(title = "Weekly Rides", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyRiderTable", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  output$weekdaysOhare <- renderUI( {
    
    weekdaysInputOhare <- weekdaysReactiveOhare()
    
    if(weekdaysInputOhare == "Both"){
      
      column(10,
             fluidRow(
               box( title = "Total Entries per Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("dayOfWeekOhare", height = 250)
               )
             ),
             fluidRow(
               box(title = "Weekly Rides", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyRiderTableOhare", height = 220)
               )
             )
      )
    }
    else if(weekdaysInputOhare == "Graph Only"){
      column(10,
             fluidRow(
               box( title = "Total Entries per Year", solidHeader = TRUE, status = "primary", width = 15,
                    plotOutput("dayOfWeekOhare", height = 250)
               )
             )
      )
    }
    else if(weekdaysInputOhare == "Table Only"){
      column(10,
             fluidRow(
               box(title = "Weekly Rides", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("dailyRiderTableOhare", height = 220)
               )
             )
      )
    }
    else{
      
    }
  })
  
  ##############################END WEEKDAYS OUTPUT ####################################
  
  ##############################START GRAPH OUTPUT #####################################
  output$startGraphRender <- renderUI( {
    
    startGraphInput <- startGraphReactive()
    
    if(startGraphInput == "Graph"){
      column(10,
             fluidRow(
               box(title = "Total Riders per Year", solidHeader = TRUE, status = "primary", width = 15,
                   plotOutput("startGraph", height = 250)
               )
             )
             
      )
    }
    else if(startGraphInput == "Table"){
      fluidRow(
        box(title = "Total Rides per Year 2001-2021", solidHeader = TRUE, status = "primary", width = 9,
            dataTableOutput("startTable", height = 220)
        )
      )
    }
    else if(startGraphInput == "Both"){
      column(10,
      fluidRow(
        box(title = "Total Riders per Year", solidHeader = TRUE, status = "primary", width = 15,
            plotOutput("startGraph", height = 250)
        )
      ),
      fluidRow(
        box(title = "Total Rides per Year 2001-2021", solidHeader = TRUE, status = "primary", width = 9,
            dataTableOutput("startTable", height = 220)
        )
      )
      )
      
    }
  })
  
  output$startGraphRenderOhare <- renderUI( {
    
    startGraphInputOhare <- startGraphReactiveOhare()
    
    if(startGraphInputOhare == "Graph"){
      column(10,
             fluidRow(
               box(title = "Total Riders per Year", solidHeader = TRUE, status = "primary", width = 15,
                   plotOutput("startGraphOhare", height = 250)
               )
             )
             
      )
    }
    else if(startGraphInputOhare == "Table"){
      fluidRow(
        box(title = "Total Rides per Year 2001-2021", solidHeader = TRUE, status = "primary", width = 9,
            dataTableOutput("startTableOhare", height = 220)
        )
      )
    }
    else if(startGraphInputOhare == "Both"){
      column(10,
             fluidRow(
               box(title = "Total Riders per Year", solidHeader = TRUE, status = "primary", width = 15,
                   plotOutput("startGraphOhare", height = 250)
               )
             ),
             fluidRow(
               box(title = "Total Rides per Year 2001-2021", solidHeader = TRUE, status = "primary", width = 9,
                   dataTableOutput("startTableOhare", height = 220)
               )
             )
      )
    }
  })
  ##############################END START GRAPH OUTPUT #####################################
  
  output$histTotalYearlyRiders <- renderPlot({
    
    ggplot(uicHalstedFrame, aes(x=yearNumber, y=rides)) + geom_bar(stat = "identity",fill="steelblue") + 
      xlab("Year") +
      ylab("Riders") +
      ggtitle("Total Riders at UIC Halsted per Year")
    
    
  })
  
  output$histTotalYearlyRidersOhare <- renderPlot({
    
    ggplot(ohareFrame, aes(x=yearNumber, y=rides)) + geom_bar(stat = "identity",fill="steelblue") + 
      xlab("Year") +
      ylab("Riders") +
      ggtitle("Total Riders at Ohare per Year")
    
  })
  
  output$histMonthlyRiders <- renderPlot({
    justOneYear <- justOneYearReactive()

    ggplot(justOneYear, aes(x=month(date), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Month") +
      ylab("Riders") +
      ggtitle("Total Monthly Riders at UIC Halsted per Year") + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
      #scale_x_continuous(breaks=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  })
  
  output$histMonthlyRidersOhare <- renderPlot({
    justOneYearOhare <- justOneYearReactiveOhare()
    
    ggplot(justOneYearOhare, aes(x=month(date), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Month") +
      ylab("Riders") +
      scale_y_continuous(labels = comma) + 
      ggtitle("Total Monthly Riders at Ohare per Year") + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
    #scale_x_continuous(breaks=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))
  })
  
  
  output$eachDayOfYear <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=date(date), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Day") +
      ylab("Riders") +
      ggtitle("Daily Riders per Year")
  })
  
  output$eachDayOfYearOhare <- renderPlot({
    justOneYearOhare <- justOneYearReactiveOhare()
    ggplot(justOneYearOhare, aes(x=date(date), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Day") +
      ylab("Riders") +
      ggtitle("Daily Riders per Year")
  })
  
  
  output$dayOfWeek <- renderPlot({
    justOneYear <- justOneYearReactive()
    
    ggplot(justOneYear, aes(x=factor(dayOfWeek, levels = dayOrder), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Day of Week") +
      ylab("Riders") +
      scale_y_continuous(labels = comma) + 
      ggtitle("Daily Riders per Week day")

  })
  
  output$dayOfWeekOhare <- renderPlot({
    justOneYearOhare <- justOneYearReactiveOhare()
    
    ggplot(justOneYearOhare, aes(x=factor(dayOfWeek, levels = dayOrder), y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Day of Week") +
      ylab("Riders") +
      scale_y_continuous(labels = comma) + 
      ggtitle("Daily Riders per Week day")
    
  })
  
  
  
  output$startGraph <- renderPlot({

    #menuChoice <- totalAcrossYearsOptionsReactive()

    ggplot(uicHalstedFrame, aes(x=yearNumber, y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Year") +
      ylab("Riders") +
      ggtitle("Total Riders at UIC Halsted per Year")
    
  })
  
  output$startGraphOhare <- renderPlot({
    
    #menuChoice <- totalAcrossYearsOptionsReactive()
    
    ggplot(ohareFrame, aes(x=yearNumber, y=rides)) + geom_bar(stat = "identity",fill="steelblue") +
      xlab("Year") +
      ylab("Riders") +
      scale_y_continuous(labels = comma) + 
      ggtitle("Total Riders at Ohare per Year")
    
  })
  
  
  ##########################################
  output$startTable <- DT::renderDataTable(
    DT::datatable({
      
      yearFrequencyFrame
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  output$startTableOhare <- DT::renderDataTable(
    DT::datatable({
      
      yearFrequencyFrameOhare
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  ############################################
  
  
  ############################################
  
  output$dailyRiderTable <- DT::renderDataTable(
    DT::datatable({
      justOneYear <- justOneYearReactive()
      drops <- c("station_id","stationname", "daytype", "yearNumber")
      justOneYear[ , !(names(justOneYear) %in% drops)]
      
      aggregate(justOneYear$rides, by=list(Category=justOneYear$dayOfWeek), FUN=sum)
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  output$dailyRiderTableOhare <- DT::renderDataTable(
    DT::datatable({
      justOneYearOhare <- justOneYearReactiveOhare()
      drops <- c("station_id","stationname", "daytype", "yearNumber")
      justOneYearOhare[ , !(names(justOneYearOhare) %in% drops)]
      
      aggregate(justOneYearOhare$rides, by=list(Category=justOneYearOhare$dayOfWeek), FUN=sum)
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  ############################################
  
  ############################################
  
  output$monthlyRiderTable <- DT::renderDataTable(
    DT::datatable({
      justOneYear <- justOneYearReactive()
      drops <- c("station_id","stationname", "daytype", "yearNumber")
      justOneYear[ , !(names(justOneYear) %in% drops)]
      
      aggregate(justOneYear$rides, by=list(Category=justOneYear$monthNumber), FUN=sum)
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  output$monthlyRiderTableOhare <- DT::renderDataTable(
    DT::datatable({
      justOneYearOhare <- justOneYearReactiveOhare()
      drops <- c("station_id","stationname", "daytype", "yearNumber")
      justOneYearOhare[ , !(names(justOneYearOhare) %in% drops)]
      
      aggregate(justOneYearOhare$rides, by=list(Category=justOneYearOhare$monthNumber), FUN=sum)
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  
  ########################################################
  
  ########################################################
  
  # day by day in a whole year
  output$dailyYearlyRiderTable <- DT::renderDataTable(
    DT::datatable({
      justOneYear <- justOneYearReactive()
      drops <- c("station_id","stationname", "daytype", "yearNumber","monthNumber", "dayNumber", "X")
      justOneYear[ , !(names(justOneYear) %in% drops)]
      
    },
    
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  output$dailyYearlyRiderTableOhare <- DT::renderDataTable(
    DT::datatable({
      justOneYearOhare <- justOneYearReactiveOhare()
      drops <- c("station_id","stationname", "daytype", "yearNumber","monthNumber", "dayNumber", "X")
      justOneYearOhare[ , !(names(justOneYearOhare) %in% drops)]
    },
  
    options = list(searching = FALSE, pageLength = 4, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE
    )
  )
  
  
  ########################################################
 
} 



# Run the application 
shinyApp(ui = ui, server = server)

