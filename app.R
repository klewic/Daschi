library("shiny")
library("ggplot2") 
library("ggmap")
library("data.table")
library("shinydashboard")                                                       
library("RColorBrewer")
library("RSocrata")
library("lubridate")

todayMinus3Mo <- Sys.Date() - months(3) # if running locally, feel free to extend this out

crimes <- read.socrata(
  paste('https://data.cityofchicago.org/resource/6zsd-86xi.json?$where=date>="', 
        todayMinus3Mo, '"', 
        sep = ""),
  app_token = "", # replace with your creds
  email     = "", # replace with your creds
  password  = "" # replace with your creds
)

crimes <- crimes[, c("case_number", "date", "block", "iucr", "primary_type", 
                     "description", "location_description", "arrest", 
                     "domestic", "beat", "ward", "fbi_code", "x_coordinate", 
                     "y_coordinate", "latitude", "longitude")]

setDT(crimes)                                                                   # convert DF to DT

newColNames <- c("CaseNo", "DateOcc", "Block", "IUCR", "PrimDesc", "SecDesc", 
                 "LocDesc", "Arrest", "Domestic", "Beat", "Ward", "FBICD", 
                 "XCoor", "YCoor", "Lat", "Long")
setnames(crimes, colnames(crimes), newColNames)

crimes <- crimes[!is.na(Lat)]                                                   # remove entries with no location - only 0.5% of the data (in 2018 - confirm on recent data) 
                                                                                # but you should be able to estimate the location from the block

crimes[, c("Lat", "Long") := lapply(.SD, as.numeric), .SDcols = c("Lat", "Long")]

crimes[, DateOcc := as.POSIXct(crimes[, DateOcc], 
                               format="%m/%d/%Y %I:%M:%S %p")]                  # convert dates from strings to date obj's
crimes[, Month := format(crimes[, DateOcc], "%m")]                              # create new col with Month alone
crimes[, Day := format(crimes[, DateOcc], "%d")]                                # create new col with Day alone
crimes[, Year := format(crimes[, DateOcc], "%Y")]                               # create new col with Year alone
crimes[, Hour := format(crimes[, DateOcc], "%H")]                               # create new col with Hour alone
crimes[, LatRound := round(crimes[, Lat], 2)]                                   # create new col with rounded Lat
crimes[, LongRound := round(crimes[, Long], 2)]                                 # create new col with rounded Long
crimes[, Date := as.Date(
  paste(crimes[, Year], crimes[, Month], crimes[, Day], sep="-"))]              # create new col with Date string suitable for shiny dateInput

str(crimes)

# Define UI --------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Chicago Crime Statistics",
                  titleWidth = 300),
  
  dashboardSidebar(width = 300,
      
      sliderInput("Lat", 
                  label = "Latitude:",
                  min = max( min(crimes[, LatRound]), 41.6 ), 
                  max = min( max(crimes[, LatRound]), 42.2 ),
                  value = c(max( min(crimes[, LatRound]), 41.6 ), 
                            min( max(crimes[, LatRound]), 42.2 )),
                  step = 0.01),
      
      sliderInput("Long", 
                  label = "Longitude:",
                  min = max( min(crimes[, LongRound]), -87.8 ), 
                  max = min( max(crimes[, LongRound]), -87.5 ), 
                  value = c(max( min(crimes[, LongRound]), -87.8 ), 
                            min( max(crimes[, LongRound]), -87.5 )),
                  step = 0.01),
      
      dateRangeInput("DateRange", label="Date Range:", 
                     start=min(crimes[, Date]), end=max(crimes[, Date]),
                     min=min(crimes[, Date]), max=max(crimes[, Date])),
      
      radioButtons("graphType", label = "Choose graph type:",
                   choices = c("Stacked", "Bar"), 
                   selected = "Bar", inline = TRUE),
      
      radioButtons("matchAx", label = "Match effective y-axes?:",
                   choices = c("Yes", "No"), selected = "No", inline = TRUE),
      
      radioButtons("crimeSel", label = "Select crime types:",
                   choices = c("All", "Manual"), selected = "Manual", 
                   inline = TRUE),
      
      conditionalPanel(
        condition = "input.crimeSel != 'All'",
        checkboxGroupInput("PrimDesc", label = NULL,
                           choices=unique(crimes[, PrimDesc]),
                           selected=sample(unique(crimes[,  PrimDesc]), 5))
        ),
      
      actionButton("go", "Submit Changes!")
      
    ),
    
    dashboardBody(
      fluidPage(
        fluidRow(splitLayout(plotOutput("bar1Y"), plotOutput("bar1N")), style='margin-bottom:8px;'),
        fluidRow(splitLayout(plotOutput("mapY"), plotOutput("mapN")), style='margin-bottom:8px;'),
        fluidRow(splitLayout(plotOutput("bar2Y"), plotOutput("bar2N")))
    ))
)

# Server logic -----------------------------------------------------------------
server <- function(input, output) {

  data <- reactive({
    
    input$go
    
    isolate({
      
      if(input$crimeSel=="All") {
        crimes[Lat >= input$Lat[1] & Lat <= input$Lat[2] & 
                 Long >= input$Long[1] & Long <= input$Long[2] &
                 Date >= input$DateRange[1] & Date <= input$DateRange[2],
               .(Block, PrimDesc, SecDesc, LocDesc, Arrest, Domestic, Month, 
                 Day, Year, Hour, LatRound, LongRound, Date)]
        } else {
          validate(
            need(length(input$PrimDesc) >= 1, 
                 "Please selecct at least one crime type")
          )
        
          crimes[Lat >= input$Lat[1] & Lat <= input$Lat[2] & 
                   Long >= input$Long[1] & Long <= input$Long[2] &
                   Date >= input$DateRange[1] & Date <= input$DateRange[2] &
                   PrimDesc %in% input$PrimDesc,
                 .(Block, PrimDesc, SecDesc, LocDesc, Arrest, Domestic, Month, 
                   Day, Year, Hour, LatRound, LongRound, Date)]
        }
      
    })
  })
  
  levelData <- reactive({
    
    print(data())
    print(str(data()))
    validate(need(nrow(data()) > 0, 
             "No crimes of the selected type within defined date/region")
    )
    
    levelData <- data()[, .(CrimeCount = .N), keyby = .(PrimDesc)]
    
    levelData <- levelData[order(CrimeCount)]
    
    levelData$PrimDesc <- factor(levelData$PrimDesc,
                                 levels = levelData$PrimDesc)
    
    levelData <- levelData$PrimDesc
  })
  
  ####################################
  ##### START OF VERT BAR GRAPHS #####
  ####################################
  
  output$bar1Y <- renderPlot({
    
    input$go
    
    isolate({
      
      if(input$graphType=="Stacked") {
        validate(need(
          length(input$PrimDesc) <= 9 & input$crimeSel == "Manual", 
          "Too many crime types! Limit selection to 9 for graph readability"))  # only 9 colors - restrict selection to 9
      }
      
      maxY <- data()[, .(CrimeCount = .N), keyby = .(Date, Arrest)]
      maxY <- max(maxY$CrimeCount)
      
      crimeGraphDataY <- data()[Arrest==TRUE, 
                                .(CrimeCount = .N), 
                                keyby = .(Date)]
      
      crimeGraphDataYFill <- data()[Arrest==TRUE, 
                                    .(CrimeCount = .N), 
                                    keyby = .(PrimDesc, Date)]
      
      levelData <- levelData()
      names(levelData) <- rep(1:length(levelData))
      
      crimeGraphDataYFill$PrimDesc <- factor(crimeGraphDataYFill$PrimDesc,
                                             levels = levelData)

      colorNums <- sort(strtoi(
        names(
          levelData[unique(crimeGraphDataYFill$PrimDesc)])))
      colors <- rev(brewer.pal(9, "Reds"))[colorNums]
      
      ggplot(crimeGraphDataY, aes(x = Date, y = CrimeCount)) + 
      {if(input$matchAx=="Yes") ylim(0, maxY)} +
        scale_x_date(breaks = pretty(crimeGraphDataY$Date, n = 9), 
                     date_labels = "%b-%d-%Y") +
        {if(input$graphType=="Bar") geom_bar(fill = "#A50F15",                  
                                             color = "#A50F15", 
                                             stat = "identity")} + 
        {if(input$graphType=="Stacked") geom_bar(data = crimeGraphDataYFill,
                                                 aes(x = Date, 
                                                     y = CrimeCount, 
                                                     fill = PrimDesc, 
                                                     color = PrimDesc), 
                                                 position = "stack", 
                                                 stat = "identity")} +
        scale_fill_manual(values = colors) + 
        scale_color_manual(values = colors) +
        labs(title = "Crimes Reported Resulting in Arrest", 
             x = "Date", y = "Crimes Reported by Day", 
             fill = "Crime Type", color = "Crime Type") +
        theme(plot.title = element_text(hjust = 0.5, 
                                        margin = margin(t = 25, b = 25), 
                                        size = 14, color = "black", 
                                        face = "bold"), 
              axis.text.x = element_text(angle = -45, color = "black"),
              axis.text.y = element_text(color = "black"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.title = element_text(size = 12, color = "black"),
              panel.background = element_rect(fill = "steelblue1", 
                                              color = "steelblue"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "black", size = 0.1),
              panel.grid.minor.y = element_line(color = "black", size = 0.1)
              )
    
    })
    
  })
  
  output$bar1N <- renderPlot({
    
    input$go
    
    isolate({
      
      if(input$graphType=="Stacked") {
        validate(need(
          length(input$PrimDesc) <= 9 & input$crimeSel == "Manual", 
          "Too many crime types! Limit selection to 9 for graph readability"))  # only 9 colors - restrict selection to 9
      }
      
      maxY <- data()[, .(CrimeCount = .N), keyby = .(Date, Arrest)]
      maxY <- max(maxY$CrimeCount)
      
      crimeGraphDataN <- data()[Arrest==FALSE, 
                                .(CrimeCount = .N), 
                                keyby = .(Date)]
      crimeGraphDataNFill <- data()[Arrest==FALSE, 
                                    .(CrimeCount = .N), 
                                    keyby = .(PrimDesc, Date)]
      
      levelData <- levelData()
      names(levelData) <- rep(1:length(levelData))
      
      crimeGraphDataNFill$PrimDesc <- factor(crimeGraphDataNFill$PrimDesc,
                                             levels = levelData)
      
      colorNums <- sort(strtoi(
        names(
          levelData[unique(crimeGraphDataNFill$PrimDesc)])))
      colors <- rev(brewer.pal(9, "Reds"))[colorNums]
      
      ggplot(crimeGraphDataN, aes(x = Date, y = CrimeCount)) + 
      {if(input$matchAx=="Yes") ylim(0, maxY)} +
        scale_x_date(breaks = pretty(crimeGraphDataN$Date, n = 9), 
                     date_labels = "%b-%d-%Y") +
        {if(input$graphType=="Bar") geom_bar(fill = "#A50F15", 
                                             color = "#A50F15", 
                                             stat = "identity")} + 
        {if(input$graphType=="Stacked") geom_bar(data = crimeGraphDataNFill, 
                                                 aes(x = Date, 
                                                     y = CrimeCount, 
                                                     fill = PrimDesc, 
                                                     color = PrimDesc), 
                                                 position = "stack", 
                                                 stat = "identity")} +
        scale_fill_manual(values = colors) +                                  
        scale_color_manual(values = colors) +
        labs(title = "Crimes Reported NOT Resulting in Arrest", 
             x = "Date", y = "Crimes Reported by Day", 
             fill = "Crime Type", color = "Crime Type") +
        theme(plot.title = element_text(hjust = 0.5, 
                                        margin = margin(t = 25, b = 25), 
                                        size = 14, color = "black", 
                                        face = "bold"), 
              axis.text.x = element_text(angle = -45, color = "black"),
              axis.text.y = element_text(color = "black"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.title = element_text(size = 12, color = "black"),
              panel.background = element_rect(fill = "steelblue1", 
                                              color = "steelblue"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "black", size = 0.1),
              panel.grid.minor.y = element_line(color = "black", size = 0.1)
              )
    
    })
    
  })

  #####################################
  ##### START OF HORIZ BAR GRAPHS #####
  #####################################
  
  output$bar2Y <- renderPlot({
    
    input$go
    
    isolate({
      maxX <- data()[, .(CrimeCount = .N), keyby = .(PrimDesc, Arrest)]
      maxX <- max(maxX$CrimeCount)
      
      crimePieDataY <- data()[Arrest==TRUE, .(CrimeCount = .N), 
                              keyby = .(PrimDesc)] 
      crimePieDataY <- crimePieDataY[order(CrimeCount)]
      
      crimePieDataY$PrimDesc <- factor(crimePieDataY$PrimDesc, 
                                       levels = crimePieDataY$PrimDesc)         # factors allow fill to sort by stack height
      
      ggplot(crimePieDataY, aes(x = PrimDesc, y = CrimeCount, 
                                label = CrimeCount)) + 
      {if(input$matchAx=="Yes") ylim(0, maxX)} +
        geom_bar(stat = "identity", color = "#A50F15", fill = "#A50F15") +      
        labs(title = "Proportion of Crimes Resulting in Arrerst", 
             y = "Crime Count Over Selected Period/Region") +
        scale_x_discrete(position = "top") +
        theme(plot.title = element_text(hjust = 0.5, 
                                        margin = margin(t = 25, b = 25), 
                                        size = 14, color = "black", 
                                        face = "bold"), 
              axis.text.x = element_text(angle = -45, color = "black"),
              axis.text.y = element_text(color = "black"),
              axis.title.x = element_text(margin = margin(b = 25, t = 25)), 
              axis.title.y = element_blank(),
              axis.title = element_text(size = 12, color = "black"),
              
              panel.background = element_rect(fill = "steelblue1", 
                                              color = "steelblue"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "black", size = 0.1),
              panel.grid.minor.y = element_line(color = "black", size = 0.1)
        ) +
        coord_flip() 
      
    })
  })
  
  output$bar2N <- renderPlot({
    
    input$go
    
    isolate({
      maxX <- data()[, .(CrimeCount = .N), keyby = .(PrimDesc, Arrest)]
      maxX <- max(maxX$CrimeCount)
      
      crimePieDataN <- data()[Arrest==FALSE, .(CrimeCount = .N), 
                              keyby = .(PrimDesc)] 
      crimePieDataN <- crimePieDataN[order(CrimeCount)]
      
      crimePieDataN$PrimDesc <- factor(crimePieDataN$PrimDesc, 
                                       levels = crimePieDataN$PrimDesc)         # factors allow fill to sort by stack height
      
      ggplot(crimePieDataN, aes(x = PrimDesc, y = CrimeCount, 
                                label = CrimeCount)) + 
      {if(input$matchAx=="Yes") ylim(0, maxX)} +
        geom_bar(stat = "identity", color = "#A50F15", fill = "#A50F15") +      
        labs(title = "Proportion of Crimes NOT Resulting in Arrerst", 
             y = "Crime Count Over Selected Period/Region") +
        scale_x_discrete(position = "top") +
        theme(plot.title = element_text(hjust = 0.5, 
                                        margin = margin(t = 25, b = 25), 
                                        size = 14, color = "black", 
                                        face = "bold"), 
              axis.text.x = element_text(angle = -45, color = "black"),
              axis.text.y = element_text(color = "black"),
              axis.title.x = element_text(margin = margin(b = 25, t = 25)), 
              axis.title.y = element_blank(),
              axis.title = element_text(size = 12, color = "black"),
              
              panel.background = element_rect(fill = "steelblue1", 
                                              color = "steelblue"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(color = "black", size = 0.1),
              panel.grid.minor.y = element_line(color = "black", size = 0.1)
        ) +
        coord_flip() 
      
    })
    
  })
  
  #########################
  ##### START OF MAPS #####
  #########################

  map <- reactive({
    
    input$go
    
    isolate({
        mapLat <- c(input$Lat[1], input$Lat[2])
        mapLong <- c(input$Long[1], input$Long[2])
    })
    
    bbox <- make_bbox(mapLong, mapLat, f=0.00)
    myMap <- get_map(bbox, maptype="toner-lite", source="stamen")

    ggmap(myMap) +
      coord_cartesian(xlim=mapLong, ylim=mapLat, default=TRUE) +
      scale_x_continuous(expand = c(0,0)) +                                     # removes ggplot padding so that geom_bin2d can fill the plot
      scale_y_continuous(expand = c(0,0)) +                                     # removes ggplot padding so that geom_bin2d can fill the plot
      coord_fixed()                                                             # make size of x ticks match size of y ticks
      
  })
  
  output$mapY <- renderPlot({
    
    if (nrow(data()) > 0) { map() +
      geom_bin2d(data=data()[Arrest==TRUE],
                 aes(x=LongRound, y=LatRound), alpha = 0.5,
                 binwidth = c(.01, .01)) +
      scale_fill_gradient(low = "steelblue1", high = "#A50F15", 
                          name = "# Crimes") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())                                     # interactive graphs with plot_ly?  Maybe a project for v2
    } else { map() }    
  })
  
  output$mapN <- renderPlot({
    
    if (nrow(data()) > 0) { map() +
      geom_bin2d(data=data()[Arrest==FALSE],
                 aes(x=LongRound, y=LatRound), alpha=0.5,
                 binwidth = c(.01, .01)) +
      scale_fill_gradient(low = "steelblue1", high = "#A50F15", 
                          name = "# Crimes") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    } else { map() }
  })

}

# Run app ----------------------------------------------------------------------
shinyApp(ui, server)