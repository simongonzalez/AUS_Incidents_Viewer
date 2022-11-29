#author: Simon Gonzalez
#email: simon.gonzalez@anu.edu.au
#date: 05 November 2019

library(shinydashboard)
library(tidyverse) #data processing
library(echarts4r) #visualisation
library(echarts4r.assets) #visualisation
library(echarts4r.maps) #visualisation
library(echarts4r.suite) #visualisation
library(reshape2) #processing
library(zoo)
library(geojsonsf) #geocoding
library(sf) #geocoding
library(leaflet)
library(lubridate)
library(varhandle)
library(shinyjqui)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "AU Incidents Viewer",
                  dropdownMenu(badgeStatus = NULL,icon = icon('info'), headerText = 'App creator', type = 'messages',
                               notificationItem(
                                 text = "Simon Gonzalez",
                                 icon("user")
                               ),
                               notificationItem(
                                 text = "www.visualcv.com/simongonzalez/",
                                 icon("link"),
                                 status = "success",
                                 href = 'https://www.visualcv.com/simongonzalez'
                               )
                  )),
  dashboardSidebar(
    bsButton("info1", label = "What's this app?", icon = icon("globe"), style = 'info'),
    radioButtons(inputId = 'byType', label = 'Plot Incidents based on', choices = c('All', 'State', 'Series'), selected = 'All', inline = T),
    uiOutput('timeUI'),
    uiOutput('plot3dIU'),
    uiOutput('colorByIU')
  ),
  dashboardBody(
    
    conditionalPanel(
      condition = "input.byType != 'State'",
      jqui_resizable(echarts4rOutput("mainMapEC"))
    ),
    
    conditionalPanel(
      condition = "input.byType == 'State'",
      jqui_resizable(leafletOutput("mainMapLL"))
    ),
    jqui_resizable(echarts4rOutput("graphPlot"))
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    df <- read.csv('accdata.csv')
    df$Age <- as.numeric(df$Age)
    return(df)
  })
  
  output$plot3dIU <- renderUI({
    if(is.null(df))
      return()
    
    if(input$byType != 'All')
      return()
    
    checkboxInput(inputId = 'plot3d', label = 'Plot 3D Map', value = F)
  })
  
  output$timeUI <- renderUI({
    if(is.null(df))
      return()
    
    if(input$byType == 'All')
      return()
    
    df <- df()
    
    if(input$byType == 'Series'){
      selectInput('timesel', label = NULL, choices = c('Month', 'Year', 'Dayweek', 'Time', 'Time.of.day', 'Age.Group', 'Speed.Limit'))
    }else{
      selectInput('timesel', label = NULL, choices = sort(unique(df$region)))
    }
  })
  
  output$colorByIU <- renderUI({
    if(is.null(df))
      return()
    
    if(input$byType != 'State')
      return()
    
    selectInput('colorBy', label = 'Color points by', choices = unlist(strsplit('Crash.Type Bus.Involvement Heavy.Rigid.Truck.Involvement Articulated.Truck.Involvement Road.User Gender National.Remoteness.Areas National.Road.Type Christmas.Period Easter.Period', ' ')), selected = 'Crash.Type')
  })
  
  #plot main chart
  
  output$mainMapEC <- renderEcharts4r({
    if(is.null(df))
      return()
    
    if(input$byType == 'State')
      return()
    
    df <- df()
    
    df$Age <- as.numeric(df$Age)
    
    if(input$byType == 'All'){
      
      if(input$plot3d){
        dfp <- as.data.frame(df %>% group_by(region) %>% count())
        dfp$n <- dfp$n/100
        
        dfp %>% 
          e_color_range(n, Color) %>% 
          e_charts(region) %>%
          em_map("Australia") %>% 
          e_geo_3d(n, Color, type = "Australia", regionHeight = 1) %>% 
          e_visual_map(n)
        
      }else{
        dfp <- as.data.frame(df %>% group_by(region) %>% count())
        
        dfp %>% 
          e_charts(region) %>% 
          em_map("Australia") %>% 
          e_map(n, map = "Australia") %>% 
          e_visual_map(n) %>% 
          e_theme("infographic") %>% e_theme("chalk")
        
      }
      
    }else if(input$byType == 'Series'){
      
      if(is.null(input$timesel))
        return()
      
      #time series
      # scale 0 to 1
      .scl <- function(x){
        (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
      }
      
      timesel <- input$timesel
      
      df[[timesel]] <- as.factor(df[[timesel]])
      
      for(ii in unique(df[[timesel]])){
        df[df[[ii]] == ii,'Age'] <- .scl(df[df[[ii]] == ii,'Age'])
      }
      
      df <- as.tibble(df[,c('region', timesel, 'Age', 'lat', 'long')])
      
      if(timesel == 'Month'){
        
        monthMapNumber <- setNames(paste0(letters[1:12], str_to_sentence(unlist(strsplit('JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC', ' ')))), str_to_sentence(unlist(strsplit('JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC', ' '))))
        
        df[[timesel]] <- monthMapNumber[as.character(df[[timesel]])]
        
      }else if(timesel == 'Dayweek'){
        
        dayMapNumber <- setNames(paste0(1:7, unlist(strsplit('Monday Tuesday Wednesday Thursday Friday Saturday Sunday', ' '))), unlist(strsplit('Monday Tuesday Wednesday Thursday Friday Saturday Sunday', ' ')))
        
        df[[timesel]] <- dayMapNumber[as.character(df[[timesel]])]
        
      }else if(timesel == 'Time'){
        df[[timesel]] <- gsub('\\:.*', '', df[[timesel]])
        df[[timesel]] <- as.numeric(df[[timesel]])
      }else if(timesel == 'Speed.Limit'){
        df[[timesel]] <- unfactor(df[[timesel]])
        df[df[[timesel]] == '<40','Speed.Limit'] <- '20'
        df[df[[timesel]] == '-9','Speed.Limit'] <- '9'
        df[[timesel]] <- as.numeric(df[[timesel]])
      }
      
      df %>%
        group_by_(.dots= timesel) %>%
        e_charts(long, timeline = TRUE) %>% 
        e_geo(
          roam = TRUE,
          boundingCoords = list(
            c(140, - 10),
            c(125, -45)
          )
        ) %>% 
        e_scatter(
          lat, Age, legend = F,
          coord_system = "geo", rm_x = T, rm_y = T
        ) %>% 
        e_visual_map(min = 0, max = 1, rm_x = T, rm_y = T) %>% 
        e_timeline_opts(autoPlay = TRUE) %>% 
        e_tooltip(trigger = "city", show = TRUE) %>% 
        e_theme("dark")
      
    }
  })
  
  output$mainMapLL <- renderLeaflet({
    
    if(is.null(df))
      return()
    
    if(input$byType != 'State')
      return()
    
    if(is.null(input$timesel))
      return()
    
    if(is.null(input$colorBy))
      return()
    
    df <- df()
    
    df$Age <- as.numeric(df$Age)
    
    #time series
    # scale 0 to 1
    df <- df[complete.cases(df),]
    df <- df[df$lat != 0 & df$long != 0,]
    
    df <- df[df$region == input$timesel,]
    
    # The palette with black:
    cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    df$type <- df[[input$colorBy]]
    
    df <- df[df$type != '',]
    
    tmpChoices <- sort(unique(df$type))
    
    palateColor <- colorFactor(cbbPalette[1:length(tmpChoices)], domain = tmpChoices)
    
    leaflet(df) %>% 
      addTiles() %>%
      setView(unique(df$longc), unique(df$latc), zoom = unique(df$initialZoom)) %>%
      addCircleMarkers(
        lng=df$long, lat=df$lat,
        radius = ~Age/5,
        color = ~palateColor(type),
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~as.character(city)
      ) %>%
      addLegend(pal = palateColor, values = ~type, opacity = 1)%>%
      addMiniMap(toggleDisplay = TRUE) #%>% addProviderTiles(providers$Stamen.Toner)
    
  })

  #graph plots
  output$graphPlot <- renderEcharts4r({
    if(is.null(df))
      return()
    
    df <- df()
    
    df$Age <- as.numeric(df$Age)
    
    if(input$byType == 'All'){
      
      if(input$plot3d){
        dfp <- as.data.frame(df %>% group_by(region) %>% count())
        dfp$n <- dfp$n/100
        
        dfp %>% 
          e_charts(region) %>% 
          e_pie(n, roseType = "radius") %>% e_theme("chalk")
        
      }else{
        dfp <- as.data.frame(df %>% group_by(region) %>% count())
        dfp$n <- dfp$n/100
        
        dfp %>%
          e_charts() %>% 
          e_funnel(n, region) %>% e_theme("chalk")
      }
      
    }else if(input$byType == 'Series'){
      
      if(is.null(input$timesel))
        return()
      
      #time series
      # scale 0 to 1
      .scl <- function(x){
        (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
      }
      
      timesel <- input$timesel
      
      df[[timesel]] <- as.factor(df[[timesel]])
      
      for(ii in unique(df[[timesel]])){
        df[df[[ii]] == ii,'Age'] <- .scl(df[df[[ii]] == ii,'Age'])
      }
      
      df <- as.tibble(df[,c('region', timesel, 'Age', 'lat', 'long')])
      
      if(timesel == 'Month'){
        
        monthMapNumber <- setNames(paste0(letters[1:12], str_to_sentence(unlist(strsplit('JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC', ' ')))), str_to_sentence(unlist(strsplit('JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC', ' '))))
        
        df[[timesel]] <- monthMapNumber[as.character(df[[timesel]])]
        
        df$x <- df[[timesel]]
        
        df2 <- df %>% group_by(region, x) %>% count()
        
        data_wide <- spread(df2, region, n)
        names(data_wide) <- gsub(' ', '', names(data_wide))
        
        data_wide$dates <- as.Date(format(seq(as.POSIXct('2019-01-01', format='%Y-%m-%d'), as.POSIXct('2019-12-31', format='%Y-%m-%d'),
                                              length.out=12), '%Y-%m-%d'))
        
        data_wide[2:8] <- lapply(data_wide[2:8], as.numeric)
        
        data_wide$x <- NULL
        
        data_wide <- data_wide[unlist(strsplit('dates NewSouthWales NorthernTerritory Queensland SouthAustralia Tasmania Victoria WesternAustralia', ' '))]
        
        data_wide %>%
          e_charts(dates) %>% 
          e_river(NewSouthWales) %>%
          e_river(NorthernTerritory) %>%
          e_river(Queensland) %>%
          e_river(SouthAustralia) %>%
          e_river(Tasmania) %>%
          e_river(Victoria) %>%
          e_river(WesternAustralia) %>% 
          e_tooltip(trigger = "axis") %>% e_theme("chalk") %>% 
          e_datazoom()
        
        
      }else if(timesel == 'Dayweek'){
        
        dayMapNumber <- setNames(paste0(1:7, unlist(strsplit('Monday Tuesday Wednesday Thursday Friday Saturday Sunday', ' '))), unlist(strsplit('Monday Tuesday Wednesday Thursday Friday Saturday Sunday', ' ')))
        
        df[[timesel]] <- dayMapNumber[as.character(df[[timesel]])]
        
        df2 <- df %>% group_by_(.dots = c('region', timesel)) %>% count()
        df2$region <- gsub(' ', '', df2$region)
        
        df2$series <- df2[[timesel]]
        df2$Dayweek <- NULL
        
        data_wide <- spread(df2, region, n)
        
        data_wide[2:8] <- lapply(data_wide[2:8], as.numeric)
        
        data_wide$series <- as.factor(data_wide$series)
        
        data_wide %>% 
          e_charts(series) %>% 
          e_radar(NewSouthWales, max = max(df2$n), name = "NewSouthWales") %>%
          e_radar(NorthernTerritory, max = max(df2$n), name = "NorthernTerritory") %>%
          e_radar(Queensland, max = max(df2$n), name = "Queensland") %>%
          e_radar(SouthAustralia, max = max(df2$n), name = "SouthAustralia") %>%
          e_radar(Tasmania, max = max(df2$n), name = "Tasmania") %>%
          e_radar(Victoria, max = max(df2$n), name = "Victoria") %>%
          e_radar(WesternAustralia, max = max(df2$n), name = "WesternAustralia") %>%
          e_tooltip(trigger = "item") %>% e_theme("chalk")
        
      }else if(timesel %in% c('Time', 'Year')){
        
        if(timesel %in% c('Time')){
          df[[timesel]] <- gsub('\\:.*', '', df[[timesel]])
          df[[timesel]] <- as.numeric(df[[timesel]])
        }
        
        df2 <- df %>% group_by_(.dots = c('region', timesel)) %>% count()
        
        df2$Series <- df2[[timesel]]
        
        df2 %>% 
          group_by(region) %>% 
          e_charts(Series) %>% 
          e_line(n) %>% 
          e_tooltip(trigger = "axis") %>% e_theme("chalk") %>% 
          e_datazoom()
        
      }else{
        
        if(timesel == 'Speed.Limit'){
          df[[timesel]] <- unfactor(df[[timesel]])
          df[df[[timesel]] == '<40','Speed.Limit'] <- '20'
          df[df[[timesel]] == '-9','Speed.Limit'] <- '9'
          df[[timesel]] <- as.numeric(df[[timesel]])
        }
        
        df2 <- df %>% group_by_(.dots = c('region', timesel)) %>% count()
        
        df2$n <- as.numeric(df2$n)
        
        df2$series <- df2[[timesel]]
        df2[[timesel]] <- NULL
        
        df2 %>% 
          group_by(region) %>% 
          e_charts(series) %>% 
          e_bar(n) %>%
          e_tooltip(trigger = "item") %>% 
          e_datazoom() %>% e_theme("chalk")
      }
      
    }else{
      if(is.null(df))
        return()
      
      if(input$byType != 'State')
        return()
      
      if(is.null(input$timesel))
        return()
      
      if(is.null(input$colorBy))
        return()
      
      df <- df()
      
      df$Age <- as.numeric(df$Age)
      
      #time series
      # scale 0 to 1
      df <- df[complete.cases(df),]
      df <- df[df$lat != 0 & df$long != 0,]
      
      df <- df[df$region == input$timesel,]
      
      df$type <- df[[input$colorBy]]
      
      df <- df[df$type != '',]
      
      df2 <- df %>% group_by(type) %>% count()
      
      df2 %>% 
        e_charts(type) %>% 
        e_bar(n) %>% 
        e_tooltip() %>% e_theme('chalk')
    }
  })
  
  observeEvent(input$info1, {
    sendSweetAlert(
      session = session,
      title = "Australian Incidents Viewer",
      text = HTML('This app allows users to see accident locations across Australia. The data is publically available at https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=ardd%20fatal%20crashes'),
      closeOnClickOutside = T, type = 'info'
    )
  })
  
}

shinyApp(ui, server)
