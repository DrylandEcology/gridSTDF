library(shiny)
library(shinydashboard)
library(leaflet)
# analysis
library(rSFSW2)
library(rSOILWAT2)
library(splines)
# data formatting
library(data.table)
library(dplyr)
library(lubridate)
library(geoknife)
library(raster)
# Plotting
library(ggplot2)
library(plotly)

source("functions/simulationFunctions.R")
source("functions/soilsAndComp.R")
source("functions/weatherFunctions.R")


# Define UI --------------------------------------
ui <- fluidPage(
  
  titlePanel("Short-term Drought Forecaster"),
  
  tabsetPanel(id = "mainTabset",             
              
              
              # Sidebar layout with site-by-site definitions ----
              tabPanel("Site information",
                       sidebarLayout(position = "left",
                                     
                                     # IDEA: site-by-site: choose 4 Inputs - These inputs actually control what will be simulated
                                     # # (1) Site location, (2) whether or not you want future simulations, (3) soil type (4) Vegetation
                                     
                                     # Sidebar panel for user inputs ----
                                     sidebarPanel( width = 5,
                                       
                                       ####################################################################
                                       ######## ------------------- LOCATIONS  -----------------  #########
                                       ####################################################################
                                       
                                       ## Location: Choose a location by entering coordinates or clicking on a map ----
                                       h4("Site Location"),
                                       
                                       leafletOutput("locMap"),#, height = 250, width = 250), 
                                       
                                       fluidRow(
                                         splitLayout(
                                           numericInput("lat", "lat", "35.1983", min = 25, max = 49),
                                           numericInput("lng", "long", "-111.6513", min = -12, max = -100)
                                         )
                                       ),
                                       br(), # break
                                       
                                       ####################################################################
                                       ######## ------------------- SOILS ----------------------  #########
                                       ####################################################################
                                       
                                       # TO DO -> Texture Triangle, not inputs
                                       
                                       radioButtons("soils", label = h4("Extract or Choose Soils?"), 
                                                    choices = list('Extract' = 1, 'Choose' = 2),
                                                    inline = TRUE, # side-by-side
                                                    selected = 2),
                                       
                                       conditionalPanel(
                                         condition = "input.soils == 2",
                                         ### input slots for soil that appear of select == TRUE
                                         fluidRow(
                                           splitLayout(
                                             numericInput("sand", "sand", "34", min = 0, max = 100),
                                             numericInput("silt", "silt", "33", min = 0, max = 100),
                                             numericInput("clay", "clay", "33", min = 0, max = 100)
                                           )
                                         )
                                         
                                       ),
                                       
                                       br(),
                                       
                                       ####################################################################
                                       ######## -------------------- VEG -----------------------  #########
                                       ####################################################################
                                       
                                       radioButtons("comp", label = h4("Estimate or Choose Comp?"), 
                                                    choices = list('Estimate' = 1, 'Choose' = 2),
                                                    inline = TRUE, # side-by-side
                                                    selected = 2),
                                       
                                       conditionalPanel(
                                         condition = "input.comp == 2",
                                         fluidRow(
                                           splitLayout(
                                             numericInput("trees", "trees", "0.0", min = 0, max = 1),
                                             numericInput("shrubs", "shrubs", "0.5", min = 0, max = 1),
                                             numericInput("grasses", "grasses", "0.5", min = 0, max = 1),
                                             numericInput("forbs", "forbs", "0.0", min = 0, max = 1),
                                             numericInput("bg", "bareground", "0.0", min = 0, max = 1)
                                           )
                                         )
                                         
                                       ),
                                       
                                       br(),
                                       
                                       ####################################################################
                                       ######## -------------------- GO! -----------------------  #########
                                       ####################################################################
                                       
                                       ## Just need this once for "... textInput"
                                       verbatimTextOutput("value"),
                                       
                                       actionButton("simulate", label = "Simulate!")
                                     ), # end of side bar panel
                                     
                                     # Main panel for outputs ----
                                     mainPanel(width = 7,"Welcome to the short-term drought forecaster!")
                       ) # end of side bar layout
              ) #end of tab
              ),# end of tabset
  uiOutput("creationPool", style = "display: none;") # for new tabs
)# end of UI


# Output interface -------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  # End Important
  # Important! : This is the make-easy wrapper for adding new tabPanels.
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  
  #################################################################################################
  ######## ------------------------------ UI Reactivity ---------------------------------  ########
  #################################################################################################
  
  # Leaflet map ---------------------------------------
  output$locMap <- renderLeaflet({ 
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = input$lng, lat = input$lat , zoom = 9) %>%
      addMarkers(input$lng, input$lat)
  })
  
  # Click functionality -----------------------------------------------------------
  
  observeEvent(input$locMap_click, { # input$MAPID_event = input$locMap_click
    
    proxy <- leafletProxy("locMap")
    
    #### 1 - Allow for visualization of click ----------------------------------
    click <- input$locMap_click
    
    proxy %>% clearMarkers() %>%
      setView(lng = click$lng, lat = click$lat , zoom = 9) %>%
      addMarkers(click$lng, click$lat)
    
    #### 2 - Allow for click lat/long to update numeric lat/long  --------------
    
    updateNumericInput(session, "lat",  value = round(input$locMap_click$lat, 4))
    updateNumericInput(session, "lng", value = round(input$locMap_click$lng, 4))
    
  })
  
  
  #################################################################################################
  ######## ---------------------------- SIMULATION!  ------------------------------------  ########
  #################################################################################################
  
  run <- reactiveValues() # set up list of values which will store simulation/function output
  
  observeEvent(input$simulate, {
    
    begintime <- proc.time() # start timer clock
    showModal(modalDialog("calculation running!"))
    
    run$SW_out <- gatherDataAndExecuteSW(input$lat, input$lng, input$soils, input$sand, input$clay,
                                         input$comp, input$trees, input$shrubs, input$grasses, input$forbs, input$bg) # the actual calculation
    
    #print(run$SWout) - What is the point of the run object. Do I need it? will I access
    #run$outs <- get_output()
    
    endtime <- proc.time() - begintime
    showModal(modalDialog(paste("calculation finished in", round(unname(endtime[3]), 0), "seconds")))
    
    
  ##################################################################################################
  ######## --------------------------------- Output UI  ---------------------------------  ########
  #################################################################################################
  insertTab(inputId = "mainTabset", 
            tabPanel(title = "Short-Term Forecasts", value = "outputs1",
                     fluidRow(
                          # Options --------------------------------
                       column(width = 3,
                           # What depth?
                           selectInput("depth", "Depth:",
                                       c( "Shallow (0-15cm)" = "Shallow",
                                         "Intermediate (15-60cm)" = "Intermediate",
                                         "Deep  (60-250cm)" = "Deep")),
  
                           # DO you want to add a specific year?
                           radioButtons("yearButton", label = h4("Year:"), 
                                        choices = list('Yes' = 1, 'No' = 2),
                                        inline = TRUE, # side-by-side
                                        selected = 2),
                           
                           conditionalPanel(
                             condition = "input.yearButton == 1",
                             
                             # Select a specific year ----------
                             numericInput("years2", label = "Select a year:", 
                                          min = 1980, max = 2015, 
                                          value = c(2002))
                           ),
                           box(width = 12,
                               # 'Here you will find summarized soil moisture information about your site, generated from an ecohydrological water 
                               # balance model, SOILWAT2. Presented is both historical soil moisture information for your site, based on gridMet climate data,
                               # as well as short-term soil moisture forecasts for the upcoming 12 months, based on climate predictions from the National Weather Service.
                               'Soil moisture is measured as soil water potential (SWP, -MPa), where higher numbers mean the soil is more wet.',
                               br(), 
                               br(),
                                'The top figure depicts daily average soil moisture for both historical (1980 - 2018, purple) and upcoming (black) year. Individual historical years can be overlain.'
                           )
                           
                           ),
                           
                           #Figures and Text ----------------------------
                       column(width = 9,
                              
                              fluidRow(
                                plotlyOutput("Annual_Plot", width = "100%")
                       )
                     )
                     ),
                     
                    fluidRow(
                      column(width = 3,
                             
                        # What date range??
                        sliderInput("dates", "Date Range:", min = as.Date('01-01', "%m-%d"),
                                    max = as.Date('12-31', '%m-%d'),
                                    value = c(as.Date('08-15', '%m-%d'), as.Date('10-15', '%m-%d')),
                                    timeFormat = '%m-%d'),
                        
                        # What limit?
                        numericInput("thresh", label = "SWP Threshold (MPa):", value = -3, min = -15, max = 0, step = .5),
                        
                        box(width = 12,
                            'The bottom figure depicts daily distribution of historical and future soil moisture for the month for the selected month and thresholds'
                        )
                  
                      ),
                      
                      column(width = 9,
                           plotlyOutput("Histo_Plot", width = '100%')
                        )
                    )
                       
            ), target = "Site information", position = 'after') # end of tab 
  
  updateTabsetPanel(session, "mainTabset", selected = "outputs1")  
  })
  #################################################################################################
  #################################################################################################
  ######## ---------------------------- Output Reactivity  -----------------------------  ########
  #################################################################################################
  #################################################################################################

  output$Annual_Plot <- renderPlotly({
   
    data <- run$SW_out
    AnomalyData  <- data[1][[1]]
    Anom_AllYears  <- data[3][[1]]
    HistData  <- data[2][[1]]
    Hist_AllYears  <- data[4][[1]]
    HistogramData <- rbind(Anom_AllYears, Hist_AllYears)

    # Var value from drop down / select depth ------------------------------------
    depth <- input$depth
    HistDataSub <- HistData[HistData$Depth %in% depth, ]
    AnomalyDataSub <- AnomalyData[AnomalyData$Depth %in% depth, ]
    Hist_AllYearsSub <- Hist_AllYears[Hist_AllYears$Depth %in% depth,]

    # Annual mean doo dads -------------------------------------- --------------------------
    line <- list(
      type = "line",
      line = list(color = "black", dash = 'dash'),
      x0 = 0, x1 = 366, y0 = input$thresh, y1 = input$thresh
    )
    
    today <- yday(Sys.time())
    
    lineToday <- list(
      type = 'line',
      line = list(color = 'darkgray', width = 3),
      y0 = 1, y1 = min(HistDataSub$SWP_min), x0 = today, x1 = today)
    
    # -------------- shape highlight for dates
    monthx0 <- yday(input$dates[1])
    monthx1 <- yday(input$dates[2])
    
    monthShape <- list(type = "rect",
                       fillcolor = "rgb(0, 177, 106)", line = list(color = "green"), opacity = 0.2,
                       x0 = monthx0, x1 = monthx1, xref = "x",
                       y0 = 0, y1 = min(HistDataSub$SWP_min), yref = "y")
    

    # PLOTTING    --------------------------------------------------------------------------------
    MEAN <- plot_ly() %>%
      # historical pattern ---------------------------------------------------
    ### ------------- sd -------------
    add_trace(data = HistDataSub, x = ~Day, y = ~SWP_max, type = 'scatter', mode = 'lines',
              line = list(color = 'transparent'),
              showlegend = FALSE, name = 'SD (1980 - 2015') %>%
      add_trace(data = HistDataSub, x = ~Day, y = ~SWP_min, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor = 'rgba(190, 144, 212,.3)',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'SD (1980 - 2015)') %>%
      ### ------------- mean -------------
    add_trace(data = HistDataSub, x = ~Day, y = ~SWP_mean,  mode = 'lines',
              line = list(color='purple', width = 3),
              # marker = list(color = 'white', size = 10, line = list(color = 'black', width = 2)),
              name = 'Historical Average (1980 - 2018)') %>%
      #upcoming pattern ---------------------------------------------------
    add_trace(data = AnomalyDataSub, x = ~Day, y = ~SWP_mean, mode = 'lines',
              line = list(color='black', width = 5),
              # marker = list(color = 'white', size = 10, line = list(color = 'purple', width = 2)),
              name = 'Coming Year') %>%
      # layout --------------------------------------------------------------
    layout(
      title = "Daily Average Soil Moisture",
      yaxis = list(title = 'soil water potential (-MPa)'), shapes = list(line,lineToday, monthShape),
           legend = list(x = 0.01, y = .01, color = 'black'),
           xaxis = list(
             ticktext = list('Jan', 'Feb', 'Mar', 'Apr','May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
             tickvals = list(15, 45, 75, 105, 135, 165, 195, 225, 255, 285, 315, 345),
             tickmode = "array"))
    
    if(input$yearButton == 1){
      Hist_OneYear <- Hist_AllYearsSub[Hist_AllYearsSub$Year == input$years2, ]

            MEAN <- MEAN %>%
              add_trace(data = Hist_OneYear,  x = ~Day, y = ~SWP_mean, mode = 'lines',
                  line = list(color='red', dash = 'dash', width = 2),
                  name = paste0('Mean SWP - ', input$years2))
    }
    
    MEAN
    
  })

  # Histo ----------------------------------------------------------------------------------------------
  
  output$Histo_Plot <- renderPlotly({
    
    data <- run$SW_out
    Anom_AllYears  <- data[3][[1]]
    Hist_AllYears  <- data[4][[1]]
    HistogramData <- rbind(Anom_AllYears, Hist_AllYears)

    # Var value from drop down / select depth ------------------------------------
    depth <- input$depth
    HistogramDataSub1 <- HistogramData[HistogramData$Depth %in% depth, ]

    # Var value from drop down / select month ------------------------------------
    print(input$dates)
    HistogramDataSub2 <- HistogramDataSub1[HistogramDataSub1$Date %in% c(input$dates[1]:input$dates[2]), ]
    print(head(HistogramDataSub2))
    
    HISTO <- ggplot() +
       ##### intercept
       geom_vline(xintercept = input$thresh, linetype = 'dashed') +
       ##### data
       geom_density(data = HistogramDataSub2, aes(SWP_mean, fill = Type), alpha = 0.5) +
       ##### colors
       scale_fill_manual(values = c('rgba(0, 177, 106, .8)', 'rgba(190, 144, 212,.8)')) +
       ##### formatting
       scale_x_continuous(expand = c(0,0)) +
       scale_y_continuous(expand = c(0,0)) +
       theme_bw() +
       labs(x = 'soil water potential (-MPa)',
            title = paste('Probability Distribution of SWP'))+
       theme(plot.title = element_text(hjust = 0.5))
    
     
     
     HISTO2 <- ggplotly(HISTO) %>%
       layout(legend = list(x = 0.01, y = 1))
     
     HISTO2

  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
