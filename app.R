library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tibble)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(DT)
library("viridis") 

# read in data, change date format, and rename columns
# if user wants to upload new dataset, have to be mindful of the column names/order
# especially in the "vargroup" variable (which is important for the dropdown menu groups)
flux <- read_csv("Update_1_24_2022.csv")
flux$backbone <- mdy_hm(flux$backbone)
flux <- flux %>% 
  rename("datetime" = backbone) %>%
  rename("CO2 Turbulent Flux (umolCO2 m-2 s-1)" = FC) %>%
  rename("Latent Heat Turbulent Flux (W/m^2)" = LE) %>%
  rename("Sensible Heat Turbulent Flux (W/m^2)" = H) %>%
  rename("Friction Velocity (m/s)" = USTAR) %>%
  rename("Air Temperate High Altitude Sensor (C)" = TA_HIGH) %>%
  rename("Air Temperate Low Altitude Sensor (C)" = TA_LOW) %>%
  rename("Wind Direction (decimal degrees)" = WD) %>%
  rename("Wind Speed (m/s)" = WS) %>%
  rename("Vapor Pressure Deficit High Altitude Sensor (hPa)" = VPD_HIGH) %>%
  rename("Vapor Pressure Deficit Low Altitude Sensor (hPa)" = VPD_LOW) %>%
  rename("Soil Heat Flux (W/m^2)" = G) %>%
  rename("Net Radiation (W/m^2)" = NETRAD) %>%
  rename("Monin-Obukhov Stability" = ZL) %>%
  rename("Shortwave Radiation, Incoming (W/m^2)" = SW_IN) %>%
  rename("Shortwave Radiation, Outgoing (W/m^2)" = SW_OUT) %>%
  rename("Longwave Radiation, Outgoing (W/m^2)" = LW_OUT) %>%
  rename("Longwave Radiation, Incoming (W/m^2)" = LW_IN) %>%
  rename("Photosynthetically Active Radiation (mmoles of light energy m -2)" = PAR) %>%
  rename("Relative Humidity, range 0-100, High Altitude Sensor (%)" = RH_HIGH) %>%
  rename("Relative Humidity, range 0-100, Low Altitude Sensor (%)" = RH_LOW) %>%
  rename("Soil Water Content 10 cm depth (%)" = SWC_1) %>%
  rename("Soil Water Content 20 cm depth (%)" = SWC_2) %>%
  rename("Soil Water Content 30 cm depth (%)" = SWC_3) %>%
  rename("Soil Water Content 50 cm depth (%)" = SWC_4) %>%
  rename("Soil Water Potential 10 cm depth (%)" = SWP_1) %>%
  rename("Soil Water Potential 20 cm depth (%)" = SWP_2) %>%
  rename("Soil Water Potential 30 cm depth (%)" = SWP_3) %>%
  rename("Soil Water Potential 50 cm depth (%)" = SWP_4) %>%
  rename("Soil Temperature 10 cm depth Site 1 (C)" = TSOIL_10.1) %>%
  rename("Soil Temperature 20 cm depth Site 1 (C)" = TSOIL_20.1) %>%
  rename("Soil Temperature 30 cm depth Site 1 (C)" = TSOIL_30.1) %>%
  rename("Soil Temperature 50 cm depth Site 1 (C)" = TSOIL_50.1) %>%
  rename("Soil Temperature 10 cm depth Site 2 (C)" = TSOIL_10.2) %>%
  rename("Soil Temperature 20 cm depth Site 2 (C)" = TSOIL_20.2) %>%
  rename("Soil Temperature 30 cm depth Site 2 (C)" = TSOIL_30.2) %>%
  rename("Soil Temperature 50 cm depth Site 2 (C)" = TSOIL_50.2) %>%
  rename("Snow Temperature 10 cm depth (C)" = TSNOW_10) %>%
  rename("Snow Temperature 20 cm depth (C)" = TSNOW_20) %>%
  rename("Snow Temperature 30 cm depth (C)" = TSNOW_30) %>%
  rename("Snow Temperature 40 cm depth (C)" = TSNOW_40) %>%
  rename("Snow Temperature 50 cm depth (C)" = TSNOW_50) %>%
  rename("Snow Temperature 60 cm depth (C)" = TSNOW_60) %>%
  rename("Snow Temperature 70 cm depth (C)" = TSNOW_70) %>%
  rename("Snow Temperature 80 cm depth (C)" = TSNOW_80) %>%
  rename("Snow Temperature 90 cm depth (C)" = TSNOW_90) %>%
  rename("Snow Temperature 100 cm depth (C)" = TSNOW_100) %>%
  rename("Snow Temperature 110 cm depth (C)" = TSNOW_110) %>%
  rename("Snow Temperature 120 cm depth (C)" = TSNOW_120) %>%
  rename("Snow Temperature 130 cm depth (C)" = TSNOW_130) %>%
  rename("Snow Temperature 140 cm depth (C)" = TSNOW_140)

# group all variables
vargroup <- list(
  'Flux' = colnames(flux)[2:5],
  'Radiation' = colnames(flux)[6:11],
  'Wind' = colnames(flux)[12:15],
  'Air Temperature' = c(colnames(flux)[16], colnames(flux)[19]),
  'Humidity' = c(colnames(flux)[17], colnames(flux)[20]),
  'Pressure' = c(colnames(flux)[18], colnames(flux)[21]),
  'Soil Water' = colnames(flux)[22:29],
  'Soil Temperature' = colnames(flux)[30:37],
  'Snow Temperature' = colnames(flux)[38:51],
  'TA' = colnames(flux)[52:61])

# pivot data longer
fluxlong <- pivot_longer(flux, colnames(flux[-1]), names_to = "variable", values_to = "values")

# daily dataset
dailyflux <- flux %>%
  mutate(DAY = str_sub(datetime, 1, 10)) %>%
  select(DAY, everything(), -datetime) %>%
  group_by(DAY) %>%
  mutate_if(is.numeric, mean, na.rm = TRUE) %>%
  distinct()

# monthly dataset
monthlyflux <- flux %>%
  mutate(MONTH = str_sub(datetime, 1, 7)) %>%
  select(MONTH, everything(), -datetime) %>%
  group_by(MONTH) %>%
  mutate_if(is.numeric, mean, na.rm = TRUE) %>%
  distinct()

# yearly dataset
yearlyflux <- flux %>%
  mutate(YEAR = str_sub(datetime, 1, 4)) %>%
  select(YEAR, everything(), -datetime) %>%
  group_by(YEAR) %>%
  mutate_if(is.numeric, mean, na.rm = TRUE) %>%
  distinct()


# user interface ===========================================================================================================

ui <- fluidPage(
  theme = shinytheme("yeti"),
  
  navbarPage(
    title = "Hubbard Brook Flux Tower Data Viewer",
    
    tabPanel("App Guide",
        h4("The Hubbard Brook Experimental Forest has been an important site for environmental research since 
            it was established in 1955 by the USDA Forest Service. The flux tower located in this forest collects
            an extremely wide range of atmospheric and environmental observations, and is the focus of this app."),
        h4("This app will take the raw data of the Hubbard Brook Flux Tower, clean it, and allow the user a number 
            of options to be able to view and explore the data."),
             
        br(), h4(strong("Information about app functionality:")),
        tags$li("Date range inputs will update across the entire app."),
             
        h5(strong("Timeseries tab:")),
        tags$li("Brushing functionality* allows users to manually highlight specific areas to zoom to that date range."),
        tags$li("Dropdown menu allows users to select one or many Y variables per plot. Check ‘Compare more than one 
                plot’ for three additional Y variable inputs."),
        tags$em("Note: It's important to only plot data that share the same unit. Each variable has been grouped based on 
                relationship to each other, and every variable name has units included."),
        tags$li("Enter a numeric value in the 'Add Y value line' input box to add a horizontal line in the first plot."),
        tags$li("Plot type options allows users to view all plots as line or points."),
             
        h5(strong("Bivariate tab:")),
        tags$li("Dropdown menu allows users to select two variables to plot against each other."),
        tags$li("Check 'Color by 3rd variable' for an additional dropdown menu. A new variable will be shown on the 
                plot as a color gradient."),
             
        h5(strong("Distribution tab:")),
        tags$li("Dropdown menu allows users to select a variable for additional summary statistics."),
        tags$li("Time frame options allows users to chose a summarized time frame to show in the summary statistics and
                density plot."), 
        tags$li("Options include daily, monthly, yearly, and the default which is 30 minute intervals. Each option 
                filtered according to the selected date range."),
             
        h5(strong("Downloading plots and data:")),
        tags$li("Right click desired plot graphics and copy or save as image."),
        tags$li("Download data button at the bottom of the ‘Raw Data’ tab."),
             
        h5(strong("Upload new data:")),
        tags$li("Should the user want to upload a new dataset, contact Dr. JP Gannon."),
             
        h5(strong("* Brushing functionality:")),
        tags$li("Click and drag until the frame covers all of the section of data you want to visualize/zoom into."),
        tags$li("Double click inside the frame you created to zoom into the area."),
        tags$li("Double click anywhere on the graph to reset back to the original date range."),
             
        tags$hr(),
        h6("Maya Bollino, Diego Ignacio, Paxton Jolly, Robert Creamer"),
        h6("Virginia Tech 2022")),
    
    
    # plots tab panel --------------------------------------------------------------------
    
    tabPanel("Plots",
        sidebarLayout(position = "left",
            sidebarPanel(width = 4,
            
            # date range user input (conditional to not show up on distribution tab)
            conditionalPanel(condition = "input.tabs !== 'c'",
                dateRangeInput("dates", "Date range:",
                               start = "2016-08-25", end = "2021-11-22",
                               separator = " to ", startview = "year")),
            
            
            # timeseries tab inputs ------------------------------------------------------
            
            conditionalPanel(condition = "input.tabs == 'a'",
                             
                helpText("Brushing dates: click and drag to create frame, double click 
                          selected frame to zoom in, double click to zoom out."),
                
                pickerInput("y", label = "Y variable:",
                            choices = vargroup, multiple = TRUE,
                            options = list(`selected-text-format` = "count")),
                helpText("Important: Only select variables with matching units."),
                
                numericInput("hline", "Add Y value line:", value = NULL),
                
                radioButtons("plotType", "Choose plot type:", c("Line", "Dot")),
                
                conditionalPanel(condition = "input.checkbox == 1",
                    pickerInput("y2", label = "Y variable (Plot 2):",
                                choices = vargroup, multiple = TRUE,
                                options = list(`selected-text-format` = "count")),
                    pickerInput("y3", label = "Y variable (Plot 3):", 
                                choices = vargroup, multiple = TRUE,
                                options = list(`selected-text-format` = "count")),
                    pickerInput("y4", label = "Y variable (Plot 4):", 
                                choices = vargroup, multiple = TRUE,
                                options = list(`selected-text-format` = "count"))),
                
                checkboxInput("checkbox", "Compare more than one plot"),
                
                actionButton("reset_date", "Reset Date"),
                actionButton("reset_t", "Reset Variables")),
            
            
            # bivariate tab inputs -------------------------------------------------------
            
            conditionalPanel(condition = "input.tabs == 'b'",
                             
                pickerInput("x", label = "X variable:", choices = vargroup),
                pickerInput("yb", label = "Y variable:", choices = vargroup),
                             
                conditionalPanel(condition = "input.checkbox_color == 1",
                    pickerInput("color", label = "Color by:", choices = vargroup)),
                             
                checkboxInput("checkbox_color", "Color by 3rd variable"),
                             
                actionButton("reset_date2", "Reset Date"),
                actionButton("reset_b", "Reset Variables")),
            
            
            # distribution tab inputs ----------------------------------------------------
            
            conditionalPanel(condition = "input.tabs == 'c'",
                             
                pickerInput("var", label = "Select variable:", choices = vargroup),
                             
                radioButtons("timeframe", "Choose time frame:", c("Default (30 min interval)" = 'D',
                                                                  "Daily", "Monthly", "Yearly")))
            
            ), # close sidebarPanel
            
            
            # main panel for plot outputs ------------------------------------------------
            
            mainPanel(
                tabsetPanel(type = "tabs", id = "tabs",
                          
                    # timeseries tab plots (with brushing)
                    tabPanel("Timeseries", value = "a",
                              plotOutput("plot1", 
                                         dblclick = "plot_dblclick",
                                         brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
                        conditionalPanel(condition = "input.checkbox == 1",
                              plotOutput("plot2", 
                                         dblclick = "plot_dblclick",
                                         brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
                              plotOutput("plot3",
                                         dblclick = "plot_dblclick",
                                         brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)),
                              plotOutput("plot4",
                                         dblclick = "plot_dblclick",
                                         brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)))),
                          
                    # bivariate tab plots
                    tabPanel("Bivariate", value = "b",
                             plotOutput("bplot", height = 700)),
                          
                    # distribution tab plots
                    tabPanel("Distribution", value = "c",
                             plotOutput("cplot", height = 450),
                             verbatimTextOutput("summary"))
                          
            )), # close mainPanel
            
        )), # close tabPanel "Plots"
    
    
    # raw data tab panel -----------------------------------------------------------------
    
    tabPanel("Raw Data",
        tabsetPanel(type = "tabs", id = "tabs_data",
                         
            tabPanel("Original",
                     dataTableOutput("rawdata"),
                     downloadButton("download", "Download Data")),
                         
            tabPanel("Daily",
                     dataTableOutput("daily"),
                     downloadButton("download2", "Download Data")),
                         
            tabPanel("Monthly",
                     dataTableOutput("monthly"),
                     downloadButton("download3", "Download Data")),
                         
            tabPanel("Yearly",
                     dataTableOutput("yearly"),
                     downloadButton("download4", "Download Data"))
                         
        )) # close tabPanel "Raw Data"
    
  ) # close navbarPage
  
)


# server ===================================================================================================================

server <- function(input, output, session) {
  
  
# reset input buttons --------------------------------------------------------------------
  
  # on timeseries tab (reset dates and variable selection)
  
  observeEvent(input$reset_date, { 
    updateDateRangeInput(session, "dates", start = "2016-08-25", end = "2021-11-22") })
  
  observeEvent(input$reset_t, {
    updatePickerInput(session, "y", selected = "")
    updatePickerInput(session, "y2", selected = "")
    updatePickerInput(session, "y3", selected = "")
    updatePickerInput(session, "y4", selected = "") })
  
  # on bivariate tab (reset dates and variable selection)
  
  observeEvent(input$reset_date2, { 
    updateDateRangeInput(session, "dates", start = "2016-08-25", end = "2021-11-22") })
  
  observeEvent(input$reset_b, {
    updatePickerInput(session, "x", selected = "CO2 Turbulent Flux (umolCO2 m-2 s-1)")
    updatePickerInput(session, "yb", selected = "CO2 Turbulent Flux (umolCO2 m-2 s-1)")
    updatePickerInput(session, "color", selected = "CO2 Turbulent Flux (umolCO2 m-2 s-1)") })
  

# brushing -------------------------------------------------------------------------------
  
  # set date range for brushing based on date range user inputs
  ranges <- reactiveValues(x = mdy(c(start = "08-25-2016", end = "11-22-2021")))
  observeEvent(input$dates, { ranges$x <- c(input$dates[1], input$dates[2]) })
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) { 
      ranges$x <- c(brush$xmin, brush$xmax) 
    } else { 
      ranges$x <- c(input$dates[1], input$dates[2]) 
    }
    
    # update date range inputs to show new brushed dates 
    updateDateRangeInput(session, "dates", start = as.POSIXct(ranges$x[1], origin = "1970-01-01"), 
                         end = as.POSIXct(ranges$x[2], origin = "1970-01-01"))
  })
  
  
# timeseries plots -----------------------------------------------------------------------
  
  # line or point graphs (radio button input options)
  observeEvent(input$plotType, {
    
    if (input$plotType == "Line") { 
      
      # PLOT 1
      output$plot1 <- renderPlot({
        
        # relabeling axis by units in column label parenthesis 
        getunit <- reactive(input$y[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        
        # filter by selected date range 
        varselect <- reactive(filter(fluxlong, variable %in% input$y, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        # if else statement for hline input (only for the first plot)
        if (!isTruthy(input$hline)) {
          ggplot(varselect(), aes(datetime, y = values, color = variable))+
            geom_line()+
            labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
            scale_color_brewer(palette = "Set1")+
            theme_light()+
            theme(text = element_text(size = 15),
                  legend.title = element_blank(),
                  legend.position = "top",
                  legend.direction = "vertical",
                  legend.margin = margin(t = 10, b = 0),
                  legend.box.margin = margin(b = -10),
                  axis.text.y = element_text(angle = 90))
        } 
        else {
          ggplot(varselect(), aes(datetime, y = values, color = variable))+
            geom_line()+
            geom_hline(yintercept = input$hline)+
            labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
            scale_color_brewer(palette = "Set1")+
            theme_light()+
            theme(text = element_text(size = 15),
                  legend.title = element_blank(),
                  legend.position = "top",
                  legend.direction = "vertical",
                  legend.margin = margin(t = 10, b = 0),
                  legend.box.margin = margin(b = -10),
                  axis.text.y = element_text(angle = 90))
        }
      })
      
      # repeat ggplot code for plots 2-4
      
      # PLOT 2
      output$plot2 <- renderPlot({
        
        getunit <- reactive(input$y2[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y2, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_line()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
      
      # PLOT 3
      output$plot3 <- renderPlot({
        
        getunit <- reactive(input$y3[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y3, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_line()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
      
      # PLOT 4
      output$plot4 <- renderPlot({
        
        getunit <- reactive(input$y4[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y4, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_line()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
    }
    
    else { # point graph option selected (same code as above 4 plots, only change to geom_point)
      
      # PLOT 1
      output$plot1 <- renderPlot({
        
        getunit <- reactive(input$y[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        if (!isTruthy(input$hline)) {
          ggplot(varselect(), aes(datetime, y = values, color = variable))+
            geom_point()+
            labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
            scale_color_brewer(palette = "Set1")+
            theme_light()+
            theme(text = element_text(size = 15),
                  legend.title = element_blank(),
                  legend.position = "top",
                  legend.direction = "vertical",
                  legend.margin = margin(t = 10, b = 0),
                  legend.box.margin = margin(b = -10),
                  axis.text.y = element_text(angle = 90))
        } 
        else {
          ggplot(varselect(), aes(datetime, y = values, color = variable))+
            geom_point()+
            geom_hline(yintercept = input$hline)+
            labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
            scale_color_brewer(palette = "Set1")+
            theme_light()+
            theme(text = element_text(size = 15),
                  legend.title = element_blank(),
                  legend.position = "top",
                  legend.direction = "vertical",
                  legend.margin = margin(t = 10, b = 0),
                  legend.box.margin = margin(b = -10),
                  axis.text.y = element_text(angle = 90))
        }
      })
      
      # PLOT 2
      output$plot2 <- renderPlot({
        
        getunit <- reactive(input$y2[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y2, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_point()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
      
      # PLOT 3
      output$plot3 <- renderPlot({
        
        getunit <- reactive(input$y3[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y3, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_point()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
      
      # PLOT 4
      output$plot4 <- renderPlot({
        
        getunit <- reactive(input$y4[1])
        unit <- reactive(sub("\\).*", "", sub(".*\\(", "", getunit())))
        varselect <- reactive(filter(fluxlong, variable %in% input$y4, datetime >= ranges$x[1], datetime <= ranges$x[2]))
        
        ggplot(varselect(), aes(datetime, y = values, color = variable))+
          geom_point()+
          labs(x = NULL, y = paste("Selected Y variables (", unit(), ")"))+
          scale_color_brewer(palette = "Set1")+
          theme_light()+
          theme(text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                legend.direction = "vertical",
                legend.margin = margin(t = 10, b = 0),
                legend.box.margin = margin(b = -10),
                legend.text = element_text(size = 12),
                axis.text.y = element_text(angle = 90))
      })
    }
  })
  
  
# bivariate plots ------------------------------------------------------------------------
  
  output$bplot <- renderPlot({
    
    # with color input
    if (input$checkbox_color == 1) { 
      
      flux %>% 
        filter(datetime >= ranges$x[1] & datetime <= ranges$x[2]) %>%
        ggplot(aes(x = get(input$x), y = get(input$yb), color = get(input$color)))+
        geom_point(alpha = 0.3)+
        labs(x = input$x, y = input$yb, color = input$color)+
        scale_color_viridis(option = "D")+
        theme_light()+
        theme(text = element_text(size = 15),
              legend.position = "bottom")
    } 
    
    # without color input
    else { 
      
      flux %>%
        filter(datetime >= ranges$x[1] & datetime <= ranges$x[2]) %>%
        ggplot(aes(x = get(input$x), y = get(input$yb)))+
        geom_point(alpha = 0.3)+
        labs(x = input$x, y = input$yb)+
        theme_light()+
        theme(text = element_text(size = 15))
    }
  })
  

# distribution outputs -------------------------------------------------------------------
  
  observeEvent(input$timeframe, {
    
    # default timeframe option selected 
    if (input$timeframe == "D") {
      output$cplot <- renderPlot({
        
        ggplot(flux, aes(x = get(input$var)))+
          geom_density()+
          labs(x = input$var)+
          theme_light()+
          theme(text = element_text(size = 15))
      })
      output$summary <- renderPrint({ summary(flux[input$var]) })
    }
    
    # daily timeframe option selected 
    else if (input$timeframe == "Daily") {
      output$cplot <- renderPlot({
        
        ggplot(dailyflux, aes(x = get(input$var)))+
          geom_density()+
          labs(x = input$var)+
          theme_light()+
          theme(text = element_text(size = 15))
      })
      output$summary <- renderPrint({ summary(dailyflux[input$var]) })
    }
    
    # monthly timeframe option selected  
    else if (input$timeframe == "Monthly") {
      output$cplot <- renderPlot({
        
        ggplot(monthlyflux, aes(x = get(input$var)))+
          geom_density()+
          labs(x = input$var)+
          theme_light()+
          theme(text = element_text(size = 15))
      })
      output$summary <- renderPrint({ summary(monthlyflux[input$var]) })
    }
    
    # yearly timeframe option selected
    else if (input$timeframe == "Yearly") {
      output$cplot <- renderPlot({
        
        ggplot(yearlyflux, aes(x = get(input$var)))+
          geom_density()+
          labs(x = input$var)+
          theme_light()+
          theme(text = element_text(size = 15))
      })
      output$summary <- renderPrint({ summary(yearlyflux[input$var]) })
    }
    
  })
  
  
# raw data page --------------------------------------------------------------------------
  
  fluxdata <- reactive(flux %>% filter(datetime >= ranges$x[1] & datetime <= ranges$x[2]))
  
  output$rawdata <- renderDataTable(
    fluxdata(), server = TRUE,
    options = list(scrollX = TRUE,
                   scrollY = 480,
                   deferRender = TRUE,
                   scroller = TRUE,
                   pageLength = 1000))
  
  output$download <- downloadHandler(
    filename = function() { paste("fluxdata_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "") },
    content = function(file) { write.csv(fluxdata(), file) })
  
  # daily timeframe data tab
  output$daily <- renderDataTable(
    dailyflux, server = TRUE,
    options = list(scrollX = TRUE,
                   scrollY = 480,
                   deferRender = TRUE,
                   scroller = TRUE,
                   pageLength = 1000))
  
  output$download2 <- downloadHandler(
    filename = function() { paste("daily_fluxdata_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "") },
    content = function(file) { write.csv(dailyflux, file) })
  
  # monthly timeframe data tab
  output$monthly <- renderDataTable(
    monthlyflux, server = TRUE,
    options = list(scrollX = TRUE,
                   scrollY = 480,
                   deferRender = TRUE,
                   scroller = TRUE,
                   pageLength = 100))
  
  output$download3 <- downloadHandler(
    filename = function() { paste("monthly_fluxdata_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "") },
    content = function(file) { write.csv(monthlyflux, file) })
  
  # yearly timeframe data tab
  output$yearly <- renderDataTable(
    yearlyflux, server = TRUE,
    options = list(scrollX = TRUE,
                   deferRender = TRUE,
                   scroller = TRUE,
                   pageLength = 10))
  
  output$download4 <- downloadHandler(
    filename = function() { paste("yearly_fluxdata_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "") },
    content = function(file) { write.csv(yearlyflux, file) })
  
}

# run app ===================================================================================================================

shinyApp(ui, server)

