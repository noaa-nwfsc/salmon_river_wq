# RShiny app for displaying empirically collected and modeled stream temperatures in the Salmon River basin, ID
# fashioned after apps for
# Wenatchee River WA: https://connect.fisheries.noaa.gov/wenatchee_stream_temperatures/
# Elwha River WA: https://connect.fisheries.noaa.gov/Elwha_River_temperatures/
# Salmon River ID: https://connect.fisheries.noaa.gov/SalmonRiverTemperatures/
# Skagit River WA: https://connect.fisheries.noaa.gov/Skagit_stream_temperatures/
# AH Fullerton, Jan 2026

# Load packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(maps)
library(sf)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(bslib)
library(data.table)
library(dygraphs)
library(xts)
library(DT)
library(nhdplusTools)
library(dplyr)
library(plotly)
library(waiter)

# UI Elements -------------------------------------------------------------------
# Sidebars ----
  # observed time series ----
filters_ts.obs <- sidebar(
  title = "Instructions",
  width = 320,
  span("Click on a site from the map to display its stream temperature. The slider controls limit what sites are displayed on the map.", 
       style = "font-size: 14px; color: gray"),
  # Choose water temperature or depth
  radioButtons(inputId = "variable_ts.obs", label = "Data to plot:",
              choices = c("water temperature" = "AvgDailyTemp", "depth" = "AvgWaterDepth"),
              selected = "AvgDailyTemp"),
  # Select based on samples in a set of years
  sliderInput(inputId = "year_slider_ts.obs",
              label = "Years when data are available:",
              min = 1993, max = 2023,
              value = c(1993, 2023),
              step = 1,
              sep = ""),
  # Select based on River km
  sliderInput(inputId = "rkm_slider_ts.obs",
              label = "Select sites by river kilometer:",
              min = 299, max = 627,
              value = c(299, 627),
              step = 10,
              sep = ""),
  span("Map: Circle size is proportional to the amount of data available at each site. Colors denote Major Population Groups. 
             Pan and zoom in the map to examine site locations. Then click on a site circle to produce plots of stream temperature.",
       style = "font-size: 14px; color: gray"),
  span("Timeseries plot (top right): the box at the origin controls the number of days for displaying a moving average.
             The range of dates displayed is controlled by the slide bar below the graph. You can also select a date range
             by drawing a rectangle in the plot window. Double-click to return to the full time series. Mousing over the plot identifies specific data.",
       style = "font-size: 14px; color: gray"),
  span("Boxplot (bottom right): annual summaries of temperatures for the selected site versus year.",
       style = "font-size: 14px; color: gray")
) # end sidebar   

  # observed metrics ----
  filters_met.obs <- sidebar(
    title = "Instructions",
    width = 370,
    span("Thermal metrics are summaries of empirical stream temperature  that summarize thermal conditions 
        experienced by Chinook salmon at a monitored location over time periods associated with different life stages.", 
        br(), br(), "Select filters from the menu to investigate thermal conditions over time and distance.", 
        style = "font-size: 14px; color: gray"),
    selectInput(inputId = "lifehist.met.obs", label = "Strategy/Stage:",
                choices = c(
                  "Adults Holding in Tributary" = "AHT",
                  "Adults Spawning in Tributary" = "AST",
                  "Eggs Incubating in Tributary" = "EIT",
                  "Fall-migrant Parr Rearing in Tributary" = "PRFT",
                  "Winter-migrant Parr Rearing in Tributary" = "PRWT",
                  "Spring-migrant Parr Rearing in Tributary" = "PRST"
                ),
                selected = "EIT"),
    selectInput(inputId = "metric.met.obs", label = "Thermal metric:",
                choices = c(
                  "Mean temperature" = "temp_mean",
                  "SD of temperature" = "temp_sd",
                  "Variance of temperature" = "temp_var",
                  "Range of temperature" = "temp_range",
                  "5th Percentile of temperature" = "q05",
                  "25th Percentile of temperature" = "q25",
                  "50th Percentile of temperature" = "q50",
                  "75th Percentile of temperature" = "q75",
                  "95th Percentile of temperature" = "q95",
                  "Day of maximum temperature" = "doy_max",
                  "Day of minimum temperature" = "doy_min",
                  "Days above a threshold value" = "days_above_thresh",
                  "Days below a threshold value" = "days_below_thresh",
                  "Degree days" = "degree_days",
                  "Cumulative heat" = "cumulative_heat",
                  "Days within suitable range" = "days_in_range",
                  "Date first exceeded" = "exceed_1st_doy",
                  "Max consecutive days above threshold" = "max_consec_above",
                  "Median consecutive days above threshold" = "median_consec_above",
                  "Number of heat events (above threshold)" = "n_heat_events",
                  "Max daily increase in temperature" = "max_daily_increase",
                  "Minimum weekly temperature" = "min_7d",
                  "Mean weekly temperature" = "mean_7d",
                  "Maximum weekly temperature" = "max_7d",
                  "Mean of weekly average minimum" = "mean_7d_min",
                  "Mean of weekly average maximum" = "mean_7d_max",
                  "Min of weekly average mean" = "min_7d_mean",
                  "Max of weekly average mean" = "max_7d_mean",
                  "Median weekly range" = "median_weekly_range"
                ),
                selected = "mean_7d"),
    selectInput(inputId = "site.met.obs", label = "Highlight a site on the bottom plot:",
                choices = c(NA, sort(unique(sites$SiteCode[sites$hasdata > 365*3]))), 
                selected = NA),
                            
    span("Symbols and whiskers display medians and 10th/90th percentiles across sites and years. 
         Symbol size reflects the number of sites (top panel) or years (bottom panel) over which data were available.", 
          style = "font-size:14px; color: gray"),
    span("Metrics were only calculated when data had gaps of no more than 20% for the selected period. Because data are downloaded during
         summer, the most recent year does not yet have enough data for a calculation. Results may be driven by just a 
         few locations or dates, which may influence interpretation of patterns.", 
         style = "font-size: 14px; color: gray"),
    span("Life stages are defined under the plots.", style = "font-size: 14px; color: gray"),
    downloadButton('download.met.obs', "Download summarized data")
  ) #end sidebar
  

# Cards ----
  # landing page ----
  photo_list <- list(
    list(file = "CHA.jpg", label = "Chamberlain Creek"),
    list(file = "BIU.jpg", label = "Upper Big Creek"),
    list(file = "BVC.jpg", label = "Bear Valley Creek"),
    list(file = "CAM.jpg", label = "Camas Creek"),
    list(file = "CHC.jpg", label = "Cape Horn Creek"),
    list(file = "HER.jpg", label = "Herd Creek"),
    list(file = "LAK.jpg", label = "Lake Creek"),
    list(file = "LOO.jpg", label = "Loon Creek"),
    list(file = "SEC.jpg", label = "Secesh River"),
    list(file = "SFS.jpg", label = "South Fork salmon River"),
    list(file = "SUL.jpg", label = "Sulphur Creek"),
    list(file = "VAL.jpg", label = "Valley Creek"),
    list(file = "WFC.jpg", label = "W.F. Chamberlain Creek"),
    list(file = "logger1.jpg", label = "Temperature sensor"),
    list(file = "logger2.jpg", label = "Temperature sensor")
  )
    cards_landing <- list(
      card(
        img(src = "nmfs.png", width = "150px"), a(href="https://www.fisheries.noaa.gov/region/west-coast/northwest-science", "Northwest Fisheries Science Center", style = "padding:8px"),
        
      span("This application provides an interactive interface for users to explore water quality data collected across the Salmon
      River basin. The motivation was to characterize thermal habitats for Pacific salmon. Data are provisional and may change
           as new information accrues.",
           style = "font-size: 14px"),
      span(
        "The tabs at the top allow interaction with the data in different ways. 
          The 'Sites monitored' tab shows information about each site where water quality data has been monitored.
          On the 'Observed data' tab, users can select an individual monitoring site from the map to see time series of empirical data.
          The 'Thermal metrics' tab dispays observed stream temperature versus time and river kilometer, 
          summarized as thermal metrics for different life histories and life stages of Chinook salmon, or as monthly summaries over all reaches.",
        style = "font-size: 14px"),
      span("Empirical observations of water quality were collected by NOAA's Northwest Fisheries Science Center
             through the BPA-funded project 1991-028-00 'Monitoring the Migrations of Wild Snake River Spring/Summer Chinook Salmon Smolts'.
             Data from 1993-1997 were provided by the Pacific Northwest National Laboratory.", 
           style = "font-size: 14px"),
      span("Please cite as: J.J. Lamb. and A.H. Fullerton. 2026. Water Quality Dataset for headwater streams in the Salmon River basin, Idaho, USA. https://connect.fisheries.noaa.gov/Salmon_River_WaterQuality.",
           style = "font-size: 14px"),
#      span("Summaries of modeled stream temperatures are provided ", tags$a(href="https://connect.fisheries.noaa.gov/Salmon_River_Temperatures", "here"), ".", 
#           style = "font-size: 14px")
    ),
    
      card(
        card_header("Sensor Locations"),
        # Use do.call here to ensure each element is treated as a separate column
        do.call(layout_columns, c(
          lapply(photo_list, function(photo) {
            tags$div(
              style = "text-align: center; padding: 5px;",
              # Ensure style='width:100%' is present so they don't vanish or overlap
              img(src = photo$file, style = "width: 100%; height: auto; display: block;"),
              tags$i(span(photo$label, style = "font-size: 14px"))
            )
          }),
          # This list() gets appended to the arguments passed to layout_columns
          list(col_widths = c(3, 3, 3, 3)) 
        ))
      )
    )
    
  # site descriptions ----
    cards_sites <- list(
      card(
        full_screen = TRUE, height = "870px", 
        card_header(
        "Sites where water temperature has been monitored, and data availability by year. Use the boxes under column
        headers to filter the data.", style = "font-size:14px"
        ),
        DTOutput("sites_table")
      )
    )
    
  # observed time series ----
    cards_ts.obs <- list(
      card(
        height = "600px",
        full_screen = TRUE,
         card_header("Locations of monitored sites"),
        leafletOutput("map_ts.obs")
      ),
      card(
        full_screen = TRUE,
        height = "600px",
        card_header("Stream temperatures at selected site over time"),
        dygraphOutput("dyplot_ts.obs"),
        hr(),
        plotlyOutput("plot.ts.obs"),
        card_footer(
          downloadButton('download.ts.obs', "Download daily data for the selected site")
        )
      )
    )

  # observed metrics ----
  cards_met.obs <- list(
    card(
      full_screen = TRUE,
      plotOutput("plot.year.met.obs", height = "300px")
    ),
    card(
      full_screen = TRUE,
      plotOutput("plot.rkm.met.obs", height = "300px")
    ),
    card(
      height = "360px",
       full_screen = TRUE,
       card_header("Definitions of life stages used in calculation of thermal metrics"),
       DTOutput("metric_defs.met.obs"),
       card_footer(
         span("'Year_begin' and 'Year_end' are the year bookends for describing the life stage, where 0 is the spawning year, 1 is the calendar 
         year after spawning, and 2 is the following calendar year. 'Month_Day_begin' and 'Month_Day_end' are analogous: these are the month and day at
         the beginning and end of the life stage. 'Pref_hi' and 'Pref_lo' (preferred range), and Thresh_hi' and 'Thresh_lo' (suitable range) are 'thresholds' 
         (degrees C) referred to in certain metric calculations. Lifestage-specific thresholds appear as dashed blue (lower) and red (upper) thresholds on some plots.", 
         style = "font-size:12px; color: gray")
       )
    )
  )
  
# Define UI --------------------------------------------------------------------
ui <- tagList(
  
  # splash screen
  useWaiter(),
  waiterShowOnLoad(spin_fading_circles()), # shows before anything else 

  # navigation controls
  navbarPage(
   title = "Salmon River water quality portal",
   theme = bs_theme(preset = "yeti"), #bootswatch_themes()
   #spacelab, flatly, litera, pulse, simplex, yeti, zephyr

# Tabs ----  
  # landing page ----
  tabPanel("About",
     fluid = TRUE,
     cards_landing[[1]],
     cards_landing[[2]]
  ), # End tab panel
  
  # site descriptions ----
  tabPanel("Sites monitored",
           fluid = TRUE,
          # Display sites table
          cards_sites[[1]]
          ), #end tab panel
  
  # observed time series ----
  tabPanel("Observed data", 
             fluid = TRUE,
           page_sidebar(
             sidebar = filters_ts.obs,
               layout_columns(
                 col_widths = c(4,8),
                 cards_ts.obs[[1]], # map
                 cards_ts.obs[[2]], # time series
                 NULL, NULL
                ), #end layout_columns
              ), # end page_sidebar
            ), # end tab panel

  # observed metrics ----
  tabPanel("Thermal metrics",
           fluid = TRUE,
           page_sidebar(
             sidebar = filters_met.obs,
               cards_met.obs[[1]],
               cards_met.obs[[2]],
               cards_met.obs[[3]]
            ) #end page sidebar
        ) #end tab panel
  ) # end navbar page
) # end tagList
  
# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  waiter_hide()
  
  # Reactive function to update the input source for plots
  updateSource <- reactive({
    return(input)
  })
  
# Tab for site descriptions ----
  td <- sites
    
    # show sites table
      output$sites_table <- renderDT({
        datatable(td,
        filter = list(position = 'top', clear = F),
        rownames= F,
        extensions = "Buttons",
        options = list(
          pageLength = 100, 
          search = list(regex = T, caseInsensitive = F),
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "csv",
              text = '<i class="fa fa-download"></i> Download filtered',
              exportOptions = list(
                modifier = list(
                  search = "applied"
                )
              )
            ),
            list(
              extend = "csv",
              text = '<i class="fa fa-download"></i> Download all',
              exportOptions = list(
                modifier = list(
                  search = NULL
                )
              )
            )
          )
        )
        )
      })
      
  
# Tab observed time series ----

  # Make leaflet markers reactive to input selecting the Section
  leaflet_marks.ts.obs <- reactive({
    # limit to years selected in slider input
    r1 <- input$rkm_slider_ts.obs[1]
    r2 <- input$rkm_slider_ts.obs[2]
    y1 <- input$year_slider_ts.obs[1]
    y2 <- input$year_slider_ts.obs[2]
    if(y1 == y2){yrs <- as.character(y1)} else {yrs <- as.character(seq(input$year_slider_ts.obs[1], input$year_slider_ts.obs[2]))}
    
    # filter sites:
    tbl_sites <- sites %>%
      # Filter for rows where at least one of the 'yrs' columns is > 0
      filter(if_any(all_of(yrs), ~ .x > 0)) %>%
      # Select specific columns plus the range/vector of 'yrs'
      select(SiteCode, Stream_Name, MPG, Latitude, Longitude, River_km, hasdata, all_of(yrs)) %>% 
      # Filter for river kilometers in range
      filter(River_km >= r1, River_km <= r2)
    })
 

    # Create the Static Base Leaflet Map (runs only once when the application loads)
    output$map_ts.obs <- renderLeaflet({
      
      # Use isolate() to get the first batch of data without creating 
      # a reactive trigger that redraws the whole map later
      initial_data <- isolate(leaflet_marks.ts.obs())
      
      # Define the color palette for the initial load
      cb_palette <- c("#009E73","#0072B2","#D55E00","#CC79A7")
      pal <- colorFactor(palette = cb_palette, domain = initial_data$MPG)
      
      leaflet(data = initial_data) %>%
        # Add the Basemap
        addProviderTiles(providers$OpenStreetMap) %>%
        # Set the initial view
        setView(lng = -114.9, lat = 44.9, zoom = 8) %>%
        # Add the static polygons and polylines
        addPolygons(data = watershed, weight = 4, col = 'lightblue') %>%
        addPolylines(data = streams, weight = 3, col = 'lightblue') %>%
        # Add the minimap for context
        addMiniMap(toggleDisplay = TRUE, position = "bottomleft") %>%
      
      # Add the INITIAL markers
      addCircleMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        radius = ~ sqrt(hasdata)/5,
        color = ~ pal(MPG),
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        fillOpacity = 0.5,
        popup = ~ paste0(SiteCode, ": ", Stream_Name),
        options = markerOptions(riseOnHover = TRUE)
      ) %>% 
        
        # Add the INITIAL legend
        leaflet::addLegend(
          position = "topright",
          pal = pal,
          values = ~ MPG,
          title = "Major Population Group",
          opacity = 1
        )
      
    })
    
    # Update the Dynamic Elements (runs whenever 'leaflet_marks.ts.obs()' changes)
    observe({
      # req() ensures the app doesn't crash if the data isn't loaded yet
      req(leaflet_marks.ts.obs()) 
      
      # Get the reactive data
      map_data <- leaflet_marks.ts.obs()
      
      # Define the color palette
      cb_palette <- c("#009E73","#0072B2","#D55E00","#CC79A7")
      pal <- colorFactor(palette = cb_palette, domain = map_data$MPG)
      
      # Target the existing map using leafletProxy
      leafletProxy("map_ts.obs", data = map_data) %>%
        
        # Clear old markers and controls so they don't stack on top of each other
        clearMarkers() %>% 
        clearControls() %>% 
        
        # Add the new, updated markers
        addCircleMarkers(
          lng = ~ Longitude,
          lat = ~ Latitude,
          radius = ~ sqrt(hasdata)/5,
          color = ~ pal(MPG),
          stroke = T,
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          popup = ~ paste0(SiteCode, ": ", Stream_Name),
          options = markerOptions(riseOnHover = T)
        ) %>% 
        
        # Add the new, updated legend
        leaflet::addLegend(
          position = "topright",
          pal = pal,
          values = ~ MPG,
          title = "Major Population Group",
          opacity = 1
        )
    })      
      
  # Create reactive object for leaflet data
  updateData.ts.obs <- reactive({
    # Ensure the data is not empty and avoid error message
    validate(
      need(input$map_ts.obs_marker_click$lat != "", "Please select a site from the map to get started.")
    )

    # Get lat and long from marker click
    lat <- input$map_ts.obs_marker_click$lat
    lng <- input$map_ts.obs_marker_click$lng
    
    # Get variable to plot
    vbl <- input$variable_ts.obs

    # Filter to selected site's records
    plotting_data <- obs_temps[obs_temps$Latitude %in% lat & obs_temps$Longitude %in% lng,]
    plotting_data <- plotting_data[,c("Date", vbl, "year", "SiteCode", "Stream_Name")]
    
    # ensure no gaps in time series
    start_date <- as.Date(min(plotting_data$Date, na.rm = T))
    end_date <- as.Date(max(plotting_data$Date, na.rm = T))
    full_dates <- as.data.frame(seq.Date(from = start_date, to = end_date, by = 1), drop = F); colnames(full_dates) <- "Date"
    plotting_data <- dplyr::left_join(full_dates, plotting_data, by = "Date")
    
    # for downloading daily data:
    sc <- unique(plotting_data$Stream_Name)
    daily <- daily_data[daily_data$Site.name %in% sc,]
    

    return(list(plotting_data, daily, sc))
  })
  
  output$dyplot_ts.obs <- renderDygraph({
    plotting_data <- updateData.ts.obs()[[1]]
    vbl <- input$variable_ts.obs
    ymax <- max(plotting_data[,vbl], na.rm = T)
    
    # Determine the Title based on input
    plot_title <- if (vbl == "AvgDailyTemp") {
      "Mean daily stream temperature (C)"
    } else {
      "Mean daily water depth (m)"
    }
    
    pd_xts <- xts::as.xts(plotting_data[,c("Date", vbl)])
    dy <- dygraph(pd_xts, x = "Date") %>% 
      dyOptions(connectSeparatedPoints = F) %>%
      dySeries(vbl, label = "Observed", color = "#267BB6") %>% 
      dyRoller(showRoller = T, rollPeriod = 1) %>%
      dyRangeSelector(height = 30, fill = "") %>%
      dyLegend(width = 165, labelsSeparateLines = T) %>%
      dyAxis("y", label = plot_title) %>%
      dyAxis("y", valueRange = c(0, (ymax + 3)))
    return(dy)
  })
  

  output$plot.ts.obs <- renderPlotly({
    plotting_data <- updateData.ts.obs()[[1]]
    vbl <- input$variable_ts.obs
    
    # Determine the Title based on input
    plot_title <- if (vbl == "AvgDailyTemp") {
      "Mean daily stream temperature (C)"
    } else {
      "Mean daily water depth (m)"
    }
    
    fig <- plotly::plot_ly(data = plotting_data, 
                   y = ~get(vbl), 
                   x = ~year,
                   type = 'box',
                   fillcolor = "#267BB6",
                   line = list(color = "#8EBAD9", width = 2)
                   ## Optional if use type = 'violin': Add an inner box plot and mean line for more detail
                   #box = list(visible = TRUE), meanline = list(visible = TRUE)
    ) %>%
      
      # Set a plot title and clean up the y-axis
      layout(title = "",
             yaxis = list(
               title = plot_title, 
               zeroline = FALSE,
               titlefont = list(size = 16),
               tickfont = list(size = 14)),
             xaxis = list(
               title = "Year",
               titlefont = list(size = 16),
               tickfont = list(size = 14))
             )
    
    return(fig)
  })
  
  # Download data for the site selected on the map
  output$download.ts.obs <- downloadHandler(
    filename = function(){paste0(updateData.ts.obs()[[3]], "_daily_data.csv")}, 
    content = function(fname){
      write.csv(updateData.ts.obs()[[2]], fname, row.names = FALSE)
    }
  )
  
  
# Tab with observed metrics ----

  # Make drop-down choice of sites to highlight match life history and life stage selected
  metric.site = reactive({
      lh <- input$lifehist.met.obs
      c(NA, sort(unique(metrics.obs$SiteCode[metrics.obs$LHS_Code %in% lh])))
  })

  observeEvent(input$lifehist.met.obs, {
    updateSelectInput(session, "site.met.obs", choices = metric.site())
  })
  observeEvent(input$lifestage.met.obs, {
    updateSelectInput(session, "site.met.obs", choices = metric.site())
  })

#####################################################################
  # Could make these metrics calculated on the fly... might be slow
#####################################################################

  updateData.met.obs.yr <- reactive({
    lh <- input$lifehist.met.obs
    met <- input$metric.met.obs

    mdat <- metrics.obs[metrics.obs$LHS_Code %in% lh, c("SiteCode", "Year", "River_km", met)]
    mdat <- mdat[!is.na(mdat[,met]),]

    if(nrow(mdat) > 0){
      dat <- mdat %>% group_by(Year) %>% summarise(q = list(quantile(.data[[met]],
             probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T))) %>% unnest_wider(q)
      colnames(dat) <- c("Year", "Min", "Q10", "Q25", "Q50", "Q75", "Q90", "Max")
      setDT(mdat)
      # Count unique sites per year
      sites.per.year <- mdat[, .(NoSites = uniqueN(SiteCode)), by = Year]
      dat <- dplyr::left_join(dat, sites.per.year, by = "Year")
      dat <- as.data.frame(dat)
      mdat <- as.data.frame(mdat)

      # check for missing data
      if(nrow(dat) == 0) {dat <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(dat))); colnames(dat) = colnames(dat)}

      return(list(dat, mdat))
    }

  })

  updateData.met.obs.km <- reactive({
    lh <- input$lifehist.met.obs
    met <- input$metric.met.obs

    mdat <- metrics.obs[metrics.obs$LHS_Code %in% lh, c("SiteCode", "Year", "River_km", met)]
    mdat <- mdat[!is.na(mdat[,met]),]
    
    if(nrow(mdat) > 0){
      dat <- mdat %>% group_by(SiteCode) %>% summarise(q = list(quantile(.data[[met]],
             probs = c(0,0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = T))) %>% unnest_wider(q)
      colnames(dat) <- c("SiteCode", "Min", "Q10", "Q25", "Q50", "Q75", "Q90", "Max")
      setDT(mdat)
      # Count unique years per SiteCode
      years.per.site <- mdat[, .(NoYears = uniqueN(Year)), by = SiteCode]
      dat <- dplyr::left_join(dat, years.per.site, by = "SiteCode")
      dat <- as.data.frame(dat)
      mdat <- as.data.frame(mdat)

      # get necessary columns and rearrange
        dat <- dplyr::left_join(dat, sites[, c("SiteCode", "River_km")], by = "SiteCode")
        dat <- dat[,c(colnames(dat)[1:8], "River_km", "NoYears")] #rename as function expects

      # check for missing data
      if(nrow(dat) == 0) {dat <- as.data.frame(matrix(0, nrow = 1, ncol = ncol(dat))); colnames(dat) = colnames(dat)}

      return(list(dat, mdat))
    }

  })

  updatePlot.met.obs.yr <- reactive({
    lh <- input$lifehist.met.obs
    met <- input$metric.met.obs

    df <- updateData.met.obs.yr()[[1]]

    if(!is.null(df)){
      fncPlotData(dat = df, lh = lh, xvar = "Year", mn.lab = lh, ylb = fncMetricName(met), xlb = "Year", met = met)
    } else{
      plot.new()
      text(x = 0.5, y = 0.5, cex = 1.2, labels = "No data available for the current selection.")
    }
    recordPlot()
  })

  updatePlot.met.obs.km <- reactive({
    lh <- input$lifehist.met.obs
    met <- input$metric.met.obs
    si <- input$site.met.obs

    df <- updateData.met.obs.km()[[1]]

    if(!is.null(df)){
      fncPlotData(dat = df, lh = lh, xvar = "River_km", si = si, mn.lab = lh, ylb = fncMetricName(met), xlb = "River kilometer", met = met)
    } else{
      plot.new()
      text(x = 0.5, y = 0.5, cex = 1.2, labels = "No data available for the current selection.")
    }
    recordPlot()
  })

  output$plot.year.met.obs <- renderPlot({
    updatePlot.met.obs.yr()
  }) %>% bindCache(input$lifehist.met.obs, input$metric.met.obs, input$site.met.obs)

  output$plot.rkm.met.obs <- renderPlot({
    updatePlot.met.obs.km()
  }) %>% bindCache(input$lifehist.met.obs, input$metric.met.obs, input$site.met.obs)

  output$metric_defs.met.obs <- renderDT({
    lh_codes <- unique(lifestages$LHS_Code)[-grep("^S", unique(lifestages$LHS_Code))]
    table_data <- lifestages[lifestages$LHS_Code %in% lh_codes,]
    datatable(table_data,
    options = list(dom = 't'), rownames= F)
  })

  output$download.met.obs <- downloadHandler(
    filename = function(){paste0(input$lifehist.met.obs, "_", input$metric.met.obs, ".csv")},
    content = function(fname){
      write.csv(updateData.met.obs.yr()[[2]], fname, row.names = FALSE)
    }
  )


}
# Create a Shiny app object ----------------------------------------------------
shinyApp(ui = ui, server = server)
