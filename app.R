library(shiny)
library(terra)
library(leaflet)
library(ggplot2)
library(ncdf4)
library(dplyr)

# Upload Limit
options(shiny.maxRequestSize = 500 * 1024^2)

ui <- fluidPage(
  titlePanel("CORDEX NetCDF Time-Series Extractor"),
  h4("Interactive extraction of point time series from rotated climate model grids"),
  
  sidebarLayout(
    sidebarPanel(
      
      fileInput("ncfiles", "Choose NetCDF files", multiple = TRUE,
                accept = c(".nc")),
      uiOutput("varselect"),
      numericInput("input_lat", "Latitude:", value = NA, step = 0.01),
      numericInput("input_lon", "Longitude:", value = NA, step = 0.01),
      
      verbatimTextOutput("point_selected"),
      div(style = "font-size:13px;",
          radioButtons("method",
                       "Extraction Method:",
                       choices = c("Nearest Neighbor" = "nearest",
                                   "IDW Interpolation" = "idw"),
                       selected = "nearest")),
      
      actionButton("run", "Extract from Selected Files"),
      br(),
      downloadButton("downloadData", "Download CSV"),
      
      hr(),
      h5("File Metadata"),
      verbatimTextOutput("file_info"),
      
      hr(),
      p("Created by Aikaterini Lyra | klyra@uth.gr",
        style = "font-size:11px; font-style:italic; color:#111111;")
    ),
    
    mainPanel(
      leafletOutput("map", height = 300),
      plotOutput("tsplot"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session){
  
  selected_point <- reactiveVal(NULL)
  idw_points <- reactiveVal(NULL)
  
  # ---- File Info ----
  output$file_info <- renderPrint({
    req(input$ncfiles)
    
    f <- input$ncfiles$datapath[1]
    nc <- nc_open(f)
    
    # Safe attribute getter
    get_att <- function(attname){
      val <- tryCatch(ncatt_get(nc, 0, attname)$value,
                      error = function(e) NA)
      if(is.null(val) || val == "") NA else val
    }
    
    # ---- Global Attributes ----
    institution  <- get_att("institution")
    title        <- get_att("title")
    driving_exp  <- get_att("driving_experiment")
    driving_mod  <- get_att("driving_model_id")
    ensemble     <- get_att("driving_model_ensemble_member")
    frequency    <- get_att("frequency")
    institute_id <- get_att("institute_id")
    model_id     <- get_att("model_id")
    domain       <- get_att("CORDEX_domain")
    
    # ---- Grid Dimensions ----
    rlat_len <- if("rlat" %in% names(nc$dim)) nc$dim$rlat$len else NA
    rlon_len <- if("rlon" %in% names(nc$dim)) nc$dim$rlon$len else NA
    
    nc_close(nc)
    
    # ---- Print Clean Output ----
    cat("Institution:", institution, "\n")
    cat("Title:", title, "\n\n")
    
    cat("Driving Experiment:", driving_exp, "\n")
    cat("Driving Model ID:", driving_mod, "\n")
    cat("Ensemble Member:", ensemble, "\n\n")
    
    cat("Frequency:", frequency, "\n")
    cat("Institute ID:", institute_id, "\n")
    cat("Model ID:", model_id, "\n")
    cat("CORDEX Domain:", domain, "\n\n")
    
    cat("Grid Size (Rotated Coordinates):\n")
    cat("  rlat:", rlat_len, "\n")
    cat("  rlon:", rlon_len, "\n")
  })
  
  # ---- Variable Selection ----
  output$varselect <- renderUI({
    req(input$ncfiles)
    nc <- nc_open(input$ncfiles$datapath[1])
    vars <- setdiff(names(nc$var), c("lat","lon"))
    nc_close(nc)
    selectInput("varname", "Select Variable:", choices = vars)
  })
  
  # ---- Map Extent ----
  map_extent <- reactive({
    req(input$ncfiles)
    nc <- nc_open(input$ncfiles$datapath[1])
    lat_vals <- as.vector(ncvar_get(nc, "lat"))
    lon_vals <- as.vector(ncvar_get(nc, "lon"))
    nc_close(nc)
    
    list(
      lat_vals = lat_vals,
      lon_vals = lon_vals,
      lat_min = min(lat_vals),
      lat_max = max(lat_vals),
      lon_min = min(lon_vals),
      lon_max = max(lon_vals)
    )
  })
  
  # ---- Map Render ----
  output$map <- renderLeaflet({
    req(map_extent())
    m <- map_extent()
    
    leaflet() %>%
      addTiles() %>%
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(metric = TRUE, imperial = FALSE)
      ) %>%
      addControl(
        html = "<div style='
                background: white;
                padding: 4px;
                font-size: 18px;
                font-weight: bold;
                text-align: center;
              '>↑<br>N</div>",
        position = "topright"
      ) %>%
      setView(lng = 24.5,
              lat = 39, zoom = 6)
  })
  
  # ---- Show Grid Points ----
  observe({
    req(map_extent())
    m <- map_extent()
    
    leafletProxy("map") %>%
      clearGroup("grid") %>%
      addCircleMarkers(
        lng = m$lon_vals,
        lat = m$lat_vals,
        radius = 2,
        color = "blue",
        fillOpacity = 0.4,
        group = "grid"
      )
  })
  
  # ---- Click Selection ----
  observeEvent(input$map_click, {
    click <- input$map_click
    selected_point(c(click$lng, click$lat))
    
    updateNumericInput(session, "input_lat", value = click$lat)
    updateNumericInput(session, "input_lon", value = click$lng)
    
    leafletProxy("map") %>%
      clearGroup("selected") %>%
      addMarkers(lng = click$lng,
                 lat = click$lat,
                 group = "selected")
  })
  
  # ---- Numeric Input Selection ----
  observeEvent({
    input$input_lat
    input$input_lon
  }, {
    req(!is.na(input$input_lat), !is.na(input$input_lon))
    
    selected_point(c(input$input_lon, input$input_lat))
    
    leafletProxy("map") %>%
      clearGroup("selected") %>%
      addMarkers(lng = input$input_lon,
                 lat = input$input_lat,
                 group = "selected")
  })
  
  output$point_selected <- renderPrint({
    req(selected_point())
    print(selected_point())
  })
  
  # ---- Extraction ----
  extracted_data <- eventReactive(input$run, {
    req(input$ncfiles, input$varname, selected_point())
    
    results <- list()
    
    for(i in seq_len(nrow(input$ncfiles))){
      
      nc <- nc_open(input$ncfiles$datapath[i])
      
      var_vals <- ncvar_get(nc, input$varname)
      lat_vals <- as.vector(ncvar_get(nc, "lat"))
      lon_vals <- as.vector(ncvar_get(nc, "lon"))
      time_var <- ncvar_get(nc, "time")
      
      time_units <- ncatt_get(nc, "time", "units")$value
      time_dates <- as.Date(time_var,
                            origin = sub(".*since ", "", time_units))
      
      nc_close(nc)
      
      dims <- dim(var_vals)
      var_mat <- matrix(var_vals,
                        nrow = dims[1]*dims[2],
                        ncol = dims[3])
      
      dist <- (lat_vals - selected_point()[2])^2 +
        (lon_vals - selected_point()[1])^2
      
      # ---- NEAREST ----
      if(input$method == "nearest"){
        
        idx <- which.min(dist)
        ts_vals <- var_mat[idx, ]
        
        idw_points(
          data.frame(lon = lon_vals[idx],
                     lat = lat_vals[idx])
        )
        
      } else {
        
        # ---- IDW (4 neighbors, power=2 fixed) ----
        neighbors <- order(dist)[1:4]
        d <- sqrt(dist[neighbors])
        d[d == 0] <- 1e-10
        
        weights <- 1 / (d^2)
        weights <- weights / sum(weights)
        
        ts_vals <- colSums(var_mat[neighbors, ] * weights)
        
        idw_points(
          data.frame(lon = lon_vals[neighbors],
                     lat = lat_vals[neighbors])
        )
      }
      
      period_label <- paste0(min(time_dates), " to ", max(time_dates))
      
      results[[i]] <- data.frame(
        Time = time_dates,
        FileLabel = period_label,
        Value = as.numeric(ts_vals)
      )
    }
    
    bind_rows(results)
  })
  
  # ---- Highlight Neighbors ----
  observe({
    pts <- idw_points()
    req(pts)
    
    leafletProxy("map") %>%
      clearGroup("idw") %>%
      addCircleMarkers(
        data = idw_points(),
        lng = ~lon,
        lat = ~lat,
        radius = 6,
        color = ifelse(input$method == "nearest", "green", "red"),
        fillOpacity = 0.8,
        group = "idw"
      )
  })
  
  # --- Plot with legend showing periods ---
  output$tsplot <- renderPlot({
    df <- extracted_data()
    validate(
      need(nrow(df) > 0, "No data to display")
    )
    ggplot(df, aes(x=Time, y=Value, color=FileLabel)) +
      geom_line() +
      theme_minimal() +
      labs(y=input$varname, color="Period") +
      theme(
        axis.text.x = element_text(size = 11),  # x-axis labels
        axis.text.y = element_text(size = 11),  # y-axis labels
        axis.title.x = element_text(size = 12, face = "bold"),  # x-axis title
        axis.title.y = element_text(size = 12, face = "bold"),  # y-axis title
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  # ---- Preview ----
  output$preview <- renderTable({
    df <- extracted_data()
    
    # Make sure df exists and has rows
    validate(
      need(!is.null(df) && nrow(df) > 0, "No data available. Select files and click 'Run'.")
    )
    
    head(df)
  })
  
  # ---- Download ----
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0("timeseries_", input$varname, ".csv")
    },
    content = function(file){
      write.csv(extracted_data(),
                file,
                row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
