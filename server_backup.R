library(shiny)
# library(shinythemes)

library(tidyverse)
# library(devtools)
library(kableExtra)

#devtools::install_github("javierfajnolla/gcmcompareR")
#library(gcmcompareR)

library(raster)

# library(shinyjs)
# library(shinycssloaders)
# library(shinysky)
library(plotly)
library(leaflet)
library(leaflet.extras)

# Maps
library(sp)
library(sf)
library(rgdal)
library(fasterize)
library(maps)
library(maptools)
# print(getwd())
world <- maps::map("world", fill = TRUE, plot = FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country = names(world_map), 
                                                 stringsAsFactors = FALSE), 
                                      FALSE)
world_sf <- st_as_sf(world_map)
biomes <- read_sf("data/biomes.shp")
biomes_sp <- readOGR("data", "biomes")
ecorregions <- read_sf("data/Ecoregions2017.shp")
ecorregions_sp <- readOGR("data", "Ecoregions2017")


# Available GCMs
table_GCMs <- read_csv("data/GCMs_details.csv") %>% 
  dplyr::select(-"Actual name")

##############################################################################################################################################################################
########################################    SERVER   ######################################################################################################################
##############################################################################################################################################################################


# initialize module parameters list
rvs <- reactiveValues(ext_user = extent(-180, 180, -60, 90),
                      still_no_analyse = NULL,
                      available_gcms = NULL)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  glue::glue("#   primer mensaje") %>% message()
  
  # ##################################################
  # #### Prepare input values and hide options
  # ##################################################
  # ### Asure that hidden input provide a valid initial value
  output$sel_gcms <- eventReactive(input$sel_gcms, TRUE, ignoreInit = TRUE)
  output$bio_vars <- eventReactive(input$bio_vars, TRUE, ignoreInit = TRUE)
  output$selection_gcms <- eventReactive(input$selection_gcms, TRUE, ignoreInit = TRUE)
  output$res_sel <- eventReactive(input$res_sel, TRUE, ignoreInit = TRUE)
  output$year_type <- eventReactive(input$year_type, TRUE, ignoreInit = TRUE)
  output$rcp_type <- eventReactive(input$rcp_type, TRUE, ignoreInit = TRUE)
  output$compare_type <- eventReactive(input$compare_type, TRUE, ignoreInit = TRUE)
  output$compare_type_bio_bio <- eventReactive(input$compare_type_bio_bio, TRUE, ignoreInit = TRUE)
  output$compare_type_bio_several <- eventReactive(input$compare_type_bio_several, TRUE, ignoreInit = TRUE)
  output$gcm_to_plot <- eventReactive(input$gcm_to_plot, TRUE, ignoreInit = TRUE)
  output$bio_to_plot <- eventReactive(input$bio_to_plot, TRUE, ignoreInit = TRUE)
  output$map_pre_delta_gcm <- eventReactive(input$map_pre_delta_gcm, TRUE, ignoreInit = TRUE)
  output$map_pre_delta_bio <- eventReactive(input$map_pre_delta_bio, TRUE, ignoreInit = TRUE)
  # output$type_scaled <- eventReactive(input$type_scaled, TRUE, ignoreInit = TRUE)
  
  # The key is suspendWhenHidden = FALSE
  outputOptions(output, "sel_gcms", suspendWhenHidden = FALSE)
  outputOptions(output, "bio_vars", suspendWhenHidden = FALSE)
  outputOptions(output, "selection_gcms", suspendWhenHidden = FALSE)
  outputOptions(output, "res_sel", suspendWhenHidden = FALSE)
  outputOptions(output, "year_type", suspendWhenHidden = FALSE)
  outputOptions(output, "rcp_type", suspendWhenHidden = FALSE)
  outputOptions(output, "compare_type", suspendWhenHidden = FALSE)
  outputOptions(output, "compare_type_bio_bio", suspendWhenHidden = FALSE)
  outputOptions(output, "compare_type_bio_several", suspendWhenHidden = FALSE)
  outputOptions(output, "bio_to_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "gcm_to_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "map_pre_delta_gcm", suspendWhenHidden = FALSE)
  outputOptions(output, "map_pre_delta_bio", suspendWhenHidden = FALSE)
  # outputOptions(output, "type_scaled", suspendWhenHidden = FALSE)
  
  
  # Disable results tab until input$go is used
  observeEvent(input$go == 1,{
    
    if(input$go == 0){    #If true enable, else disable
      js$disabletab("abc")
    }else{
      js$enabletab("abc")
    }
    
  })
  
  ### Remove old saved results if GO! is pressed again, so these results are not included in 
  ### the report
  observeEvent(input$go, {
    rvs$saved_bbox <- NULL
    rvs$plot_comp_download_unscaled <- NULL
    rvs$plot_comp_download_deltas <- NULL
    rvs$plot_comp_download_scaled <- NULL
    rvs$plot_temp_prec_realunscaled <- NULL
    rvs$plot_gcm_pattern <- NULL
    rvs$plot_delta_pattern <- NULL
    rvs$plot_gcm_differences <- NULL
  })
  
  
  
  # Table of Available GCMs and data
  output$GCMs_table <- function(){
    table_GCMs %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F)
  }
  
  
  ##################################################
  #### Scenario selection tab
  ##################################################
  
  ########### CODE FOR UI ###########
  
  ### Available GCMs - dynamically change
  # For "scenario - scenario_map" tab  (definition scenario) - MAIN OPTIONS SELECTION
  observe({
    output$selection_gcms <- renderUI({
      glue::glue("# Getting list of available GCMs") %>% message
      scenarios <- list.files(paste0("clim_data/ccafs/rds"), pattern = input$res_sel) %>%
        dplyr::as_tibble() %>%
        magrittr::set_names("GCM") %>%
        tidyr::separate(col = GCM, into = c("gcm_", "resolution_", "year_", "rcp_", "borrar"), sep = "\\.") %>% dplyr::select(-borrar) %>%
        mutate(year_ = year_ %>% str_replace_all("year", ""),
               rcp_ = rcp_ %>% str_replace_all("rcp", ""))
      
      glue::glue("#   - Available GCMs with current setting") %>% message
      available_gcms <- scenarios %>%
        filter(resolution_ == input$res_sel) %>%
        filter(year_ == input$year_type) %>%
        filter(rcp_ == input$rcp_type %>% str_replace("rcp", "")) %>%
        pull(gcm_)
      
      glue::glue("#   - Generating GCMs options") %>% message
      checkboxGroupInput(inputId = "sel_gcms", "",
                         inline = FALSE,
                         choices  = available_gcms,
                         selected = available_gcms)
    })
  })
  
  
  ### SELECTED OPTIONS
  observe({
    n_gcms <- length(input$sel_gcms)
    output$selected_gcms <- renderText(
      glue::glue("{n_gcms} GCMs") %>% print()
    )
    print_rcp <- case_when(input$rcp_type  == "rcp26" ~ "2.6",
                           input$rcp_type == "rcp45" ~ "4.5",
                           input$rcp_type == "rcp60" ~ "6.0",
                           input$rcp_type == "rcp85" ~ "8.5")
    output$selected_scenario <- renderText(
      glue::glue("Year {input$year_type}, RCP {print_rcp}, resolution: {input$res_sel}") %>% print()
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("1 bioclim X 1 bioclim"),
                                          input$compare_type == "bio_several" ~ "Multiple comparison")
    output$selected_comparison <- renderText(
      glue::glue("{type_selected_comparison}") %>% print()
    )
  })
  
  
  # Hide unselection of GCMs
  observeEvent(input$GCMs_button, {
    if (input$GCMs_button %% 2 == 1){
      shinyjs::hide(id = "selected_gcms", anim = T)
      shinyjs::show(id = "selection_gcms", anim = T)
    } else {
      shinyjs::show(id = "selected_gcms", anim = T)
      shinyjs::hide(id = "selection_gcms", anim = T)
    }
  })
  
  # Hide Climate Change Scenario
  observeEvent(input$CC_scenario, {
    if (input$CC_scenario %% 2 == 1){
      shinyjs::show(id = "year_type", anim = T)
      shinyjs::show(id = "rcp_type", anim = T)
      shinyjs::show(id = "res_sel", anim = T)
      shinyjs::hide(id = "selected_scenario", anim = T)
    } else {
      shinyjs::hide(id = "year_type", anim = T)
      shinyjs::hide(id = "rcp_type", anim = T)
      shinyjs::hide(id = "res_sel", anim = T)
      shinyjs::show(id = "selected_scenario", anim = T)
    }
  })
  
  # Hide Type of comparison   
  observeEvent(input$Compar_type, {
    if (input$Compar_type %% 2 == 1){
      shinyjs::show(id = "compare_type", anim = T)
      shinyjs::show(id = "compare_type_bio_bio", anim = T)
      shinyjs::show(id = "compare_type_bio_several", anim = T)
      shinyjs::hide(id = "selected_comparison", anim = T)
    } else {
      shinyjs::hide(id = "compare_type", anim = T)
      shinyjs::hide(id = "compare_type_bio_bio", anim = T)
      shinyjs::hide(id = "compare_type_bio_several", anim = T)
      shinyjs::show(id = "selected_comparison", anim = T)
    }
  })
  
  ### Disable download buttons at the beggining
  shinyjs::disable("download_prec_temp")
  observeEvent(input$go, {
    shinyjs::enable("download_prec_temp")
  })
  
  # shinyjs::disable("download_comparison_table")
  # observeEvent(input$go, {
  #   shinyjs::enable("download_comparison_table")
  # })
  # 
  
  ### Bioclims to compare
  # One to One comparison
  output$compare_type_bio_bio <- renderUI({
    conditionalPanel(condition = "input.compare_type == 'bio_bio'",
                     selectInput(inputId = "bio_bio_x", 
                                 label = "X axis",
                                 choices = c("(bio 1) Annual Mean Temperature",
                                             "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                                             "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                                             "(bio 4) Temperature Seasonality (standard deviation *100)",
                                             "(bio 5) Max Temperature of Warmest Month",
                                             "(bio 6) Min Temperature of Coldest Month",
                                             "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                                             "(bio 8) Mean Temperature of Wettest Quarter",
                                             "(bio 9) Mean Temperature of Driest Quarter",
                                             "(bio 10) Mean Temperature of Warmest Quarter",
                                             "(bio 11) Mean Temperature of Coldest Quarter",
                                             "(bio 12) Annual Precipitation",
                                             "(bio 13) Precipitation of Wettest Month",
                                             "(bio 14) Precipitation of Driest Month",
                                             "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                                             "(bio 16) Precipitation of Wettest Quarter",
                                             "(bio 17) Precipitation of Driest Quarter",
                                             "(bio 18) Precipitation of Warmest Quarter",
                                             "(bio 19) Precipitation of Coldest Quarter"),
                                 multiple = FALSE, 
                                 selected = NULL),
                     selectInput(inputId = "bio_bio_y", 
                                 label = "Y axis",
                                 choices = c("(bio 1) Annual Mean Temperature",
                                             "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                                             "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                                             "(bio 4) Temperature Seasonality (standard deviation *100)",
                                             "(bio 5) Max Temperature of Warmest Month",
                                             "(bio 6) Min Temperature of Coldest Month",
                                             "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                                             "(bio 8) Mean Temperature of Wettest Quarter",
                                             "(bio 9) Mean Temperature of Driest Quarter",
                                             "(bio 10) Mean Temperature of Warmest Quarter",
                                             "(bio 11) Mean Temperature of Coldest Quarter",
                                             "(bio 12) Annual Precipitation",
                                             "(bio 13) Precipitation of Wettest Month",
                                             "(bio 14) Precipitation of Driest Month",
                                             "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                                             "(bio 16) Precipitation of Wettest Quarter",
                                             "(bio 17) Precipitation of Driest Quarter",
                                             "(bio 18) Precipitation of Warmest Quarter",
                                             "(bio 19) Precipitation of Coldest Quarter"),
                                 multiple = FALSE, 
                                 selected = "(bio 12) Annual Precipitation"))
  })
  ## With multiple comparisons
  output$compare_type_bio_several <- renderUI({
    conditionalPanel(condition = "input.compare_type == 'bio_several'",
                     selectInput(inputId = "bio_several_x", 
                                 label = "X axis",
                                 choices = c("(bio 1) Annual Mean Temperature",
                                             "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                                             "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                                             "(bio 4) Temperature Seasonality (standard deviation *100)",
                                             "(bio 5) Max Temperature of Warmest Month",
                                             "(bio 6) Min Temperature of Coldest Month",
                                             "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                                             "(bio 8) Mean Temperature of Wettest Quarter",
                                             "(bio 9) Mean Temperature of Driest Quarter",
                                             "(bio 10) Mean Temperature of Warmest Quarter",
                                             "(bio 11) Mean Temperature of Coldest Quarter",
                                             "(bio 12) Annual Precipitation",
                                             "(bio 13) Precipitation of Wettest Month",
                                             "(bio 14) Precipitation of Driest Month",
                                             "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                                             "(bio 16) Precipitation of Wettest Quarter",
                                             "(bio 17) Precipitation of Driest Quarter",
                                             "(bio 18) Precipitation of Warmest Quarter",
                                             "(bio 19) Precipitation of Coldest Quarter"),
                                 multiple = TRUE, 
                                 selected = c("(bio 1) Annual Mean Temperature", "(bio 5) Max Temperature of Warmest Month")),
                     selectInput(inputId = "bio_several_y", 
                                 label = "Y axis",
                                 choices = c("(bio 1) Annual Mean Temperature",
                                             "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                                             "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                                             "(bio 4) Temperature Seasonality (standard deviation *100)",
                                             "(bio 5) Max Temperature of Warmest Month",
                                             "(bio 6) Min Temperature of Coldest Month",
                                             "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                                             "(bio 8) Mean Temperature of Wettest Quarter",
                                             "(bio 9) Mean Temperature of Driest Quarter",
                                             "(bio 10) Mean Temperature of Warmest Quarter",
                                             "(bio 11) Mean Temperature of Coldest Quarter",
                                             "(bio 12) Annual Precipitation",
                                             "(bio 13) Precipitation of Wettest Month",
                                             "(bio 14) Precipitation of Driest Month",
                                             "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                                             "(bio 16) Precipitation of Wettest Quarter",
                                             "(bio 17) Precipitation of Driest Quarter",
                                             "(bio 18) Precipitation of Warmest Quarter",
                                             "(bio 19) Precipitation of Coldest Quarter"),
                                 multiple = TRUE, 
                                 selected = c("(bio 12) Annual Precipitation", "(bio 13) Precipitation of Wettest Month")))
  })
  
  
  # ### Available GCMs - dynamically change For scenario - scenario_individual" tab (tab 1 - scenario definition - detail)
  # output$map_individual <- renderUI({
  #   glue::glue("# Getting list of available GCMs") %>% message
  #   scenarios <- list.files(paste0("clim_data/ccafs/rds"), pattern = input$res_sel) %>%
  #     dplyr::as_tibble() %>%
  #     magrittr::set_names("GCM") %>%
  #     tidyr::separate(col = GCM, into = c("gcm_", "resolution_", "year_", "rcp_", "borrar"), sep = "\\.") %>% dplyr::select(-borrar) %>%
  #     mutate(year_ = year_ %>% str_replace_all("year", ""),
  #            rcp_ = rcp_ %>% str_replace_all("rcp", ""))
  #   glue::glue("#   - Available GCMs with current setting") %>% message
  #   available_gcms <- scenarios %>%
  #     filter(resolution_ == input$res_sel) %>%
  #     filter(year_ == input$year_type) %>%
  #     filter(rcp_ == input$rcp_type %>% str_replace("rcp", "")) %>%
  #     pull(gcm_)
  #   glue::glue("#   - Generating GCMs options") %>% message
  #   selectInput(inputId = "gcm_to_plot", "Select a GCM",
  #               choices  = available_gcms)
  # })
  
  
  ######## MAPPING      
  ### Area study map
  # create map
  m <- leaflet(world_sf) %>% 
    addTiles() %>%
    addProviderTiles("Esri.WorldPhysical", group = "Relieve") %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group = "Countries") %>%
    addLayersControl(baseGroups = c("Relieve", "Countries"),
                     options = layersControlOptions(collapsed = FALSE)) %>% 
    setView(0,0, zoom = 2) %>% 
    addDrawToolbar(targetGroup = 'draw', 
                   singleFeature = TRUE,
                   rectangleOptions = filterNULL(list(
                     shapeOptions = drawShapeOptions(fillColor = "#8e113f",
                                                     color = "#595959"))),
                   polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE, 
                   circleMarkerOptions = FALSE, markerOptions = FALSE)#,
  # editOptions = editToolbarOptions())
  
  output$map <- renderLeaflet(m)
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  
  observe({
    # Extent selected by drawing rectangle
    if (input$extent_type == "map_draw") {
      map %>% 
        hideGroup("country") %>% 
        hideGroup("countrySel") %>% 
        hideGroup("biomes") %>% 
        hideGroup("biomesSel") %>% 
        hideGroup("ecorregions") %>% 
        hideGroup("ecorregionsSel") %>% 
        hideGroup("bbox") %>% 
        clearGroup("draw") %>%
        showGroup("draw") 
      
      req(input$map_draw_new_feature)
      # Get coordinates
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol = 2) %>% 
        unique %>% 
        extent
      rvs$polySelXY <- xy
      rvs$saved_bbox <- xy
    }
    
    # bounding-box extent
    if (input$extent_type == 'map_bbox'){
      
      req(input$xmin, input$xmax, input$ymin, input$ymax) # This stops it from failing while the number is being typed
      
      map %>% 
        clearGroup("bbox") %>% 
        clearGroup("draw") %>%
        clearGroup("biomes") %>% 
        clearGroup("biomesSel") %>% 
        clearGroup("ecorregions") %>% 
        clearGroup("ecorregionsSel") %>% 
        hideGroup("country") %>% 
        hideGroup("countrySel") %>% 
        # hideGroup("draw") %>% 
        hideGroup("biomes") %>%
        hideGroup("biomesSel") %>%
        hideGroup("ecorregions") %>%
        hideGroup("ecorregionsSel") %>%
        showGroup("bbox") %>% 
        addRectangles(group = "bbox",
                      lng1 = input$xmin, 
                      lng2 = input$xmax, 
                      lat1 = input$ymin,
                      lat2 = input$ymax,
                      weight = 1,
                      fillColor = "#1ebea6",
                      color = "#158b7a")
      
      rvs$polySelXY <- extent(input$xmin,
                              input$xmax,
                              input$ymin,
                              input$ymax)
      rvs$saved_bbox <- c(input$xmin, input$xmax, input$ymin, input$ymax)
    }
    
    
    # Countries selected by country name
    if (input$extent_type == "map_country"){
      
      # Country names manually added - subset layer to overlay
      selected_countries <- world_sf %>%
        filter(country %in% input$ext_name_country)
      
      # Add polygons to leaflet
      map %>% 
        clearGroup("draw") %>%
        clearGroup("bbox") %>% 
        clearGroup("biomes") %>% 
        clearGroup("biomesSel") %>% 
        clearGroup("ecorregions") %>% 
        clearGroup("ecorregionsSel") %>% 
        clearGroup("countrySel") %>% 
        hideGroup("biomes") %>% 
        hideGroup("biomesSel") %>% 
        hideGroup("bbox") %>% 
        # hideGroup("draw") %>%
        hideGroup("ecorregions") %>% 
        hideGroup("ecorregionsSel") %>% 
        showGroup("countrySel") %>% 
        showGroup("country") %>% 
        addPolygons(data = world_sf, 
                    group = "country",
                    weight = 1,
                    fillOpacity = 0,
                    opacity = 0.5,
                    color = "#595959") %>% 
        addPolygons(data = selected_countries, 
                    group = "countrySel",
                    weight = 1,
                    fillColor = "#8e113f",
                    fillOpacity = 0.4,
                    color = "#561a44") 
      
      rvs$polySelXY <- selected_countries
      
      req(input$map_draw_new_feature)
      glue::glue("#>   Crop Country layer to the extent drawn") %>% message
      # Get coordinates
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol = 2) %>% 
        unique %>% 
        extent
      
      glue::glue("#>   Crop Countries to EXTENT {xy}") %>% message
      selected_countries <- selected_countries %>% 
        st_crop(xmin = xmin(xy), xmax = xmax(xy), ymin = ymin(xy), ymax = ymax(xy))
      
      rvs$saved_bbox <- c(xmin(xy), xmax(xy), ymin(xy), ymax(xy))
      rvs$polySelXY <- selected_countries
    }
    
    # Countries selected by biome name
    if (input$extent_type == "map_biomes"){
      
      selected_biomes <- biomes %>%
        filter(BIOME_NAME %in% input$ext_name_biomes)
      
      # Add polygons to leaflet
      map %>% 
        hideGroup("country") %>% 
        hideGroup("bbox") %>% 
        # hideGroup("draw") %>% 
        hideGroup("ecorregions") %>% 
        clearGroup("draw") %>%
        clearGroup("country") %>% 
        clearGroup("countrySel") %>% 
        clearGroup("ecorregions") %>% 
        clearGroup("ecorregionsSel") %>% 
        clearGroup("biomesSel") %>% 
        showGroup("biomesSel") %>% 
        showGroup("biomes") %>% 
        addPolygons(data = biomes,
                    group = "biomes",
                    weight = 1,
                    fillOpacity = 0,
                    opacity = 0.5,
                    # smoothFactor = 3,
                    color = "#595959") %>% 
        # Highlight selected biomes
        addPolygons(data = selected_biomes, 
                    group = "biomesSel",
                    weight = 1,
                    fillColor = "#8e113f",
                    fillOpacity = 0.4,
                    color = "#561a44")
      
      rvs$polySelXY <- selected_biomes
      
      req(input$map_draw_new_feature)
      glue::glue("#>   Crop biomes layer to the extent drawn") %>% message
      # Get coordinates
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol = 2) %>% 
        unique %>% 
        extent
      
      glue::glue("#>   Crop biomes EXTENT {xy}") %>% message
      selected_biomes <- selected_biomes %>% 
        st_crop(xmin = xmin(xy), xmax = xmax(xy), ymin = ymin(xy), ymax = ymax(xy))
      
      rvs$saved_bbox <- c(xmin(xy), xmax(xy), ymin(xy), ymax(xy))
      rvs$polySelXY <- selected_biomes
    }
    
    # Countries selected by ecorregion name
    if (input$extent_type == "map_ecorregions"){
      
      selected_ecorregions <- ecorregions %>%
        filter(ECO_NAME %in% input$ext_name_ecorregions)
      
      # Add polygons to leaflet
      map %>% 
        hideGroup("country") %>% 
        hideGroup("bbox") %>% 
        hideGroup("biomes") %>% 
        clearGroup("draw") %>% 
        clearGroup("countrySel") %>% 
        clearGroup("biomesSel") %>% 
        showGroup("ecorregions") %>% 
        showGroup("ecorregionsSel") %>% 
        addPolygons(data = ecorregions,
                    group = "ecorregions",
                    weight = 1,
                    fillOpacity = 0,
                    opacity = 0.5,
                    # smoothFactor = 3,
                    color = "#595959") %>% 
        # Highlight selected ecorregions
        addPolygons(data = selected_ecorregions, 
                    group = "ecorregionsSel",
                    weight = 1,
                    fillColor = "#8e113f",
                    fillOpacity = 0.4,
                    color = "#561a44")
      
      rvs$polySelXY <- selected_ecorregions
      
      req(input$map_draw_new_feature)
      glue::glue("#>   Crop ecorregions layer to the extent drawn") %>% message
      # Get coordinates
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol = 2) %>% 
        unique %>% 
        extent
      
      glue::glue("#>   Crop ecorregions EXTENT {xy}") %>% message
      selected_ecorregions <- selected_ecorregions %>% 
        st_crop(xmin = xmin(xy), xmax = xmax(xy), ymin = ymin(xy), ymax = ymax(xy))
      
      rvs$saved_bbox <- c(xmin(xy), xmax(xy), ymin(xy), ymax(xy))
      rvs$polySelXY <- selected_ecorregions
    }
  })
  
  
  
  ####################################################################################
  #### Load layers
  ####################################################################################
  reactive({
    # When hidden options for GCM selection, consider some default
    glue::glue("# DEF: Getting list of available GCMs") %>% message
    scenarios <- list.files(paste0("clim_data/ccafs/rds"), pattern = input$res_sel) %>% 
      dplyr::as_tibble() %>% 
      magrittr::set_names("GCM") %>%
      tidyr::separate(col = GCM, into = c("gcm_", "resolution_", "year_", "rcp_", "borrar"), sep = "\\.") %>% dplyr::select(-borrar) %>%
      mutate(year_ = year_ %>% str_replace_all("year", ""),
             rcp_ = rcp_ %>% str_replace_all("rcp", ""))
    
    glue::glue("#   - DEF: Available GCMs with current setting") %>% message
    rvs$def_available_gcms <- scenarios %>% 
      filter(resolution_ == "10m") %>% 
      filter(year_ == "2070") %>% 
      filter(rcp_ == "45") %>% 
      pull(gcm_)
    
  })
  
  observeEvent(input$go, {
    if (input$compare_type == "bio_bio"){
      bio_vars_x <- input$bio_bio_x
      bio_vars_y <- input$bio_bio_y
    }
    if (input$compare_type == "bio_several"){
      bio_vars_x <- input$bio_several_x
      bio_vars_y <- input$bio_several_y
    }
    
    # Translate to regular names
    bio_vars_x2 <- case_when(bio_vars_x == "(bio 1) Annual Mean Temperature" ~ "bio_1",
                             bio_vars_x == "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))" ~ "bio_2",
                             bio_vars_x == "(bio 3) Isothermality (BIO2/BIO7) (* 100)" ~ "bio_3",
                             bio_vars_x == "(bio 4) Temperature Seasonality (standard deviation *100)" ~ "bio_4",
                             bio_vars_x == "(bio 5) Max Temperature of Warmest Month" ~ "bio_5",
                             bio_vars_x == "(bio 6) Min Temperature of Coldest Month" ~ "bio_6",
                             bio_vars_x == "(bio 7) Temperature Annual Range (BIO5-BIO6)" ~ "bio_7",
                             bio_vars_x == "(bio 8) Mean Temperature of Wettest Quarter" ~ "bio_8",
                             bio_vars_x == "(bio 9) Mean Temperature of Driest Quarter" ~ "bio_9",
                             bio_vars_x == "(bio 10) Mean Temperature of Warmest Quarter" ~ "bio_10",
                             bio_vars_x == "(bio 11) Mean Temperature of Coldest Quarter" ~ "bio_11",
                             bio_vars_x == "(bio 12) Annual Precipitation" ~ "bio_12",
                             bio_vars_x == "(bio 13) Precipitation of Wettest Month" ~ "bio_13",
                             bio_vars_x == "(bio 14) Precipitation of Driest Month" ~ "bio_14",
                             bio_vars_x == "(bio 15) Precipitation Seasonality (Coefficient of Variation)" ~ "bio_15",
                             bio_vars_x == "(bio 16) Precipitation of Wettest Quarter" ~ "bio_16",
                             bio_vars_x == "(bio 17) Precipitation of Driest Quarter" ~ "bio_17",
                             bio_vars_x == "(bio 18) Precipitation of Warmest Quarter" ~ "bio_18",
                             bio_vars_x == "(bio 19) Precipitation of Coldest Quarter" ~ "bio_19")
    bio_vars_y2 <- case_when(bio_vars_y == "(bio 1) Annual Mean Temperature" ~ "bio_1",
                             bio_vars_y == "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))" ~ "bio_2",
                             bio_vars_y == "(bio 3) Isothermality (BIO2/BIO7) (* 100)" ~ "bio_3",
                             bio_vars_y == "(bio 4) Temperature Seasonality (standard deviation *100)" ~ "bio_4",
                             bio_vars_y == "(bio 5) Max Temperature of Warmest Month" ~ "bio_5",
                             bio_vars_y == "(bio 6) Min Temperature of Coldest Month" ~ "bio_6",
                             bio_vars_y == "(bio 7) Temperature Annual Range (BIO5-BIO6)" ~ "bio_7",
                             bio_vars_y == "(bio 8) Mean Temperature of Wettest Quarter" ~ "bio_8",
                             bio_vars_y == "(bio 9) Mean Temperature of Driest Quarter" ~ "bio_9",
                             bio_vars_y == "(bio 10) Mean Temperature of Warmest Quarter" ~ "bio_10",
                             bio_vars_y == "(bio 11) Mean Temperature of Coldest Quarter" ~ "bio_11",
                             bio_vars_y == "(bio 12) Annual Precipitation" ~ "bio_12",
                             bio_vars_y == "(bio 13) Precipitation of Wettest Month" ~ "bio_13",
                             bio_vars_y == "(bio 14) Precipitation of Driest Month" ~ "bio_14",
                             bio_vars_y == "(bio 15) Precipitation Seasonality (Coefficient of Variation)" ~ "bio_15",
                             bio_vars_y == "(bio 16) Precipitation of Wettest Quarter" ~ "bio_16",
                             bio_vars_y == "(bio 17) Precipitation of Driest Quarter" ~ "bio_17",
                             bio_vars_y == "(bio 18) Precipitation of Warmest Quarter" ~ "bio_18",
                             bio_vars_y == "(bio 19) Precipitation of Coldest Quarter" ~ "bio_19")
    rvs$bio_vars_x2 <- bio_vars_x2
    rvs$bio_vars_x_full <- bio_vars_x
    rvs$bio_vars_y2 <- bio_vars_y2
    rvs$bio_vars_y_full <- bio_vars_y
    rvs$bio_vars_all <- c(bio_vars_x2, bio_vars_y2)
  })
  
  
  ####################################################################################
  #### Create ensemble, deltas and differences
  ####################################################################################
  
  
  observeEvent(input$go, {
    withProgress(message = 'Loading climatic data',
                 detail = NULL,
                 value = 0, {
                   incProgress(amount = 0.1, message = 'Loading climatic data')
                   ### Load variables
                   glue::glue("#>>   Loading climatic data") %>% message
                   if(!is.null(input$sel_gcms)){
                     # clim_vars_files <- paste0("clim_data/ccafs/raw_rasters/",
                     #                           input$sel_gcms, ".", 
                     #                           input$res_sel, 
                     #                           ".year", input$year_type, ".",
                     #                           input$rcp_type, 
                     #                           ".grd")
                     clim_vars_files <- paste0("clim_data/ccafs/rds/",
                                               input$sel_gcms, ".", 
                                               input$res_sel, 
                                               ".year", input$year_type, ".",
                                               input$rcp_type, 
                                               ".rds")
                   } else {
                     # clim_vars_files <- paste0("clim_data/ccafs/raw_rasters/",
                     #                           rvs$def_available_gcms, ".", 
                     #                           input$res_sel, 
                     #                           ".year", input$year_type, ".",
                     #                           input$rcp_type, 
                     #                           ".grd")
                     clim_vars_files <- paste0("clim_data/ccafs/rds/",
                                               rvs$def_available_gcms, ".",
                                               input$res_sel,
                                               ".year", input$year_type, ".",
                                               input$rcp_type,
                                               ".rds")
                   }
                   rvs$clim_vars_complete <- clim_vars_files %>%
                     # purrr::map(~ raster::stack(.x)) %>%
                     purrr::map(~ readRDS(.x)) %>%
                     magrittr::set_names(input$sel_gcms)
                   
                   # Select a subset of bioclim variables
                   incProgress(amount = 0.1, message = "Dropping unnecessary layers")
                   glue::glue("#>>   Subsetting selected variables") %>% message
                   rvs$clim_vars <- rvs$clim_vars_complete %>%
                     purrr::map(~ raster::subset(.x, subset = c(rvs$bio_vars_x2, rvs$bio_vars_y2)))
                   
                   ### Crop to study area
                   incProgress(amount = 0.1, message = "Cropping to study area")
                   req(rvs$polySelXY)
                   glue::glue("#>>   Cropping to study area") %>% message
                   # When it is not a polygon
                   if (!is(rvs$polySelXY, "sf")){
                     rvs$clim_vars <- rvs$clim_vars %>%
                       purrr::map(~ raster::crop(.x, rvs$polySelXY))
                     
                     # Save a copy of croped countries
                     rvs$sf_country <- world_sf %>% 
                       st_crop(xmin = xmin(rvs$polySelXY), xmax = xmax(rvs$polySelXY), ymin = ymin(rvs$polySelXY), ymax = ymax(rvs$polySelXY))
                   }
                   # When it is a polygon
                   if (is(rvs$polySelXY, "sf")){
                     glue::glue("#     - Rasterizing polygon") %>% message
                     rvs$polySelXY <- rvs$polySelXY %>% 
                       mutate(id = 1) %>% group_by(id) %>% summarise
                     rvs$polySelXY_r <- fasterize::fasterize(rvs$polySelXY,
                                                             rvs$clim_vars[[1]][[1]] %>%
                                                               raster::crop(extent(xmin(extent(rvs$polySelXY)),
                                                                                   xmax(extent(rvs$polySelXY)),
                                                                                   ymin(extent(rvs$polySelXY)),
                                                                                   ymax(extent(rvs$polySelXY)))))
                     
                     glue::glue("#     - Masking") %>% message
                     # message(rvs$polySelXY)
                     rvs$clim_vars <- rvs$clim_vars %>%
                       purrr::map(~ raster::crop(.x, rvs$polySelXY_r)) %>%
                       purrr::map(~ raster::mask(.x, rvs$polySelXY_r))
                     
                     # Save a copy of croped countries
                     rvs$sf_country <- rvs$polySelXY
                   }
                   # message(class(rvs$sf_country))
                   # plot(rvs$sf_country)
                   ### Divide by 10 layers
                   glue::glue("#>>  Transforming temperature layers to regular units of ºC") %>% message
                   for (i in 1:length(rvs$clim_vars)){
                     for (j in names(rvs$clim_vars[[i]])[names(rvs$clim_vars[[i]]) %in% c("bio_1", "bio_2", "bio_4", "bio_5", "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11")]){
                       rvs$clim_vars[[i]][[j]] <- rvs$clim_vars[[i]][[j]] / 10
                     }
                   }
                   
                   ### Comapre each GCM with baseline
                   incProgress(amount = 0.3, message = "Comparing GCMs with baseline")
                   glue::glue("#>>   Loading and preparing baseline") %>% message
                   # Load baseline
                   # rvs$clim_baseline_complete <- stack(paste0("clim_data/baseline/raw_rasters/baseline_ccafs.", input$res_sel, ".gri"))
                   rvs$clim_baseline_complete <- readRDS(paste0("clim_data/baseline/rds/baseline.", input$res_sel, ".rds"))
                   rvs$clim_baseline <- rvs$clim_baseline_complete %>% 
                     crop(extent(rvs$clim_vars[[1]])) %>% 
                     mask(rvs$clim_vars[[1]][[1]])
                   glue::glue("#>>   Subsetting bioclims in baseline") %>% message
                   rvs$clim_baseline <- rvs$clim_baseline %>% 
                     raster::subset(c(rvs$bio_vars_x2, rvs$bio_vars_y2))
                   for (j in names(rvs$clim_baseline)[names(rvs$clim_baseline) %in% c("bio_1", "bio_2", "bio_4", "bio_5", "bio_6", "bio_7", "bio_8", "bio_9", "bio_10", "bio_11")]){
                     rvs$clim_baseline[[j]] <- rvs$clim_baseline[[j]] / 10
                   }
                   
                   
                   
                   glue::glue("#>>   Comparing GCMs with baseline") %>% message
                   rvs$clim_delta <- rvs$clim_vars %>%
                     purrr::map(~ .x - rvs$clim_baseline)
                   
                   ### Calculate ensemble
                   incProgress(amount = 0.3, message = "Calculating mean ensemble")
                   glue::glue("#>>   Calculating the ensemble from all different GCM projections") %>% message
                   rvs$clim_ens <- rvs$clim_vars %>%
                     purrr::reduce(`+`) %>%                                  # Sum all layers
                     raster::calc(fun = function(x){x / length(rvs$clim_vars)})  # Divide by the number of layers
                   rvs$clim_delta_ensemble <- rvs$clim_ens - rvs$clim_baseline
                   
                   ### Compare each GCM with ensemble
                   incProgress(amount = 0.3, message = "Computing differences with ensemble")
                   glue::glue("#>>   Computing differences between each GCM and the GCM ensemble") %>% message
                   rvs$clim_diff <- rvs$clim_vars %>%
                     purrr::map(~ .x - rvs$clim_ens)
                 })
  })
  
  
  ####################################################################################
  ### TAB 2: VISUALIZE GCMs
  ####################################################################################
  
  ################# OPTIONS FOR UI #################
  ### SELECTED OPTIONS
  observe({
    n_gcms <- length(input$sel_gcms)
    output$selected_gcms_2_tab <- renderText(
      glue::glue("{n_gcms} GCMs") %>% print()
    )
    print_rcp <- input$rcp_type %>% str_replace("rcp", "")
    output$selected_scenario_2_tab <- renderText(
      glue::glue("Year {input$year_type}, RCP{print_rcp}, resolution: {input$res_sel}") %>% print()
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("bio X bio"),
                                          input$compare_type == "bio_several" ~ "multiple")
    output$selected_comparison_2_tab <- renderText(
      glue::glue("Comparison: {type_selected_comparison}") %>% print()
    )
  })
  
  
  
  ####################################################################################
  ### TAB 3: VISUALIZE DIFFERENCES WITH PRESENT
  ####################################################################################
  
  
  ########### CODE FOR UI ###########
  ### SELECTED OPTIONS
  observe({
    n_gcms <- length(input$sel_gcms)
    output$selected_gcms_pre_tab <- renderText(
      glue::glue("{n_gcms} GCMs") %>% print()
    )
    print_rcp <- input$rcp_type %>% str_replace("rcp", "")
    output$selected_scenario_pre_tab <- renderText(
      glue::glue("Year {input$year_type}, RCP{print_rcp}, resolution: {input$res_sel}") %>% print()
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("bio X bio"),
                                          input$compare_type == "bio_several" ~ "multiple")
    output$selected_comparison_pre_tab <- renderText(
      glue::glue("Comparison: {type_selected_comparison}") %>% print()
    )
  })
  
  output$no_analyze_tabl <- renderText({
    if(input$go == 0){
      glue::glue("A table will values of the comparison will appear here after you define a scenario and press 'Analyze' in the tab 'Definition of scenario'")
    } else {return()}
  })
  output$no_analyze <- renderText({
    if(input$go == 0){
      glue::glue("Maps will appear here when you define a scenario and press 'Analyze' in the tab 'Definition of scenario'")
    } else {return()}
  })
  
  
  
  ########### Explore GCM ###########
  
  observeEvent({input$go
    input$add_layer}, {
      
      output$gcm_patterns <- renderUI({
        # if (!is.null(rvs$still_no_analyse)){
        glue::glue("#> Creating GCM pattern plots") %>% message
        # plot_gcm_pattern <- plot_gcm_patterns2(rvs$clim_comp,
        #                                        country_borders = FALSE,
        #                                        return_ensemble = TRUE)
        ################################
        plot_gcm_pattern <- list()
        
        for (b in names(rvs$clim_vars[[1]])){
          # Combine all variables to plot
          scenario_data <- rvs$clim_vars %>%
            purrr::map(~ raster::subset(.x, b)) %>%
            raster::stack()
          
          scenario_data <- rvs$clim_ens %>%
            raster::subset(., b) %>%
            setNames("ENSEMBLE") %>%
            stack(scenario_data)
          
          scenario_data <- rvs$clim_baseline %>%
            raster::subset(., b) %>%
            setNames("BASELINE") %>%
            stack(scenario_data)
          
          long_name <- case_when(b == "bio_1" ~ "(bio 1) Annual Mean Temperature",
                                 b == "bio_2" ~ "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                                 b == "bio_3" ~ "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                                 b == "bio_4" ~ "(bio 4) Temperature Seasonality (standard deviation *100)",
                                 b == "bio_5" ~  "(bio 5) Max Temperature of Warmest Month",
                                 b == "bio_6" ~  "(bio 6) Min Temperature of Coldest Month",
                                 b == "bio_7" ~  "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                                 b == "bio_8" ~  "(bio 8) Mean Temperature of Wettest Quarter",
                                 b == "bio_9" ~  "(bio 9) Mean Temperature of Driest Quarter",
                                 b == "bio_10" ~ "(bio 10) Mean Temperature of Warmest Quarter",
                                 b == "bio_11" ~ "(bio 11) Mean Temperature of Coldest Quarter",
                                 b == "bio_12" ~ "(bio 12) Annual Precipitation",
                                 b == "bio_13" ~ "(bio 13) Precipitation of Wettest Month",
                                 b == "bio_14" ~ "(bio 14) Precipitation of Driest Month",
                                 b == "bio_15" ~ "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                                 b == "bio_16" ~ "(bio 16) Precipitation of Wettest Quarter",
                                 b == "bio_17" ~ "(bio 17) Precipitation of Driest Quarter",
                                 b == "bio_18" ~ "(bio 18) Precipitation of Warmest Quarter",
                                 b == "bio_19" ~ "(bio 19) Precipitation of Coldest Quarter")
          
          # Get range of values
          range_values <- c(min(values(scenario_data), na.rm = T),
                            max(values(scenario_data), na.rm = T))
          range_by <- (range_values[2] - range_values[1]) / 100
          
          # Plot
          scenario_plot <- scenario_data %>%
            rasterVis::levelplot(main = paste0(long_name, " - raw GCMs"),
                                 # layout = lp_layout,
                                 at = seq(range_values[[1]],
                                          range_values[[2]],
                                          by = range_by))
          if (input$add_layer == "countries"){
            sp_add <- world_map %>% crop(extent(rvs$clim_vars[[1]]))
            scenario_plot <- scenario_plot +
              latticeExtra::layer(sp::sp.polygons(sp_add,
                                                  lwd = 3,
                                                  alpha = 0.8,
                                                  col = "black"),
                                  data = list(sp_add = sp_add))
          }
          if (input$add_layer == "biomes"){
            sp_add <- biomes_sp %>% crop(extent(rvs$clim_vars[[1]]))
            scenario_plot <- scenario_plot +
              latticeExtra::layer(sp::sp.polygons(sp_add,
                                                  lwd = 3,
                                                  alpha = 0.8,
                                                  col = "black"),
                                  data = list(sp_add = sp_add))
          }
          if (input$add_layer == "ecoregions"){
            sp_add <- ecorregions_sp %>% crop(extent(rvs$clim_vars[[1]]))
            scenario_plot <- scenario_plot +
              latticeExtra::layer(sp::sp.polygons(sp_add,
                                                  lwd = 3,
                                                  alpha = 0.8,
                                                  col = "black"),
                                  data = list(sp_add = sp_add))
          }
          
          plot_gcm_pattern[[length(plot_gcm_pattern) + 1]] <- scenario_plot
          names(plot_gcm_pattern)[length(plot_gcm_pattern)] <- b
          rm(scenario_plot)
        }
        rvs$plot_gcm_pattern <- plot_gcm_pattern
        ########################################################
        
        glue::glue("#> Manipulating plot pattern list") %>% message
        for (i in 1:length(plot_gcm_pattern)) {
          local({
            my_i <- i
            plotname <- paste0("plot_pattern", my_i)
            output[[plotname]] <- renderPlot({
              plot_gcm_pattern[[my_i]]
            }, height = 1000)
          })
        }
        
        glue::glue("#> Printing plot pattern") %>% message
        plot_gcm_pattern_ui <- lapply(1:length(plot_gcm_pattern), function(i) {
          plotname <- paste0("plot_pattern", i)
          plotOutput(plotname, height = "100%")
        })
        do.call(tagList, plot_gcm_pattern_ui) %>%      # Convert the list to a tagList
          withSpinner(type = 7, color = "#5fbc93")
        # } else {""}
      })
    })
  
  
  
  ########### Explore Deltas ###########
  
  
  observeEvent(input$go, {
    
    output$delta_patterns <- renderUI({
      # if (!is.null(rvs$still_no_analyse)){
      glue::glue("#> Creating delta pattern plots") %>% message
      # plot_gcm_pattern <- plot_gcm_patterns2(rvs$clim_comp,
      #                                        country_borders = FALSE,
      #                                        return_ensemble = TRUE)
      ################################
      plot_delta_pattern <- list()
      
      for (b in names(rvs$clim_delta[[1]])){
        # Combine all variables to plot
        scenario_data <- rvs$clim_delta %>%
          purrr::map(~ raster::subset(.x, b)) %>%
          raster::stack()
        
        # scenario_data <- rvs$clim_baseline %>%
        #   raster::subset(., b) %>%
        #   setNames("BASELINE") %>%
        #   stack(scenario_data)
        
        long_name <- case_when(b == "bio_1" ~ "(bio 1) Annual Mean Temperature",
                               b == "bio_2" ~ "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                               b == "bio_3" ~ "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                               b == "bio_4" ~ "(bio 4) Temperature Seasonality (standard deviation *100)",
                               b == "bio_5" ~  "(bio 5) Max Temperature of Warmest Month",
                               b == "bio_6" ~  "(bio 6) Min Temperature of Coldest Month",
                               b == "bio_7" ~  "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                               b == "bio_8" ~  "(bio 8) Mean Temperature of Wettest Quarter",
                               b == "bio_9" ~  "(bio 9) Mean Temperature of Driest Quarter",
                               b == "bio_10" ~ "(bio 10) Mean Temperature of Warmest Quarter",
                               b == "bio_11" ~ "(bio 11) Mean Temperature of Coldest Quarter",
                               b == "bio_12" ~ "(bio 12) Annual Precipitation",
                               b == "bio_13" ~ "(bio 13) Precipitation of Wettest Month",
                               b == "bio_14" ~ "(bio 14) Precipitation of Driest Month",
                               b == "bio_15" ~ "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                               b == "bio_16" ~ "(bio 16) Precipitation of Wettest Quarter",
                               b == "bio_17" ~ "(bio 17) Precipitation of Driest Quarter",
                               b == "bio_18" ~ "(bio 18) Precipitation of Warmest Quarter",
                               b == "bio_19" ~ "(bio 19) Precipitation of Coldest Quarter")
        
        # Get range of values
        range_values <- c(min(values(scenario_data), na.rm = T),
                          max(values(scenario_data), na.rm = T))
        range_values <- c(- max(abs(range_values)), max(abs(range_values)))
        range_by <- (range_values[2] - range_values[1]) / 100
        
        # Plot
        scenario_plot <- scenario_data %>%
          rasterVis::levelplot(main = paste0(long_name, " - Deltas"),
                               par.settings = rasterVis::RdBuTheme(region = rev(RColorBrewer::brewer.pal(9, 'RdGy'))),
                               # layout = lp_layout,
                               at = seq(range_values[[1]],
                                        range_values[[2]],
                                        by = range_by))
        if (input$add_layer2 == "countries"){
          sp_add <- world_map %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        if (input$add_layer2 == "biomes"){
          sp_add <- biomes_sp %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        if (input$add_layer2 == "ecoregions"){
          sp_add <- ecorregions_sp %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        
        plot_delta_pattern[[length(plot_delta_pattern) + 1]] <- scenario_plot
        names(plot_delta_pattern)[length(plot_delta_pattern)] <- b
        rm(scenario_plot)
      }
      ########################################################
      rvs$plot_delta_pattern <- plot_delta_pattern
      
      
      glue::glue("#> Manipulating plot delta pattern list") %>% message
      for (i in 1:length(plot_delta_pattern)) {
        local({
          my_i <- i
          plotname <- paste0("plot_delta_pattern", my_i)
          output[[plotname]] <- renderPlot({
            plot_delta_pattern[[my_i]]
          }, height = 1000)
        })
      }
      
      glue::glue("#> Printing delta pattern") %>% message
      plot_delta_pattern_ui <- lapply(1:length(plot_delta_pattern), function(i) {
        plotname <- paste0("plot_delta_pattern", i)
        plotOutput(plotname, height = "100%")
      })
      do.call(tagList, plot_delta_pattern_ui) %>%      # Convert the list to a tagList
        withSpinner(type = 7, color = "#5fbc93")
      # } else {""}
    })
  })
  
  
  ############ Visualize Deltas individually ############
  
  ### Available GCMs - dynamically change
  # For "current - maps_pre_detail" tab (tab 1 - scenario definition - detail)
  output$map_pre_delta_gcm <- renderUI({
    glue::glue("# Getting list of available GCMs") %>% message
    scenarios <- list.files(paste0("clim_data/ccafs/rds"), pattern = input$res_sel) %>% 
      dplyr::as_tibble() %>%
      magrittr::set_names("GCM") %>%
      tidyr::separate(col = GCM, into = c("gcm_", "resolution_", "year_", "rcp_", "borrar"), sep = "\\.") %>% dplyr::select(-borrar) %>%
      mutate(year_ = year_ %>% str_replace_all("year", ""),
             rcp_ = rcp_ %>% str_replace_all("rcp", ""))
    glue::glue("#   - Available GCMs with current setting") %>% message
    available_gcms <- scenarios %>%
      filter(resolution_ == input$res_sel) %>%
      filter(year_ == input$year_type) %>%
      filter(rcp_ == input$rcp_type %>% str_replace("rcp", "")) %>%
      pull(gcm_)
    glue::glue("#   - Generating GCMs options") %>% message
    selectInput(inputId = "map_pre_delta_gcm", "Select a GCM",
                choices  = available_gcms,
                selected = available_gcms[1])
  })
  
  output$map_pre_delta_bio <- renderUI({
    selectInput(inputId = "map_pre_delta_bio",
                label = NULL,
                choices = rvs$bio_vars_all, 
                selected = rvs$bio_vars_all[1])
  })
  
  ### create map for visualization of future differences
  map_pre_delt <- leaflet(world_sf) %>%
    setView(0, 0, zoom = 2) %>% 
    addProviderTiles("Esri.WorldPhysical", group = "Relieve") %>% 
    addTiles(options = providerTileOptions(noWrap = TRUE), group = "Countries") %>%
    addLayersControl(baseGroups = c("Relieve", "Countries"),
                     options = layersControlOptions(collapsed = FALSE))
  
  output$map_pre_delta <- renderLeaflet(map_pre_delt)
  # create map proxy to make further changes to existing map
  map_pre_delta <- leafletProxy("map_pre_delta")
  map_pre_delta %>%
    addProviderTiles("Esri.WorldPhysical")
  
  ### Display selected map
  observeEvent({input$map_pre_delta_gcm
    input$map_pre_delta_bio}, {
      # observeEvent(input$add_map,{  
      req(rvs$clim_delta)         # Stop it from crashing when no result has been produced         
      
      # if(input$add_map == TRUE){
      glue::glue("#>  - ADDING RASTER TO LEAFLET") %>% message
      
      rvs$delta_layer <- rvs$clim_delta[[input$map_pre_delta_gcm]][[input$map_pre_delta_bio]]
      
      ## Range of values and color palette from them
      rvs$delta_stack <- rvs$clim_delta %>% 
        purrr::map(~ .x[[input$map_pre_delta_bio]]) %>% 
        raster::stack()
      glue::glue("#>   Calculating data limits") %>% message
      range_values <- c(min(values(rvs$delta_stack), na.rm = T),
                        max(values(rvs$delta_stack), na.rm = T))
      range_values <- c(- max(abs(range_values)), max(abs(range_values)))
      
      glue::glue("#>   Making color palette") %>% message
      pal <- colorNumeric(palette = c("#c0002d", "#f8f5f5", "#0069a8"), 
                          domain = range_values,
                          na.color = "transparent",
                          reverse = TRUE)
      
      # Add polygons to leaflet
      glue::glue("#>   Including the raster to leaflet") %>% message
      map_pre_delta %>%
        clearControls() %>%    # Refreshes the legend
        clearGroup("map_pre_delta") %>%
        fitBounds(lng1 = extent(rvs$delta_layer)[1],    # ponerse alrededor de la capa
                  lat1 = extent(rvs$delta_layer)[3], 
                  lng2 = extent(rvs$delta_layer)[2], 
                  lat2 = extent(rvs$delta_layer)[4]) %>% 
        addRasterImage(rvs$delta_layer,
                       colors = pal,
                       opacity = 0.8,
                       group = "map_pre_delta") %>%
        addLegend(pal = pal, 
                  values = range_values,
                  group = "map_pre_delta",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      
      rvs$allow_download_layer_pre_delta <- TRUE  # Signal to permit download
    })
  
  
  ### Download layer
  output$allow_download_layer_pre_delta <- downloadHandler(
    filename = function() {
      paste0("gcm_compareR_", input$map_pre_delta_gcm, "_", input$year_type, "_", input$rcp_type, "_", input$map_pre_delta_bio, "_", input$res_sel, ".", input$download_raster_format)
    },
    content = function(file) {
      writeRaster(rvs$delta_layer, 
                  file,
                  format = case_when(input$download_pre_delta_format == "grd" ~ "raster",
                                     input$download_pre_delta_format == "tif" ~ "GTiff"),
                  overwrite = TRUE)
    }
  )
  # Disable it until a layer is shown
  shinyjs::disable("download_raster_format")
  observeEvent(rvs$allow_download_layer_pre_delta, {
    shinyjs::enable("download_raster_format")
  })
  shinyjs::disable("download_mapIII")
  observeEvent(rvs$allow_download_layer_pre_delta, {
    shinyjs::enable("download_mapIII")
  })
  
  
  
  
  ####################################################################################
  ### TAB 3 - FUTURE (DIFFERENCES BETWEEN GCMs)
  ####################################################################################
  
  
  ################# OPTIONS FOR UI #################
  ### SELECTED OPTIONS
  observe({
    n_gcms <- length(input$sel_gcms)
    output$selected_gcms_fut_tab <- renderText(
      glue::glue("{n_gcms} GCMs") %>% print()
    )
    print_rcp <- input$rcp_type %>% str_replace("rcp", "")
    output$selected_scenario_fut_tab <- renderText(
      glue::glue("Year {input$year_type}, RCP{print_rcp}, resolution: {input$res_sel}") %>% print()
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("bio X bio"),
                                          input$compare_type == "bio_several" ~ "multiple")
    output$selected_comparison_fut_tab <- renderText(
      glue::glue("Comparison: {type_selected_comparison}") %>% print()
    )
  })
  
  ###### Disable non-scaled option if more than 2 variables
  shinyjs::disable("type_scaled")
  
  output$no_analyze_fig <- renderText({
    if(input$go == 0){
      glue::glue("A scatterplot will values of the comparison will appear here after you define a scenario and press 'Analyze' in the tab 'Definition of scenario'")
    } else {return()}
  })
  
  # observeEvent(input$go, {   # Usar esto hace que se ponga gris al cambiar aunque antes no le des a ANALYZE
  observeEvent(input$compare_type, {
    if (input$compare_type == "bio_bio"){
      shinyjs::enable("type_scaled")
    }
    if (input$compare_type == "bio_several"){
      shinyjs::disable("type_scaled")
    }
  })
  
  ## Text between scatterplot and table
  output$tab3_ideas_for_selecting <- renderUI({
    if (input$go == 0) {""} else {
      includeMarkdown("Rmd/tab3_ideas_for_selecting.Rmd")
    }
  })
  output$tab3_ideas_for_selecting_copy <- renderUI({
    if (input$go == 0) {""} else {
      includeMarkdown("Rmd/tab3_ideas_for_selecting_copy.Rmd")
    }
  })
  output$tab3_ideas_for_selecting_copy2 <- renderUI({
    if (input$go == 0) {""} else {
      includeMarkdown("Rmd/tab3_ideas_for_selecting_copy2.Rmd")
    }
  })
  
  
  
  ################## CALCULATIONS FOR SCATTERPLOT AND TABLE ##################
  
  observeEvent({#input$type_scaled
    # input$type_scaled_pre_delta
    input$go
  }, {
    req(rvs$clim_ens)
    withProgress(message = 'Comparing GCM values',
                 detail = NULL,
                 value = 0, {
                   # req(input$go)
                   # req(input$type_scaled)
                   
                   ##### Axis and column labels
                   if(input$compare_type == "bio_bio"){
                     glue::glue("#   1 hola") %>% message()
                     rvs$xaxis_lab <- case_when(rvs$bio_vars_x2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_x_full, " (ºC)"),
                                                rvs$bio_vars_x2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_x_full, " (mm)"),
                                                rvs$bio_vars_x2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_x2, ""))
                     rvs$yaxis_lab <- case_when(rvs$bio_vars_y2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_y_full, " (ºC)"),
                                                rvs$bio_vars_y2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_y_full, " (mm)"),
                                                rvs$bio_vars_y2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_y2, ""))
                   }
                   if(input$compare_type == "bio_several"){
                     glue::glue("#   1b") %>% message()
                     rvs$xaxis_lab <- paste0(rvs$bio_vars_x2, collapse = " - ")
                     rvs$yaxis_lab <- paste0(rvs$bio_vars_y2, collapse = " - ")
                   }
                   
                   if(input$compare_type == "bio_bio"){
                     glue::glue("#   2") %>% message()
                     rvs$xcol_sc_lab <- paste0(rvs$bio_vars_x2, "(scaled)")
                     rvs$ycol_sc_lab <- paste0(rvs$bio_vars_y2, "(scaled)")
                     
                     rvs$xcol_unsc_lab <- case_when(rvs$bio_vars_x2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_x2, " (ºC)"),
                                                    rvs$bio_vars_x2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_x2, " (mm)"),
                                                    rvs$bio_vars_x2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_x2, ""))
                     rvs$ycol_unsc_lab <- case_when(rvs$bio_vars_y2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_y2, " (ºC)"),
                                                    rvs$bio_vars_y2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_y2, " (mm)"),
                                                    rvs$bio_vars_y2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_y2, ""))
                     
                     rvs$xcol_delta_lab <- case_when(rvs$bio_vars_x2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_x2, " (ºC) - delta"),
                                                     rvs$bio_vars_x2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_x2, " (mm) - delta"),
                                                     rvs$bio_vars_x2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_x2, " - delta"))
                     rvs$ycol_delta_lab <- case_when(rvs$bio_vars_y2 %in% paste0("bio_", c(1:2, 5:11)) ~ paste0(rvs$bio_vars_y2, " (ºC) - delta"),
                                                     rvs$bio_vars_y2 %in% paste0("bio_", c(12:19)) ~ paste0(rvs$bio_vars_y2, " (mm) - delta"),
                                                     rvs$bio_vars_y2 %in% paste0("bio_", c(3:4)) ~ paste0(rvs$bio_vars_y2, " - delta"))
                   }
                   if(input$compare_type == "bio_several"){
                     rvs$xcol_sc_lab <- paste0(rvs$bio_vars_x2, collapse = " - ")
                     rvs$ycol_sc_lab <- paste0(rvs$bio_vars_y2, collapse = " - ")
                     rvs$xcol_unsc_lab <- paste0(rvs$bio_vars_x2, collapse = " - ")
                     rvs$ycol_unsc_lab <- paste0(rvs$bio_vars_y2, collapse = " - ")
                     rvs$xcol_delta_lab <- paste0(paste0(rvs$bio_vars_x2, collapse = " - "), "(delta)")
                     rvs$ycol_delta_lab <- paste0(paste0(rvs$bio_vars_y2, collapse = " - "), "(delta)")
                   }
                   
                   
                   
                   
                   ########### MAKE THE COMPARISON
                   
                   glue::glue("#>>> Computing differences between GCMs in temperature and precipitation") %>% message()
                   ### Table of differences
                   # Convert rasters to a table with values
                   glue::glue("#   Creating table of differences") %>% message()
                   
                   ##############################  Scaled  ##############################
                   table_diff_scaled <- list()
                   for (v in names(rvs$clim_vars[[1]])){
                     temp_table <- rvs$clim_vars %>%
                       purrr::map(~ .x[[v]]) %>%
                       purrr::map_dfc(~ raster::values(.x)) %>% t() %>% 
                       scale()
                     glue::glue("#   Creating table of differences -2") %>% message()
                     temp_table <- temp_table %>%
                       as.data.frame() %>%
                       tibble::rownames_to_column("GCM") %>%
                       tibble::as_tibble()
                     glue::glue("#   Creating table of differences -3") %>% message()
                     table_diff_scaled[[length(table_diff_scaled) + 1]] <- temp_table
                     names(table_diff_scaled)[length(table_diff_scaled)] <- v
                   }
                   # Calculare means for each GCM and combine in one unique table
                   glue::glue("#   Creating table of differences -4") %>% message()
                   table_diff_scaled <- table_diff_scaled %>%
                     purrr::map_dfc(~ rowMeans(.x[,2:ncol(.x)], na.rm = T)) %>%
                     dplyr::bind_cols(table_diff_scaled[[1]][,1])
                   # Combine temperature and precipitation variables separatedly
                   glue::glue("#   Creating table of differences -5") %>% message()
                   table_scaled <- tibble::tibble(GCM = table_diff_scaled %>%
                                                    dplyr::select(GCM) %>% dplyr::pull(GCM),
                                                  x_axis = table_diff_scaled %>%
                                                    dplyr::select(rvs$bio_vars_x2) %>%
                                                    rowMeans(na.rm = T),
                                                  y_axis = table_diff_scaled %>%
                                                    dplyr::select(rvs$bio_vars_y2) %>%
                                                    rowMeans(na.rm = T)) %>%
                     dplyr::mutate(Distance = raster::pointDistance(c(0, 0),
                                                                    .[,2:3],
                                                                    lonlat = FALSE)) %>%
                     dplyr::arrange(Distance)
                   
                   ## Is within confidence intervals?
                   std_error <- mean(table_scaled$Distance) + sd(table_scaled$Distance)/sqrt(nrow(table_scaled))
                   table_scaled <- table_scaled %>%
                     dplyr::mutate(Within_circle = ifelse(Distance <= std_error, TRUE, FALSE))
                   
                   rvs$table_scaled <- table_scaled
                   # Prepare circle for plot
                   circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){r = diameter / 2
                   tt <- seq(0,2*pi,length.out = npoints)
                   xx <- center[1] + r * cos(tt)
                   yy <- center[2] + r * sin(tt)
                   return(data.frame(x = xx, y = yy))}
                   circle <- circleFun(center = c(0,0), diameter = std_error * 2, npoints = 300)
                   
                   ##############################  Real unscaled  ##############################
                   table_diff_realunscaled <- list()
                   for (v in names(rvs$clim_diff[[1]])){
                     temp_table <- rvs$clim_diff %>%
                       purrr::map(~ .x[[v]]) %>%
                       purrr::map_dfc(~ raster::values(.x)) %>% t()
                     glue::glue("#   Creating table of differences -2") %>% message()
                     temp_table <- temp_table %>%
                       as.data.frame() %>%
                       tibble::rownames_to_column("GCM") %>%
                       tibble::as_tibble()
                     glue::glue("#   Creating table of differences -3") %>% message()
                     table_diff_realunscaled[[length(table_diff_realunscaled) + 1]] <- temp_table
                     names(table_diff_realunscaled)[length(table_diff_realunscaled)] <- v
                   }
                   # Calculare means for each GCM and combine in one unique table
                   glue::glue("#   Creating table of differences -4") %>% message()
                   table_diff_realunscaled <- table_diff_realunscaled %>%
                     purrr::map_dfc(~ rowMeans(.x[,2:ncol(.x)], na.rm = T)) %>%
                     dplyr::bind_cols(table_diff_realunscaled[[1]][,1])
                   # Combine temperature and precipitation variables separatedly
                   glue::glue("#   Creating table of differences -5") %>% message()
                   table_realunscaled <- tibble::tibble(GCM = table_diff_realunscaled %>%
                                                          dplyr::select(GCM) %>% dplyr::pull(GCM),
                                                        x_axis = table_diff_realunscaled %>%
                                                          dplyr::select(rvs$bio_vars_x2) %>%
                                                          rowMeans(na.rm = T),
                                                        y_axis = table_diff_realunscaled %>%
                                                          dplyr::select(rvs$bio_vars_y2) %>%
                                                          rowMeans(na.rm = T)) %>%
                     dplyr::mutate(Distance = raster::pointDistance(c(0, 0),
                                                                    .[,2:3],
                                                                    lonlat = FALSE)) #%>%
                   # dplyr::arrange(Distance)
                   rvs$table_realunscaled <- table_realunscaled
                   
                   #############################################################################
                   
                   
                   ##############################  Unscaled  ##############################
                   glue::glue("#   Creating table of differences -6") %>% message()
                   rvs$clim_vars_uns <- rvs$clim_vars
                   rvs$clim_vars_uns[[length(rvs$clim_vars_uns) + 1]] <- rvs$clim_baseline
                   names(rvs$clim_vars_uns)[length(rvs$clim_vars_uns)] <- "BASELINE"
                   rvs$clim_vars_uns[[length(rvs$clim_vars_uns) + 1]] <- rvs$clim_ens
                   names(rvs$clim_vars_uns)[length(rvs$clim_vars_uns)] <- "ENSEMBLE"
                   
                   
                   table_diff_unscaled <- list()
                   for (v in names(rvs$clim_vars_uns[[1]])){
                     temp_table <- rvs$clim_vars_uns %>%
                       purrr::map(~ .x[[v]]) %>%
                       purrr::map_dfc(~ raster::values(.x)) %>% t()
                     temp_table <- temp_table %>%
                       as.data.frame() %>%
                       tibble::rownames_to_column("GCM") %>%
                       tibble::as_tibble()
                     table_diff_unscaled[[length(table_diff_unscaled) + 1]] <- temp_table
                     names(table_diff_unscaled)[length(table_diff_unscaled)] <- v
                   }
                   
                   # Calculare means for each GCM and combine in one unique table
                   table_diff_unscaled <- table_diff_unscaled %>%
                     purrr::map_dfc(~ rowMeans(.x[,2:ncol(.x)], na.rm = T)) %>%
                     dplyr::bind_cols(table_diff_unscaled[[1]][,1])
                   
                   # table_diff_unscaled <- table_diff_unscaled %>%
                   #   mutate_at(., vars(one_of(paste0("bio_", 1:11))), funs(. / 10))
                   # Combine temperature and precipitation variables separatedly
                   table_unscaled <- tibble::tibble(GCM = table_diff_unscaled %>%
                                                      dplyr::select(GCM) %>% dplyr::pull(GCM),
                                                    x_axis = table_diff_unscaled %>%
                                                      dplyr::select(rvs$bio_vars_x2) %>%
                                                      rowMeans(na.rm = T),
                                                    y_axis = table_diff_unscaled %>%
                                                      dplyr::select(rvs$bio_vars_y2) %>%
                                                      rowMeans(na.rm = T)) %>%
                     dplyr::mutate(Distance = raster::pointDistance(c(0, 0),
                                                                    .[,2:3],
                                                                    lonlat = FALSE)) %>%
                     dplyr::arrange(Distance)
                   rvs$table_unscaled <- table_unscaled
                   
                   ##############################  Deltas  ##############################
                   rvs$clim_delta2 <- rvs$clim_delta
                   rvs$clim_delta2[[length(rvs$clim_delta2) + 1]] <- rvs$clim_delta_ensemble
                   names(rvs$clim_delta2)[length(rvs$clim_delta2)] <- "ENSEMBLE"
                   
                   table_diff_deltas <- list()
                   for (v in names(rvs$clim_delta2[[1]])){
                     temp_table <- rvs$clim_delta2 %>%
                       purrr::map(~ .x[[v]]) %>%
                       purrr::map_dfc(~ raster::values(.x)) %>% t()
                     temp_table <- temp_table %>%
                       as.data.frame() %>%
                       tibble::rownames_to_column("GCM") %>%
                       tibble::as_tibble()
                     table_diff_deltas[[length(table_diff_deltas) + 1]] <- temp_table
                     names(table_diff_deltas)[length(table_diff_deltas)] <- v
                   }
                   
                   # Calculare means for each GCM and combine in one unique table
                   table_diff_deltas <- table_diff_deltas %>%
                     purrr::map_dfc(~ rowMeans(.x[,2:ncol(.x)], na.rm = T)) %>%
                     dplyr::bind_cols(table_diff_deltas[[1]][,1])
                   
                   table_deltas <- tibble::tibble(GCM = table_diff_deltas %>%
                                                    dplyr::select(GCM) %>% dplyr::pull(GCM),
                                                  x_axis = table_diff_deltas %>%
                                                    dplyr::select(rvs$bio_vars_x2) %>%
                                                    rowMeans(na.rm = T),
                                                  y_axis = table_diff_deltas %>%
                                                    dplyr::select(rvs$bio_vars_y2) %>%
                                                    rowMeans(na.rm = T)) %>%
                     dplyr::mutate(Distance = raster::pointDistance(c(0, 0),
                                                                    .[,2:3],
                                                                    lonlat = FALSE)) %>%
                     dplyr::arrange(Distance)
                   
                   table_deltas <- table_deltas %>% 
                     bind_rows(tibble(GCM = "BASELINE", x_axis = 0, y_axis = 0, Distance = 0))
                   
                   rvs$table_deltas <- table_deltas
                   
                   rvs$table_combined <- table_unscaled %>% dplyr::select(-Distance) %>% 
                     left_join(table_deltas %>% dplyr::select(-Distance) %>% 
                                 set_names(c("GCM", paste0("delta_", names(table_deltas %>% dplyr::select(-Distance))[-1]))), by = "GCM")
                   rvs$table_combined2 <- rvs$table_combined
                   names(rvs$table_combined2)[2] <- rvs$xcol_unsc_lab
                   names(rvs$table_combined2)[3] <- rvs$ycol_unsc_lab
                   names(rvs$table_combined2)[4] <- paste0("Delta ", rvs$xcol_unsc_lab)
                   names(rvs$table_combined2)[5] <- paste0("Delta ", rvs$ycol_unsc_lab)
                   
                   ##########################################################################################
                   ############################## Plot of differences in temperature vs precipitation
                   
                   ##############################  Scaled  ##############################
                   
                   incProgress(amount = 0.3, message = "Preparing the plots")
                   # Plot Temp vs prec
                   glue::glue("#   Making a plot to interpretate differences") %>% message()
                   ## Scaled
                   rvs$plot_comp_scaled <- ggplot2::ggplot(rvs$table_scaled)
                   rvs$plot_comp_scaled <- rvs$plot_comp_scaled +
                     ggplot2::geom_path(data = circle, aes(x, y), alpha = .3)
                   rvs$plot_comp_scaled <- rvs$plot_comp_scaled +
                     ggplot2::geom_vline(xintercept = 0, color="grey") +
                     ggplot2::geom_hline(yintercept = 0, color="grey") +
                     ggplot2::geom_point(ggplot2::aes(text = GCM,
                                                      x = x_axis,
                                                      y = y_axis,
                                                      color = factor(Within_circle))) +
                     ggplot2::scale_color_manual(values = c("#ad2f48", "#414685"),
                                                 guide = FALSE) +
                     ggplot2::coord_fixed() +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(legend.position = 'none')
                   # Add proper axis names
                   glue::glue("#   Making a plot to interpretate differences unscaled") %>% message()
                   rvs$plot_comp_scaled <- rvs$plot_comp_scaled +
                     ggplot2::xlab(rvs$xaxis_lab) +
                     ggplot2::ylab(rvs$yaxis_lab)
                   rvs$plot_comp_download_scaled <- rvs$plot_comp_scaled +
                     ggrepel::geom_text_repel(ggplot2::aes(label = GCM,
                                                           x = x_axis,
                                                           y = y_axis),
                                              size = 3)
                   
                   ##############################  Real unscaled  ##############################
                   
                   incProgress(amount = 0.3, message = "Preparing the plots")
                   # Plot Temp vs prec
                   glue::glue("#   Making a plot to interpretate differences") %>% message()
                   ## Scaled
                   rvs$plot_comp_realunscaled <- ggplot2::ggplot(rvs$table_realunscaled)
                   rvs$plot_comp_realunscaled <- rvs$plot_comp_realunscaled +
                     ggplot2::geom_vline(xintercept = 0, color="grey") +
                     ggplot2::geom_hline(yintercept = 0, color="grey") +
                     ggplot2::geom_point(ggplot2::aes(text = GCM,
                                                      x = x_axis,
                                                      y = y_axis)) +
                     ggplot2::scale_color_manual(values = c("#ad2f48", "#414685"),
                                                 guide = FALSE) +
                     # ggplot2::coord_fixed() +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(legend.position = 'none')
                   # Add proper axis names
                   glue::glue("#   Making a plot to interpretate differences unscaled") %>% message()
                   rvs$plot_comp_realunscaled <- rvs$plot_comp_realunscaled +
                     ggplot2::xlab(rvs$xaxis_lab) +
                     ggplot2::ylab(rvs$yaxis_lab)
                   rvs$plot_comp_download_realunscaled <- rvs$plot_comp_realunscaled +
                     ggrepel::geom_text_repel(ggplot2::aes(label = GCM,
                                                           x = x_axis,
                                                           y = y_axis),
                                              size = 3)
                   
                   
                   ##############################  Unscaled  ##############################
                   
                   rvs$plot_comp_unscaled <- ggplot2::ggplot(rvs$table_unscaled %>% filter(GCM != "BASELINE" | GCM != "ENSEMBLE"))
                   rvs$plot_comp_unscaled <- rvs$plot_comp_unscaled +
                     ggplot2::geom_point(ggplot2::aes(text = GCM,
                                                      x = x_axis,
                                                      y = y_axis,
                                                      color = "#414685")) +
                     geom_point(data = rvs$table_unscaled %>% filter(GCM == "BASELINE"), 
                                # aes(x = get(rvs$bio_vars_x2), y = get(rvs$bio_vars_y2)), color = "#d33271") +
                                aes(x = x_axis, y = y_axis), color = "#d33271") +
                     geom_point(data = rvs$table_unscaled %>% filter(GCM == "ENSEMBLE"), 
                                # aes(x = get(rvs$bio_vars_x2), y = get(rvs$bio_vars_y2)), color = "#65dc91") +
                                aes(x = x_axis, y = y_axis), color = "#d33271") +
                     # ggplot2::scale_color_manual(values = c("#414685"), guide = FALSE) +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(legend.position = 'none')
                   # Add proper axis names
                   rvs$plot_comp_unscaled <- rvs$plot_comp_unscaled +
                     ggplot2::xlab(rvs$xaxis_lab) +
                     ggplot2::ylab(rvs$yaxis_lab)
                   rvs$plot_comp_download_unscaled <- rvs$plot_comp_unscaled +
                     ggrepel::geom_text_repel(ggplot2::aes(label = rvs$table_unscaled$GCM,
                                                           x = rvs$table_unscaled$x_axis,
                                                           y = rvs$table_unscaled$y_axis),
                                              size = 3)
                   
                   
                   ##############################  Deltas  ##############################
                   
                   rvs$plot_comp_deltas <- ggplot2::ggplot(rvs$table_deltas %>% filter(GCM != "BASELINE" | GCM != "ENSEMBLE"))
                   rvs$plot_comp_deltas <- rvs$plot_comp_deltas +
                     ggplot2::geom_point(ggplot2::aes(#text = GCM,
                       x = x_axis,
                       y = y_axis,
                       color = "#414685")) +
                     geom_point(data = rvs$table_deltas %>% filter(GCM == "BASELINE"), 
                                # aes(x = get(rvs$bio_vars_x2), y = get(rvs$bio_vars_y2)), color = "#d33271") +
                                aes(x = x_axis, y = y_axis), color = "#d33271") +
                     geom_point(data = rvs$table_deltas %>% filter(GCM == "ENSEMBLE"), 
                                # aes(x = get(rvs$bio_vars_x2), y = get(rvs$bio_vars_y2)), color = "#65dc91") +
                                aes(x = x_axis, y = y_axis), color = "#d33271") +
                     # ggplot2::scale_color_manual(values = c("#414685"), guide = FALSE) +
                     ggplot2::theme_minimal() +
                     ggplot2::theme(legend.position = 'none')
                   # Add proper axis names
                   rvs$plot_comp_deltas <- rvs$plot_comp_deltas +
                     ggplot2::xlab(rvs$xaxis_lab) +
                     ggplot2::ylab(rvs$yaxis_lab)
                   rvs$plot_comp_download_deltas <- rvs$plot_comp_deltas +
                     ggrepel::geom_text_repel(ggplot2::aes(label = rvs$table_deltas$GCM,
                                                           x = rvs$table_deltas$x_axis,
                                                           y = rvs$table_deltas$y_axis),
                                              size = 3)
                   
                   ############### PLOT_LY PLOTS ###############
                   
                   incProgress(amount = 0.3, message = "Preparing the scatterplot of diferences")
                   
                   
                   ##############################  Scaled  ##############################
                   
                   output$plot_temp_prec <- renderPlotly({
                     glue::glue("#> (plotly) Creating temp vs prec plot") %>% message
                     ggplotly(rvs$plot_comp_scaled, 
                              tooltip = c("GCM", "Distance", "x_axis", "y_axis"),
                              height = 600) %>%
                       layout(xaxis = list(scaleanchor = "y",   # iguala escala de ejes
                                           scaleratio = 1),
                              annotations = list(x = rvs$table_scaled$x_axis,    # anotaciones fijas
                                                 y = rvs$table_scaled$y_axis,
                                                 text = rvs$table_scaled$GCM,
                                                 showarrow = T,
                                                 arrowhead = 4,
                                                 arrowsize = 0.4,
                                                 arrowwidth = 0.4,
                                                 textposition = "center left",
                                                 opacity = 0.5,
                                                 ax = 3,
                                                 font = list(family = "Arial",
                                                             size = 10)))
                   })
                   
                   ##############################  Real unscaled  ##############################
                   
                   output$plot_temp_prec_realunscaled <- renderPlotly({
                     glue::glue("#> (plotly) Creating temp vs prec plot") %>% message
                     ggplotly(rvs$plot_comp_realunscaled, 
                              tooltip = c("GCM", "Distance", "x_axis", "y_axis"),
                              # tooltip = c("GCM", "Distance", "bio_1", "bio_12"),
                              height = 600) %>%
                       layout(#xaxis = list(scaleanchor = "y",   # iguala escala de ejes
                         # scaleratio = 1),
                         annotations = list(x = rvs$table_realunscaled$x_axis,    # anotaciones fijas
                                            y = rvs$table_realunscaled$y_axis,
                                            text = rvs$table_realunscaled$GCM,
                                            showarrow = T,
                                            arrowhead = 4,
                                            arrowsize = 0.4,
                                            arrowwidth = 0.4,
                                            textposition = "center left",
                                            opacity = 0.5,
                                            ax = 3,
                                            font = list(family = "Arial",
                                                        size = 10)))
                   })
                   
                   
                   
                   ##############################  Unscaled  ##############################
                   
                   incProgress(amount = 0.3)
                   output$plot_temp_prec_no_scaled <- renderPlotly({
                     glue::glue("#> (plotly unscaled) Creating temp vs prec plot") %>% message
                     
                     plot_ly(width = 1000, height = 700) %>% 
                       add_markers(data = rvs$table_unscaled %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE"), 
                                   x = ~ rvs$table_unscaled %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$x_axis,
                                   y = ~ rvs$table_unscaled %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$y_axis,
                                   text = ~ rvs$table_unscaled %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$GCM,
                                   name = "GCMs") %>% 
                       add_markers(data = rvs$table_unscaled %>% filter(GCM == "ENSEMBLE"), 
                                   x = ~ rvs$table_unscaled %>% filter(GCM == "ENSEMBLE") %>% .$x_axis,
                                   y = ~ rvs$table_unscaled %>% filter(GCM == "ENSEMBLE") %>% .$y_axis,
                                   text = ~ rvs$table_unscaled %>% filter(GCM == "ENSEMBLE") %>% .$GCM,
                                   name = "ENSEMBLE") %>% 
                       add_markers(data = rvs$table_unscaled %>% filter(GCM == "BASELINE"), 
                                   x = ~ rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% .$x_axis,
                                   y = ~ rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% .$y_axis,
                                   text = ~ rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% .$GCM,
                                   name = "BASELINE") %>% 
                       layout(
                         xaxis = list(title = rvs$xaxis_lab,
                                      zeroline = FALSE),
                         yaxis = list(title = rvs$yaxis_lab,
                                      zeroline = FALSE),    
                         shapes = list(list(type = "line", y0 = rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% pull(y_axis),
                                            y1 = rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% pull(y_axis),
                                            x0=min(rvs$table_unscaled %>% pull(x_axis)), x1=max(rvs$table_unscaled %>% pull(x_axis)),
                                            line=list(width = 0.4, opacity = 0.4)),
                                       list(type = "line", x0 = rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% pull(x_axis),
                                            x1 = rvs$table_unscaled %>% filter(GCM == "BASELINE") %>% pull(x_axis),
                                            y0=min(rvs$table_unscaled %>% pull(y_axis)), y1=max(rvs$table_unscaled %>% pull(y_axis)),
                                            line=list(width = 0.4, opacity = 0.4))),
                         annotations = list(x = rvs$table_unscaled$x_axis,    # anotaciones fijas
                                            y = rvs$table_unscaled$y_axis,
                                            text = rvs$table_unscaled$GCM,
                                            showarrow = T, arrowhead = 4, arrowsize = 0.4, arrowwidth = 0.4,
                                            textposition = "center left",
                                            opacity = 0.5,
                                            ax = 3,
                                            font = list(family = "Arial",
                                                        size = 11)))
                   }) 
                   
                   ##############################  Deltas  ##############################
                   
                   incProgress(amount = 0.3)
                   output$plot_temp_prec_deltas <- renderPlotly({
                     glue::glue("#> (plotly deltas) Creating temp vs prec plot") %>% message
                     plot_ly(width = 1000, height = 700) %>% 
                       add_markers(data = rvs$table_deltas %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE"), 
                                   x = ~ rvs$table_deltas %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$x_axis,
                                   y = ~ rvs$table_deltas %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$y_axis,
                                   text = ~ rvs$table_deltas %>% filter(GCM != "ENSEMBLE" & GCM != "BASELINE") %>% .$GCM,
                                   name = "GCMs") %>% 
                       add_markers(data = rvs$table_deltas %>% filter(GCM == "ENSEMBLE"), 
                                   x = ~ rvs$table_deltas %>% filter(GCM == "ENSEMBLE") %>% .$x_axis,
                                   y = ~ rvs$table_deltas %>% filter(GCM == "ENSEMBLE") %>% .$y_axis,
                                   text = ~ rvs$table_deltas %>% filter(GCM == "ENSEMBLE") %>% .$GCM,
                                   name = "ENSEMBLE") %>% 
                       add_markers(data = rvs$table_deltas %>% filter(GCM == "BASELINE"), 
                                   x = ~ rvs$table_deltas %>% filter(GCM == "BASELINE") %>% .$x_axis,
                                   y = ~ rvs$table_deltas %>% filter(GCM == "BASELINE") %>% .$y_axis,
                                   text = ~ rvs$table_deltas %>% filter(GCM == "BASELINE") %>% .$GCM,
                                   name = "BASELINE") %>% 
                       layout(
                         xaxis = list(title = rvs$xaxis_lab,
                                      line=list(width = 0.4, opacity = 0.3)
                                      # zeroline = FALSE
                         ),
                         yaxis = list(title = rvs$yaxis_lab,
                                      line=list(width = 0.4, opacity = 0.3)
                                      # zeroline = FALSE
                         ),
                         annotations = list(x = rvs$table_deltas$x_axis,    # anotaciones fijas
                                            y = rvs$table_deltas$y_axis,
                                            text = rvs$table_deltas$GCM,
                                            showarrow = T, arrowhead = 4, arrowsize = 0.4, arrowwidth = 0.4,
                                            textposition = "center left",
                                            opacity = 0.5,
                                            ax = 3,
                                            font = list(family = "Arial",
                                                        size = 11)))
                     
                     # plot_ly(data = rvs$table_deltas, 
                     #         x = ~ rvs$table_deltas$x_axis,
                     #         y = ~ rvs$table_deltas$y_axis,
                     #         text = ~ rvs$table_deltas$GCM,
                     #         width = 900, height = 800) %>% 
                     #   add_markers() %>% 
                     #   layout(
                     # xaxis = list(title = rvs$xaxis_lab,
                     #              zeroline = FALSE),
                     # yaxis = list(title = rvs$yaxis_lab,
                     #              zeroline = FALSE),
                     #     annotations = list(x = rvs$table_deltas$x_axis,    # anotaciones fijas
                     #                        y = rvs$table_deltas$y_axis,
                     #                        text = rvs$table_deltas$GCM,
                     #                        showarrow = T, arrowhead = 4, arrowsize = 0.4, arrowwidth = 0.4,
                     #                        textposition = "center left",
                     #                        opacity = 0.5,
                     #                        ax = 3,
                     #                        font = list(family = "Arial",
                     #                                    size = 11)))
                   })
                   
                   
                   ############## KABLE TABLES ##############
                   
                   
                   output$comparison_table <- function(){
                     glue::glue("#> Creating temp vs prec table") %>% message
                     table_scaled2 <- rvs$table_scaled
                     names(table_scaled2)[2] <- rvs$xcol_sc_lab
                     names(table_scaled2)[3] <- rvs$ycol_sc_lab
                     
                     table_scaled2 %>%
                       dplyr::select(-Within_circle) %>%
                       knitr::kable("html") %>%
                       kable_styling("striped", full_width = F)
                   }
                   
                   # req(rvs$table_combined)
                   # rvs$table_combined2 <- rvs$table_combined
                   # names(rvs$table_combined2)[2] <- rvs$xcol_unsc_lab
                   # names(rvs$table_combined2)[3] <- rvs$ycol_unsc_lab
                   # names(rvs$table_combined2)[4] <- paste0("Delta ", rvs$xcol_unsc_lab)
                   # names(rvs$table_combined2)[5] <- paste0("Delta ", rvs$ycol_unsc_lab)
                   
                   output$comparison_table_realunscaled <- function(){
                     glue::glue("#> Creating temp vs prec table") %>% message
                     table_realunscaled2 <- rvs$table_realunscaled
                     names(table_realunscaled2)[2] <- rvs$xcol_sc_lab
                     names(table_realunscaled2)[3] <- rvs$ycol_sc_lab
                     
                     table_realunscaled2 %>%
                       dplyr::select(-Within_circle) %>%
                       knitr::kable("html") %>%
                       kable_styling("striped", full_width = F)
                   }
                   
                   output$comparison_table_no_scaled <- function(){
                     glue::glue("#> Creating temp vs prec table") %>% message
                     # table_unscaled2 <- rvs$table_unscaled
                     # names(table_unscaled2)[2] <- rvs$xcol_unsc_lab
                     # names(table_unscaled2)[3] <- rvs$ycol_unsc_lab
                     
                     
                     rvs$table_combined2 %>% 
                       # table_unscaled2 %>%
                       #   dplyr::select(-Distance) %>%
                       knitr::kable("html") %>%
                       kable_styling("striped", full_width = F)
                   }
                   # output$comparison_table_delta <- function(){
                   #   glue::glue("#> Creating temp vs prec table (delta)") %>% message
                   #   # table_deltas2 <- rvs$table_deltas
                   #   # names(table_deltas2)[2] <- rvs$xcol_delta_lab
                   #   # names(table_deltas2)[3] <- rvs$ycol_delta_lab
                   #   # 
                   #   # table_combined2 <- rvs$table_combined
                   #   # names(table_combined2)[2] <- rvs$xcol_unsc_lab
                   #   # names(table_combined2)[3] <- rvs$ycol_unsc_lab
                   #   # names(table_combined2)[4] <- paste0("Delta ", rvs$xcol_unsc_lab)
                   #   # names(table_combined2)[5] <- paste0("Delta ", rvs$ycol_unsc_lab)
                   #   
                   #   rvs$table_combined2 %>%
                   #     # table_deltas2 %>%
                   #     #   dplyr::select(-Distance) %>%
                   #     knitr::kable("html") %>%
                   #     kable_styling("striped", full_width = F)
                   # }
                   
                   ############## DOWNLOAD BUTTONS FOR TABLES AND FIGURES ##############
                   # Download plots
                   output$download_prec_temp_scaled <- downloadHandler(
                     filename = function() {
                       paste0("plot_temp_prec_", input$year_type, "_", input$rcp_type, ".png")
                     },
                     content = function(file) {
                       ggsave(file,
                              plot = rvs$plot_comp_download_scaled,
                              device = "png")#,
                       # width = 300, height = 300)
                     }
                   )
                   output$download_prec_temp_realunscaled <- downloadHandler(
                     filename = function() {
                       paste0("plot_temp_prec_", input$year_type, "_", input$rcp_type, ".png")
                     },
                     content = function(file) {
                       ggsave(file,
                              plot = rvs$plot_comp_download_realunscaled,
                              device = "png")#,
                       # width = 300, height = 300)
                     }
                   )
                   output$download_prec_temp_unscaled <- downloadHandler(
                     filename = function() {
                       paste0("plot_temp_prec_", input$year_type, "_", input$rcp_type, ".png")
                     },
                     content = function(file) {
                       ggsave(file,
                              plot = rvs$plot_comp_download_unscaled,
                              device = "png")#,
                       # width = 300, height = 300)
                     }
                   )
                   output$download_prec_temp_deltas <- downloadHandler(
                     filename = function() {
                       paste0("plot_temp_prec_", input$year_type, "_", input$rcp_type, ".png")
                     },
                     content = function(file) {
                       ggsave(file,
                              plot = rvs$plot_comp_download_deltas,
                              device = "png")#,
                       # width = 300, height = 300)
                     }
                   )
                   
                   # Download button
                   output$download_comparison_table <- downloadHandler(
                     filename = function() {
                       paste0("Comparison_tableA_", input$year_type, "_", input$rcp_type, ".csv")
                     },
                     content = function(file) {
                       write.csv(rvs$table_scaled %>%
                                   mutate(confidence95 = Within_circle) %>% 
                                   dplyr::select(-Within_circle), 
                                 file, row.names = F)
                     }
                   )
                   output$download_comparison_table_realunscaled <- downloadHandler(
                     filename = function() {
                       paste0("Comparison_tableA_", input$year_type, "_", input$rcp_type, ".csv")
                     },
                     content = function(file) {
                       write.csv(rvs$table_realunscaled,# %>%
                                 # mutate(confidence95 = Within_circle) %>% 
                                 # dplyr::select(-Within_circle), 
                                 file, row.names = F)
                     }
                   )
                   output$download_comparison_table_no_scaled <- downloadHandler(
                     filename = function() {
                       paste0("Comparison_tableB_", input$year_type, "_", input$rcp_type, ".csv")
                     },
                     content = function(file) {
                       write.csv(rvs$table_combined2, 
                                 file, row.names = F)
                     }
                   )
                   
                 })
  })
  
  
  
  ############### GCM DIFFERENCES ###############
  
  observeEvent(input$go, {
    output$gcm_differences <- renderUI({
      glue::glue("#> Creating GCM difference plots") %>% message
      # plot_gcm_differences <- plot_gcm_differences(rvs$clim_comp,
      #                                              country_borders = FALSE)
      ###################################
      plot_gcm_differences <- list()
      for (b in names(rvs$clim_diff[[1]])){
        # Load layers
        scenario_data <- rvs$clim_diff %>%
          purrr::map(~ raster::subset(.x, b)) %>%
          raster::stack()
        
        long_name <- case_when(b == "bio_1" ~ "(bio 1) Annual Mean Temperature",
                               b == "bio_2" ~ "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))",
                               b == "bio_3" ~ "(bio 3) Isothermality (BIO2/BIO7) (* 100)",
                               b == "bio_4" ~ "(bio 4) Temperature Seasonality (standard deviation *100)",
                               b == "bio_5" ~  "(bio 5) Max Temperature of Warmest Month",
                               b == "bio_6" ~  "(bio 6) Min Temperature of Coldest Month",
                               b == "bio_7" ~  "(bio 7) Temperature Annual Range (BIO5-BIO6)",
                               b == "bio_8" ~  "(bio 8) Mean Temperature of Wettest Quarter",
                               b == "bio_9" ~  "(bio 9) Mean Temperature of Driest Quarter",
                               b == "bio_10" ~ "(bio 10) Mean Temperature of Warmest Quarter",
                               b == "bio_11" ~ "(bio 11) Mean Temperature of Coldest Quarter",
                               b == "bio_12" ~ "(bio 12) Annual Precipitation",
                               b == "bio_13" ~ "(bio 13) Precipitation of Wettest Month",
                               b == "bio_14" ~ "(bio 14) Precipitation of Driest Month",
                               b == "bio_15" ~ "(bio 15) Precipitation Seasonality (Coefficient of Variation)",
                               b == "bio_16" ~ "(bio 16) Precipitation of Wettest Quarter",
                               b == "bio_17" ~ "(bio 17) Precipitation of Driest Quarter",
                               b == "bio_18" ~ "(bio 18) Precipitation of Warmest Quarter",
                               b == "bio_19" ~ "(bio 19) Precipitation of Coldest Quarter")
        
        # Define range of values
        range_values <- c(min(values(scenario_data), na.rm = T),
                          max(values(scenario_data), na.rm = T))
        range_values <- c(- max(abs(range_values)), max(abs(range_values)))
        range_by <- (range_values[2] - range_values[1]) / 100
        
        # Plot
        scenario_plot <- scenario_data %>%
          rasterVis::levelplot(main = paste0(long_name, " - Spatial difference to ensemble prediction"),
                               par.settings = rasterVis::RdBuTheme(region = rev(RColorBrewer::brewer.pal(9, 'RdBu'))),
                               # layout = lp_layout,
                               at = seq(range_values[[1]],
                                        range_values[[2]],
                                        by = range_by))
        if (input$add_layer3 == "countries"){
          sp_add <- world_map %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        if (input$add_layer3 == "biomes"){
          sp_add <- biomes_sp %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        if (input$add_layer3 == "ecoregions"){
          sp_add <- ecorregions_sp %>% crop(extent(rvs$clim_vars[[1]]))
          scenario_plot <- scenario_plot +
            latticeExtra::layer(sp::sp.polygons(sp_add,
                                                lwd = 3,
                                                alpha = 0.8,
                                                col = "black"),
                                data = list(sp_add = sp_add))
        }
        
        plot_gcm_differences[[length(plot_gcm_differences) + 1]] <- scenario_plot
        names(plot_gcm_differences)[length(plot_gcm_differences)] <- b
        rm(scenario_plot)
      }
      ###################################
      rvs$plot_gcm_differences <- plot_gcm_differences
      
      glue::glue("#> Manipulating plot differences list") %>% message
      for (i in 1:length(plot_gcm_differences)) {
        local({
          my_i <- i
          plotname <- paste0("plot_differences", my_i)
          output[[plotname]] <- renderPlot({
            plot_gcm_differences[[my_i]]
          }, height = 1000)
        })
      }
      
      glue::glue("#> Printing differences plot") %>% message
      plot_gcm_differences_ui <- lapply(1:length(plot_gcm_differences), function(i) {
        plotname <- paste0("plot_differences", i)
        plotOutput(plotname,
                   height = "100%")
      })
      do.call(tagList, plot_gcm_differences_ui) %>%     # Convert the list to a tagList
        withSpinner(type = 7, color = "#5fbc93")
    })
  })
  
  
  
  ############################# MAP DETAIL #############################
  
  ### Available GCMs - dynamically change
  # For future - maps_fut_detail" tab (visualization of differences)
  output$map_fut_detail_gcm <- renderUI({
    glue::glue("# Getting list of available GCMs") %>% message
    scenarios <- list.files(paste0("clim_data/ccafs/rds"), pattern = input$res_sel) %>% 
      dplyr::as_tibble() %>%
      magrittr::set_names("GCM") %>%
      tidyr::separate(col = GCM, into = c("gcm_", "resolution_", "year_", "rcp_", "borrar"), sep = "\\.") %>% dplyr::select(-borrar) %>%
      mutate(year_ = year_ %>% str_replace_all("year", ""),
             rcp_ = rcp_ %>% str_replace_all("rcp", ""))
    glue::glue("#   - Available GCMs with current setting") %>% message
    available_gcms <- scenarios %>%
      filter(resolution_ == input$res_sel) %>%
      filter(year_ == input$year_type) %>%
      filter(rcp_ == input$rcp_type %>% str_replace("rcp", "")) %>%
      pull(gcm_)
    glue::glue("#   - Generating GCMs options") %>% message
    selectInput(inputId = "map_fut_detail_gcm", label = "Select a GCM",
                choices = available_gcms)
  })
  
  output$map_fut_detail_bio <- renderUI({
    selectInput(inputId = "map_fut_detail_bio",
                label = "Select a bioclimatic variable",
                choices = rvs$bio_vars_all)
  })
  
  ### create map for visualization of future differences
  map_fut_det <- leaflet(world_sf) %>%
    setView(0, 0, zoom = 2) %>% 
    addProviderTiles("Esri.WorldPhysical", group = "Relieve") %>% 
    addTiles(options = providerTileOptions(noWrap = TRUE), group = "Countries") %>%
    addLayersControl(baseGroups = c("Relieve", "Countries"),
                     options = layersControlOptions(collapsed = FALSE))
  
  output$map_fut_detail <- renderLeaflet(map_fut_det)
  # create map proxy to make further changes to existing map
  map_fut_detail <- leafletProxy("map_fut_detail")
  map_fut_detail %>%
    addProviderTiles("Esri.WorldPhysical")
  
  ### Display selected map
  observeEvent({input$map_fut_detail_gcm
    input$map_fut_detail_bio}, {
      req(rvs$clim_diff)         # Stop it from crashing when no result has been produced         
      
      # if(input$add_map == TRUE){
      glue::glue("#>  - ADDING RASTER TO LEAFLET") %>% message
      
      ## Subset raster to map
      rvs$diff_layer <- rvs$clim_diff[[input$map_fut_detail_gcm]][[input$map_fut_detail_bio]]
      
      ## Range of values and color palette from them
      rvs$diff_stack <- rvs$clim_diff %>% 
        purrr::map(~ .x[[input$map_fut_detail_bio]]) %>% 
        raster::stack()
      glue::glue("#>   Calculating data limits") %>% message
      range_values <- c(min(values(rvs$diff_stack), na.rm = T),
                        max(values(rvs$diff_stack), na.rm = T))
      range_values <- c(- max(abs(range_values)), max(abs(range_values)))
      
      glue::glue("#>   Making color palette") %>% message
      pal <- colorNumeric(palette = c("#c0002d", "#f8f5f5", "#0069a8"), 
                          domain = range_values,
                          na.color = "transparent",
                          reverse = TRUE)
      
      # Add polygons to leaflet
      glue::glue("#>   Including the raster to leaflet") %>% message
      map_fut_detail %>%
        clearControls() %>%    # Refreshes the legend
        clearGroup("map_fut_detail") %>%
        fitBounds(lng1 = extent(rvs$diff_layer)[1],    # ponerse alrededor de la capa
                  lat1 = extent(rvs$diff_layer)[3], 
                  lng2 = extent(rvs$diff_layer)[2], 
                  lat2 = extent(rvs$diff_layer)[4]) %>% 
        addRasterImage(rvs$diff_layer,
                       colors = pal,
                       opacity = 0.8,
                       group = "map_fut_detail") %>%
        addLegend(pal = pal, 
                  values = range_values,
                  group = "map_fut_detail",
                  labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      
      rvs$allow_download_layer_fut_detail <- TRUE  # Signal to permit download
    })
  
  ### Download layer
  output$download_map_fut_detail <- downloadHandler(
    filename = function() {
      paste0("gcm_compareR_", input$map_fut_detail_gcm, "_", input$year_type, "_", input$rcp_type, "_", input$map_fut_detail_bio, "_", input$res_sel, ".", input$download_raster_format)
    },
    content = function(file) {
      writeRaster(rvs$diff_layer, 
                  file,
                  format = case_when(input$download_raster_format == "grd" ~ "raster",
                                     input$download_raster_format == "tif" ~ "GTiff"),
                  overwrite = TRUE)
    }
  )
  # Disable it until a layer is shown
  shinyjs::disable("download_raster_format")
  observeEvent(rvs$allow_download_layer_fut_detail, {
    shinyjs::enable("download_raster_format")
  })
  shinyjs::disable("download_mapIII")
  observeEvent(rvs$allow_download_layer_fut_detail, {
    shinyjs::enable("download_mapIII")
  })
  
  
  
  
  ##########################################
  ### Tab 5 -Download report
  ##########################################
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      withProgress(message = 'Preparing the report',
                   detail = NULL,
                   value = 0, {
                     incProgress(amount = 0.1, message = 'Recovering all data')
                     # Copy the report file to a temporary directory before processing it, in
                     # case we don't have write permissions to the current working dir (which
                     # can happen when deployed).
                     tempReport <- file.path(tempdir(), "report.Rmd")
                     file.copy(from = "Rmd/report.Rmd", to = tempReport, overwrite = TRUE)
                     file.copy(from = "Rmd/biblio.bib", to = tempdir(), overwrite = TRUE)  # Copy to that temp directory also bibliography, or it will not by find by render()
                     
                     # Set up parameters to pass to Rmd document
                     params <- list(Year = input$year_type,
                                    rcp = input$rcp_type,
                                    Variables = input$bio_vars,
                                    maptype = input$extent_type,
                                    Coords = rvs$saved_bbox,
                                    countries = input$ext_name_country,
                                    biomes = input$ext_name_biomes,
                                    ecoregions = input$ext_name_ecorregions,
                                    plot_uns = rvs$plot_comp_download_unscaled,
                                    plot_delta = rvs$plot_comp_download_deltas,
                                    plot_sc = rvs$plot_comp_download_scaled,
                                    plot_realuns = rvs$plot_temp_prec_realunscaled,
                                    map_gcm = rvs$plot_gcm_pattern,
                                    map_delta = rvs$plot_delta_pattern,
                                    map_dif = rvs$plot_gcm_differences
                     )
                     
                     # Knit the document, passing in the `params` list, and eval it in a
                     # child of the global environment (this isolates the code in the document
                     # from the code in this app).
                     incProgress(amount = 0.3, message = 'Printing the pdf')
                     rmarkdown::render(tempReport, output_file = file,
                                       params = params,
                                       envir = new.env(parent = globalenv()))
                   })
    }
  )
  
}
