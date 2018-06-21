library(shiny)
library(shinythemes)

library(tidyverse)
library(devtools)
library(kableExtra)

#devtools::install_github("javierfajnolla/gcmcompareR")
#library(gcmcompareR)

library(raster)

library(shinyjs)
library(shinycssloaders)
library(shinysky)
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
library(leaflet)
print(getwd())
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



##############################################################################################################################################################################
########################################      UI       ######################################################################################################################
##############################################################################################################################################################################


# Define UI for application that draws a histogram
ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    theme = shinytheme("paper"),    #"lumen
    id = "navbar",
    title = "GCM compareR",
    position = "fixed-top",
    selected = "scenario",
    
    # Main page tabs
    tags$style(type="text/css", 
               'body {padding-top: 85px;}',
               '.navbar { background-color: #5bbd92;
               font-family: Arial;
               font-size: 16px;
               color: #f5f5f5; }',
               '.navbar-default .navbar-brand {color: #f5f5f5;}'
    ),
    # Tabs
    tags$style(HTML(".tabbable > .nav > li > a              {background-color: #eeeeee;  color:black}
                    .tabbable > .nav > li[class=active] > a {background-color: #5fbc93; color:white}")),
    
    ### Intro ###
    tabPanel(title = "INTRO",
             value = "intro",
             # icon = icon("book"),
             
             sidebarPanel(wellPanel(
               h5("A web application from:"),
               div(img(src = "marquet_lab.svg", width = 200), style = "text-align: center;"),
               hr(),
               div(img(src = "sparc.svg", width = 250), style = "text-align: center;"),
               hr(),
               h5("With the support of:"),
               div(img(src = "gef.png", width = 100, style = "text-align: center;"), 
                   img(src = "ieb.png", width = 160), style = "text-align: center;")
             )
             ),
             
             mainPanel(
               tabsetPanel(id = "Intro_panel",
                           tabPanel("Intro",
                                    includeHTML("Rmd/intro_main.html")),
                           
                           tabPanel("Workflow",
                                    includeMarkdown("Rmd/intro_tab.Rmd")),
                           
                           tabPanel("About",
                                    includeMarkdown("Rmd/intro_about.Rmd"),
                                    img(src = "marquet_lab.svg", align = "left", width = 200),
                                    img(src = "sparc.svg", align = "left", width = 250)
                           )
               )
             )
    ),
    
    ### Scenario definition ###
    tabPanel(title = "SCENARIO",
             value = "scenario",
             # icon = icon("globe"),
             
             sidebarPanel(
               
               h4("SELECT A SCENARIO"),
               hr(),
               
               conditionalPanel("input.map_panel != 'scenario_individual'",
                                
                                ############## Global Circulation Models (GCMs)
                                div(style="display:inline-block", h5("Global Circulation Models (GCMs)")),
                                # h6(""),
                                
                                div(style="display:inline-block",shiny::actionButton(inputId = "GCMs_button", 
                                                                                     label = "",
                                                                                     icon = icon("angle-down", class = "fas"))),
                                
                                hidden(uiOutput("selection_gcms")),
                                textOutput("selected_gcms"),
                                
                                #############
                                hr(),
                                
                                ############# Climate Change Scenario
                                div(style="display:inline-block", h5("Climate Change Scenario")),
                                div(style="display:inline-block", shiny::actionButton(inputId = "CC_scenario",
                                                                                      label = NULL,
                                                                                      icon = icon("angle-down", class = "fas"))),
                                
                                # Year
                                hidden(radioButtons("year_type", "Year",
                                                    inline = TRUE,
                                                    c("2050" = "2050",
                                                      "2070" = "2070"),
                                                    selected = "2070")),
                                
                                # RCP
                                hidden(radioButtons("rcp_type", "Representative Concentration Pathway (RCP)",
                                                    inline = TRUE,
                                                    c("RCP 2.6" = "rcp26",
                                                      "RCP 4.5" = "rcp45",
                                                      "RCP 6.0" = "rcp60",
                                                      "RCP 8.5" = "rcp85"),
                                                    selected = "rcp45")),
                                
                                # Resolution of climatic data
                                hidden(radioButtons(inputId = "res_sel", 
                                                    label = "Climatic data resolution",
                                                    inline = TRUE,
                                                    choices = c("10 arc-min" = "10m",
                                                                "5 arc-min" = "5m"),
                                                    selected = "10m")),
                                textOutput("selected_scenario"),
                                hr(),
                                
                                ############## Type of Comparison
                                div(style="display:inline-block", h5("Type of Comparison")),
                                div(style="display:inline-block", shiny::actionButton(inputId = "Compar_type",
                                                                                      label = NULL,
                                                                                      icon = icon("angle-down", class = "fas"))),
                                
                                hidden(radioButtons(inputId = "compare_type", 
                                                    label = "Type of comparison",
                                                    inline = TRUE,
                                                    choices = c("1bio X 1bio " = "bio_bio",
                                                                "Multiple comparison" = "bio_several"),
                                                    selected = "bio_bio")),
                                
                                conditionalPanel(condition = "input.compare_type == 'bio_bio'",
                                                 hidden(uiOutput("compare_type_bio_bio"))),
                                
                                conditionalPanel(condition = "input.compare_type == 'bio_several'",
                                                 hidden(uiOutput("compare_type_bio_several"))),
                                
                                textOutput("selected_comparison"),
                                hr(),
                                
                                
                                ### STUDY AREA
                                
                                h5("Study Area"),
                                # Drawing a extent
                                radioButtons(inputId = "extent_type", 
                                             label = NULL,
                                             choices = c("Select drawing a rectangle over the map (press square icon)" = "map_draw",
                                                         "Select by country/countries" = "map_country",
                                                         "Select by biome(s)" = "map_biomes",
                                                         "Select by ecorregion(s)" = "map_ecorregions",
                                                         "Enter bounding-box coordinates" = "map_bbox"),
                                             selected = "map_draw"),
                                
                                # Options for entering country names
                                conditionalPanel(condition = "input.extent_type == 'map_country'",
                                                 selectInput("ext_name_country", "Enter country name(s)",
                                                             c("Afghanistan", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua", "Argentina", "Armenia", "Aruba", "Ascension Island", "Australia", "Austria", "Azerbaijan", "Azores", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Barbuda", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bonaire", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Canada", "Canary Islands", "Cape Verde", "Cayman Islands", "Central African Republic", "Chad", "Chagos Archipelago", "Chile", "China", "Christmas Island", "Cocos Islands", "Colombia", "Comoros", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Curacao", "Cyprus", "Czech Republic", "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern and Antarctic Lands", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Greenland", "Grenada", "Grenadines", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Madeira Islands", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "Nevis", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea", "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar", "Republic of Congo", "Reunion", "Romania", "Russia", "Rwanda", "Saba", "Saint Barthelemy", "Saint Helena", "Saint Kitts", "Saint Lucia", "Saint Martin", "Saint Pierre and Miquelon", "Saint Vincent", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Siachen Glacier", "Sierra Leone", "Singapore", "Sint Eustatius", "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Georgia", "South Korea", "South Sandwich Islands", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Tobago", "Togo", "Tonga", "Trinidad", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", "Uganda", "UK", "Ukraine", "United Arab Emirates", "Uruguay", "USA", "Uzbekistan", "Vanuatu", "Vatican", "Venezuela", "Vietnam", "Virgin Islands, British", "Virgin Islands, US", "Wallis and Futuna", "Western Sahara", "Yemen", "Zambia", "Zimbabwe"), multiple = TRUE, selected = NULL)),
                                # Options for entering biome names
                                conditionalPanel(condition = "input.extent_type == 'map_biomes'",
                                                 selectInput("ext_name_biomes", "Enter biome(s) name(s)",
                                                             c("Boreal Forests/Taiga", "Deserts & Xeric Shrublands", "Flooded Grasslands & Savannas", 
                                                               "Mangroves", "Mediterranean Forests, Woodlands & Scrub", "Montane Grasslands & Shrublands", 
                                                               "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", 
                                                               "Temperate Grasslands, Savannas & Shrublands", "Tropical & Subtropical Coniferous Forests", 
                                                               "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Grasslands, Savannas & Shrublands", 
                                                               "Tropical & Subtropical Moist Broadleaf Forests", "Tundra"), multiple = TRUE, selected = NULL)),
                                # Options for entering ecorregion names
                                conditionalPanel(condition = "input.extent_type == 'map_ecorregions'",
                                                 selectInput("ext_name_ecorregions", "Enter ecorregion(s) name(s)",
                                                             c("Adelie Land tundra", "Admiralty Islands lowland rain forests", "Aegean and Western Turkey sclerophyllous and mixed forests", "Afghan Mountains semi-desert", "Ahklun and Kilbuck Upland Tundra", "Al-Hajar foothill xeric woodlands and shrublands", "Al-Hajar montane woodlands and shrublands", "Alai-Western Tian Shan steppe", "Alashan Plateau semi-desert", "Alaska-St. Elias Range tundra", "Alaska Peninsula montane taiga", "Albany thickets", "Alberta-British Columbia foothills forests", "Albertine Rift montane forests", "Aldabra Island xeric scrub", "Aleutian Islands tundra", "Allegheny Highlands forests", "Alps conifer and mixed forests", "Altai alpine meadow and tundra", "Altai montane forest and forest steppe", "Altai steppe and semi-desert", "Alto Paraná Atlantic forests", "Amazon-Orinoco-Southern Caribbean mangroves", "Amsterdam-Saint Paul Islands temperate grasslands", "Amur meadow steppe", "Anatolian conifer and deciduous mixed forests", "Andaman Islands rain forests", "Angolan montane forest-grassland", "Angolan mopane woodlands", "Angolan scarp savanna and woodlands", "Angolan wet miombo woodlands", "Antipodes Subantarctic Islands tundra", "Appalachian-Blue Ridge forests", "Appalachian mixed mesophytic forests", "Appalachian Piedmont forests", "Appenine deciduous montane forests", "Apure-Villavicencio dry forests", "Arabian sand desert", "Araucaria moist forests", "Araya and Paria xeric scrub", "Arctic coastal tundra", "Russian Arctic desert", "Arctic foothills tundra", "Arizona Mountains forests", "Arnhem Land tropical savanna", "Ascension scrub and grasslands", "Atacama desert", "Atlantic Coast restingas", "Saharan Atlantic coastal desert", "Atlantic coastal pine barrens", "Congolian coastal forests", "European Atlantic mixed forests", "Australian Alps montane grasslands", "Azerbaijan shrub desert and steppe", "Azores temperate mixed forests", "Badghyz and Karabil semi-desert", "Bahamian-Antillean mangroves", "Bahia coastal forests", "Bahia interior forests", "Baja California desert", "Bajío dry forests", "Balkan mixed forests", "Balsas dry forests", "Baltic mixed forests", "Baluchistan xeric woodlands", "Banda Sea Islands moist deciduous forests", "Belizian pine savannas", "Beni savanna", "Russian Bering tundra", "Beringia lowland tundra", "Beringia upland tundra", "Bermuda subtropical conifer forests", "Biak-Numfoor rain forests", "Blue Mountains forests", "Bohai Sea saline meadow", "Bolivian montane dry forests", "Bolivian Yungas", "Borneo lowland rain forests", "Borneo peat swamp forests", "Brahmaputra Valley semi-evergreen forests", "Brigalow tropical savanna", "British Columbia coastal conifer forests", "Brooks-British Range tundra", "Buru rain forests", "Caatinga", "Caatinga Enclaves moist forests", "Caledon conifer forests", "California Central Valley grasslands", "California coastal sage and chaparral", "California interior chaparral and woodlands", "California montane chaparral and woodlands", "Cameroon Highlands forests", "Campos Rupestres montane savanna", "Canadian Aspen forests and parklands", "Canadian Low Arctic tundra", "Canary Islands dry woodlands and forests", "Cantabrian mixed forests", "Canterbury-Otago tussock grasslands", "Cape Verde Islands dry forests", "Cape York Peninsula tropical savanna", "Caqueta moist forests", "Cardamom Mountains rain forests", "Caribbean shrublands", "Carnarvon xeric shrublands", "Carolines tropical moist forests", "Carpathian montane forests", "Carpentaria tropical savanna", "Caspian Hyrcanian mixed forests", "Caspian lowland desert", "Catatumbo moist forests", "Cauca Valley dry forests", "Cauca Valley montane forests", "Caucasus mixed forests", "Cayos Miskitos-San Andrés and Providencia moist forests", "Celtic broadleaf forests", "Central Afghan Mountains xeric woodlands", "Central African mangroves", "Central American Atlantic moist forests", "Central American dry forests", "Central American montane forests", "Central American pine-oak forests", "Central Anatolian steppe", "Central Anatolian steppe and woodlands", "Central-Southern Cascades Forests", "Central-Southern US mixed grasslands", "Central Andean dry puna", "Central Andean puna", "Central Andean wet puna", "Central Asian northern desert", "Central Asian riparian woodlands", "Central Asian southern desert", "Central British Columbia Mountain forests", "Central bushveld", "Central Canadian Shield forests", "Central China Loess Plateau mixed forests", "Central Congolian lowland forests", "Central Deccan Plateau dry deciduous forests", "Central European mixed forests", "Central US forest-grasslands transition", "Central Indochina dry forests", "Central Korean deciduous forests", "Central Mexican matorral", "Central Pacific Northwest coastal forests", "Central Persian desert basins", "Central Polynesian tropical moist forests", "Central Range Papuan montane rain forests", "Papuan Central Range sub-alpine grasslands", "Central Ranges xeric scrub", "Central South Antarctic Peninsula tundra", "Central Tallgrass prairie", "Central Tibetan Plateau alpine steppe", "Central Zambezian wet miombo woodlands", "Cerrado", "Changbai Mountains mixed forests", "Changjiang Plain evergreen forests", "Chao Phraya freshwater swamp forests", "Chao Phraya lowland moist deciduous forests", "Chatham Island temperate forests", "Cherskii-Kolyma mountain tundra", "Chhota-Nagpur dry deciduous forests", "Chiapas Depression dry forests", "Chiapas montane forests", "Chihuahuan desert", "Chilean Matorral", "Chimalapas montane forests", "Chin Hills-Arakan Yoma montane forests", "Chiquitano dry forests", "Chocó-Darién moist forests", "Christmas and Cocos Islands tropical forests", "Chukchi Peninsula tundra", "Clipperton Island shrub and grasslands", "Cocos Island moist forests", "Colorado Plateau shrublands", "Colorado Rockies forests", "Comoros forests", "Cook Inlet taiga", "Cook Islands tropical moist forests", "Coolgardie woodlands", "Copper Plateau taiga", "Cordillera Central páramo", "Cordillera de Merida páramo", "Cordillera La Costa montane forests", "Cordillera Oriental montane forests", "Corsican montane broadleaf and mixed forests", "Costa Rican seasonal moist forests", "Crete Mediterranean forests", "Crimean Submediterranean forest complex", "Cross-Niger transition forests", "Cross-Sanaga-Bioko coastal forests", "Cross-Timbers savanna-woodland", "Cuban cactus scrub", 
                                                               "Cuban dry forests", "Cuban moist forests", "Cuban pine forests", "Cuban wetlands", "Cyprus Mediterranean forests", "Da Hinggan-Dzhagdy Mountains conifer forests", "Daba Mountains evergreen forests", "Daurian forest steppe", "Davis Highlands tundra", "Deccan thorn scrub forests", "Dinaric Mountains mixed forests", "Djibouti xeric shrublands", "Drakensberg Escarpment savanna and thicket", "Drakensberg grasslands", "Rock and Ice", "Dry Chaco", "East Afghan montane conifer forests", "East African halophytics", "East African mangroves", "East Antarctic tundra", "East Arabian fog shrublands and sand desert", "East Central Texas forests", "East Deccan dry-evergreen forests", "East European forest steppe", "East Sahara Desert", "East Saharan montane xeric woodlands", "East Siberian taiga", "East Sudanian savanna", "Eastern Anatolian deciduous forests", "Eastern Anatolian montane steppe", "Eastern Australia mulga shrublands", "Eastern Australian temperate forests", "Eastern Canadian forests", "Eastern Canadian Shield taiga", "Eastern Cascades forests", "Eastern Congolian swamp forests", "Eastern Cordillera Real montane forests", "Eastern Canadian Forest-Boreal transition", "Eastern Gobi desert steppe", "Eastern Great Lakes lowland forests", "Eastern Guinean forests", "East Deccan moist deciduous forests", "Eastern Himalayan alpine shrub and meadows", "Eastern Himalayan broadleaf forests", "Eastern Himalayan subalpine conifer forests", "Eastern Java-Bali montane rain forests", "Eastern Java-Bali rain forests", "Eastern Mediterranean conifer-broadleaf forests", "Eastern Micronesia tropical moist forests", "Eastern Panamanian montane forests", "Ecuadorian dry forests", "Edwards Plateau savanna", "Einasleigh upland savanna", "Elburz Range forest steppe", "Ellsworth Land tundra", "Ellsworth Mountains tundra", "Emin Valley steppe", "Enderby Land tundra", "English Lowlands beech forests", "Enriquillo wetlands", "Eritrean coastal desert", "Esperance mallee", "Espinal", "Ethiopian montane forests", "Ethiopian montane grasslands and woodlands", "Ethiopian montane moorlands", "Etosha Pan halophytics", "Euxine-Colchic broadleaf forests", "Everglades flooded grasslands", "Eyre and York mallee", "Faroe Islands boreal grasslands", "Fernando de Noronha-Atol das Rocas moist forests", "Fiji tropical dry forests", "Fiji tropical moist forests", "Fiordland temperate forests", "Flinders-Lofty montane woodlands", "Flint Hills tallgrass prairie", "Fraser Plateau and Basin conifer forests", "Fynbos shrubland", "Galápagos Islands xeric scrub", "Gariep Karoo", "Ghorat-Hazarajat alpine meadow", "Gibson desert", "Gissaro-Alai open woodlands", "Godavari-Krishna mangroves", "Gobi Lakes Valley desert steppe", "Granitic Seychelles forests", "Great Basin montane forests", "Great Basin shrub steppe", "Great Lakes Basin desert steppe", "Great Sandy-Tanami desert", "Great Victoria desert", "Greater Negros-Panay rain forests", "Guajira-Barranquilla xeric scrub", "Guayaquil flooded grasslands", "Guianan freshwater swamp forests", "Guianan Highlands moist forests", "Guianan lowland moist forests", "Guianan piedmont moist forests", "Guianan savanna", "Guinean forest-savanna", "Guinean mangroves", "Guinean montane forests", "Guizhou Plateau broadleaf and mixed forests", "Gulf of California xeric scrub", "Gulf of St. Lawrence lowland forests", "Gurupa várzea", "Hainan Island monsoon rain forests", "Halmahera rain forests", "Hampton mallee and woodlands", "Hawai'i tropical dry forests", "Hawai'i tropical high shrublands", "Hawai'i tropical low shrublands", "Hawai'i tropical moist forests", "Helanshan montane conifer forests", "Hengduan Mountains subalpine conifer forests", "Canadian High Arctic tundra", "High Monte", "Highveld grasslands", "Himalayan subtropical broadleaf forests", "Himalayan subtropical pine forests", "Hindu Kush alpine meadow", "Hispaniolan dry forests", "Hispaniolan moist forests", "Hispaniolan pine forests", "Hobyo grasslands and shrublands", "Hokkaido deciduous forests", "Hokkaido montane conifer forests", "Honshu alpine conifer forests", "Horn of Africa xeric bushlands", "Huang He Plain mixed forests", "Humid Chaco", "Humid Pampas", "Huon Peninsula montane rain forests", "Iberian conifer forests", "Iberian sclerophyllous and semi-deciduous forests", "Iceland boreal birch forests and alpine tundra", "Illyrian deciduous forests", "Indochina mangroves", "Indus River Delta-Arabian Sea mangroves", "Indus Valley desert", "Inner Niger Delta flooded savanna", "Interior Alaska-Yukon lowland taiga", "Interior Plateau US Hardwood Forests", "Interior Yukon-Alaska alpine tundra", "Iquitos várzea", "Irrawaddy dry forests", "Irrawaddy freshwater swamp forests", "Irrawaddy moist deciduous forests", "Islas Revillagigedo dry forests", "Isthmian-Atlantic moist forests", "Isthmian-Pacific moist forests", "Italian sclerophyllous and semi-deciduous forests", "Itigi-Sumbu thicket", "Jalisco dry forests", "Jamaican dry forests", "Jamaican moist forests", "Japurá-Solimões-Negro moist forests", "Jarrah-Karri forest and shrublands", "Jian Nan subtropical evergreen forests", "Jos Plateau forest-grassland", "Juan Fernández Islands temperate forests", "Junggar Basin semi-desert", "Juruá-Purus moist forests", "Kalaallit Nunaat High Arctic tundra", "Kalahari Acacia woodlands", "Kalahari xeric savanna", "Kamchatka-Kurile meadows and sparse forests", "Kamchatka taiga", "Kaokoveld desert", "Karakoram-West Tibetan Plateau alpine steppe", "Kayah-Karen montane rain forests", "Kazakh forest steppe", "Kazakh semi-desert", "Kazakh steppe", "Kazakh upland steppe", "Kermadec Islands subtropical moist forests", "Khangai Mountains alpine meadow", "Khangai Mountains conifer forests", "Khathiar-Gir dry deciduous forests", "Kimberly tropical savanna", "Klamath-Siskiyou forests", "Knysna-Amatole montane forests", "Kola Peninsula tundra", "Kopet Dag semi-desert", "Kopet Dag woodlands and forest steppe", "Kuh Rud and Eastern Iran montane woodlands", "Kwazulu Natal-Cape coastal forests", "La Costa xeric shrublands", "Lake Chad flooded savanna", "Lara-Falcón dry forests", "Leeward Islands moist forests", "Lesser Sundas deciduous forests", "Limpopo lowveld", "Llanos", "Lord Howe Island subtropical forests", "Louisiade Archipelago rain forests", "Low Monte", "Lower Gangetic Plains moist deciduous forests", "Luang Prabang montane rain forests", "Luzon montane rain forests", 
                                                               "Luzon rain forests", "Luzon tropical pine forests", "Madagascar dry deciduous forests", "Madagascar ericoid thickets", "Madagascar humid forests", "Madagascar mangroves", "Madagascar spiny thickets", "Madagascar subhumid forests", "Madagascar succulent woodlands", "Madeira-Tapajós moist forests", "Madeira evergreen forests", "Magdalena-Urabá moist forests", "Magdalena Valley dry forests", "Magdalena Valley montane forests", "Magellanic subpolar forests", "Makgadikgadi halophytics", "Malabar Coast moist forests", "Maldives-Lakshadweep-Chagos Archipelago tropical moist forests", "Malpelo Island xeric scrub", "Manchurian mixed forests", "Mandara Plateau woodlands", "Maputaland coastal forests and woodlands", "Maracaibo dry forests", "Marajó várzea", "Maranhão Babaçu forests", "Marañón dry forests", "Marianas tropical dry forests", "Marquesas tropical moist forests", "Masai xeric grasslands and shrublands", "Mascarene forests", "Mato Grosso tropical dry forests", "Mediterranean Acacia-Argania dry woodlands and succulent thickets", "Mediterranean conifer and mixed forests", "Mediterranean dry woodlands and steppe", "Mediterranean High Atlas juniper steppe", "Mediterranean woodlands and forests", "Meghalaya subtropical forests", "Mentawai Islands rain forests", "Meseta Central matorral", "Mesoamerican Gulf-Caribbean mangroves", "Mesopotamian shrub desert", "Mid-Canada Boreal Plains forests", "Canadian Middle Arctic Tundra", "Mid-Atlantic US coastal savannas", "Midwest Canadian Shield forests", "Mindanao-Eastern Visayas rain forests", "Mindanao montane rain forests", "Mindoro rain forests", "Miskito pine forests", "Mississippi lowland forests", "Mitchell Grass Downs", "Mizoram-Manipur-Kachin rain forests", "Mojave desert", "Mongolian-Manchurian grassland", "Montana Valley and Foothill grasslands", "Monte Alegre várzea", "Motagua Valley thornscrub", "Mount Cameroon and Bioko montane forests", "Mulanje Montane forest-grassland", "Murray-Darling woodlands and mallee", "Muskwa-Slave Lake taiga", "Myanmar Coast mangroves", "Myanmar coastal rain forests", "Nama Karoo shrublands", "Namaqualand-Richtersveld steppe", "Namib Desert", "Namibian savanna woodlands", "Nansei Islands subtropical evergreen forests", "Napo moist forests", "Naracoorte woodlands", "Narmada Valley dry deciduous forests", "Nebraska Sand Hills mixed grasslands", "Negro-Branco moist forests", "Nelson Coast temperate forests", "Nenjiang River grassland", "New Britain-New Ireland lowland rain forests", "New Britain-New Ireland montane rain forests", "New Caledonia dry forests", "New Caledonia rain forests", "New England-Acadian forests", "New Guinea mangroves", "Nicobar Islands rain forests", "Niger Delta swamp forests", "Nigerian lowland forests", "Nihonkai evergreen forests", "Nihonkai montane deciduous forests", "Nile Delta flooded savanna", "Norfolk Island subtropical forests", "Northeast Antarctic Peninsula tundra", "Northwest Antarctic Peninsula tundra", "North Arabian desert", "North Arabian highland shrublands", "New Zealand North Island temperate forests", "North Saharan Xeric Steppe and Woodland", "Somali montane xeric woodlands", "North Tibetan Plateau-Kunlun Mountains alpine desert", "North Victoria Land tundra", "North Western Ghats moist deciduous forests", "North Western Ghats montane rain forests", "Northeast China Plain deciduous forests", "Northeast India-Myanmar pine forests", "Northeast Siberian coastal tundra", "Northeast Siberian taiga", "Northeast Brazil restingas", "Northeast Congolian lowland forests", "Northeast Himalayan subalpine conifer forests", "Northeast Spain and Southern France Mediterranean forests", "Northeast US Coastal forests", "Northern Acacia-Commiphora bushlands and thickets", "Northern Anatolian conifer and deciduous forests", "Northern Andean páramo", "Northern Annamites rain forests", "Northern California coastal forests", "Northern Canadian Shield taiga", "Northern Congolian Forest-Savanna", "North Deccan dry deciduous forests", "Northern Indochina subtropical forests", "Northern Khorat Plateau moist deciduous forests", "Northern Mesoamerican Pacific mangroves", "Northern New Guinea lowland rain and freshwater swamp forests", "Northern New Guinea montane rain forests", "Northern Shortgrass prairie", "Northern Swahili coastal forests", "Northern Tallgrass prairie", "Northern Thailand-Laos moist deciduous forests", "Northern Triangle subtropical forests", "Northern Triangle temperate forests", "Northern Vietnam lowland rain forests", "Northland temperate kauri forests", "Northwest Iberian montane forests", "Northwest Russian-Novaya Zemlya tundra", "Northwest Andean montane forests", "Northwest Congolian lowland forests", "Northwest Hawai'i scrub", "Northwestern Himalayan alpine shrub and meadows", "Aravalli west thorn scrub forests", "Novosibirsk Islands Arctic desert", "Nujiang Langcang Gorge alpine conifer and mixed forests", "Nullarbor Plains xeric shrublands", "Nyanga-Chimanimani Montane forest-grassland", "Oaxacan montane forests", "Ogasawara subtropical moist forests", "Ogilvie-MacKenzie alpine tundra", "Okanogan dry forests", "Okhotsk-Manchurian taiga", "Ordos Plateau steppe", "Orinoco Delta swamp forests", "Orinoco wetlands", "Orissa semi-evergreen forests", "Ozark Highlands mixed forests", "Ozark Mountain forests", "Pacific Coastal Mountain icefields and tundra", "Palau tropical moist forests", "Palawan rain forests", "Palouse prairie", "Pamir alpine desert and tundra", "Panamanian dry forests", "Pannonian mixed forests", "Pantanal", "Pantanos de Centla", "Pantepui forests & shrublands", "Paraguaná xeric scrub", "Paraná flooded savanna", "Paropamisus xeric woodlands", "Patagonian steppe", "Patía valley dry forests", "Peninsular Malaysian montane rain forests", "Peninsular Malaysian peat swamp forests", "Peninsular Malaysian rain forests", "Pernambuco coastal forests", "Pernambuco interior forests", "Peruvian Yungas", "Petén-Veracruz moist forests", "Pilbara shrublands", "Pindus Mountains mixed forests", "Piney Woods", "Po Basin mixed forests", "Pontic steppe", "Prince Charles Mountains tundra", "Puerto Rican dry forests", "Puerto Rican moist forests", "Puget lowland forests", "Purus-Madeira moist forests", "Purus várzea", "Pyrenees conifer and mixed forests", "Qaidam Basin semi-desert", "Qilian Mountains conifer forests", "Qilian Mountains subalpine meadows", "Qin Ling Mountains deciduous forests", 
                                                               "Qionglai-Minshan conifer forests", "Queensland tropical rain forests", "Rakiura Island temperate forests", "Rann of Kutch seasonal salt marsh", "Rapa Nui and Sala y Gómez subtropical forests", "Red River freshwater swamp forests", "Red Sea-Arabian Desert shrublands", "Red Sea coastal desert", "Red Sea mangroves", "Registan-North Pakistan sandy desert", "Renosterveld shrubland", "Richmond temperate forests", "Rio Negro campinarana", "Rodope montane mixed forests", "Rwenzori-Virunga montane moorlands", "Saharan halophytics", "Sahelian Acacia savanna", "Sakhalin Island taiga", "Samoan tropical moist forests", "San Félix-San Ambrosio Islands temperate forests", "San Lucan xeric scrub", "Santa Lucia Montane Chaparral & Woodlands", "Santa Marta montane forests", "Santa Marta páramo", "São Tomé, Príncipe, and Annobón forests", "Sarmatic mixed forests", "Sayan alpine meadows and tundra", "Sayan Intermontane steppe", "Sayan montane conifer forests", "Scandinavian and Russian taiga", "Scandinavian coastal conifer forests", "Scandinavian Montane Birch forest and grasslands", "Scotia Sea Islands tundra", "Sechura desert", "Selenge-Orkhon forest steppe", "Seram rain forests", "Serengeti volcanic grasslands", "Serra do Mar coastal forests", "Sichuan Basin evergreen broadleaf forests", 
                                                               "Sierra de la Laguna dry forests", "Sierra de la Laguna pine-oak forests", "Sierra de los Tuxtlas", "Sierra Madre de Chiapas moist forests", "Sierra Madre de Oaxaca pine-oak forests", "Sierra Madre del Sur pine-oak forests", "Sierra Madre Occidental pine-oak forests", "Sierra Madre Oriental pine-oak forests", "Sierra Nevada forests", "Simpson desert", "Sinaloan dry forests", "Sinú Valley dry forests", "Snake-Columbia shrub steppe", "Society Islands tropical moist forests", "Socotra Island xeric shrublands", "Solimões-Japurá moist forests", "Solomon Islands rain forests", "Somali Acacia-Commiphora bushlands and thickets", "Sonoran-Sinaloan subtropical dry forest", "Sonoran desert", "South American Pacific mangroves", "South Antarctic Peninsula tundra", "South Apennine mixed montane forests", "South Central Rockies forests", "South China-Vietnam subtropical evergreen forests", "South China Sea Islands", "South Deccan Plateau dry deciduous forests", "South Iran Nubo-Sindian desert and semi-desert", "New Zealand South Island montane grasslands", "New Zealand South Island temperate forests", "South Orkney Islands tundra", "South Sahara desert", "South Siberian forest steppe", "South Taiwan monsoon rain forests", "South Victoria Land tundra", "South Western Ghats moist deciduous forests", "South Western Ghats montane rain forests", "Southeast Australia temperate forests", "Southeast Australia temperate savanna", "Southeast Tibet shrublands and meadows", "Southeast US mixed woodlands and savannas", "Southeast US conifer savannas", "Southeast Iberian shrubs and woodlands", "Southeast Indochina dry evergreen forests", "Southeast Papuan rain forests", "Southern Acacia-Commiphora bushlands and thickets", "Southern Africa mangroves", 
                                                               "Southern Anatolian montane conifer and deciduous forests", "Southern Andean steppe", "Southern Andean Yungas", "Southern Annamites montane rain forests", "Southern Atlantic Brazilian mangroves", "Southern Cone Mesopotamian savanna", "Southern Congolian forest-savanna", "Southern Great Lakes forests", "Southern Hudson Bay taiga", "Southern Indian Ocean Islands tundra", "Southern Korea evergreen forests", "Southern Mesoamerican Pacific mangroves", "Southern New Guinea freshwater swamp forests", "Southern New Guinea lowland rain forests", "Southern Pacific dry forests", "Southern Rift Montane forest-grassland", "Southern Swahili coastal forests and woodlands", "Southern Vietnam lowland dry forests", "Southwest Amazon moist forests", "Southwest Arabian Escarpment shrublands and woodlands", "Southwest Arabian highland xeric scrub", "Southwest Australia savanna", "Southwest Australia woodlands", "Southwest Borneo freshwater swamp forests", "Southwest Iberian Mediterranean sclerophyllous and mixed forests", "Sri Lanka dry-zone dry evergreen forests", "Sri Lanka lowland rain forests", "Sri Lanka montane rain forests", "St. Peter and St. Paul Rocks", "Succulent Karoo xeric shrublands", "Sudd flooded grasslands", "Suiphun-Khanka meadows and forest meadows", "Sulaiman Range alpine meadows", "Sulawesi montane rain forests", "Sulu Archipelago rain forests", "Sumatran freshwater swamp forests", "Sumatran lowland rain forests", "Sumatran montane rain forests", "Sumatran peat swamp forests", "Sumatran tropical pine forests", "Sumba deciduous forests", "Sunda Shelf mangroves", "Sundaland heath forests", "Sundarbans freshwater swamp forests", "Sundarbans mangroves", "Syrian xeric grasslands and shrublands", "Taiheiyo evergreen forests", "Taiheiyo montane deciduous forests", "Taimyr-Central Siberian tundra", "Taiwan subtropical evergreen forests", "Taklimakan desert", "Talamancan montane forests", "Tamaulipan matorral", "Tamaulipan mezquital", "Tapajós-Xingu moist forests", "Tarim Basin deciduous forests and steppe", "Tasmanian Central Highland forests", "Tasmanian temperate forests", "Tasmanian temperate rain forests", "Tehuacán Valley matorral", "Tenasserim-South Thailand semi-evergreen rain forests", "Terai-Duar savanna and grasslands", "Texas blackland prairies", "Thar desert", "Tian Shan foothill arid steppe", "Tian Shan montane conifer forests", "Tian Shan montane steppe and meadows", "Tibesti-Jebel Uweinat montane xeric woodlands", "Tibetan Plateau alpine shrublands and meadows", "Tigris-Euphrates alluvial salt marsh", "Timor and Wetar deciduous forests", "Tirari-Sturt stony desert", "Tongan tropical moist forests", "Tonle Sap-Mekong peat swamp forests", "Tonle Sap freshwater swamp forests", "Torngat Mountain tundra", "Trans-Baikal Bald Mountain tundra", "Trans-Baikal conifer forests", "Trans-Mexican Volcanic Belt pine-oak forests", "Trans Fly savanna and grasslands", "Transantarctic Mountains tundra", "Trinidad and Tobago moist forest", "Tristan Da Cunha-Gough Islands shrub and grasslands", "Trobriand Islands rain forests", "Tuamotu tropical moist forests", "Tubuai tropical moist forests", "Tumbes-Piura dry forests", "Tyrrhenian-Adriatic sclerophyllous and mixed forests", "Uatumã-Trombetas moist forests", "Ucayali moist forests", "Upper Gangetic Plains moist deciduous forests", "Upper Midwest US forest-savanna transition", "Urals montane forest and taiga", "Uruguayan savanna", "Ussuri broadleaf and mixed forests", "Valdivian temperate forests", "Vanuatu rain forests", "Venezuelan Andes montane forests", "Veracruz dry forests", "Veracruz moist forests", "Veracruz montane forests", "Victoria Plains tropical savanna", "Vogelkop-Aru lowland rain forests", "Vogelkop montane rain forests", "Wasatch and Uinta montane forests", "Watson Highlands taiga", "West Sahara desert", "West Saharan montane xeric woodlands", "West Siberian taiga", "West Sudanian savanna", "Western Australian Mulga shrublands", "Western Congolian swamp forests", "Western Ecuador moist forests", "Western European broadleaf forests", "Western Great Lakes forests", "Western Guinean lowland forests", "Western Gulf coastal grasslands", "Western Himalayan alpine shrub and meadows", "Western Himalayan broadleaf forests", "Western Himalayan subalpine conifer forests", "Western Java montane rain forests", "Western Java rain forests", "Western Polynesian tropical moist forests", "Western shortgrass prairie", "Western Siberian hemiboreal forests", "Westland temperate forests", "Willamette Valley oak savanna", "Windward Islands moist forests", "Wrangel Island Arctic desert", "Wyoming Basin shrub steppe", "Xingu-Tocantins-Araguaia moist forests", "Yamal-Gydan tundra", "Yap tropical dry forests", "Yapen rain forests", "Yarlung Zanbo arid steppe", "Yellow Sea saline meadow", "Yucatán dry forests", "Yucatán moist forests", "Yunnan Plateau subtropical evergreen forests", "Zagros Mountains forest steppe", "Zambezian-Limpopo mixed woodlands", "Zambezian Baikiaea woodlands", "Zambezian coastal flooded savanna", "Dry miombo woodlands", "Zambezian evergreen dry forests", "Zambezian flooded grasslands", "Zambezian mopane woodlands", "Brazilian Atlantic dry forests", "Northern Cordillera forests", "St. Helena scrub and woodlands", "Victoria Basin forest-savanna", "Western Congolian forest-savanna", "East African montane moorlands", "Queen Charlotte Islands conifer forests", "Northern Pacific Alaskan coastal forests", "Northwest Territories taiga", "Southwest Arabian coastal xeric shrublands", "Arabian-Persian Gulf coastal plain desert", "South Arabian plains and plateau desert", "Arabian desert", "Bahamian pineyards", "Ile Europa and Bassas da India xeric scrub", "Kamchatka tundra", "Tocantins/Pindare moist forests", "Kalaallit Nunaat Arctic steppe", "North Cascades conifer forests", "Northern Rockies conifer forests", "Trindade-Martin Vaz Islands tropical forests", "Southwest Arabian montane woodlands and grasslands", "South Arabian fog woodlands, shrublands, and dune", "Trinidad and Tobago dry forest", "Lesser Antillean dry forests", "North Atlantic moist mixed forests", "Dronning Maud Land tundra", "Marie Byrd Land tundra", "Sulawesi lowland rain forests", "East African montane forests", "Eastern Arc forests", "Borneo montane rain forests", "Kinabalu montane alpine meadows"), multiple = TRUE, selected = NULL)
                                ),
                                # Options for entering bounding-box
                                conditionalPanel(condition = "input.extent_type == 'map_bbox'",
                                                 div(style = "display:inline-block",
                                                     numericInput("xmin", "xmin", -180, min = -180, max = 180, step = 0.5)),
                                                 div(style = "display:inline-block",
                                                     numericInput("xmax", "xmax", 180, min = -180, max = 180, step = 0.5)),
                                                 div(style = "display:inline-block",
                                                     numericInput("ymin", "ymax", -60, min = -60, max = 90, step = 0.5)),
                                                 div(style = "display:inline-block",
                                                     numericInput("ymax", "ymax", 90, min = -90, max = 90, step = 0.5)))#,
               ),
               
               conditionalPanel("input.map_panel == 'scenario_individual'",
                                selectInput("bio_to_plot", "Select a bioclimatic variable",
                                            choices= c("(bio 1) Annual Mean Temperature" = "bio_1",
                                                       "(bio 2) Mean Diurnal Range (Mean of monthly (max temp - min temp))" = "bio_2",
                                                       "(bio 3) Isothermality (BIO2/BIO7) (* 100)" = "bio_3",
                                                       "(bio 4) Temperature Seasonality (standard deviation *100)" = "bio_4",
                                                       "(bio 5) Max Temperature of Warmest Month" = "bio_5",
                                                       "(bio 6) Min Temperature of Coldest Month" = "bio_6",
                                                       "(bio 7) Temperature Annual Range (BIO5-BIO6)" = "bio_7",
                                                       "(bio 8) Mean Temperature of Wettest Quarter" = "bio_8",
                                                       "(bio 9) Mean Temperature of Driest Quarter" = "bio_9",
                                                       "(bio 10) Mean Temperature of Warmest Quarter" = "bio_10",
                                                       "(bio 11) Mean Temperature of Coldest Quarter" = "bio_11",
                                                       "(bio 12) Annual Precipitation" = "bio_12",
                                                       "(bio 13) Precipitation of Wettest Month" = "bio_13",
                                                       "(bio 14) Precipitation of Driest Month" = "bio_14",
                                                       "(bio 15) Precipitation Seasonality (Coefficient of Variation)" = "bio_15",
                                                       "(bio 16) Precipitation of Wettest Quarter" = "bio_16",
                                                       "(bio 17) Precipitation of Driest Quarter" = "bio_17",
                                                       "(bio 18) Precipitation of Warmest Quarter" = "bio_18",
                                                       "(bio 19) Precipitation of Coldest Quarter" = "bio_19")),
                                uiOutput("map_individual"))
             ),
             
             ## Map
             mainPanel(
               
               tabsetPanel(id = "map_panel",
                           # type = "pills",
                           type = "tabs",
                           
                           tabPanel("Map", 
                                    value = "scenario_map",
                                    
                                    leaflet::leafletOutput("map", height = 600),
                                    hr(),
                                    
                                    ## Button ANALYZE
                                    fluidRow(
                                      column(6, h6("Press here when ready:")),
                                      column(1, shiny::actionButton(inputId = "go",
                                                                    label = "ANALYZE",
                                                                    icon = icon("bolt"),
                                                                    style="color: #fff; background-color: #57be91; 
                                                                    border-color: #2e6da4; font-size:160%; padding:10px")))
                                      ),
                           
                           # tabPanel("Explore individual layers",
                           #          value = "scenario_individual",
                           #          leaflet::leafletOutput("map_ind", height = 600)),
                           
                           tabPanel("Instructions", 
                                    value = "scenario_instructions",
                                    column(8,
                                           includeMarkdown("Rmd/map_instructions.Rmd")))
               )
             )
    ),
    
    ### Selected scenario
    tabPanel(title = "SELECTED GCMs",
             value = "selected",
             
             sidebarLayout(position = "left",
                           fluid = TRUE,
                           
                           sidebarPanel(
                             
                             h5("SELECTED OPTIONS:"),
                             # textOutput("selected_gcms_pre_tab"),
                             # textOutput("selected_scenario_pre_tab"),
                             # textOutput("selected_comparison_pre_tab"),
                             hr(),
                             
                             # Añadir descripción
                             
                             h5("Visualization options"),
                             radioButtons("add_layer", "Overlay on maps:",
                                          c("Do not overlay anything" = "nothing",
                                            "Country borders" = "countries",
                                            "Biomes" = "biomes",
                                            "Ecoregions" = "ecoregions"),
                                          selected = "nothing"),
                             
                             includeMarkdown("Rmd/tab2_visualize_gcm.Rmd")
                           ),
                           
                           mainPanel(
                             textOutput("no_analyze"),
                             uiOutput("gcm_patterns")
                             
                           ))),
    
    ### Compare with Present Climate
    tabPanel(title = "CHANGE FROM PRESENT",
             value = "current",
             
             sidebarLayout(position = "left",
                           fluid = TRUE,
                           
                           sidebarPanel(
                             
                             h5("SELECTED OPTIONS:"),
                             textOutput("selected_gcms_pre_tab"),
                             textOutput("selected_scenario_pre_tab"),
                             textOutput("selected_comparison_pre_tab"),
                             hr(),
                             
                             conditionalPanel("input.maps_present == 'fig_pre_delta'",
                                              # includeMarkdown("Rmd/tab3_figure_table.Rmd"),
                                              hr(),
                                              
                                              radioButtons(inputId = "type_scaled_pre_delta",
                                                           label = "Data for the scatterplot:",
                                                           choices = c(#"Scaled differences to ensemble" = "scaled", 
                                                             "Average of bioclimatic variables (ºC, mm)" = "no_scaled",
                                                             "Deltas (GCM - baseline; dºC, dmm)" = "deltas"),
                                                           selected = "no_scaled"),
                                              # includeMarkdown("Rmd/tab3_scaled_unscaled.Rmd"),
                                              hr(),
                                              
                                              downloadButton('download_prec_temp_unscaled', 'Download fig'),
                                              downloadButton('download_prec_temp_deltas', 'Download fig deltas'),
                                              hr(),
                                              downloadButton('download_comparison_table_no_scaled', 'Download table')
                             ),
                             
                             conditionalPanel("input.maps_present == 'maps_pre_delta'",
                                              h5("Visualization options"),
                                              radioButtons("add_layer2", "Overlay on maps:",
                                                           c("Do not overlay anything" = "nothing",
                                                             "Country borders" = "countries",
                                                             "Biomes" = "biomes",
                                                             "Ecoregions" = "ecoregions"),
                                                           selected = "nothing"),
                                              
                                              includeMarkdown("Rmd/tab2_delta_gcm.Rmd"))#,
                             
                             # conditionalPanel("input.maps_present == 'leaflet_pre_delta'",
                             #                  uiOutput("map_pre_delta_bio"),
                             #                  uiOutput("map_pre_delta_gcm"),
                             #                  
                             #                  h6("Download layer"),
                             #                  selectInput(inputId = "download_pre_delta_format",
                             #                              label = "Raster file format",
                             #                              choices = c("rds", "tif"),
                             #                              selected = "tif",
                             #                              width = "400"),
                             #                  downloadButton('allow_download_layer_pre_delta', 'Download visible raster layer'))
                             
                           ),
                           
                           mainPanel(
                             tabsetPanel(id = "maps_present",
                                         selected = "fig_pre_delta",
                                         type = "pills",
                                         # type = "tabs",
                                         
                                         tabPanel(title = "Spread of GCMs",
                                                  value = "fig_pre_delta",
                                                  
                                                  # textOutput("no_analyze_fig"),
                                                  
                                                  # conditionalPanel("input.type_scaled == 'scaled'",
                                                  #                  plotlyOutput("plot_temp_prec", height = "100%"),
                                                  #                  hr(),
                                                  #                  uiOutput("tab3_ideas_for_selecting"), 
                                                  #                  hr(),
                                                  #                  tableOutput("comparison_table")),
                                                  conditionalPanel("input.type_scaled_pre_delta == 'deltas'",
                                                                   plotlyOutput("plot_temp_prec_deltas", height = "100%"),
                                                                   hr(),
                                                                   uiOutput("tab3_ideas_for_selecting_copy2"),
                                                                   hr(),
                                                                   tableOutput("comparison_table_delta")),
                                                  conditionalPanel("input.type_scaled_pre_delta == 'no_scaled'",
                                                                   plotlyOutput("plot_temp_prec_no_scaled", height = "100%"),
                                                                   hr(),
                                                                   uiOutput("tab3_ideas_for_selecting_copy"),
                                                                   hr(),
                                                                   tableOutput("comparison_table_no_scaled"))
                                         ),
                                         
                                         tabPanel(title = "Maps of differences (GCM - baseline)",
                                                  value = "maps_pre_delta",
                                                  uiOutput("delta_patterns"))#,
                                         
                                         # tabPanel(title = "Explore in detail",
                                         #          value = "leaflet_pre_delta",
                                         #          leaflet::leafletOutput("map_pre_delta", height = 600))
                             )
                           )
             )
    ),
    
    
    ### MAPS (I): Explore GCMS
    tabPanel(title = "CHANGE BETWEEN FUTURES",
             value = "future",
             
             sidebarLayout(
               sidebarPanel(
                 
                 h5("SELECTED OPTIONS:"),
                 textOutput("selected_gcms_fut_tab"),
                 textOutput("selected_scenario_fut_tab"),
                 textOutput("selected_comparison_fut_tab"),
                 hr(),
                 
                 conditionalPanel("input.future_panel == 'fig_table'",
                                  includeMarkdown("Rmd/tab3_figure_table.Rmd"),
                                  hr(),
                                  
                                  radioButtons(inputId = "type_scaled",
                                               label = "Data for the scatterplot:",
                                               choices = c("Scaled differences to ensemble" = "scaled"#, 
                                                           # "Delta values (only 1x1 comparison)" = "deltas",
                                                           # "Raw values (only 1x1 comparison)" = "no_scaled"
                                               ),
                                               selected = "scaled"),
                                  includeMarkdown("Rmd/tab3_scaled_unscaled.Rmd"),
                                  hr(),
                                  
                                  downloadButton('download_prec_temp_scaled', 'Download figure'),
                                  downloadButton('download_comparison_table', 'Download table')
                 ),
                 
                 conditionalPanel("input.future_panel == 'maps_fut_diff'",
                                  
                                  h5("Visualization options"),
                                  radioButtons("add_layer3", "Overlay on maps:",
                                               c("Do not overlay anything" = "nothing",
                                                 "Country borders" = "countries",
                                                 "Biomes" = "biomes",
                                                 "Ecoregions" = "ecoregions"),
                                               selected = "nothing"),
                                  
                                  includeMarkdown("Rmd/mapII_tab.Rmd")
                                  
                 )#,
                 
                 # conditionalPanel("input.future_panel == 'maps_fut_detail'",
                 #                  
                 #                  uiOutput("map_fut_detail_bio"),
                 #                  uiOutput("map_fut_detail_gcm"),
                 #                  
                 #                  hr(),
                 #                  
                 #                  h6("Download layer"),
                 #                  selectInput(inputId = "download_raster_format",
                 #                              label = "Raster file format",
                 #                              choices = c("rds", "tif"),
                 #                              selected = "tif",
                 #                              width = "400"),
                 #                  downloadButton('download_map_fut_detail', 'Download visible raster layer')
                 # )
                 
               ),
               
               mainPanel(
                 
                 tabsetPanel(id = "future_panel",
                             selected = "fig_table",
                             type = "pills",
                             # type = "tabs",
                             
                             tabPanel(title = "Spread of GCMs",
                                      value = "fig_table",
                                      
                                      textOutput("no_analyze_fig"),
                                      
                                      conditionalPanel("input.type_scaled == 'scaled'",
                                                       plotlyOutput("plot_temp_prec", height = "100%"),
                                                       hr(),
                                                       uiOutput("tab3_ideas_for_selecting"), 
                                                       hr(),
                                                       tableOutput("comparison_table"))#,
                                      # conditionalPanel("input.type_scaled == 'deltas'",
                                      #                  plotlyOutput("plot_temp_prec_deltas", height = "100%"),
                                      #                  hr(),
                                      #                  uiOutput("tab3_ideas_for_selecting_copy2"),
                                      #                  hr(),
                                      #                  tableOutput("comparison_table_delta")),
                                      # conditionalPanel("input.type_scaled == 'no_scaled'",
                                      #                  plotlyOutput("plot_temp_prec_no_scaled", height = "100%"),
                                      #                  hr(),
                                      #                  uiOutput("tab3_ideas_for_selecting_copy"),
                                      #                  hr(),
                                      #                  tableOutput("comparison_table_no_scaled"))
                             ),
                             
                             tabPanel(title = "Maps of differences (GCM - Ensemble)",
                                      value = "maps_fut_diff",
                                      
                                      uiOutput("gcm_differences")
                             )#,
                             
                             # tabPanel(title = "Explore differences in detail",
                             #          value = "maps_fut_detail",
                             #          
                             #          leaflet::leafletOutput("map_fut_detail", height = 600)
                             # )
                 )
                 
                 
               )
             )
    ),
    
    #### TAB 4: REPORT
    
    tabPanel(title = "REPORT",
             downloadButton("report", "Generate report"))
    )
)








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
  
  ##################################################
  #### Prepare input values and hide options
  ##################################################
  ### Asure that hidden input provide a valid initial value
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
    # print_rcp <- input$rcp_type %>% str_replace("rcp", "") %>% case_when(. == "rcp26" ~ "2.6",
    #                                                                      . == "rcp45" ~ "4.5",
    #                                                                      . == "rcp60" ~ "6.0",
    #                                                                      . == "rcp85" ~ "8.5")
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
  
  
  ## Tab Panel options
  # Hide bioclimatic variables
  # observeEvent(input$bio_vars_button, {
  #   if (input$bio_vars_button %% 2 == 1){
  #     shinyjs::show(id = "bio_vars", anim = T)
  #   } else {
  #     shinyjs::hide(id = "bio_vars", anim = T)
  #   }
  # })
  
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
  
  shinyjs::disable("download_comparison_table")
  observeEvent(input$go, {
    shinyjs::enable("download_comparison_table")
  })
  
  
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
  
  
  ### Available GCMs - dynamically change For scenario - scenario_individual" tab (tab 1 - scenario definition - detail)
  output$map_individual <- renderUI({
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
    selectInput(inputId = "gcm_to_plot", "Select a GCM",
                choices  = available_gcms)
  })
  
  
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
                   polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE, circleMarkerOptions = FALSE, markerOptions = FALSE)
  
  output$map <- renderLeaflet(m)
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  
  observe({
    # Extent selected by drawing rectangle
    if (input$extent_type == "map_draw") {
      map %>% 
        hideGroup("country") %>% 
        hideGroup("biomes") %>% 
        hideGroup("ecorregions") %>% 
        hideGroup("bbox") %>% 
        showGroup("draw") 
      
      req(input$map_draw_new_feature)
      # Get coordinates
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol = 2) %>% 
        unique %>% 
        extent
      rvs$polySelXY <- xy
    }
    
    # bounding-box extent
    if (input$extent_type == 'map_bbox'){
      
      req(input$xmin, input$xmax, input$ymin, input$ymax) # This stops it from failing while the number is being typed
      
      map %>% 
        hideGroup("country") %>% 
        hideGroup("draw") %>% 
        hideGroup("biomes") %>%
        hideGroup("ecorregions") %>%
        clearGroup("bbox") %>% 
        clearGroup("draw") %>% 
        clearGroup("biomesSel") %>% 
        clearGroup("ecorregionsSel") %>% 
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
    }
    
    
    # Countries selected by country name
    if (input$extent_type == "map_country"){
      
      # Country names manually added - subset layer to overlay
      selected_countries <- world_sf %>%
        filter(country %in% input$ext_name_country)
      
      # Add polygons to leaflet
      map %>% 
        hideGroup("biomes") %>% 
        hideGroup("bbox") %>% 
        hideGroup("ecorregions") %>% 
        clearGroup("draw") %>% 
        clearGroup("bbox") %>% 
        clearGroup("biomesSel") %>% 
        clearGroup("ecorregionsSel") %>% 
        showGroup("country") %>% 
        # clearGroup("countrySel") %>% 
        showGroup("countrySel") %>% 
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
        hideGroup("ecorregions") %>% 
        clearGroup("draw") %>% 
        clearGroup("countrySel") %>% 
        clearGroup("ecorregionsSel") %>% 
        showGroup("biomes") %>% 
        showGroup("biomesSel") %>% 
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
      
      rvs$polySelXY <- selected_ecorregions
    }
  })
  
  
  ### Individual plot map
  # m2 <- leaflet(world_sf) %>% 
  #   addTiles() %>% 
  #   addProviderTiles("Esri.WorldPhysical", group = "Relieve") %>%
  #   addTiles(options = providerTileOptions(noWrap = TRUE), group = "Countries") %>%
  #   addLayersControl(baseGroups = c("Relieve", "Countries"),
  #                    options = layersControlOptions(collapsed = FALSE)) %>% 
  #   setView(0,0, zoom = 2)
  # output$map_ind <- renderLeaflet(m2)
  # map_ind <- leafletProxy("map_ind")
  # 
  # observeEvent({
  #   input$bio_to_plot
  #   input$gcm_to_plot}, {
  #     # raster_to_plot <- stack(paste0("clim_data/ccafs/raw_rasters/",
  #     #                                input$gcm_to_plot, ".",
  #     #                                input$res_sel, ".year",
  #     #                                input$year_type, ".",
  #     #                                input$rcp_type, ".gri"
  #     raster_to_plot <- readRDS(paste0("clim_data/ccafs/rds/",
  #                                      input$gcm_to_plot, ".",
  #                                      input$res_sel, ".year",
  #                                      input$year_type, ".",
  #                                      input$rcp_type, ".rds"
  #                                      
  #     )) %>%
  #       .[[input$bio_to_plot]]
  #     
  #     map_ind %>%
  #       addRasterImage(raster_to_plot,
  #                      # colors = pal,
  #                      opacity = 0.8) #%>%
  #     # addLegend(pal = pal,
  #     #           values = range_values,
  #     #           group = "map_res",
  #     #           labFormat = ifelse(input$mapIII_gcm == "ENSEMBLE" | input$mapIII_gcm == "BASELINE",
  #     #                              labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
  #     #                              labelFormat(transform = function(x) sort(x, decreasing = TRUE))))
  #     
  #   }) 
  
  
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
                   incProgress(amount = 0.1)
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
                     message(rvs$polySelXY)
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
  ### TAB 2: VISUALIZE DIFFERENCES WITH PRESENT
  ####################################################################################
  
  
  ########### CODE FOR UI ###########
  ### SELECTED OPTIONS
  observe({
    n_gcms <- length(input$sel_gcms)
    output$selected_gcms_pre_tab <- renderText(
      glue::glue("{n_gcms} GCMs") %>% message
    )
    print_rcp <- input$rcp_type %>% str_replace("rcp", "")
    output$selected_scenario_pre_tab <- renderText(
      glue::glue("Year {input$year_type}, RCP{print_rcp}, resolution: {input$res_sel}") %>% message
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("bio X bio"),
                                          input$compare_type == "bio_several" ~ "multiple")
    output$selected_comparison_pre_tab <- renderText(
      glue::glue("Comparison: {type_selected_comparison}") %>% message
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
          withSpinner(type = 5, color = "#9ab899")
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
        withSpinner(type = 5, color = "#9ab899")
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
      glue::glue("{n_gcms} GCMs") %>% message
    )
    print_rcp <- input$rcp_type %>% str_replace("rcp", "")
    output$selected_scenario_fut_tab <- renderText(
      glue::glue("Year {input$year_type}, RCP{print_rcp}, resolution: {input$res_sel}") %>% message
    )
    type_selected_comparison <- case_when(input$compare_type == "bio_bio" ~ c("bio X bio"),
                                          input$compare_type == "bio_several" ~ "multiple")
    output$selected_comparison_fut_tab <- renderText(
      glue::glue("Comparison: {type_selected_comparison}") %>% message
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
  
  observeEvent({input$type_scaled; input$go}, {
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
        withSpinner(type = 5, color = "#9ab899")
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
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("Rmd/report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(Year = input$year_type,
                     rcp = input$rcp_type,
                     Variables = input$bio_vars,
                     maptype = input$extent_type,
                     Coords = rvs$polySelXY,
                     plot_uns = rvs$plot_comp_download_unscaled,
                     plot_delta = rvs$plot_comp_download_deltas,
                     plot_sc = rvs$plot_comp_download_scaled,
                     map_gcm = rvs$plot_gcm_pattern,
                     map_delta = rvs$plot_delta_pattern,
                     map_dif = rvs$plot_gcm_differences
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)


