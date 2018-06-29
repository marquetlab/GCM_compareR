library(shiny)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
# library(shinysky)
library(plotly)
library(leaflet)
library(leaflet.extras)


##############################################################################################################################################################################
########################################      UI       ######################################################################################################################
##############################################################################################################################################################################

##JS Code for enabling and diabling
jscode <- "shinyjs.disabletab =function(name){
$('ul li:has(a[data-value= \"selected\"])').addClass('disabled');
$('ul li:has(a[data-value= \"current\"])').addClass('disabled');
$('ul li:has(a[data-value= \"future\"])').addClass('disabled');
$('.nav li.disabled a').prop('disabled',true)
}

shinyjs.enabletab =function(name){
$('.nav li.disabled a').prop('disabled',false)
$('ul li:has(a[data-value= \"selected\"])').removeClass('disabled');
$('ul li:has(a[data-value= \"current\"])').removeClass('disabled');
$('ul li:has(a[data-value= \"future\"])').removeClass('disabled');
} "


# Define UI for application that draws a histogram
ui <- tagList(
  useShinyjs(),
  extendShinyjs(text = jscode),
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
               color: #f5f5f5; }'#,
               # '.navbar-default .navbar-brand {color: #f5f5f5;}'
    ),
    
    # css styles
    tags$head(
      # Title in navbar
      tags$style(HTML('.navbar-brand {color: #f5f5f5 !important; font-size: 40px; 
                      font-family: Lucida Console; padding-left: 85px}')),
      # navbar text (pages names)
      tags$style(HTML('.navbar-nav {color: #f5f5f5 !important; font-size: 30px !important;
                      font-family: Lucida Console}')),
      tags$style(HTML('p{font-size: 16px}')),
      tags$style(HTML('li{font-size: 16px}')),
      tags$style(HTML('#mu_prior{background-color: rgba(27, 158, 119, 0.5);
                      border-color: rgb(27, 158, 119)}')),
      tags$style(HTML('#sd_prior{background-color: rgba(27, 158, 119, 0.5);
                      border-color: rgb(27, 158, 119)}')),
      tags$style(HTML('#sample_data{color: white;
                      background-color: rgba(80, 80, 80, 0.8);
                      border-color: black; border-width: 1.5px}')),
      tags$style(HTML('#prior{color: #1b9e77; font-weight: bold}')),
      tags$style(HTML('#likelihood{color: #d95f02; font-weight: bold}')),
      tags$style(HTML('#posterior{color: #7570b3; font-weight: bold}')),
      tags$style(HTML('#bold{color: #000000; font-weight: bold}')),
      tags$style(HTML('#mean{color: #000000}'))
      ),
    
    # Tabs
    tags$style(HTML(".tabbable > .nav > li > a              {background-color: #eeeeee;  color:black}
                    .tabbable > .nav > li[class=active] > a {background-color: #5fbc93; color:white}")),
    
    tags$head(
      tags$style(HTML('#intro{background-color: rgba(27, 158, 119, 0.5);
                                 border-color: rgb(27, 158, 119)}'))
    ),
    
    ### Intro ###
    tabPanel(title = "INTRO",
             value = "intro",
             # icon = icon("book"),
             
             sidebarPanel(
               wellPanel(
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
                                    column(12,
                                           hr(),
                                           includeHTML("Rmd/intro_main.html"))),
                                    # column(12, 
                                    #        hr(),
                                    #        h4("GCM compareR"),
                                    #        p(HTML("<span id = 'bold'>GCM compareR</span>"),
                                    #          "is a web application that assists the selection of Global Circulation Models (GCMs) for climate change research."))
                                    #        ),
                           
                           tabPanel("Workflow",
                                    column(12,
                                           hr(),
                                           includeMarkdown("Rmd/intro_tab.Rmd"))),
                           
                           tabPanel("About",
                                    column(12,
                                           hr(),
                                           includeHTML("Rmd/intro_about.html")))
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
                                      column(4),
                                      column(3, h6("Press here when ready:")),
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
                             
                             wellPanel(#12, 
                                    h5("SELECTED OPTIONS:"),
                                    textOutput("selected_gcms_2_tab"),
                                    textOutput("selected_scenario_2_tab"),
                                    textOutput("selected_comparison_2_tab"),
                                    hr(),
                                    
                                    # Description of the tab
                                    h4("Selected GCMs"),
                                    p("These maps show all selected GCMs for a climate change scenario with 
                                      a common variable and color scale. The first two maps for each 
                                      bioclimatic layer are the baseline scenario (current climatic conditions)
                                      and the ensemble of the mean values among all GCMs."),
                                    
                                    # Visualization options
                                    h5("Visualization options"),
                                    radioButtons("add_layer", "Overlay on maps:",
                                                 c("Do not overlay anything" = "nothing",
                                                   "Country borders" = "countries",
                                                   "Biomes" = "biomes",
                                                   "Ecoregions" = "ecoregions"),
                                                 selected = "nothing")
                                    )
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
                             wellPanel(#12,
                                    h5("SELECTED OPTIONS:"),
                                    textOutput("selected_gcms_pre_tab"),
                                    textOutput("selected_scenario_pre_tab"),
                                    textOutput("selected_comparison_pre_tab"),
                                    hr(),
                                    
                                    h4("Change from present"),
                                    p("These results focus on differences between GCMs projections and the baseline (current climate). 
                                      They can be used to identify which GCMs forecast larger or smaller changes in climate
                                      (e.g. units of increase in mean annual temperature) and to diagnose in which direction 
                                      are produced those changes (e.g. some models might project an average reduction in annual
                                      precipitation and others an increase)"),
                                    hr(),
                                    
                                    
                                    conditionalPanel("input.maps_present == 'fig_pre_delta'",
                                                     h5("Scatterplot"),
                                                     p("the averaged projected value for two selected bioclimatic variables is plotted 
                                                       for each GCM, the ensemble and the baseline, similarly to the method described 
                                                       in Vano et al. (2015). Axis values can be displayed as unmodified projected 
                                                       values or as deltas (i.e. difference between GCMs and baseline average values)"),
                                                     radioButtons(inputId = "type_scaled_pre_delta",
                                                                  label = "Type of plotted data",
                                                                  choices = c(#"Scaled differences to ensemble" = "scaled", 
                                                                    "Average of bioclimatic variables (ºC, mm)" = "no_scaled",
                                                                    "Deltas (GCM - baseline; dºC, dmm)" = "deltas"),
                                                                  selected = "no_scaled"),
                                                     hr(),
                                                     
                                                     downloadButton('download_prec_temp_unscaled', 'Download fig'),
                                                     downloadButton('download_prec_temp_deltas', 'Download fig deltas'),
                                                     hr(),
                                                     downloadButton('download_comparison_table_no_scaled', 'Download table')
                                    ),
                                    
                                    conditionalPanel("input.maps_present == 'maps_pre_delta'",
                                                     h5("Maps"),
                                                     p("These maps show the spatial pattern of changes projected by GCMs from 
                                                       current climate (BASELINE). The average ensemble projection across GCMs
                                                       is also included (ENSEMBLE)"),
                                                     h5("Visualization options"),
                                                     radioButtons("add_layer2", "Overlay on maps:",
                                                                  c("Do not overlay anything" = "nothing",
                                                                    "Country borders" = "countries",
                                                                    "Biomes" = "biomes",
                                                                    "Ecoregions" = "ecoregions"),
                                                                  selected = "nothing")#,
                                                     
                                                     # includeMarkdown("Rmd/tab2_delta_gcm.Rmd")
                                                     )#,
                                    
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
                                    
                             )
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
                                                                   # uiOutput("tab3_ideas_for_selecting_copy2"),
                                                                   # hr(),
                                                                   h5("Table with the underlying data"),
                                                                   tableOutput("comparison_table_delta")),
                                                  conditionalPanel("input.type_scaled_pre_delta == 'no_scaled'",
                                                                   plotlyOutput("plot_temp_prec_no_scaled", height = "100%"),
                                                                   hr(),
                                                                   # uiOutput("tab3_ideas_for_selecting_copy"),
                                                                   # hr(),
                                                                   h5("Table with the underlying data"),
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
             
             sidebarLayout(position = "left",
                           fluid = TRUE,
                           sidebarPanel(
                             wellPanel(#12,
                                    h5("SELECTED OPTIONS:"),
                                    textOutput("selected_gcms_fut_tab"),
                                    textOutput("selected_scenario_fut_tab"),
                                    textOutput("selected_comparison_fut_tab"),
                                    hr(),
                                    
                                    h4("Change between futures"),
                                    p("Results displayed here focus on differences within the universe of selected GCMs. 
                                      As a first step, a mean ensemble climate projection is calculated by averaging the
                                      value in each downscaled GCM on a pixel basis. Then, the value in each model is 
                                      compared to the ensemble, to determine whether its projection is greater or smaller 
                                      than the average."),
                                    hr(),
                                    conditionalPanel("input.future_panel == 'fig_table'",
                                                     h5("Scatterplot"),
                                                     p("The scatterplot is centered on the average ensemble, 
                                                       and each GCM is plotted using their mean difference to the ensemble values. 
                                                       Difference values can be displayed scaled or unscaled (preserving the layers’ units)."),
                                                     radioButtons(inputId = "type_scaled",
                                                                  label = NULL,
                                                                  choices = c("Scaled differences to ensemble" = "scaled",
                                                                              "Unscaled differences to ensemble" = "unscaled"
                                                                              # "Delta values (only 1x1 comparison)" = "deltas",
                                                                              # "Raw values (only 1x1 comparison)" = "no_scaled"
                                                                  ),
                                                                  selected = "scaled"),
                                                     p("When the comparison uses scaled values, several variables are permitted in each
                                                       axis used in a combined fashion, allowing for comparing differences in more than 
                                                       two variables altogether. When using this approach, only variables that are comparable 
                                                       should be combined in one axis (i.e. precipitation and temperature variables should not
                                                       be combined, and only variables that vary in the same fashion –that is, larger means greater,
                                                       shorter means smaller– should be mixed). In this scaled figure, a 95% confidence level of 
                                                       the ensemble mean is drawn to highlight the models that do not differ larger than these limits
                                                       in their projections."),
                                                     downloadButton('download_prec_temp_scaled', 'Download figure'),
                                                     downloadButton('download_comparison_table', 'Download table')),
                                    conditionalPanel("input.future_panel == 'maps_fut_diff'",
                                                     h5("Maps"),
                                                     p("These maps show the spatial distribution of each GCM deviations 
                                                       from the mean ensemble for each bioclimatic variable."),
                                                     h5("Visualization options"),
                                                     radioButtons("add_layer3", "Overlay on maps:",
                                                                  c("Do not overlay anything" = "nothing",
                                                                    "Country borders" = "countries",
                                                                    "Biomes" = "biomes",
                                                                    "Ecoregions" = "ecoregions"),
                                                                  selected = "nothing"))
                                    )
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
                                                       # uiOutput("tab3_ideas_for_selecting"), 
                                                       # hr(),
                                                       h5("Table with the underlying data"),
                                                       tableOutput("comparison_table")),
                                      conditionalPanel("input.type_scaled == 'unscaled'",
                                                       plotlyOutput("plot_temp_prec_realunscaled", height = "100%"),
                                                       hr(),
                                                       # uiOutput("tab3_ideas_for_selecting"), 
                                                       hr())#,
                                      # tableOutput("comparison_table"))#,
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
             mainPanel(
               column(5),
               column(7,
                      h4("Report of the session"),
                      hr(),
                      p("In this tab, you can generate a report as a pdf file containing all the
                        results produced during the session."),
                      p("This report is intended at 3 things:"),
                      p("- Assure the repeatability of the analysis, as it will contain all the 
                        setting that you used to obtain the results"),
                      p("- Provide you with a full summary of all the results that were obtained
                        during the session"),
                      p("- Complement the results with information relevant to their interpretation"),
                      p("Make sure to generate each one of the results in all the pages available in
                        the app for them to appear in your report."),
                      hr(),
                      downloadButton("report", "Generate report",
                                     style="color: #fff; background-color: #57be91; 
                                     border-color: #2e6da4; font-size:140%; padding-left:10px"))
             )
             )
    )
)
