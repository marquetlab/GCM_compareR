# Climatic variables
# devtools::install_github("ropensci/ccafs")
library(ccafs)
library(raster)
library(tidyverse)
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


resolution <- c("10m"#,  # 20 km2
                # "5m"#,   # 10 km2
                # "180s", # 5 km2
                # "30s"  # 1 km2
)

dir.create("clim_data")
dir.create("clim_data/ccafs")
dir.create("clim_data/ccafs/raw_rasters")
dir.create("clim_data/ccafs/rds")

for (res in resolution){
  glue::glue("# Getting data for resolution: {res}") %>% print
  for (year in c(
    "2050"#,
    # "2070"
    )){
    glue::glue("#  Year: {year}") %>% print
    for (rcp in c("rcp26",
                  "rcp45",
                  "rcp60",
                  "rcp85")){
      ### rcp 4.5
      # URL of variables
      glue::glue("#   Searching files for rcp {rcp}") %>% print
      var_urls <- cc_search(file_set = 12, # Delta method IPCC AR5
                            extent = "global",
                            format = "ascii",
                            scenario = case_when(rcp == "rcp26" ~ 7,
                                                 rcp == "rcp45" ~ 8,
                                                 rcp == "rcp60" ~ 9,
                                                 rcp == "rcp85" ~ 10),
                            period = case_when(year == "2050" ~ 6,
                                               year == "2070" ~ 8),
                            variable = 1, # bioclimatic
                            resolution = case_when(res == "10m" ~ 4,
                                                   res == "5m" ~ 3,
                                                   res == "180s" ~ 2,
                                                   res == "30s" ~ 1))
      
      # Names of variables
      glue::glue("#     Getting variable names") %>% print
      var_names <- var_urls %>%
        str_split(pattern = "/") %>%
        map(~ .x[[10]])
      
      # Download variables
      glue::glue("#     Downloading variables") %>% print
      var_data <- var_urls %>%
        map(~ cc_data_fetch(.x, progress = TRUE))
      
      # Read variables
      glue::glue("#     Reading variables") %>% print
      # Sometimes, a GCM does not contain files -> remove these
      for (j in length(var_data):1){
        if(length(var_data[[j]]) == 0) {
          var_data[j] <- NULL
          var_names[j] <- NULL
        }
      }
      
      # Read data as raster
      var_rast <- var_data %>%
        map(~ cc_data_read(.x)) %>%
        set_names(var_names)
      # var_rast <- var_rast %>%   # Read rasters in memory, so they can be saved lateron
      #   map(readAll)
      
      # Save to rds
      glue::glue("#     Saving variables as rds") %>% print
      for (i in 1:length(var_rast)){
        var_i <- var_rast[[i]]
        proj4string(var_i) <- wgs
        name <- paste0(names(var_rast[i]), ".", res, ".", "year2070", ".", rcp)
        names <- names(var_i)
        # Save it as separate layers (to use GTiff, which is compresed unlike gri)
        var_i %>% as.list %>%
          walk2(.y = names, ~ writeRaster(.x, paste0("clim_data/ccafs/raw_rasters/", name, ".", .y,".tif"), format = "GTiff", overwrite = T))
        
        # Re-read stack from the newly created files, and save that stack as a rds
        var_j <- names %>%
          map(~ raster(paste0("clim_data/ccafs/raw_rasters/", name, ".", .x,".tif"))) %>%
          stack() %>%
          setNames(names)
        
        saveRDS(var_j, paste0("clim_data/ccafs/rds/", name, ".rds"))
        # assign(name, var_i)
        # save(list = name,
        #      file = paste0("data/", name, ".rda"),
        #      ascii = TRUE)
      }
      
      # # Save to rda
      # glue::glue("#     Saving variables as rda") %>% print
      # for (i in 1:length(var_rast)){
      #   var_i <- var_rast[[i]]
      #   proj4string(var_i) <- wgs
      #   name <- paste0(names(var_rast[i]), ".", res, ".", "year2070", ".", rcp)
      #   assign(name, var_i)
      #   save(list = name,
      #        file = paste0("data/", name, ".rda"),
      #        ascii = TRUE)
      # }
      
      # Clean workspace
      rm(var_rast, var_i, name, var_data)  # rm(var_rast2)
      cc_cache_delete_all()
    }
  }
}


# convertir a rds

# setwd(paste0("../gcmcompareR_data/ccafs_vars/", res, "/"))
#
# for (a in list.files()){
#   a_name <- a %>% str_replace(".rda", "")
#   load(a)
#   writeRaster(get(a_name), paste0("../raw_rasters/", a_name, ".grd"), format = "raster", overwrite = T)
#   a_stack <- stack(paste0("../raw_rasters/", a_name, ".grd"))
#   saveRDS(a_stack, paste0(a_name, ".rds"))
# }





##### BASELINE
# A mano
names <- paste0("bio_", 1:19)

# Read layers
var_j <- names %>%
  purrr::map(~ raster(paste0("clim_data/baseline/raw_rasters/baseline.10m.", .x,".tif"))) %>%
  stack() %>%
  setNames(names)
saveRDS(var_j, paste0("clim_data/baseline/rds/baseline.10m.rds"))


# library(raster)
# dir.create("clim_data/baseline")
# dir.create("clim_data/baseline/raw_rasters")
# dir.create("clim_data/baseline/rds")
# #
# bio10 <- getData("worldclim", var = "bio", res = 10)
# names(bio10) <- paste0("bio_", 1:19)
# # bio5 <- getData("worldclim", var = "bio", res = 5)
# # bio2.5 <- getData("worldclim", var = "bio", res = 2.5)
# #
# names <- names(bio10)
# # Save it as separate layers (to use GTiff, which is compresed unlike gri)
# bio10 %>% as.list %>%
#   walk2(.y = names, ~ writeRaster(.x, paste0("clim_data/baseline/raw_rasters/baseline.10m.", .y,".tif"), format = "GTiff", overwrite = T))
# # writeRaster(var_i, paste0("clim_data2/ccafs/raw_rasters/", name, ".tif"), format = "GTiff", overwrite = T)
# 
# # Re-read stack from the newly created files, and save that stack as a rds
# var_j <- names %>%
#   map(~ raster(paste0("clim_data/baseline/raw_rasters/baseline.10m.", .x,".tif"))) %>%
#   stack() %>%
#   setNames(names)
# 
# saveRDS(var_j, paste0("clim_data/baseline/rds/baseline.10m.rds"))
# 
# 
# # writeRaster(bio10, paste0("clim_data/baseline/raw_rasters/baseline.10m.grd"), format = "raster", overwrite = T)
# # saveRDS(bio10, "clim_data/baseline/10m/baseline.10m.rds")
# #
# # writeRaster(bio5, paste0("clim_data/baseline/raw_rasters/baseline.5m.grd"), format = "raster", overwrite = T)
# # saveRDS(bio5, "clim_data/baseline/5m/baseline.5m.rds")
# #
# # writeRaster(bio2.5, paste0("clim_data/baseline/raw_rasters/baseline.2_5m.grd"), format = "raster", overwrite = T)
# # saveRDS(bio2.5, "clim_data/baseline/2_5m/baseline.2_5m.rds")
# 
# 
# # Future from Worldclim
# library(raster)
# dir.create("clim_data/worldclim")
# dir.create("clim_data/worldclim/raw_rasters")
# dir.create("clim_data/worldclim/rds")
# 
# resolution <- c(10#,  # 20 km2
#                 # "5m"#,   # 10 km2
#                 # "180s", # 5 km2
#                 # "30s"  # 1 km2
# )
# models <- c("AC",
#             "BC", "CC", "CE", "CN", "GF",
#             "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", "NO")
# 
# for (res in resolution){
#   glue::glue("# Getting data for resolution: {res}") %>% print
#   for (year in c(#50,
#     70)){
#     glue::glue("#  Year: {year}") %>% print
#     for (rcp in c(26,
#                   # 45,
#                   60,
#                   85
#     )){
#       for (m in models){
#         vars <- try(getData("CMIP5", var = "bio", res = res, year = year, rcp = rcp, model = m))
#         # if(!inherits(vars, "try-error")){
#         if(length(vars) != 0){
#           names(vars) <- paste0("bio_", 1:19)
#           
#           names <- names(vars)
#           name <- paste0(m, ".", res, "m.", "year2070", ".rcp", rcp)
#           
#           vars %>% as.list %>%
#             walk2(.y = names, ~ writeRaster(.x, paste0("clim_data/worldclim/raw_rasters/", name, ".", .y,".tif"), format = "GTiff", overwrite = T))
#           
#           vars2 <- names %>%
#             purrr::map(~ raster(paste0("clim_data/worldclim/raw_rasters/", name, ".", .x,".tif"))) %>%
#             stack() %>%
#             setNames(names)
#           
#           saveRDS(vars2, paste0("clim_data/worldclim/rds/", name, ".rds"))
#         }
#       }
#     }
#   }
# }
# 
# 
# 
