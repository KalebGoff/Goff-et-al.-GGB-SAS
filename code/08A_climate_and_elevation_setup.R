# Title: 08_07B_climate_and_elevation_setup
# Author: Kaleb Goff
# Date: October 2023

################################################################################################################
# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

#make vector of packages needed

packages_needed <- c("tidyverse", "elevatr", "sp", "ggrepel", "ggpubr")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

##################################################################################################

#Import and prep GLORIA summit data for Climate NA:

#import summit coordinate data:
summit_coords <- read_csv("./data-raw/ggb-summit-coordinates_KAG.csv")

#Pull elevation data for each peak using elevatr package:
elev_df <- summit_coords[,3:4]

prj_dd <- "EPSG:4326" 

# Create a SpatialPoints
examp_sp <- SpatialPoints(elev_df, proj4string = CRS(prj_dd))

# Create a SpatialPointsDataFrame
examp_spdf <- SpatialPointsDataFrame(coords = examp_sp, data = elev_df)

spdf_GGB <- get_elev_point(examp_sp, src = c("epqs"))

#prep data for Climate NA:

#add elevation column to GGB_data sheet, rename to match requirements for ClimateNA (id1, lat, long, elev)

ggb_geo_elev <- cbind(summit_coords, spdf_GGB$elevation)

names(ggb_geo_elev)[5] <- 'elev'

ggb_geo_elev <- ggb_geo_elev %>%
  rename(id1 = target_region,
         id2 = peak)

#combine id1 and id2

ggb_geo_elev <- ggb_geo_elev %>%
  unite("id2", id2:id1, sep = "_")

#create numbered 1-29 id1 column (needed for ClimateNA):

ggb_geo_elev$id1 <- c(1:29)

col_order <- c("id1", "id2", "lat", "long",
               "el")

ggb_geo_elev <- ggb_geo_elev[, col_order]

#write organized, ClimateNA ready data into csv so you don't have to re-do this process:
#write_csv(ggb_geo_elev, "./data-derived/ggb_geo_elev_forClimNA.csv")

ggb_geo_elev <- read_csv("./data-derived/ggb_geo_elev_forClimNA.csv")

########################################################################################################
#Get climate data from point/elevation information for each peak and year use Climate NA desktop.
#make sure data is formatted correctly, go to bottom, historical time series, click years, then download! Remember that ClimNA needs columns with the names id1, id2, lat, long, el; or it will not work!