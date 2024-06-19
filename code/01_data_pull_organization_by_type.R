# Title: 01_data_pull_organization_by_type
# Author: Meagan Oldfather, Seema Sheth, Kaleb Goff
# Date: October 2022

##############################################################################################################

# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse", "googledrive", "pbapply","janitor","fuzzyjoin")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

#############################################################################################################

#https://github.com/meaganfoldfather/GGB_species_compilation/blob/main/01_Downloading_FromDrive.R
#Intent: Bring in all data files under data_summit from google drive

#Connect to google drive before jumping in
#drive_auth()

#Create a new folder
#dir.create("C:/Users/caloc/Desktop/Academic/GLORIA/GGB Data Analysis/data-raw/outML")

#Vector of summits
#summits <- c("us_cat", "us_dev", "us_grb", "us_lan", "us_snd", "us_swe", "us_wds", "us_wim")

# Make google drive file pathways for each summit folder
#summit_pathways <- lapply(summits, FUN = function(x) {
#  this_pathway <-  drive_ls(path = paste0("copied_data_summit/", x), type = "csv", recursive = TRUE)
#  return(this_pathway)
#})

# Loop over file pathway names to download each file and put in 'out' folder; google drive doesn't let you bulk download files
#lapply(summit_pathways, FUN = function(this_pathway){ 
#  for(i in 1:nrow(this_pathway)){

#    drive_download(file = this_pathway[i,], path = file.path("C:/Users/caloc/Desktop/Academic/GLORIA/GGB Data Analysis/data-raw/outML", this_pathway[i,"name"]), #overwrite = TRUE)
#  }
#})

###########################################################################################################################################

#https://github.com/meaganfoldfather/GGB_species_compilation/blob/main/02_Subsetting_Reading_Files.R
# get the list of all of the .csv files that we just downloaded
# from google drive and put them into a simple dataframe that
# includes the full name (including the out/ folder) and a separate
# column that just includes the basename (for simplicity and dividing
# up the metadata encoded within the file name)
files <- 
  list.files("./data-raw/outML", full.names = TRUE) %>%
  as_tibble() %>% 
  setNames("fullname") %>% 
  dplyr::mutate(basename = basename(fullname))

# Some of the .csv files (remember we just downloaded *all* of them from
# the google drive) are HOBO data and are distinguished by being shorter
# filenames because all of the metadata aren't included in the filename
# we subset out those files here.
files <-
  files %>% 
  dplyr::filter(nchar(basename) > 30)

# Now we use the {tidyr} package to break apart the "basename" column
# in order to extract the individual pieces of metadata and create a 
# new filename
files <-
  files %>% 
  tidyr::separate(col = basename, into = c("country_code", "target_region", "peak", "data_type", "obs_or_attr", "raw_or_cooked", "year"), sep = "_", remove = FALSE)

# subset to survey observation files and separate the data types 
files
unique(files$data_type)
data_type_list <-
  files %>% 
  dplyr::filter(obs_or_attr == "survey-observations") %>% 
  dplyr::group_by(data_type) %>% 
  dplyr::group_split()
data_type_list

#read in files from all data types 
data_type_list_df <-
  pblapply(data_type_list, FUN = function(x) {
    this_data_type_list <- 
      lapply(1:nrow(x), FUN = function(y) {
        this_specific_survey_data <- read.csv(file = x$fullname[y], colClasses = "character")
        return(this_specific_survey_data)
      })
    this_data_type <- dplyr::bind_rows(this_data_type_list) %>% as_tibble()
    
    return(this_data_type)
  })

data_type_list_df
length(data_type_list_df)
data_type_list_df[[1]] #10x10 data
data_type_list_df[[2]] #1x1 data
data_type_list_df[[3]] #1x1 quad cell data
data_type_list_df[[4]] #SAS data

#############################################################################################################

#pull out individual data types, check them, and merge into a single data frame:
#10x10:
tenBYten_data <- data_type_list_df[[1]]

#1x1:
oneByone_data <- data_type_list_df[[2]]
oneByoneCELL_data <- data_type_list_df[[3]]
oneByone_merged <- bind_rows(oneByone_data, oneByoneCELL_data)

#SAS:
sas_data <- data_type_list_df[[4]]

#All data!
ggb_all_data <- bind_rows(tenBYten_data, oneByone_merged, sas_data)

######################################################################################################
#Data cleaning: check country, target region, peak, year, data_type, aspect for obvious problems:

#country
unique(ggb_all_data$country)
ggb_all_data$country <- "us" #but perhaps we should investigate those blanks/NAs?

#target region
unique(ggb_all_data$target_region) #problems: "wms", "tel"
ggb_all_data$target_region[ggb_all_data$target_region == "wms"] <- "wds"
ggb_all_data$target_region[ggb_all_data$target_region == "tel"] <- "dev"

#peak
unique(ggb_all_data$peak) #should be 29 peaks!
#dlo == 332
ggb_all_data$peak[ggb_all_data$peak == "dlo"] <- "332"
#dmi == 357
ggb_all_data$peak[ggb_all_data$peak == "dmi"] <- "357"
#dhi == 374
ggb_all_data$peak[ggb_all_data$peak == "dhi"] <- "374"
#gro == grl
ggb_all_data$peak[ggb_all_data$peak == "gro"] <- "grl"
#lsw == lws
ggb_all_data$peak[ggb_all_data$peak == "lsw"] <- "lws"
#swe == sme
ggb_all_data$peak[ggb_all_data$peak == "swe"] <- "sme"
#fws == fsw
ggb_all_data$peak[ggb_all_data$peak == "fws"] <- "fsw"

#year
unique(ggb_all_data$year) #good

#data_type
unique(ggb_all_data$data_type) #there are 4 data types.

#aspect
unique(ggb_all_data$aspect) #issues: "e or s", "sup s "
aspect_issues <- ggb_all_data %>% filter(ggb_all_data$aspect == "e or s") #this was only recorded on snd_374_2009 10x10, what's the story? It was incomplete and they never actually figured out if the one that was done was e or s.
aspect_issues2 <- ggb_all_data %>% filter(ggb_all_data$aspect == "sup s ") #this was only recorded on wim_wmt_2009 10x10, what aspect does it actually correspond to?

#######################################################################################################################################
#Species column cleaning: this is a relic of a past cleaning step, which persists now because I sent Jan a list after making these changes. This means that if we get rid of these changes, some might be missed in the following script. And, if I add them to the following script, some might be made multiple times. So, they are getting dumped here until I can figure out how to fix this issue. The other issue here was that this was generously and time-consumingly written by Seema, who wisely followed the Jepson taxonomy.

#https://github.com/meaganfoldfather/GGB_species_compilation/blob/main/04_Resolving_SpeciesNames.R

source("./code/01A_relict_taxonomic_changes.R", local = knitr::knit_global())

#######################################################################################################################
#write out this first step and move on!

write_csv(ggb_all_data, "./data-derived/cl_ggb_all_data.csv")

cl_SAS <- ggb_all_data %>% 
  filter(data_type == "summit-area-section")

write_csv(cl_SAS, "./data-derived/cl_SAS.csv")

cl_tenBY <- ggb_all_data %>% 
  filter(data_type == "10m-x-10m-plot")

write_csv(cl_tenBY, "./data-derived/cl_tenBY.csv")

cl_oneBY <- ggb_all_data %>% 
  filter(data_type == "1m-x-1m-quadrat" | data_type == "1m-x-1m-quadrat-cell")

write_csv(cl_oneBY, "./data-derived/cl_oneBY.csv")

