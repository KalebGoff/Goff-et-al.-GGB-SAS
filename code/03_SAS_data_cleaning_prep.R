# Title: 03_SAS_data_cleaning_prep
# Author: Kaleb Goff
# Date: September 2023

##############################################################################################################
#The goal of this script is to export a data frame that can be used for all subsequent analyses using the summit-area-section data type. This means further cleaning of the abundance data, removing intraranks, removing species only seen once at one location, remove everything only identified to genus ("sp."), and dealing with duplicates. 


################################################################################################################
# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

#################################################################################################################
#Load in data post-taxonomic updates via Jan's List:

#import data:
SAS_data <- read_csv("./data-derived/cl_SAS_janslist.csv")

#Select only the columns that belong to this data type, first check that they are all empty:

#columns that don't belong: lane, presence, X, x, y, cover, row, column
# select columns
SAS_data <- SAS_data[,c("country", "target_region", "peak", "year", "aspect", "data_type", "contour", "species", "cf", "abundance", "pct_cover")]

#################################################################################################################
#Clean SAS abundance and pct_cover columns:

#There are still a number of unresolved issues in this section. Namely, how to reconcile the old SAS abundance method (quantitative in m2) with the qualitative letter codes system. This will be addressed in the cover data analysis script.

#Another issue that needs to be addressed in this section is how to merge the cover of what became duplicates after we committed the name changes. For example, if an SAS plot included Androsace sp. and Androsace septentrionalis, I changed the name Androsace sp. to Androsace septentrionalis. This creates instances where there are two of the same names in the same plot with different covers.

source("./code/03A_SAS_cover_cleaning.R", local = knitr::knit_global())

#Check:

table(SAS_qual$abundance)
table(SAS_qual$pct_cover)

SAS_dat_3 <- SAS_qual #re-name post cover cleaning.

#################################################################################################################
#Remove intra-specific ranks, following Boch et al. 2022 on reducing pseudoturnover.

SAS_dat_3$species <- gsub("\\ var..*","",
                         gsub("\\ ssp..*","",
                         gsub("\\ spp..*","",
                         gsub("\\ subsp..*","", SAS_dat_3$species))))

table(SAS_dat_3$species)

####################################################################################################################
#Remove everything not identified to the species level ("sp."). In the future, we could decide to leave some of these records selectively, to aggregate our uncertainty about groups of difficult to identify taxa.

#SAS_dat_sp_rm <- SAS_dat_3[!grepl("sp.$", SAS_dat_3$species),]

#We do not need to remove everything identified to "sp." any longer, because we addressed the peak x year lists and in many cases "sp." was caused by observers being overly cautious and putting sp. even when there was only one option.

#However, we now need to address places where members of a given genus were unreliably identified across all years on a given peak. The following sourced script addresses those issues, and aggregates to the genus level:

source("./code/03B_SAS_species_aggregation.R", local = knitr::knit_global())

####################################################################################################################
#Check distinct species:

sp_check_f <- SAS_dat_3 %>% 
  distinct(species)

#remove the word "hybrid" on Artemisia nova x rothrockii hybrid:

SAS_dat_3$species[SAS_dat_3$species == "Artemisia nova x rothrockii hybrid"] <- "Artemisia nova x rothrockii"

SAS_cl <- SAS_dat_3

###############################################################################################
#Final changes (or near final) set of taxonomic changes from Jan, 12 SEPT 2023:

#removals:
#Agropyron sp.
SAS_cl <- SAS_cl %>% 
  filter(!species == "Agropyron sp.")

#Elymus sp.
SAS_cl <- SAS_cl %>% 
  filter(!species == "Elymus sp.")

#Elymus trachycaulus x scribneri, and xElyleymus aristatus
SAS_cl <- SAS_cl %>% 
  filter(!species == "Elymus trachycaulus x scribneri")

SAS_cl <- SAS_cl %>% 
  filter(!species == "xElyleymus aristatus")

#Chenopodium sp.
SAS_cl <- SAS_cl %>% 
  filter(!species == "Chenopodium sp.")

#fixes:
#Minuartia sp. @ PMD 2008 -> Minuartia obtusiloba (sp. was only seen at PMD 2008 so this works).
SAS_cl$species[SAS_cl$species == "Minuartia sp."] <- "Minuartia obtusiloba"

#Senecio werneriifolius to Packera werneriifolia
SAS_cl$species[SAS_cl$species == "Senecio werneriifolius"] <- "Packera werneriifolia"

#Artemisia at SWE:
#rothrockii ONLY at FRY
#rothrockii and nova on BEL
#how many records are assigned to the hybrid?
art_hybrid <- SAS_cl %>% 
  filter(species == "Artemisia nova x rothrockii")
#three records!
#remove the hybrid:
SAS_cl <- SAS_cl %>% 
  filter(!species == "Artemisia nova x rothrockii")

#Change nova at FRY to rothrockii (we know all the plants there are roth):
roth_fry <- SAS_cl %>% 
  filter((peak == "fry" & species == "Artemisia nova"))

SAS_cl <- SAS_cl %>% 
  anti_join(roth_fry)

roth_fry$species[roth_fry$species == "Artemisia nova"] <- "Artemisia rothrockii"

SAS_cl <- SAS_cl %>% 
  bind_rows(roth_fry)

####################################################################################################
#As a final check, and for a supplementary data file, create a csv with species as rows, and tr/peak/year grouped as columns, with presence/absence of all species noted:

#remove columns we don't need and aggregate to peak level (group by peak/year, and keep distinct species inside those groups)
supp_SAS_dat <- SAS_cl %>% 
  select(-cf, -abundance, -pct_cover, -country) %>% 
  group_by(target_region, peak, year) %>% 
  distinct(species)

#remove cover classes (solid rock, bare ground, scree, litter, bryophytes, lichen):

cover_class <- c("solid rock", "bare ground", "scree", "litter", "bryophytes", "lichen")

supp_SAS_dat <- supp_SAS_dat %>% 
  filter(!species %in% cover_class)

#now we do some kind of complicated pivot_wider where the species become the rows and the combinations of TR/peak/year become columns:

#first, create a column with 1 for present:

supp_SAS_dat$seen <- 1

#pivot, fill missing values with 0 for absent:
test_supp <- supp_SAS_dat %>% 
  pivot_wider(names_from = c(target_region, peak, year), values_from = seen, values_fill = 0)

#write this out and clean up in excel:

write_csv(test_supp, "./data-derived/SAS_supplemental_data_v2raw.csv")
  
#################################################################################################################
#Export clean data:

write_csv(SAS_cl, "./data-derived/SAS_clean_data.csv")



