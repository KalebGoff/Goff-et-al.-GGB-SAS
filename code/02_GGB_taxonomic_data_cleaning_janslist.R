# Title: 02_GGB_taxonomic_data_cleaning_janslist
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#The purpose of this script is to clean the GLORIA Great Basin Data on the whole-dataset scale, with a focus on including updates and changes from taxonomic experts such as Jan Nachlinger. The broad goal is to export a version of the GGB data that can be used for many different subsequent analyses.

#This script is the backbone, and contains several source scripts:

#02A: Typo, Name Change
#02B: Mis-Identifications
#02C: Issues
#02D: Additions
#02E: Omissions
#02F: Manual cleaning from peak x species lists (organized by TR)

################################################################################################################

# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse", "googledrive")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

#################################################################################################################
#Bring in list of cleaning changes recommended by Jan Nachlinger, clean up columns:
janslist <- read_csv("./data-raw/draft03_JansList_save3_for_2022_analysis.csv")

janslist <- janslist %>% 
  select(target_region, peak, year, aspect, species, `Jan's Notes`) %>% 
  rename(Jan.s.Notes = `Jan's Notes`)

#"janslist" is a spreadsheet with all species in each target region, peak and year. This list was given to Jan Nachlinger, GLORIA Great Basin expert botanist, for taxonomic cleaning and represents her attempt to look through every data sheet, comment, note, photo, voucher and voucher scrap available and discern whether or not a given taxon is valid in a given target region, peak and year.
#Jan went row by row and marked each record with a ! for valid, or a problem code (see below) if it was invalid or needed to be changed. For these, Jan wrote her rationale, and for all rows, Jan provided a taxonomic reference.

################################################################################################################

#Create a list of "problem codes", devised by Jan to sort types of cleaning.
janslist$problem_code <- substr(janslist$Jan.s.Notes, 1, 2)

table(janslist$problem_code)

#Remove things that are not problems "!" and " ".
janslist_problems <- janslist %>% 
  filter(problem_code != "! ") %>% 
  filter(problem_code != "  ") %>% 
  filter(problem_code != " !") %>% 
  filter(problem_code != "Cl") %>%
  arrange(problem_code)

#Clean and relabel problem codes:
janslist_problems$problem_code[janslist_problems$problem_code == c("Ad")] <- "Add"
janslist_problems$problem_code[janslist_problems$problem_code == c("AD")] <- "Add"

janslist_problems$problem_code[janslist_problems$problem_code == c("Fe")] <- "NC"

janslist_problems$problem_code[janslist_problems$problem_code == c("Is")] <- "Issue"

janslist_problems$problem_code[janslist_problems$problem_code == c("Mi")] <- "Misident"

janslist_problems$problem_code[janslist_problems$problem_code == c("Om")] <- "Omit"

janslist_problems$problem_code[janslist_problems$problem_code == c("Ty")] <- "Typo"

janslist_problems$problem_code[janslist_problems$problem_code == c("Su")] <- "Issue"

janslist_problems$problem_code[janslist_problems$problem_code == c("En")] <- "NC"

table(janslist_problems$problem_code)

#So, we are left with the following problem codes and definitions:

#Add: Taxa that need to be added to the GGB data set in specific years/locations/plots. From Jan's photos, vouchers and notes.
#Issue: Problems requiring collective decision-making, generally falling into the other problem classes.
#Mis-identification (Misident): Known identification problems, often for very specific years/peaks/plots.
#Name Change (NC): Taxonomic updates and collating names.
#Omit: Taxa that need to be removed altogether.
#Typo: 4 cases Jan noticed where "grb" was entered instead of "snd" for target region.

# For length and clarity, I am going to attempt to address these issues in sourced sub-scripts. These will take the following order:
#02A: Typo and Name Change
#02B: Mis-identifications
#02C: Issues
#02D: Adds
#02E: Omits
#02F: Manual cleaning by target region from peak x species lists

#########################################################################################################################
#Start cleaning!

#import data:
ggb_all_data <- read_csv("./data-derived/cl_ggb_all_data.csv")

#########################################################################################################################
#TYPO & Name Change: Fix typo and name change problem codes:

source("./code/02A_typo_nc.R", local = knitr::knit_global())

#########################################################################################################################
#MISIDENT: Fix mis-identification problem codes:

source("./code/02B_misident.R", local = knitr::knit_global())

##########################################################################################################################
#ISSUE: Fix issue problem code:

source("./code/02C_issue.R", local = knitr::knit_global())

#NOTE: this was essentially used as a catch-all for problems that Jan couldn't readily work out on her own, and needed a second or collective decision on. There are still unresolved/open issues in this section, some of which we might not be able to ever fully resolve. There was a meeting between Jan, Kaleb and Seema where we addressed as many as possible on July 18th, 2022. In some cases, the fixes to these issue require dropping all species in a given genus (i.e. Boechera) to the genus level on given peaks across all years. The list of what genera and peaks this needs to be done for is still in progress.

###########################################################################################################################
#ADD: Address species that need to be added to particular years/peaks/plots. 

#The methodology for this section was to export the "Add" problem codes to a .csv, and then manually add rows. This was necessary because this additions are only for the SAS data type, and if the contour or aspect wasn't noted, I looked up where it was seen and picked the best set of aspects/contours based on prior surveys. Below are the checks and rationale for each of these cases:

Add_janslist <- janslist_problems %>% 
  filter(problem_code == "Add")

#Export to csv:

#write_csv(Add_janslist, "./data-derived/Add_janslist.csv")

#I used this sheet as a reference, and created a new .xlsx, "converted_add_ggb_dataset", in which I plan to get Jan's add information into the correct order to bind to the ggb data set.

#Rationale/checks for adds:

source("./code/02D_adds.R", local = knitr::knit_global())

#converted_add_ggb_dataset is the sheet I worked on, Add_janslist was the original export, jans_add_list_final is the appended sheet:

add_to_GGBdata <-
  read_csv("./data-derived/jans_add_list_final.csv")

ggb_all_data <- ggb_all_data %>% 
  bind_rows(add_to_GGBdata)

############################################################################################################################
#OMIT: Address instances where Jan noted that a particular record needs to be omitted. This was in part a problem related to data being entered as "not present", etc., and part due to Jan noticing things on the datasheets that were not valid, and due to some entry errors. I also ended up adding some 2015 records in the White Mountains via the adds list, and the data for the Whites is lost in 2015, so those are removed. Rationale for each change can be found in the script:

source("./code/02E_omits.R", local = knitr::knit_global())

############################################################################################################################
#Species by peak by year lists: Another round of cleaning was performed, this time starting with creating lists of all the species seen in a given peak and year combination, and displaying this lists side-by-side for a given peak. We put each species in its own row, to facilitate comparison across years on a given peak. This was done manually in excel thanks to Sophie Meng and Emma Wilson. The lists were send to Jan, who devised a method of coloring the cells of a given peak spreadsheet by Green = add the taxon, Red = remove the taxon, Yellow = revise taxon name, Orange = problematic, no clear solution at this time. This process started with the CAT target region, because we visited that target region in 2022 and noticed several problems.

#Many of the changes in this section came down to cases where observers were being extra-conservative about species ID, so they put "sp." on the data sheet when there was really only one option.

source("./code/02F_species_year_peak_lists.R", local = knitr::knit_global())

#If there is only one option I am going to change it to a species, if there are more than one option for species I am going to leave it alone, and let the line that gets rid of all other "sp." lines do so. This is done on a peak-basis, or peak and year basis where needed.

#Clarity from a little more working through this process: it is through these lists that Jan made that we can move from removing sp. to solving sp. entires. Jan marked them as red (remove), but what we are actually often able to do is combine the sp. record with a single species of the same genus. In this way, we will not have to remove sp., the ones we are left with will be those that are actually correct and unknown. Aggregation of suspected misIDs will still need to happen.

#One this this does however, is creates lots of duplicate species names within the same plot, and for the abundance data we need to figure out how to isolate this, and merge (average?) their abundances.

###########################################################################################################################
#Other issues that need to be addressed:

#Duplicates: I noticed a clear instance of duplication, with Agropyron_bld_2013_s10. In this case it was written twice on the original data sheet in the field, and entered twice. In our analysis this is not a big problem because distinct(species) within x groups is usually called. However, this will be a problem for the abundance data, because in some cases two different abundance are listed. Another issue is the way the different data types are entered, which may make cleaning duplicates by data type an easier option.

#misc. changes -- snd 5 and 10m sheets flipped for example, parking this here for now:

#Rename 5 10 and 10 5
#5

sas_snd_flip_5 <- ggb_all_data %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2004")) %>% 
  filter((data_type == "summit-area-section")) %>% 
  filter((aspect == "w")) %>% 
  filter((contour == "5"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(sas_snd_flip_5)

sas_snd_flip_5$contour[sas_snd_flip_5$contour == "5"] <- "10"

as.double(sas_snd_flip_5$contour)

#10

sas_snd_flip_10 <- ggb_all_data %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2004")) %>% 
  filter((data_type == "summit-area-section")) %>% 
  filter((aspect == "w")) %>% 
  filter((contour == "10"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(sas_snd_flip_10)

sas_snd_flip_10$contour[sas_snd_flip_10$contour == "10"] <- "5"

as.double(sas_snd_flip_10$contour)

#join

ggb_all_data <- rbind(sas_snd_flip_10, ggb_all_data)

ggb_all_data <- rbind(sas_snd_flip_5, ggb_all_data)

######################################################################################################
#Output the clean data by data type:

#SAS

write_csv(ggb_all_data, "./data-derived/cl_ggb_all_data_janslist.csv")

cl_SAS_janslist <- ggb_all_data %>% 
  filter(data_type == "summit-area-section")

write_csv(cl_SAS_janslist, "./data-derived/cl_SAS_janslist.csv")

#10x10

cl_tenBY_janslist <- ggb_all_data %>% 
  filter(data_type == "10m-x-10m-plot")

#1x1:

write_csv(cl_tenBY_janslist, "./data-derived/cl_tenBY_janslist.csv")

cl_oneBY_janslist <- ggb_all_data %>% 
  filter(data_type == "1m-x-1m-quadrat" | data_type == "1m-x-1m-quadrat-cell")

write_csv(cl_oneBY_janslist, "./data-derived/cl_oneBY_janslist.csv")




