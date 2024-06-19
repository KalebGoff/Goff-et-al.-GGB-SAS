# Title: 03B_SAS_species_aggregation
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#The goal of this script is to go by target region (or peak) and aggregate taxa that have not been reliably identified across all years to the genus level. Rationale for aggregation is also provided. A combination of the peak x year cleaning lists (script 02F), an analysis of species-level gains and losses, and expert opinion were used to decide what should be aggregated and where.

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
#Candidates for aggregation across all study areas:

#Poa glauca/secunda/cusickii

#Castilleja nana/pilosa - only where both have been seen

#Boechera (all of them?) or at least howellii/lemmonii/depauperata/pendulocarpa

#Draba oligosperma/subumbellata (only at BAR/RNA)

#Stipa pinetorum/Stipa occidentalis - only where both have been seen

#Festuca saximontana/brachyphylla/minutiflora - only where both have been seen

#Oreocarya flavoculata/humilis - only where both have been seen

###########################################################################################################
#CAT

#Across the entire TR:

#Aggregate all Boechera to Boechera sp.:

boe_cat <- SAS_dat_3 %>% 
  filter(target_region == "cat") %>% 
  filter(str_detect(species, "Boechera")) #partial matching of anything with Boechera __ in the species column!

#Now change all these to Boechera sp.:

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_cat)

boe_cat$species <- replace(boe_cat$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_cat)

#works!

#Aggregate Poa secunda/glauca:
poa_cat <- SAS_dat_3 %>% 
  filter(target_region == "cat") %>% 
  filter(str_detect(species, "Poa"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(poa_cat)

poa_cat$species <- replace(poa_cat$species, values = "Poa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(poa_cat)

#Aggregate Oreocarya humilis/flavoculata:
oreo_cat <- SAS_dat_3 %>% 
  filter(target_region == "cat") %>% 
  filter(str_detect(species, "Oreocarya"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(oreo_cat)

oreo_cat$species <- replace(oreo_cat$species, values = "Oreocarya sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(oreo_cat)

#Aggregate Castilleja nana/pilosa:

casti_cat <- SAS_dat_3 %>% 
  filter(target_region == "cat") %>% 
  filter(str_detect(species, "Castilleja"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(casti_cat)

casti_cat$species <- replace(casti_cat$species, values = "Castilleja nana/pilosa")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(casti_cat)

################################################################################################
#DEV - This would be a good place to ask Jan if there are any issues.

#aggregate all Boechera:
boe_dev <- SAS_dat_3 %>% 
  filter(target_region == "dev") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_dev)

boe_dev$species <- replace(boe_dev$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_dev)

#Aggregate Stipa pinetorum/Stipa occidentalis on ben? There is only one record of occidentalis.

#################################################################################################
#GRB

#Aggregate Poa secunda/glauca? almost always was the case that they were both seen.

#Aggregate Festuca brachyphylla/minutiflora/saximontana

festu_grb <- SAS_dat_3 %>% 
  filter(target_region == "grb") %>% 
  filter(str_detect(species, "Festuca"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(festu_grb)

festu_grb$species <- replace(festu_grb$species, values = "Festuca sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(festu_grb)

#Aggregate Poa cusickii/Poa fendleriana?

##############################################################################################
#LAN

#All Boechera to Boechera sp.

boe_lan <- SAS_dat_3 %>% 
  filter(target_region == "lan") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_lan)

boe_lan$species <- replace(boe_lan$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_lan)

#Stipa occidentalis and Stipa pinetorum to Stipa sp.

stipa_lan <- SAS_dat_3 %>% 
  filter(target_region == "lan") %>% 
  filter(species == "Stipa occidentalis" | species == "Stipa pinetorum")

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(stipa_lan)

stipa_lan$species <- replace(stipa_lan$species, values = "Stipa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(stipa_lan)

###############################################################################################
#SND

#Aggregate all Boechera

boe_snd <- SAS_dat_3 %>% 
  filter(target_region == "snd") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_snd)

boe_snd$species <- replace(boe_snd$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_snd)

#Aggregate all Poa

poa_snd <- SAS_dat_3 %>% 
  filter(target_region == "snd") %>% 
  filter(str_detect(species, "Poa"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(poa_snd)

poa_snd$species <- replace(poa_snd$species, values = "Poa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(poa_snd)

#Aggregate Oreocarya flavoculata/humilis

oreo_snd <- SAS_dat_3 %>% 
  filter(target_region == "snd") %>% 
  filter(str_detect(species, "Oreocarya"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(oreo_snd)

oreo_snd$species <- replace(oreo_snd$species, values = "Oreocarya sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(oreo_snd)

#Aggregate Draba?

###############################################################################################
#SWE

#Aggregate Antennaria

ant_swe <- SAS_dat_3 %>% 
  filter(target_region == "swe") %>% 
  filter(str_detect(species, "Antennaria"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(ant_swe)

ant_swe$species <- replace(ant_swe$species, values = "Antennaria sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(ant_swe)

#Aggregate all Boechera

boe_swe <- SAS_dat_3 %>% 
  filter(target_region == "swe") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_swe)

boe_swe$species <- replace(boe_swe$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_swe)

#Aggregate all Poa

poa_swe <- SAS_dat_3 %>% 
  filter(target_region == "swe") %>% 
  filter(str_detect(species, "Poa"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(poa_swe)

poa_swe$species <- replace(poa_swe$species, values = "Poa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(poa_swe)

############################################################################################
#WDS

#Aggregate Boechera

boe_wds <- SAS_dat_3 %>% 
  filter(target_region == "wds") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_wds)

boe_wds$species <- replace(boe_wds$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_wds)

#Aggregate Poa

poa_wds <- SAS_dat_3 %>% 
  filter(target_region == "wds") %>% 
  filter(str_detect(species, "Poa"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(poa_wds)

poa_wds$species <- replace(poa_wds$species, values = "Poa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(poa_wds)

#Aggregate Festuca brachyphylla/minutiflora

festu_wds <- SAS_dat_3 %>% 
  filter(target_region == "wds") %>% 
  filter(str_detect(species, "Festuca"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(festu_wds)

festu_wds$species <- replace(festu_wds$species, values = "Festuca sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(festu_wds)

#Aggregate Oreocarya

oreo_wds <- SAS_dat_3 %>% 
  filter(target_region == "wds") %>% 
  filter(str_detect(species, "Oreocarya"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(oreo_wds)

oreo_wds$species <- replace(oreo_wds$species, values = "Oreocarya sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(oreo_wds)

##############################################################################################
#WIM

#Aggregate Boechera

boe_wim <- SAS_dat_3 %>% 
  filter(target_region == "wim") %>% 
  filter(str_detect(species, "Boechera"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(boe_wim)

boe_wim$species <- replace(boe_wim$species, values = "Boechera sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(boe_wim)

#Aggregate Poa

poa_wim <- SAS_dat_3 %>% 
  filter(target_region == "wim") %>% 
  filter(str_detect(species, "Poa"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(poa_wim)

poa_wim$species <- replace(poa_wim$species, values = "Poa sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(poa_wim)

#Aggregate Oreocarya

oreo_wim <- SAS_dat_3 %>% 
  filter(target_region == "wim") %>% 
  filter(str_detect(species, "Oreocarya"))

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(oreo_wim)

oreo_wim$species <- replace(oreo_wim$species, values = "Oreocarya sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(oreo_wim)

#Aggregate Draba subumbelata and Draba oligosperma on bar and rna:

draba_rna_bar <- SAS_dat_3 %>% 
  filter(peak == "rna" | peak == "bar") %>% 
  filter(species == "Draba subumbellata" | species == "Draba oligosperma")

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(draba_rna_bar)

draba_rna_bar$species <- replace(draba_rna_bar$species, values = "Draba sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(draba_rna_bar)

#Aggregate Festuca brachyphlla/saximontana/minutiflora

festu_wim <- SAS_dat_3 %>% 
  filter(target_region == "wim") %>% 
  filter(species == "Festuca brachyphylla" | species == "Festuca saximontana" | species == "Festuca minutiflora")

SAS_dat_3 <- SAS_dat_3 %>% 
  anti_join(festu_wim)

festu_wim$species <- replace(festu_wim$species, values = "Festuca sp.")

SAS_dat_3 <- SAS_dat_3 %>% 
  bind_rows(festu_wim)
