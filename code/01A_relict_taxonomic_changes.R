# Title: 01A_relict_taxonomic_changes
# Author: Meagan Oldfather, Seema Sheth, Kaleb Goff
# Date: October 2022

##############################################################################################################

# accepted name is Aphyllon but Orobanche is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Aphyllon fasciculatum"] <- "Orobanche fasciculata" 

# A. alpestre is listed as a synonym on Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Athyrium alpestre"] <- "Athyrium distentifolium var. americanum" 

# some instances of Athyrium distentifolium var. americanum (rows 874-877) contain hidden characters and are unmatched; since all remaining Athyrium are this species, this correction works
ggb_all_data$species <- gsub("Athyrium.*","Athyrium distentifolium var. americanum", ggb_all_data$species, fixed = F) 

#misspelled
ggb_all_data$species[ggb_all_data$species =="Astragulus kentrophyta"] <- "Astragalus kentrophyta"

# note this species is non-native and not present in occurrence dataset; we may want to omit! 
ggb_all_data$species[ggb_all_data$species =="Bromus madritensis subsp. rubens"] <- "Bromus rubens"

# minor typo correction
ggb_all_data$species[ggb_all_data$species =="Castilleja applegatei subsp. Martinii"] <- "Castilleja applegatei subsp. martinii" 
ggb_all_data$species[ggb_all_data$species =="Castilleja applegatei ssp. Martinii"] <- "Castilleja applegatei ssp. martinii" 
ggb_all_data$species[ggb_all_data$species =="Castilleja applegatei ssp. martinii"] <- "Castilleja applegatei subsp. martinii" 

# accepted name is Ericameria and Ericameria is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Chrysothamnus parryi"] <- "Ericameria parryi" 

# accepted name is Ericameria and Ericameria is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Chrysothamnus parryi subsp. monocephalus"] <- "Ericameria parryi var. monocephala"

# minor typo correction
ggb_all_data$species[ggb_all_data$species =="Chrysothamnus viscidiflorus subsp. Viscidiflorus"] <- "Chrysothamnus viscidiflorus subsp. viscidiflorus"
ggb_all_data$species[ggb_all_data$species =="Chrysothamnus viscidiflorus ssp. Viscidiflorus"] <- "Chrysothamnus viscidiflorus ssp. viscidiflorus"
ggb_all_data$species[ggb_all_data$species =="Chrysothamnus viscidiflorus ssp. viscidiflorus"] <- "Chrysothamnus viscidiflorus subsp. viscidiflorus" 

# accepted name on Jepson
ggb_all_data$species[ggb_all_data$species =="Cryptantha cinerea"] <- "Cryptantha cinerea var. abortiva" 

# accepted name is Diplacus but Mimulus is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Diplacus bigelovii"] <- "Mimulus bigelovii"

# accepted name is Diplacus but Mimulus is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Diplacus bigelovii var. cuspidatus"] <- "Mimulus bigelovii var. cuspidatus" 

# accepted name is Diplacus but Mimulus is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Diplacus leptaleus"] <- "Mimulus leptaleus"

# accepted name is Diplacus but Mimulus nanus var. mephiticus is in occurrence dataset
# not ideal and issues with delimiting the occurrences of this species remain
ggb_all_data$species[ggb_all_data$species =="Diplacus mephiticus"] <- "Mimulus nanus var. mephiticus"

# to match occurrence dataset & Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Eriogonum lobbii var. lobbii"] <- "Eriogonum lobbii"

# accepted name is microtheca but microthecum is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Eriogonum microtheca"] <- "Eriogonum microthecum"

# accepted name is microtheca but microthecum is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Eriogonum microtheca var. panamintense"] <- "Eriogonum microthecum var. panamintense"

# spelling correction
ggb_all_data$species[ggb_all_data$species =="Eriogonum umbellatum var. dicrocephalum"] <- "Eriogonum umbellatum var. dichrocephalum" 

# accepted name is Diplacus leptaleus but Mimulus is in occurrence dataset
ggb_all_data$species[ggb_all_data$species =="Erythranthe leptalea"] <- "Mimulus leptaleus" 

# Festuca brachyphylla subsp. brachyphylla does not match Baldwin et al. 2017 occurrence dataset; only recorded on one row in California and remaining instances are in Great Basin National Park; Likely an error in swe_bel

# minor typo correction
ggb_all_data$species[ggb_all_data$species =="FEstuca brachyphylla ssp. breviculmis"] <- "Festuca brachyphylla ssp. breviculmis"

# to match occurrence dataset & Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Festuca saximontana subsp. purpusiana"] <- "Festuca saximontana" 

# to match occurrence dataset & Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Heuchera rubescens var. alpicola"] <- "Heuchera rubescens"

# to match occurrence dataset & Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Hymenoxys cooperi var. canescens"] <- "Hymenoxys cooperi" 

# to match occurrence dataset & Jepson eflora
ggb_all_data$species[ggb_all_data$species =="Ipomopsis congesta var. montana"] <- "Ipomopsis congesta subsp. montana" 

# Juncus arcticus is not an accepted name on Jepson eflora or in Baldwin dataset; I suspect it should be J. balticus since that's what was recorded in 2017 on same SAS in 2012 and 2017

# Poa glauca does not show up as current_name in Baldwin dataset or in Jepson eflora; just Poa glauca subsp. rupicola; Jan Nachlinger confirmed this is correct for Great Basin National Park too
ggb_all_data$species[ggb_all_data$species =="Poa glauca"] <- "Poa glauca subsp. rupicola" 

# minor typo correction
ggb_all_data$species[ggb_all_data$species =="Poa glauca subsp. Rupicola"] <- "Poa glauca subsp. rupicola" 
ggb_all_data$species[ggb_all_data$species =="Poa glauca ssp. Rupicola"] <- "Poa glauca ssp. rupicola" 
ggb_all_data$species[ggb_all_data$species =="Poa glauca ssp. rupicola"] <- "Poa glauca subsp. rupicola" 

#typo
ggb_all_data$species[ggb_all_data$species =="Poa secunda ssp. Secunda"] <- "Poa secunda ssp. secunda"
ggb_all_data$species[ggb_all_data$species =="Poa secunda ssp. secunda"] <- "Poa secunda subsp. secunda"

# to match occurrence dataset & Jepson eflora; note Baldwin does not include "subsp." but it was added here
ggb_all_data$species[ggb_all_data$species =="Sedum roseum"] <- "Rhodiola integrifolia subsp. integrifolia" #there is also ssp.
ggb_all_data$species[ggb_all_data$species =="Rhodiola integrifolia ssp. integrifolia "] <- "Rhodiola integrifolia subsp. integrifolia"

ggb_all_data$species[ggb_all_data$species =="Carex sect. Ovales sp."] <- "Carex sp."
ggb_all_data$species[ggb_all_data$species =="Carex sect. Ovales sp. "] <- "Carex sp."

ggb_all_data$species[ggb_all_data$species =="Astragalus calycosus"] <- "Astragalus calycosus var. calycosus"

ggb_all_data$species[ggb_all_data$species =="Astragalus whitneyi"] <- "Astragalus whitneyi var. whitneyi"

ggb_all_data$species[ggb_all_data$species =="Chrysothamnus viscidiflorus"] <- "Chrysothamnus viscidiflorus subsp. viscidiflorus"

ggb_all_data$species[ggb_all_data$species =="Erysimum capitatum"] <- "Erysimum capitatum var. capitatum"

ggb_all_data$species[ggb_all_data$species =="Ivesia lycopodioides"] <- "Ivesia lycopodioides var. lycopodioides"

ggb_all_data$species[ggb_all_data$species =="Ivesia shockleyi"] <- "Ivesia shockleyi var. shockleyi"

ggb_all_data$species[ggb_all_data$species =="Keckiella rothrockii"] <- "Keckiella rothrockii var. rothrockii"

ggb_all_data$species[ggb_all_data$species =="Penstemon davidsonii"] <- "Penstemon davidsonii var. davidsonii"

ggb_all_data$species[ggb_all_data$species =="Penstemon humilis"] <- "Penstemon humilis subsp. humilis"

ggb_all_data$species[ggb_all_data$species =="Penstemon leiophyllus"] <- "Penstemon leiophyllus var. francisci-pennellii"

ggb_all_data$species[ggb_all_data$species =="Penstemon newberryi"] <- "Penstemon newberryi ssp. newberryi"

ggb_all_data$species[ggb_all_data$species =="Phacelia crenulata"] <- "Phacelia crenulata var. crenulata"

ggb_all_data$species[ggb_all_data$species =="Polemonium pulcherrimum"] <- "Polemonium pulcherrimum var. pulcherrimum"

ggb_all_data$species[ggb_all_data$species =="Potentilla glaucophylla"] <- "Potentilla glaucophylla var. glaucophylla"

ggb_all_data$species[ggb_all_data$species =="Ranunculus eschscholtzii"] <- "Ranunculus eschscholtzii var. eschscholtzii"

ggb_all_data$species[ggb_all_data$species =="Rhodiola integrifolia"] <- "Rhodiola integrifolia subsp. integrifolia"

ggb_all_data$species[ggb_all_data$species =="Ribes cereum"] <- "Ribes cereum var. cereum"

ggb_all_data$species[ggb_all_data$species =="Pinus balfouriana"] <- "Pinus balfouriana subsp. austrina"

ggb_all_data$species[ggb_all_data$species =="Pinus contorta"] <- "Pinus contorta var. murrayana"

ggb_all_data$species[ggb_all_data$species =="Pinus balfouriana"] <- "Pinus balfouriana subsp. austrina"