# Title: 02D_adds
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################

#Agoseris x elata:

Ago_elata <- ggb_all_data %>% 
  filter((species == "Agoseris x elata"))

#Seen on the 5m contour only, so only add it to the 5.

#Ericameria discoidea wds_sme_2005_n

E_disco <- ggb_all_data %>% 
  filter((species == "Ericameria discoidea")) %>% 
  filter((peak == "sme"))

#Not otherwise seen on the north aspect, so add to both contours on the north aspect.

Fest_sax_add <- ggb_all_data %>% 
  filter((species == "Festuca saximontana")) %>% 
  filter((peak == "idr"))

#seen on all aspects in 2021, but only the e_10 and s_5. Added to everything except e_5 and s_10.

#Physaria kingii sme_2005_e

Physaria_add <- ggb_all_data %>% 
  filter((species == "Physaria kingii subsp. kingii")) %>% 
  filter((peak == "sme")) %>% 
  filter((aspect == "e"))

#only seen in the 10m contour section, so only add it there.

#Poa cusickii subsp. pallida on cws/sme/rna

P_cus_cws <- ggb_all_data %>% 
  filter((species == "Poa cusickii subsp. pallida")) %>% 
  filter((peak == "cws"))

#cws: e_10, w_5, s_5/10

P_cus_sme <- ggb_all_data %>% 
  filter((species == "Poa cusickii subsp. pallida")) %>% 
  filter((peak == "sme"))

#sme: e_5/10, n_10, s_10

P_cus_rna <- ggb_all_data %>% 
  filter((species == "Poa cusickii subsp. pallida")) %>% 
  filter((peak == "rna"))

#rna: w_5/10, n_5/10, s_5/10, e_5/10

#Poa keckii on lss

poa_keck_add <- ggb_all_data %>% 
  filter((species == "Poa keckii")) %>% 
  filter((peak == "lss"))

#in filtering this I noticed "not present" in the cover of a 31 1x1, and a note from the SAS that the plant wasn't in 31. Seems safe to remove that entry...

poa_keck_omit <- ggb_all_data %>% 
  filter((data_type == "1m-x-1m-quadrat")) %>% 
  filter((species == "Poa keckii")) %>% 
  filter((peak == "lss"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(poa_keck_omit) #83229 is the magic number now

#only add to n_5 and w_5


#Potentilla morefieldii nom. inq. on bld. 

p_moref_add <- ggb_all_data %>% 
  filter((species == "Potentilla morefieldii")) %>% 
  filter((peak == "bld"))

#Potentilla morefieldii was not seen on bld, but there were two Potentilla sp. from the e_5, so only add there.


#Potentilla pseudosericea on idr

p_pseudo_add <- ggb_all_data %>% 
  filter((species == "Potentilla pseudosericea")) %>% 
  filter((peak == "idr"))

#Add to n_5, s_5/10

#Pyrrocoma apargioides on sme e and s

pyrro_add <- ggb_all_data %>% 
  filter((species == "Pyrrocoma apargioides")) %>% 
  filter((peak == "sme"))

#add to s_10 and e_5/10


#Senecio fremontii var. occidentalis on cfk s

sene_fre_add <- ggb_all_data %>% 
  filter((species == "Senecio fremontii var. occidentalis")) %>% 
  filter((peak == "cfk"))

#add to s_10 only


#Solidago multiradiata on cfk

silene_add <- ggb_all_data %>% 
  filter((species == "Solidago multiradiata")) %>% 
  filter((peak == "cfk"))

#add to s_5/10

#Stipa pinetorum on cfk

stipa_pin_add <- ggb_all_data %>% 
  filter((species == "Stipa pinetorum")) %>% 
  filter((peak == "cfk"))

#add to n_5/10, w_5/10, e_10, s_10