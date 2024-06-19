# Title: 02B_misident
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################
#Fix the "misident" problem code, which corresponds to mis-identifications:

#Agoseris glauca -> Agoseris x elata on idr_2010_n.

Agoseris <- ggb_all_data %>% 
  filter((peak == "idr" & species == "Agoseris glauca"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Agoseris)

Agoseris$species[Agoseris$species == "Agoseris glauca"] <- "Agoseris x elata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Agoseris)

#Antennaria corymbosa to A. media on idr_2010:

A_cory <- ggb_all_data %>% 
  filter((peak == "idr" & species == "Antennaria corymbosa"))

#check - just one record

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_cory)

A_cory$species[A_cory$species == "Antennaria corymbosa"] <- "Antennaria media"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_cory)

#Antennaria media -> corymbosa on idr_n_5:
A_medi_idr_n5 <- ggb_all_data %>% 
  filter((peak == "idr")) %>% 
  filter((species == "Antennaria media")) %>% 
  filter((year == "2021")) %>% 
  filter((aspect == "n")) %>% 
  filter((contour == "5"))

#remove this record from the whole data set:

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_medi_idr_n5)

#Make the needed change:

A_medi_idr_n5$species[A_medi_idr_n5$species == "Antennaria media"] <- "Antennaria corymbosa"

#add it back to whole data set:

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_medi_idr_n5)

#Antennaria media -> corymbosa on sme_2021_s10:
A_medi_sme_s10 <- ggb_all_data %>% 
  filter((peak == "sme")) %>% 
  filter((species == "Antennaria media")) %>% 
  filter((year == "2021"))

#remove this record from the whole data set:

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_medi_sme_s10)

#Make the needed change:

A_medi_sme_s10$species[A_medi_sme_s10$species == "Antennaria media"] <- "Antennaria corymbosa"

#add it back to whole data set:

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_medi_sme_s10)

#Artemisia arbuscula -> Sphaeromeria cana on 374_2004_e

A_arb_S_can <- ggb_all_data %>% 
  filter((peak == "374")) %>% 
  filter((species == "Artemisia arbuscula"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_arb_S_can)

A_arb_S_can$species[A_arb_S_can$species == "Artemisia arbuscula"] <- "Sphaeromeria cana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_arb_S_can)

#Artemisia arbuscula -> Artemisia rothrockii on pgs_2010_s, sme_2010_n, shf_2004_n/e/s/w. Collect records:

A_arb_pgs_2010_s <- ggb_all_data %>% 
  filter((peak == "pgs")) %>% 
  filter((species == "Artemisia arbuscula")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "s"))

A_arb_sme_2010_n <- ggb_all_data %>% 
  filter((peak == "sme")) %>% 
  filter((species == "Artemisia arbuscula")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "n"))

A_arb_shf_2004 <- ggb_all_data %>% 
  filter((peak == "shf")) %>% 
  filter((species == "Artemisia arbuscula")) %>% 
  filter((year == "2004"))

A_arb_A_roth <- bind_rows(A_arb_pgs_2010_s, A_arb_sme_2010_n, A_arb_shf_2004)

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_arb_A_roth)

A_arb_A_roth$species[A_arb_A_roth$species == "Artemisia arbuscula"] <- "Artemisia rothrockii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_arb_A_roth)

#Artemisia tridentata ssp. vaseyana -> Artemisia rothrockii on sme_2021_s and shf_2004_e/n/s

A_tri_sme_2021_s <- ggb_all_data %>% 
  filter((species == "Artemisia tridentata subsp. vaseyana")) %>% 
  filter((peak == "sme")) %>% 
  filter((year == "2021"))

A_tri_shf_2004 <- ggb_all_data %>% 
  filter((species == "Artemisia tridentata subsp. vaseyana")) %>% 
  filter((peak == "shf")) %>% 
  filter((year == "2004")) #don't need to filter by aspect because n/e/s are only obs.

#nothing is returned because we changed ssp. to subsp.

A_tri <- bind_rows(A_tri_sme_2021_s, A_tri_shf_2004)

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_tri)

A_arb_A_roth$species[A_arb_A_roth$species == "Artemisia tridentata subsp. vaseyana"] <- "Artemisia rothrockii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_tri)

#Astragalus kentrophyta -> Oxytropis parryi on bar_2014_e

A_ken_bar_2014_e <- ggb_all_data %>% 
  filter((species == "Astragalus kentrophyta var. tegetarius")) %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2014")) %>% 
  filter((aspect =="e"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_ken_bar_2014_e)

A_ken_bar_2014_e$species[A_ken_bar_2014_e$species == "Astragalus kentrophyta var. tegetarius"] <- "Oxytropis parryi"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_ken_bar_2014_e)

#Astragalus platytropis -> Oxytropis parryi on

#cws_2010_e/n/w, pgs_2010_n/e/s/w, sme_2010_n/e/s/w, bar_2004_n/e/s/w, bar_2009_n/s/w, bar_2014_s, rna_2009_n

A_platy_1 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "cws")) %>% 
  filter((year == "2010"))

A_platy_2 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "pgs")) %>% 
  filter((year == "2010"))

A_platy_3 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "sme")) %>% 
  filter((year == "2010"))

A_platy_4 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2004"))

A_platy_5 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2009"))

A_platy_6 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2014")) %>% 
  filter((aspect == "s"))

A_platy_7 <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "rna")) %>% 
  filter((year == "2009")) %>% 
  filter((aspect == "n"))

A_platy <- bind_rows(A_platy_1, A_platy_2, A_platy_3, A_platy_4, A_platy_5, A_platy_6, A_platy_7)

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_platy)

A_platy$species[A_platy$species == "Astragalus platytropis"] <- "Oxytropis parryi"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_platy)

#Boechera depauperata -> Boechera lemmonii on idr_2015, lss_2015_e/s/w

B_dep_1 <- ggb_all_data %>% 
  filter((species == "Boechera depauperata")) %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2015"))

B_dep_2 <- ggb_all_data %>% 
  filter((species == "Boechera depauperata")) %>% 
  filter((peak == "lss")) %>% 
  filter((year == "2015"))

B_dep <- bind_rows(B_dep_1, B_dep_2)

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_dep)

B_dep$species[B_dep$species == "Boechera depauperata"] <- "Boechera lemmonii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_dep)

#Boechera inyoensis -> Draba breweri on 357_2004_e/s

B_inyo <- ggb_all_data %>% 
  filter((species == "Boechera inyoensis")) %>% 
  filter((peak == "357")) %>% 
  filter((year == "2004"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_inyo)

B_inyo$species[B_inyo$species == "Boechera inyoensis"] <- "Draba breweri"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_inyo)

#Boechera lemmonii -> Boechera depauperata on sme_2021_s

B_lem <- ggb_all_data %>% 
  filter((species == "Boechera lemmonii")) %>% 
  filter((peak == "sme")) %>% 
  filter((year == "2021")) %>% 
  filter((aspect == "s"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_lem)

B_lem$species[B_lem$species == "Boechera lemmonii"] <- "Boechera depauperata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_lem)

#Boechera pulchra -> Boechera puberula on grl_2019_n5

B_pul <- ggb_all_data %>% 
  filter((species == "Boechera pulchra")) %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2019")) %>% 
  filter((aspect == "n"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_pul)

B_pul$species[B_pul$species == "Boechera pulchra"] <- "Boechera puberula"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_pul)

#Boechera pulchra -> Boechera cf. howellii on grl_2019_s

B_pul2 <- ggb_all_data %>% 
  filter((species == "Boechera pulchra")) %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2019")) %>% 
  filter((aspect == "s"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_pul2)

B_pul2$species[B_pul2$species == "Boechera pulchra"] <- "Boechera cf. howellii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_pul2)

#Carex microptera -> Carex haydeniana on bar_2004_e

C_micro <- ggb_all_data %>% 
  filter((species == "Carex microptera")) %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2004")) %>% 
  filter((aspect == "e"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_micro)

C_micro$species[C_micro$species == "Carex microptera"] <- "Carex haydeniana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_micro)

#Carex proposita -> Carex phaeocephala on idr_2010_e/n/w and cfk_2010_n/w

C_pro1 <- ggb_all_data %>% 
  filter((species == "Carex proposita")) %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2010"))

C_pro2 <- ggb_all_data %>% 
  filter((species == "Carex proposita")) %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2010"))

C_pro <- bind_rows(C_pro1, C_pro2)

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_pro)

C_pro$species[C_pro$species == "Carex proposita"] <- "Carex phaeocephala"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_pro)

#Carex rossii -> Carex duriuscula on pgs_2005_n/s/w and sme_2005_n

C_ross1 <- ggb_all_data %>% 
  filter((species == "Carex rossii")) %>% 
  filter((peak == "pgs")) %>% 
  filter((year == "2005"))

C_ross2 <- ggb_all_data %>% 
  filter((species == "Carex rossii")) %>% 
  filter((peak == "sme")) %>% 
  filter((year == "2005"))

C_ross <- bind_rows(C_ross1, C_ross2)

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_ross)

C_ross$species[C_ross$species == "Carex rossii"] <- "Carex duriuscula"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_ross)

#Carex subnigricans -> Carex filifolia var. erostrata on cfk_2010_e

C_subn <- ggb_all_data %>% 
  filter((species == "Carex subnigricans")) %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2010")) 

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_subn)

C_subn$species[C_subn$species == "Carex subnigricans"] <- "Carex filifolia var. erostrata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_subn)

#Castilleja applegatei ssp. pallida -> Castilleja applegatei subsp. martinii on ben_2013_n

C_appl <- ggb_all_data %>% 
  filter((species == "Castilleja applegatei ssp. pallida")) %>% 
  filter((peak == "ben")) %>% 
  filter((year == "2013")) 

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_appl)

C_appl$species[C_appl$species == "Castilleja applegatei ssp. pallida"] <- "Castilleja applegatei subsp. martinii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_appl)

#	Chenopodium album -> Chenopodium fremontii on mid_2018_n/w

C_albu <- ggb_all_data %>% 
  filter((species == "Chenopodium album")) %>% 
  filter((peak == "mid")) %>% 
  filter((year == "2018")) 

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_albu)

C_albu$species[C_albu$species == "Chenopodium album"] <- "Chenopodium fremontii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_albu)

#	Chenopodium desiccatum -> Chenopodium pratericola on shf: 2004_e/n, 2009 all aspects, 2014 all aspects and 2019_e/n/s.

C_desi <- ggb_all_data %>% 
  filter((species == "Chenopodium desiccatum")) %>% 
  filter((peak == "shf"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_desi)

C_desi$species[C_desi$species == "Chenopodium desiccatum"] <- "Chenopodium pratericola"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_desi)

#Chenopodium foliosum -> Chenopodium capitatum var. parvicapitatum on shf, 2004_s, 2009_n/s/w, 2014_n/s/w

C_foli <- ggb_all_data %>% 
  filter((species == "Chenopodium foliosum")) %>% 
  filter((peak == "shf"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_foli)

C_foli$species[C_foli$species == "Chenopodium foliosum"] <- "Chenopodium capitatum var. parvicapitatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_foli)

#	Cryptantha confertiflora -> Oreocarya nubigena on rna_2009_n, but I changed C. confertifolia to Oreocarya confertifolia earlier.

O_confert <- ggb_all_data %>% 
  filter((species == "Oreocarya confertiflora")) %>% 
  filter((peak == "rna")) %>% 
  filter((year == "2009"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(O_confert)

O_confert$species[O_confert$species == "Oreocarya confertiflora"] <- "Oreocarya nubigena"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(O_confert)

#Cryptantha flavoculata -> Oreocarya humilis subsp. humilis on pgs: 2005_e/n/s, 2010_e/s. Changed to O. flavoculata earlier in this script.

O_flavo <- ggb_all_data %>% 
  filter((species == "Oreocarya flavoculata")) %>% 
  filter((peak == "pgs"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(O_flavo)

O_flavo$species[O_flavo$species == "Oreocarya flavoculata"] <- "Oreocarya humilis subsp. humilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(O_flavo)

#	Draba breweri -> Draba cana on bld_2013_n

D_brew_cana <- ggb_all_data %>% 
  filter((species == "Draba breweri")) %>% 
  filter((peak == "bld"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(D_brew_cana)

D_brew_cana$species[D_brew_cana$species == "Draba breweri"] <- "Draba cana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(D_brew_cana)

#	Draba breweri -> Anelsonia eurycarpa on lss and lws (all years/aspects)

D_brew <- ggb_all_data %>% 
  filter((species == "Draba breweri")) %>% 
  filter((peak == "lss" | peak == "lws"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(D_brew)

D_brew$species[D_brew$species == "Draba breweri"] <- "Anelsonia eurycarpa"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(D_brew)

#	Draba breweri -> Draba oligosperma on cws_2010

D_brew_olig <- ggb_all_data %>% 
  filter((species == "Draba breweri")) %>% 
  filter((peak == "cws")) %>% 
  filter((year == "2010"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(D_brew_olig)

D_brew_olig$species[D_brew_olig$species == "Draba breweri"] <- "Draba oligosperma"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(D_brew_olig)

#Draba monoensis -> Anelsonia eurycarpa on lss_2021_e/s/w

D_mono <- ggb_all_data %>% 
  filter((species == "Draba monoensis")) %>% 
  filter((peak == "lss")) %>% 
  filter((year == "2021"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(D_mono)

D_mono$species[D_mono$species == "Draba monoensis"] <- "Anelsonia eurycarpa"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(D_mono)

#	Drymocallis lactea -> Drymocallis pseudoruprestris var. saxicola on idr_2021_n. Jan calls this "tentative".

D_lact <- ggb_all_data %>% 
  filter((species == "Drymocallis lactea")) %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2021"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(D_lact)

D_lact$species[D_lact$species == "Drymocallis lactea"] <- "Drymocallis pseudoruprestris var. saxicola"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(D_lact)

#Erigeron algidus -> Packera werneriifolia on grl_2004_e/n

E_algi <- ggb_all_data %>% 
  filter((species == "Erigeron algidus")) %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2004"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(E_algi)

E_algi$species[E_algi$species == "Erigeron algidus"] <- "Packera werneriifolia"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(E_algi)

#Erigeron compositus -> Erigeron vagus for lss_2010 N5 "vicinity" -- only going to change the N5

E_comp <- ggb_all_data %>% 
  filter((species == "Erigeron compositus")) %>% 
  filter((peak == "lss")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "n")) %>% 
  filter((contour == "5"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(E_comp)

E_comp$species[E_comp$species == "Erigeron compositus"] <- "Erigeron vagus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(E_comp)

#Erigeron foliosus -> Erigeron algidus on idr_2010_e

E_foli <- ggb_all_data %>% 
  filter((species == "Erigeron foliosus")) %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "e"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(E_foli)

E_foli$species[E_foli$species == "Erigeron foliosus"] <- "Erigeron algidus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(E_foli)

#Eriogonum glaucum -> Eriogonum gracilipes on cws_2021_n

Er_glauc <- ggb_all_data %>% 
  filter((species == "Eriogonum glaucum")) %>% 
  filter((peak == "cws")) %>% 
  filter((year == "2021")) %>% 
  filter((aspect == "n"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Er_glauc)

Er_glauc$species[Er_glauc$species == "Eriogonum glaucum"] <- "Eriogonum gracilipes"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Er_glauc)

#Eriogonum ovalifolium -> Eriogonum gracilipes on sme_2010_n

Er_oval <- ggb_all_data %>% 
  filter((species == "Eriogonum ovalifolium var. nivale")) %>% 
  filter((peak == "sme")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "n"))

#oops --  I changed all these to E. o. var nivale

ggb_all_data <- ggb_all_data %>% 
  anti_join(Er_oval)

Er_oval$species[Er_oval$species == "Eriogonum ovalifolium var. nivale"] <- "Eriogonum gracilipes"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Er_oval)

#Eriogonum shockleyi var. shockleyi -> Eriogonum ovalifolium var. nivale on bar_2004, 2009, 2014 east aspects

Er_shock <- ggb_all_data %>% 
  filter((species == "Eriogonum shockleyi var. shockleyi")) %>% 
  filter((peak == "bar"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Er_shock)

Er_shock$species[Er_shock$species == "Eriogonum shockleyi var. shockleyi"] <- "Eriogonum ovalifolium var. nivale"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Er_shock)

# Erysimum capitatum var. capitatum -> Erysimum perenne on fes_2011_n, fpk_2011_n/w, cfk_2010_n, idr_2010_e, idr_2015_e, 332_2004_e/n/w, 332_2009_e/n, 332_2014_e/n, 357_2004_e/w, 357_2009_e/n/w, 357_2014_n/w, grl_2004, grl_2014, grl_2019_n

Ery_cap <- ggb_all_data %>% 
  filter((species == "Erysimum capitatum var. capitatum")) #BOOM all the records we need to change!

ggb_all_data <- ggb_all_data %>% 
  anti_join(Ery_cap)

Ery_cap$species[Ery_cap$species == "Erysimum capitatum var. capitatum"] <- "Erysimum perenne"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Ery_cap)

#Festuca ovina -> Festuca brachyphylla subsp. breviculmis on bck_2008_e

F_ovina <- ggb_all_data %>% 
  filter((species == "Festuca ovina")) %>% 
  filter((peak == "bck")) %>% 
  filter((year == "2008")) %>% 
  filter((aspect == "e"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(F_ovina)

F_ovina$species[F_ovina$species == "Festuca ovina"] <- "Festuca brachyphylla subsp. breviculmis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(F_ovina)

# Festuca saximontana -> Festuca brachyphylla subsp. breviculmis on grl_2019_s/w

F_saxi_misident <- ggb_all_data %>% 
  filter((species == "Festuca saximontana")) %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2019"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(F_saxi_misident)

F_saxi_misident$species[F_saxi_misident$species == "Festuca saximontana"] <- "Festuca brachyphylla subsp. breviculmis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(F_saxi_misident)

# Ipomopsis congesta ssp. congesta -> Ipomopsis congesta subsp. montana on fes_2006_n/s

Ipo_con <- ggb_all_data %>% 
  filter((species == "Ipomopsis congesta ssp. congesta")) %>% 
  filter((peak == "fes")) %>% 
  filter((year == "2006"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Ipo_con)

Ipo_con$species[Ipo_con$species == "Ipomopsis congesta ssp. congesta"] <- "Ipomopsis congesta subsp. montana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Ipo_con)

# Lewisia nevadensis -> Lewisia glandulosa on idr_2010_e

L_nevad <- ggb_all_data %>% 
  filter((species == "Lewisia nevadensis")) %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2010"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(L_nevad)

L_nevad$species[L_nevad$species == "Lewisia nevadensis"] <- "Lewisia glandulosa"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(L_nevad)

#Lomatium torreyi -> Cymopterus terebinthinus on 332_2004_s/w

L_torr <- ggb_all_data %>% 
  filter((species == "Lomatium torreyi")) %>% 
  filter((peak == "332")) %>% 
  filter((year == "2004"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(L_torr)

L_torr$species[L_torr$species == "Lomatium torreyi"] <- "Cymopterus terebinthinus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(L_torr)

#Lupinus argenteus var. palmeri -> Lupinus argenteus var. argenteus on all peaks in dev in 2013? Yep.

Lup_argent <- ggb_all_data %>% 
  filter((species == "Lupinus argenteus var. palmeri")) %>% 
  filter((target_region == "dev")) %>% 
  filter((year == "2013"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Lup_argent)

Lup_argent$species[Lup_argent$species == "Lupinus argenteus var. palmeri"] <- "Lupinus argenteus var. argenteus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Lup_argent)

#	Lupinus lepidus var. lobbii -> Lupinus lepidus var. ramosus on fry_2012_e/n

L_lep_ramos <- ggb_all_data %>% 
  filter((species == "Lupinus lepidus var. lobbii")) %>% 
  filter((peak == "fry")) %>% 
  filter((year == "2012"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(L_lep_ramos)

L_lep_ramos$species[L_lep_ramos$species == "Lupinus lepidus var. lobbii"] <- "Lupinus lepidus var. ramosus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(L_lep_ramos)

#Lupinus lepidus var. sellulus -> Lupinus lepidus var. lobbii on idr_2015, grl_2009/2014

L_lep_sell <- ggb_all_data %>% 
  filter((species == "Lupinus lepidus var. sellulus")) %>% 
  filter((peak == "idr" | peak == "grl")) 

#got it with just that :)

ggb_all_data <- ggb_all_data %>% 
  anti_join(L_lep_sell)

L_lep_sell$species[L_lep_sell$species == "Lupinus lepidus var. sellulus"] <- "Lupinus lepidus var. lobbii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(L_lep_sell)

# Minuartia nuttallii var. fragilis -> Minuartia nuttallii var. gracilis on cfk_2010, 332_2009/2014

M_nutall <- ggb_all_data %>% 
  filter((species == "Minuartia nuttallii var. fragilis")) %>% 
  filter((peak == "cfk" | peak == "332"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(M_nutall)

M_nutall$species[M_nutall$species == "Minuartia nuttallii var. fragilis"] <- "Minuartia nuttallii var. gracilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(M_nutall)

#Nothocalais alpestris -> Agoseris monticola on fry_2012_e/n and fry_2017_s

N_alp <- ggb_all_data %>% 
  filter((species == "Nothocalais alpestris")) %>% 
  filter((peak == "fry"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(N_alp)

N_alp$species[N_alp$species == "Nothocalais alpestris"] <- "Agoseris monticola"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(N_alp)

#	Oreonana purpurascens -> Oreonana clementis on idr_2010_e/n/w

Oreo_purpur <- ggb_all_data %>% 
  filter((species == "Oreonana purpurascens")) %>% 
  filter((peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Oreo_purpur)

Oreo_purpur$species[Oreo_purpur$species == "Oreonana purpurascens"] <- "Oreonana clementis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Oreo_purpur)

#Oxytropis oreophila -> Oxytropis parryi on pmd_2018_w

Oxy_oreo <- ggb_all_data %>% 
  filter((species == "Oxytropis oreophila")) %>% 
  filter((peak == "pmd")) %>% 
  filter((year == "2018"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Oxy_oreo)

Oxy_oreo$species[Oxy_oreo$species == "Oxytropis oreophila"] <- "Oxytropis parryi"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Oxy_oreo)

#	Pinus ponderosa -> Pinus jeffreyi on cfk_2015_e

P_ponder <- ggb_all_data %>% 
  filter((species == "Pinus ponderosa")) %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2015"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(P_ponder)

P_ponder$species[P_ponder$species == "Pinus ponderosa"] <- "Pinus jeffreyi"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_ponder)

#Poa cusickii subsp. pallida (changed previously) -> Poa glauca subsp. rupicola on rna_2004_n

Pcus_Pglauc <- ggb_all_data %>% 
  filter((species == "Poa cusickii subsp. pallida")) %>% 
  filter((peak == "rna")) %>% 
  filter((year == "2004")) %>% 
  filter((aspect == "n"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Pcus_Pglauc)

Pcus_Pglauc$species[Pcus_Pglauc$species == "Poa cusickii subsp. pallida"] <- "Poa glauca subsp. rupicola"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Pcus_Pglauc)

#Poa lettermanii -> Poa keckii everywhere

P_letter1 <- ggb_all_data %>% 
  filter((species == "Poa lettermanii"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(P_letter1)

P_letter1$species[P_letter1$species == "Poa lettermanii"] <- "Poa keckii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_letter1)

#Poa secunda subsp. juncifolia -> Poa secunda subsp. secunda on cfk_2015 and idr_2015

P_sec_junc <- ggb_all_data %>% 
  filter((species == "Poa secunda subsp. juncifolia")) %>% 
  filter((peak == "cfk" | peak == "idr")) %>% 
  filter((year == "2015"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(P_sec_junc)

P_sec_junc$species[P_sec_junc$species == "Poa secunda subsp. juncifolia"] <- "Poa secunda subsp. secunda"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_sec_junc)

#	Potentilla ovina var. ovina -> Potentilla ovina var. decurrens

p_ovina2 <- ggb_all_data %>% 
  filter((species == "Potentilla ovina var. ovina"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(p_ovina2)

p_ovina2$species[p_ovina2$species == "Potentilla ovina var. ovina"] <- "Potentilla ovina var. decurrens"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(p_ovina2)

#Ranunculus eschscholtzii var. eschscholtzii -> Ranunculus eschscholtzii var. oxynotus on wlr and idr

R_oxyno <- ggb_all_data %>% 
  filter((species == "Ranunculus eschscholtzii var. eschscholtzii")) %>% 
  filter((peak == "wlr" | peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(R_oxyno)

R_oxyno$species[R_oxyno$species == "Ranunculus eschscholtzii var. eschscholtzii"] <- "Ranunculus eschscholtzii var. oxynotus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(R_oxyno)

#	Scrophularia lanceolata -> Scrophularia desertorum on fry_2012_s

S_lanc <- ggb_all_data %>% 
  filter((species == "Scrophularia lanceolata")) %>% 
  filter((peak == "fry")) %>% 
  filter((year == "2012"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(S_lanc)

S_lanc$species[S_lanc$species == "Scrophularia lanceolata"] <- "Scrophularia desertorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(S_lanc)

#Sedum obtusatum -> Sedum lanceolatum on idr_2010_e/n, idr_2015_n

S_obtus <- ggb_all_data %>% 
  filter((species == "Sedum obtusatum")) %>% 
  filter((peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(S_obtus)

S_obtus$species[S_obtus$species == "Sedum obtusatum"] <- "Sedum lanceolatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(S_obtus)

# Sedum stenopetalum -> Sedum lanceolatum on idr_2021_e/n

S_steno <- ggb_all_data %>% 
  filter((species == "Sedum stenopetalum")) %>% 
  filter((peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(S_steno)

S_steno$species[S_steno$species == "Sedum stenopetalum"] <- "Sedum lanceolatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(S_steno)

#	Stipa occidentalis var. occidentalis -> Stipa pinetorum on idr

S_occ_idr <- ggb_all_data %>% 
  filter((species == "Stipa occidentalis var. occidentalis")) %>% 
  filter((peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(S_occ_idr)

S_occ_idr$species[S_occ_idr$species == "Stipa occidentalis var. occidentalis"] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(S_occ_idr)

# Taraxacum ceratophorum -> Taraxacum officinale on bld

T_cerat <- ggb_all_data %>% 
  filter((species == "Taraxacum ceratophorum")) %>% 
  filter((peak == "bld"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(T_cerat)

T_cerat$species[T_cerat$species == "Taraxacum ceratophorum"] <- "Taraxacum officinale"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(T_cerat)

#overstory Pinus flexilis in mid 2018 -> Pinus flexilis

p_flex_mid_2018 <- ggb_all_data %>% 
  filter((species == "overstory Pinus flexilis")) %>% 
  filter((peak == "mid")) %>% 
  filter((year == "2018"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(p_flex_mid_2018)

p_flex_mid_2018$species[p_flex_mid_2018$species == "overstory Pinus flexilis"] <- "Pinus flexilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(p_flex_mid_2018)