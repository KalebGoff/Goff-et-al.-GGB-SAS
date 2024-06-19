# Title: 02A_typo_nc
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################
#Fix the "typo" problem code, which is simply four rows that Jan found from the east aspect of grl in 2004 that need to be changed from "grb" to "snd". This problem backtraces to some very tired field biologists...

typo <- ggb_all_data %>% 
  filter((target_region =='grb' & peak =='grl')) #filter into their own data frame for change to target_region = snd

ggb_all_data <- ggb_all_data %>% 
  filter(!(target_region =='grb' & peak =='grl')) #filter them out of the whole data frame

typo$target_region[typo$target_region == "grb"] <- "snd"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(typo)

###################################################################################################################
#Fixes for the name change problem codes. To do this, I filtered the problem code = "NC" and cleaned row by row. Rationale for changes can be found on janslist.

#Start with the Ivesia problem that was originally listed as "Entry Error" -- Wheew that is a lot of code for one small change.

Ivesia_NC <- ggb_all_data %>% 
  filter((target_region=='snd' & peak=='374' & year == '2009' & species == 'Ivesia shockleyi var. shockleyi'))

ggb_all_data <- ggb_all_data %>% 
  filter(!(target_region=='snd' & peak=='374' & year == '2009' & species == 'Ivesia shockleyi var. shockleyi'))

Ivesia_NC$species[Ivesia_NC$species == "Ivesia shockleyi var. shockleyi"] <- "Ivesia lycopodioides var. lycopodioides"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Ivesia_NC)

#Festuca brachyphylla ssp. breviculmis change to "subsp." everywhere:

ggb_all_data$species[ggb_all_data$species == "Festuca brachyphylla ssp. breviculmis"] <- "Festuca brachyphylla subsp. breviculmis"

#Antennaria rosea -> A. rosea subsp. confinis

ggb_all_data$species[ggb_all_data$species == "Antennaria rosea"] <- "Antennaria rosea subsp. confinis"

#Aquilegia caerulea -> Aquilegia coerulea var. ochroleuca

ggb_all_data$species[ggb_all_data$species == "Aquilegia caerulea"] <- "Aquilegia coerulea var. ochroleuca"

#Artemisia tridentata and Artemisia tridentata ssp. vaseyana -> Artemisia tridentata subsp. vaseyana

ggb_all_data$species[ggb_all_data$species == "Artemisia tridentata"] <- "Artemisia tridentata subsp. vaseyana"
ggb_all_data$species[ggb_all_data$species == "Artemisia tridentata ssp. vaseyana"] <- "Artemisia tridentata subsp. vaseyana"

#A. k. tegetarius in grb/wds/wim, A. k. danaus in snd/swe

#start by pulling all the Astragalus kentrophyta records in the entire dataset:

A_kentrophyta <- ggb_all_data[grepl("^Astragalus kentrophyta", ggb_all_data$species),]

table(A_kentrophyta$target_region)

#Then subset this list by the target regions above:

A_k_danaus <- A_kentrophyta %>% 
  filter((target_region == "snd" | target_region == "swe"))

A_k_danaus$species[A_k_danaus$species == "Astragalus kentrophyta"] <- "Astragalus kentrophyta var. danaus"

A_k_tegetarius <- A_kentrophyta %>% 
  filter((target_region == "grb" | target_region == "wds" | target_region == "wim"))

A_k_tegetarius$species[A_k_tegetarius$species == "Astragalus kentrophyta"] <- "Astragalus kentrophyta var. tegetarius"

#Remove all A. kentrophytas from the entire data set, replace them with this updated group

ggb_all_data <- ggb_all_data[!grepl("^Astragalus kentrophyta", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_k_danaus) %>% 
  bind_rows(A_k_tegetarius)

#Add var. to all Astragalus lentiginosus
ggb_all_data$species[ggb_all_data$species == "Astragalus lentiginosus"] <- "Astragalus lentiginosus var. ineptus"

#Add var. to all Astragalus purshii
ggb_all_data$species[ggb_all_data$species == "Astragalus purshii"] <- "Astragalus purshii var. lectulus"

#Boechera drummondii -> B. stricta

ggb_all_data$species[ggb_all_data$species == "Boechera drummondii"] <- "Boechera stricta"

#Calyptridium monospermum -> Calyptridium umbellatum

ggb_all_data$species[ggb_all_data$species == "Calyptridium monospermum"] <- "Calyptridium umbellatum"

#Carex filifolia -> Carex filifolia var. erostrata (add var.)

ggb_all_data$species[ggb_all_data$species == "Carex filifolia"] <- "Carex filifolia var. erostrata"

#Castilleja applegatei -> Castilleja applegatei subsp. martinii

ggb_all_data$species[ggb_all_data$species == "Castilleja applegatei"] <- "Castilleja applegatei subsp. martinii"

#Chaenactis douglasii var. alpina on cat and var. douglasii on dev/snd/swe

#start by pulling all the Chaenactis douglasii records in the entire dataset:

C_douglasii <- ggb_all_data[grepl("^Chaenactis douglasii", ggb_all_data$species),]

table(C_douglasii$target_region)

#One extra TR pulled in the table, swe, and I don't know what variety is in the sweetwaters. From CCH2 search it looks like var. douglasii.

#Then subset this list by the target regions above:

C_d_alpina <- C_douglasii %>% 
  filter((target_region == "cat"))

C_d_alpina$species[C_d_alpina$species == "Chaenactis douglasii"] <- "Chaenactis douglasii var. alpina"

C_d_douglasii <- C_douglasii %>% 
  filter((target_region == "dev" | target_region == "snd" | target_region == "swe"))

C_d_douglasii$species[C_d_douglasii$species == "Chaenactis douglasii"] <- "Chaenactis douglasii var. douglasii"

#Remove all C. douglasii from the entire data set, replace them with this updated group

ggb_all_data <- ggb_all_data[!grepl("^Chaenactis douglasii", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_d_alpina) %>% 
  bind_rows(C_d_douglasii)

#Chenopodium capitatum -> Chenopodium capitatum var. parvicapitatum

ggb_all_data$species[ggb_all_data$species == "Chenopodium capitatum"] <- "Chenopodium capitatum var. parvicapitatum"

#Chrysothamnus parryi ssp. monocephalus -> Ericameria parryi var. monocephala

ggb_all_data$species[ggb_all_data$species == "Chrysothamnus parryi ssp. monocephalus"] <- "Ericameria parryi var. monocephala"

#Cryptantha cinerea var. abortiva -> Oreocarya abortiva

ggb_all_data$species[ggb_all_data$species == "Cryptantha cinerea var. abortiva"] <- "Oreocarya abortiva"

#Cryptantha confertiflora -> Oreocarya confertiflora

ggb_all_data$species[ggb_all_data$species == "Cryptantha confertiflora"] <- "Oreocarya confertiflora"

#Cryptantha flavoculata -> Oreocarya flavoculata

ggb_all_data$species[ggb_all_data$species == "Cryptantha flavoculata"] <- "Oreocarya flavoculata"

#Cryptantha hoffmannii -> Oreocarya hoffmannii

ggb_all_data$species[ggb_all_data$species == "Cryptantha hoffmannii"] <- "Oreocarya hoffmannii"

#Cryptantha humilis -> Oreocarya humilis subsp. humilis

ggb_all_data$species[ggb_all_data$species == "Cryptantha humilis"] <- "Oreocarya humilis subsp. humilis"

#Cryptantha nubigena -> Oreocarya nubigena

ggb_all_data$species[ggb_all_data$species == "Cryptantha nubigena"] <- "Oreocarya nubigena"

#Cymopterus terebinthinus -> Cymopterus terebinthinus var. californicus

ggb_all_data$species[ggb_all_data$species == "Cymopterus terebinthinus"] <- "Cymopterus terebinthinus var. californicus"

#Draba oreibata and Draba oreibata var. serpentina -> Draba serpentina

ggb_all_data$species[ggb_all_data$species == "Draba oreibata"] <- "Draba serpentina"
ggb_all_data$species[ggb_all_data$species == "Draba oreibata var. serpentina"] <- "Draba serpentina"

#Elymus elymoides var. californicus and Elymus elymoides var. elymoides -> Elymus elymoides (remove all E. elymoides intraranks)

ggb_all_data$species[ggb_all_data$species == "Elymus elymoides var. californicus"] <- "Elymus elymoides"
ggb_all_data$species[ggb_all_data$species == "Elymus elymoides var. elymoides"] <- "Elymus elymoides"

#Elymus elymoides x cinereus -> xElyleymus aristatus

ggb_all_data$species[ggb_all_data$species == "Elymus elymoides x cinereus"] <- "xElyleymus aristatus"

#Elymus trachycaulus -> Agropyron trachycaulum

ggb_all_data$species[ggb_all_data$species == "Elymus trachycaulus"] <- "Agropyron trachycaulum"

#Eremogone congesta and Eremogone congesta var. simulans -> Eremogone congesta var. wheelerensis

ggb_all_data$species[ggb_all_data$species == "Eremogone congesta"] <- "Eremogone congesta var. wheelerensis"
ggb_all_data$species[ggb_all_data$species == "Eremogone congesta var. simulans"] <- "Eremogone congesta var. wheelerensis"

#Eremogone kingii -> Eremogone kingii var. glabrescens

ggb_all_data$species[ggb_all_data$species == "Eremogone kingii"] <- "Eremogone kingii var. glabrescens"

#Ericameria parryi -> Ericameria parryi var. monocephala

ggb_all_data$species[ggb_all_data$species == "Ericameria parryi"] <- "Ericameria parryi var. monocephala"

#Erigeron clokeyi -> Erigeron clokeyi var. pinzliae

ggb_all_data$species[ggb_all_data$species == "Erigeron clokeyi"] <- "Erigeron clokeyi var. pinzliae"

#Erigeron simplex -> Erigeron grandiflorus

ggb_all_data$species[ggb_all_data$species == "Erigeron simplex"] <- "Erigeron grandiflorus"

#Eriogonum microthecum -> Eriogonum microthecum var. panamintense

ggb_all_data$species[ggb_all_data$species == "Eriogonum microthecum"] <- "Eriogonum microthecum var. panamintense"

#Eriogonum nudum -> Eriogonum nudum var. scapigerum

ggb_all_data$species[ggb_all_data$species == "Eriogonum nudum"] <- "Eriogonum nudum var. scapigerum"

#Eriogonum ovalifolium has two varieties in our study system, E. o. nivale and E. o. caelestinum. It looks like nivale is on wim/swe/snd/lan/grb/cat and caelestinum is on snd_grl.

#start by pulling all the Eriogonum ovalifolium records in the entire dataset:

E_oval <- ggb_all_data[grepl("^Eriogonum ovalifolium", ggb_all_data$species),]

#Then subset grl out:

E_o_caelestinum <- E_oval %>% 
  filter((peak == "grl"))

E_o_caelestinum$species[E_o_caelestinum$species == "Eriogonum ovalifolium"] <- "Eriogonum ovalifolium var. caelestinum"
E_o_caelestinum$species[E_o_caelestinum$species == "Eriogonum ovalifolium var. ovalifolium"] <- "Eriogonum ovalifolium var. caelestinum"

#we did not manipulate the var. nivale IDs on this peak, as a way to address Jan's comment that grl was the only place with caelestinum, but nivale was also probably there.

#Now we just remove the peaks that had E. o. caelestinum on them and change them all to E. o. nivale.

E_o_nivale <- E_oval %>% 
  filter(!(peak == "grl"))

E_o_nivale$species[E_o_nivale$species == "Eriogonum ovalifolium"] <- "Eriogonum ovalifolium var. nivale"
E_o_nivale$species[E_o_nivale$species == "Eriogonum ovalifolium var. ovalifolium"] <- "Eriogonum ovalifolium var. nivale"

#Remove all E. ovalifolium from the entire data set, replace them with this updated group

ggb_all_data <- ggb_all_data[!grepl("^Eriogonum ovalifolium", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(E_o_caelestinum) %>% 
  bind_rows(E_o_nivale)

E_umbellatum <- ggb_all_data[grepl("Eriogonum umbellatum", ggb_all_data$species),]

#looks like we also have var. subaridum AND var. dichrocephalum on dev_low. Ok so this time I think I really just want to pull only the records that lack a variety.

E_umbellatum <- ggb_all_data[grepl("^Eriogonum umbellatum$", ggb_all_data$species),]

#there is one E. umbellatum record in here from dev_low. Need to ask Jan what this is. For now I am not going to change it.

E_u_versicolor <- E_umbellatum %>% 
  filter((peak == "tel"))

E_u_versicolor$species[E_u_versicolor$species == "Eriogonum umbellatum"] <- "Eriogonum umbellatum var. versicolor"

E_u_covillei <- E_umbellatum %>% 
  filter((peak == "idr"))

E_u_covillei$species[E_u_covillei$species == "Eriogonum umbellatum"] <- "Eriogonum umbellatum var. covillei"

E_u_issue <- E_umbellatum %>% 
  filter((peak == "low"))

E_u_issue$species[E_u_issue$species == "Eriogonum umbellatum"] <- "Eriogonum umbellatum var. versicolor"

#FIGURE OUT WHAT var and fix it here!

#remove and add back in our fixes:

ggb_all_data <- ggb_all_data[!grepl("^Eriogonum umbellatum$", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(E_u_versicolor) %>% 
  bind_rows(E_u_covillei) %>% 
  bind_rows(E_u_issue)
#Eriophyllum lanatum -> Eriophyllum lanatum var. integrifolium

ggb_all_data$species[ggb_all_data$species == "Eriophyllum lanatum"] <- "Eriophyllum lanatum var. integrifolium"

#Festuca brachyphylla and Festuca brachyphylla ssp. brachyphylla -> Festuca brachyphylla subsp. breviculmis

ggb_all_data$species[ggb_all_data$species == "Festuca brachyphylla"] <- "Festuca brachyphylla subsp. breviculmis"
ggb_all_data$species[ggb_all_data$species == "Festuca brachyphylla ssp. brachyphylla"] <- "Festuca brachyphylla subsp. breviculmis"

#Festuca saximontana var. pupusiana on grb_pmd and grb_bld and without a var. everywhere else:

F_saximontana <- ggb_all_data[grepl("Festuca saximontana", ggb_all_data$species),]

F_sax_pupu <- F_saximontana %>% 
  filter((peak == "grb" | peak == "bld"))

F_sax_pupu$species[F_sax_pupu$species == "Festuca saximontana"] <- "Festuca saximontana var. purpusiana"

F_sax <- F_saximontana %>% 
  filter(!(peak == "grb" | peak == "bld"))

F_sax$species[F_sax$species == "Festuca saximontana"] <- "Festuca saximontana"
F_sax$species[F_sax$species == "Festuca saximontana ssp. purpusiana"] <- "Festuca saximontana"

ggb_all_data <- ggb_all_data[!grepl("Festuca saximontana", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(F_sax) %>% 
  bind_rows(F_sax_pupu)

# Geum rossii -> Geum rossii var. turbinatum

ggb_all_data$species[ggb_all_data$species == "Geum rossii"] <- "Geum rossii var. turbinatum"

#Holodiscus discolor and Holodiscus discolor var. microphyllus -> Holodiscus microphyllus var. microphyllus

ggb_all_data$species[ggb_all_data$species == "Holodiscus discolor"] <- "Holodiscus microphyllus var. microphyllus"
ggb_all_data$species[ggb_all_data$species == "Holodiscus discolor var. microphyllus"] <- "Holodiscus microphyllus var. microphyllus"

#Ipomopsis congesta -> Ipomopsis congesta subsp. montana

ggb_all_data$species[ggb_all_data$species == "Ipomopsis congesta"] <- "Ipomopsis congesta subsp. montana"

#Jamesia americana -> Jamesia americana var. rosea

ggb_all_data$species[ggb_all_data$species == "Jamesia americana"] <- "Jamesia americana var. rosea"

#Juncus arcticus -> Juncus balticus subsp. ater

ggb_all_data$species[ggb_all_data$species == "Juncus arcticus"] <- "Juncus balticus subsp. ater"

#Juniperus communis -> Juniperus communis var. depressa

ggb_all_data$species[ggb_all_data$species == "Juniperus communis"] <- "Juniperus communis var. depressa"

#Lupinus argenteus -> Lupinus argenteus var. argenteus

ggb_all_data$species[ggb_all_data$species == "Lupinus argenteus"] <- "Lupinus argenteus var. argenteus"

#Lupinus breweri -> Lupinus breweri var. bryoides

ggb_all_data$species[ggb_all_data$species == "Lupinus breweri"] <- "Lupinus breweri var. bryoides"

#Lupinus lepidus -> Lupinus lepidus var. lobbii

ggb_all_data$species[ggb_all_data$species == "Lupinus lepidus"] <- "Lupinus lepidus var. lobbii"

#Mimulus bigelovii var. cuspidatus -> Diplacus bigelovii var. cuspidatus

ggb_all_data$species[ggb_all_data$species == "Mimulus bigelovii var. cuspidatus"] <- "Diplacus bigelovii var. cuspidatus"

#Mimulus leptaleus -> Diplacus leptaleus

ggb_all_data$species[ggb_all_data$species == "Mimulus leptaleus"] <- "Diplacus leptaleus"

#Mimulus nanus var. mephiticus -> Diplacus mephiticus

ggb_all_data$species[ggb_all_data$species == "Mimulus nanus var. mephiticus"] <- "Diplacus mephiticus"

#Minuartia nuttallii -> Minuartia nuttallii var. gracilis

ggb_all_data$species[ggb_all_data$species == "Minuartia nuttallii"] <- "Minuartia nuttallii var. gracilis"

#Oenothera cespitosa -> Oenothera cespitosa subsp. crinita

ggb_all_data$species[ggb_all_data$species == "Oenothera cespitosa"] <- "Oenothera cespitosa subsp. crinita"

#Opuntia polyacantha -> Opuntia polyacantha var. erinacea

ggb_all_data$species[ggb_all_data$species == "Opuntia polyacantha"] <- "Opuntia polyacantha var. erinacea"

#Orobanche fasciculata -> Aphyllon fasciculatum

ggb_all_data$species[ggb_all_data$species == "Orobanche fasciculata"] <- "Aphyllon fasciculatum"

#Penstemon heterodoxus -> Penstemon heterodoxus var. heterodoxus

ggb_all_data$species[ggb_all_data$species == "Penstemon heterodoxus"] <- "Penstemon heterodoxus var. heterodoxus"

#Penstemon humilis subsp. humilis -> Penstemon humilis var. humilis

ggb_all_data$species[ggb_all_data$species == "Penstemon humilis subsp. humilis"] <- "Penstemon humilis var. humilis"

#Penstemon newberryi ssp. newberryi -> Penstemon newberryi var. newberryi

ggb_all_data$species[ggb_all_data$species == "Penstemon newberryi ssp. newberryi"] <- "Penstemon newberryi var. newberryi"

#Phacelia hastata -> Phacelia hastata var. compacta

ggb_all_data$species[ggb_all_data$species == "Phacelia hastata"] <- "Phacelia hastata var. compacta"

#Phlox stansburyi -> Phlox stansburyi var. brevifolia

ggb_all_data$species[ggb_all_data$species == "Phlox stansburyi"] <- "Phlox stansburyi var. brevifolia"

#Physaria kingii -> Physaria kingii subsp. kingii

ggb_all_data$species[ggb_all_data$species == "Physaria kingii"] <- "Physaria kingii subsp. kingii"

#Picea engelmannii -> Picea engelmannii var. engelmannii

ggb_all_data$species[ggb_all_data$species == "Picea engelmannii"] <- "Picea engelmannii var. engelmannii"

#Poa cusickii... the changes are mostly to add subsp. pallida, but a small group of records are noted to have no subsp. recognized. The intrarank deletions are on grb_bld_2008, grb_bld_2013, grb_pmd_2008, grb_bld_2018.

P_cusickii <- ggb_all_data[grepl("Poa cusickii", ggb_all_data$species),]

P_cus_probs <- P_cusickii %>% 
  filter((peak == "bld" | peak == "pmd"))

#yeah, this includes two subsp. pallida that are noted for change, and the subsp. epillis that need to be changed. Great!

P_cus_probs$species[P_cus_probs$species == "Poa cusickii ssp. epilis"] <- "Poa cusickii"
P_cus_probs$species[P_cus_probs$species == "Poa cusickii ssp. pallida"] <- "Poa cusickii"

P_cus_pallida <- P_cusickii %>% 
  filter(!(peak == "bld" | peak == "pmd"))

P_cus_pallida$species[P_cus_pallida$species == "Poa cusickii"] <- "Poa cusickii subsp. pallida"
P_cus_pallida$species[P_cus_pallida$species == "Poa cusickii ssp. pallida"] <- "Poa cusickii subsp. pallida"
P_cus_pallida$species[P_cus_pallida$species == "Poa cusickii ssp. cusickii"] <- "Poa cusickii subsp. pallida"

#remove and bind:

ggb_all_data <- ggb_all_data[!grepl("Poa cusickii", ggb_all_data$species),]

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_cus_probs) %>% 
  bind_rows(P_cus_pallida)

#Poa fendleriana -> Poa fendleriana subsp. longiligula

ggb_all_data$species[ggb_all_data$species == "Poa fendleriana"] <- "Poa fendleriana subsp. longiligula"

#Poa secunda -> Poa secunda subsp. secunda

ggb_all_data$species[ggb_all_data$species == "Poa secunda"] <- "Poa secunda subsp. secunda"
ggb_all_data$species[ggb_all_data$species == "Poa secunda ssp. secunda"] <- "Poa secunda subsp. secunda"

#Potentilla concinna -> Potentilla concinna var. proxima

ggb_all_data$species[ggb_all_data$species == "Potentilla concinna"] <- "Potentilla concinna var. proxima"

#Potentilla drummondii -> Potentilla bruceae

ggb_all_data$species[ggb_all_data$species == "Potentilla drummondii"] <- "Potentilla bruceae"

#Potentilla hookeriana -> Potentilla hookeriana var. charletii

ggb_all_data$species[ggb_all_data$species == "Potentilla hookeriana"] <- "Potentilla hookeriana var. charletii"

#Potentilla nivea -> Potentilla holmgrenii

ggb_all_data$species[ggb_all_data$species == "Potentilla nivea"] <- "Potentilla holmgrenii"

#Potentilla ovina -> Potentilla ovina var. decurrens

ggb_all_data$species[ggb_all_data$species == "Potentilla ovina"] <- "Potentilla ovina var. decurrens"

#Potentilla rubricaulis -> Potentilla hookeriana var. charletii

ggb_all_data$species[ggb_all_data$species == "Potentilla rubricaulis"] <- "Potentilla hookeriana var. charletii"

#Purshia tridentata -> Purshia tridentata var. tridentata

ggb_all_data$species[ggb_all_data$species == "Purshia tridentata"] <- "Purshia tridentata var. tridentata"

#Ranunculus adoneus and Ranunculus adoneus var. alpinus ->  Ranunculus adoneus var. caespitosus

ggb_all_data$species[ggb_all_data$species == "Ranunculus adoneus"] <- "Ranunculus adoneus var. caespitosus"
ggb_all_data$species[ggb_all_data$species == "Ranunculus adoneus var. alpinus"] <- "Ranunculus adoneus var. caespitosus"

#Ribes cereum var. cereum -> Ribes cereum

ggb_all_data$species[ggb_all_data$species == "Ribes cereum var. cereum"] <- "Ribes cereum"

#Rubus idaeus and Rubus idaeus subsp. strigosus  -> Rubus idaeus var. strigosus

ggb_all_data$species[ggb_all_data$species == "Rubus idaeus"] <- "Rubus idaeus var. strigosus"
ggb_all_data$species[ggb_all_data$species == "Rubus idaeus subsp. strigosus"] <- "Rubus idaeus var. strigosus"

#Senecio fremontii -> Senecio fremontii var. occidentalis

ggb_all_data$species[ggb_all_data$species == "Senecio fremontii"] <- "Senecio fremontii var. occidentalis"

#Stipa occidentalis to var. californica
ggb_all_data$species[ggb_all_data$species == "Stipa occidentalis"] <- "Stipa occidentalis var. californica"

#Trifolium andersonii and Trifolium andersonii var. beatleyae -> Trifolium andersonii subsp. beatleyae

ggb_all_data$species[ggb_all_data$species == "Trifolium andersonii"] <- "Trifolium andersonii subsp. beatleyae"
ggb_all_data$species[ggb_all_data$species == "Trifolium andersonii var. beatleyae"] <- "Trifolium andersonii subsp. beatleyae"

#Woodsia scopulina -> Woodsia scopulina subsp. scopulina

ggb_all_data$species[ggb_all_data$species == "Woodsia scopulina"] <- "Woodsia scopulina subsp. scopulina"

#Problems noticed in the species summary table:

ggb_all_data$species[ggb_all_data$species == "Crystopteris fragilis"] <- "Cystopteris fragilis"

ggb_all_data$species[ggb_all_data$species == "Erigerion pygmaeus"] <- "Erigeron pygmaeus"

ggb_all_data$species[ggb_all_data$species == "Physaria kingii subsi. kingii"] <- "Physaria kingii subsp. kingii"

ggb_all_data$species[ggb_all_data$species == "Salvia pachypylla"] <- "Salvia pachyphylla"



