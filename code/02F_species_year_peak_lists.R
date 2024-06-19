# Title: 02F_species_year_peak_lists
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################
#The first step was making changes that were noticed from looking at the species list across the entire data set. This included cases when there was only representative of a given genus.

#Androsace sp. to Androsace septentrionalis

ggb_all_data$species[ggb_all_data$species == "Androsace sp."] <- "Androsace septentrionalis"

#Antennaria (microphylla) rosea -> Antennaria rosea subsp. confinis

ggb_all_data$species[ggb_all_data$species == "Antennaria (microphylla) rosea"] <- "Antennaria rosea subsp. confinis"

#Check that we do not have A. microphylla in the White Mountains:

ant_check2 <- ggb_all_data %>% 
  filter((target_region == "wim" | target_region == "wds")) %>% 
  filter((species == "Antennaria microphylla"))

#Remove Astragalus sp. or Oxytropis sp.

Astra_probs <- ggb_all_data %>% 
  filter((species == "Astragalus sp. or Oxytropis sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Astra_probs)

#Remove Brassicaceae sp.

Brass_sp <- ggb_all_data %>% 
  filter((species == "Brassicaceae sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Brass_sp)

#Change "Carex duriscula" to "Carex duriuscula"

ggb_all_data$species[ggb_all_data$species == "Carex duriscula"] <- "Carex duriuscula"

#Change Carex hellerii to Carex helleri

ggb_all_data$species[ggb_all_data$species == "Carex hellerii"] <- "Carex helleri"

#Chamaebatiaria sp. to Chamaebatiaria millefolium

ggb_all_data$species[ggb_all_data$species == "Chamaebatiaria sp."] <- "Chamaebatiaria millefolium"

#Chrysothamnus sp. to Chrysothamnus viscidiflorus subsp. viscidiflorus

ggb_all_data$species[ggb_all_data$species == "Chrysothamnus sp."] <- "Chrysothamnus viscidiflorus subsp. viscidiflorus"

#Cymopterus terebinthinus to Cymopterus terebinthinus var. californicus

ggb_all_data$species[ggb_all_data$species == "Cymopterus terebinthinus"] <- "Cymopterus terebinthinus var. californicus"

#Cryptantha sp. -> Oreocarya sp.

ggb_all_data$species[ggb_all_data$species == "Cryptantha sp."] <- "Oreocarya sp."

#Diplacus bigelovii to Diplacus bigelovii var. cuspidatus

ggb_all_data$species[ggb_all_data$species == "Diplacus bigelovii"] <- "Diplacus bigelovii var. cuspidatus"

#Eremogone sp. to Eremogone kingii var. glabrescens

ggb_all_data$species[ggb_all_data$species == "Eremogone sp."] <- "Eremogone kingii var. glabrescens"

#Ericameria sp. to Ericameria parryi var. monocephala

ggb_all_data$species[ggb_all_data$species == "Ericameria sp."] <- "Ericameria parryi var. monocephala"

#Juncus balticus to Juncus balticus subsp. ater

ggb_all_data$species[ggb_all_data$species == "Juncus balticus"] <- "Juncus balticus subsp. ater"

#Juniperus sp. to Juniperus communis var. depressa at GRB (only record)

ggb_all_data$species[ggb_all_data$species == "Juniperus sp."] <- "Juniperus communis var. depressa"

#Keckiella sp. to Keckiella rothrockii var. rothrockii

ggb_all_data$species[ggb_all_data$species == "Keckiella sp."] <- "Keckiella rothrockii var. rothrockii"

#Koeleria sp. to Koeleria macrantha

ggb_all_data$species[ggb_all_data$species == "Koeleria sp."] <- "Koeleria macrantha"

#Linum sp. to Linum lewisii at dev (all records)

ggb_all_data$species[ggb_all_data$species == "Linum sp."] <- "Linum lewisii"

#Opuntia sp. to Opuntia polyacantha var. erinacea

ggb_all_data$species[ggb_all_data$species == "Opuntia sp."] <- "Opuntia polyacantha var. erinacea"

#Oxyria sp. to Oxyria digyna

ggb_all_data$species[ggb_all_data$species == "Oxyria sp."] <- "Oxyria digyna"

#Oxytropis sp. to Oxytropis parryi

ggb_all_data$species[ggb_all_data$species == "Oxytropis sp."] <- "Oxytropis parryi"

#Packera sp. remove from grl only:

packera_grl <- ggb_all_data %>% 
  filter((species == "Packera sp.")) %>% 
  filter((target_region == "grl"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(packera_grl)

#Physaria sp. to Physaria kingii subsp. kingii

ggb_all_data$species[ggb_all_data$species == "Physaria sp."] <- "Physaria kingii subsp. kingii"

#Pleiacanthus sp. to Pleiacanthus spinosus

ggb_all_data$species[ggb_all_data$species == "Pleiacanthus sp."] <- "Pleiacanthus spinosus"

#Poa cusickii/secunda to Poa sp.

ggb_all_data$species[ggb_all_data$species == "Poa cusickii/secunda"] <- "Poa sp."

#Primula sp. to Primula parryi on wlr (all records)

ggb_all_data$species[ggb_all_data$species == "Primula sp."] <- "Primula parryi"

#Remove Pteris sp.

pteris_sp <- ggb_all_data %>% 
  filter((species == "Pteris sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(pteris_sp)

#Pyrrocoma sp. to Pyrrocoma apargioides

ggb_all_data$species[ggb_all_data$species == "Pyrrocoma sp."] <- "Pyrrocoma apargioides"

#Change "Rhodiola integrifolia ssp. integrifolia" to "Rhodiola integrifolia subsp. integrifolia"

ggb_all_data$species[ggb_all_data$species == "Rhodiola integrifolia ssp. integrifolia"] <- "Rhodiola integrifolia subsp. integrifolia"

#Ribes sp. to Ribes cereum (all records, on tel and rna)

ggb_all_data$species[ggb_all_data$species == "Ribes sp."] <- "Ribes cereum"

#Salvia sp. to Salvia pachypyhlla

ggb_all_data$species[ggb_all_data$species == "Salvia sp."] <- "Salvia pachyphylla"

#Selaginella sp. to Selaginella watsonii

ggb_all_data$species[ggb_all_data$species == "Selaginella sp."] <- "Selaginella watsonii"

#Silene sp.: Combine with cf. verucunda at MID; combine with S. acaulis at PMD/remove

silene_sp_mid <- ggb_all_data %>% 
  filter((species == "Silene sp.")) %>% 
  filter((peak == "mid"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(silene_sp_mid)

silene_sp_mid$species[silene_sp_mid$species == "Silene sp."] <- "Silene cf. verucunda"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(silene_sp_mid)

silene_sp_pmd <- ggb_all_data %>% 
  filter((species == "Silene sp.")) %>% 
  filter((peak == "pmd"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(silene_sp_pmd)

silene_sp_pmd$species[silene_sp_pmd$species == "Silene sp."] <- "Silene acaulis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(silene_sp_pmd)

#Solidago sp. to Solidago multiradiata

ggb_all_data$species[ggb_all_data$species == "Solidago sp."] <- "Solidago multiradiata"

#Tetradymia sp. to Tetradymia canescens

ggb_all_data$species[ggb_all_data$species == "Tetradymia sp."] <- "Tetradymia canescens"

#Trisetum sp. to Trisetum spicatum

ggb_all_data$species[ggb_all_data$species == "Trisetum sp."] <- "Trisetum spicatum"

###############################################################################################################
#Species by year by peak cleaning, organized by target region and by peak within target region:

##############################################################################################################
#CAT

##FES

#Change all Eriogonum rosense to Eriogonum rosense var. rosense:

ggb_all_data$species[ggb_all_data$species == "Eriogonum rosense"] <- "Eriogonum rosense var. rosense"

#Remove Eremogone kingii var. glabrescens, replace with Minuartia nuttallii var. gracilis

cat_ere <- ggb_all_data %>% 
  filter((target_region == "cat")) %>% 
  filter((species == "Eremogone kingii var. glabrescens"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(cat_ere)

cat_ere$species[cat_ere$species == "Eremogone kingii var. glabrescens"] <- "Minuartia nuttallii var. gracilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cat_ere)

#Change Erigeron Pygmaeus to Erigerion pygmaeus

ggb_all_data$species[ggb_all_data$species == "Erigeron Pygmaeus"] <- "Erigerion pygmaeus"

#Change Oreocarya humilis to Oreocarya humilis subsp. humilis -- I made this exact same change on line 438 -- did it not work?

ggb_all_data$species[ggb_all_data$species == "Oreocarya humilis"] <- "Oreocarya humilis subsp. humilis"


## FPK

#Same changes to Eriogonum rosense and Eremogone kingii.

#Remove Chaenactis alpigena from fpk_2011

chae_alp_cat <- ggb_all_data %>% 
  filter((peak == "fpk")) %>% 
  filter((species == "Chaenactis alpigena"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(chae_alp_cat)

#Change "linanthus pungens" to "Linanthus pungens"

ggb_all_data$species[ggb_all_data$species == "linanthus pungens"] <- "Linanthus pungens"

# FSW

#Eriogonum rosense, already changed.

################################################################################################################
#DEV - changes across the entire TR:

#Lupinus sp. -> Lupinus argenteus var. argenteus

lup_dev_sp <- ggb_all_data %>% 
  filter(target_region == "dev") %>% 
  filter(species == "Lupinus sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(lup_dev_sp)

lup_dev_sp$species[lup_dev_sp$species == "Lupinus sp."] <- "Lupinus argenteus var. argenteus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(lup_dev_sp)

#Oenothera cespitosa and Oenothera sp. -> Oenothera cespitosa ssp. crinita

oeno_dev_sp <- ggb_all_data %>% 
  filter(target_region == "dev") %>% 
  filter(species == "Oenothera cespitosa" | species == "Oenothera sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(oeno_dev_sp)

oeno_dev_sp$species[oeno_dev_sp$species == "Oenothera cespitosa"] <- "Oenothera cespitosa ssp. crinita"
oeno_dev_sp$species[oeno_dev_sp$species == "Oenothera sp."] <- "Oenothera cespitosa ssp. crinita"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(oeno_dev_sp)

#LOW

#Aphyllon sp. -> Aphyllon fasciculatum

Aph.sp <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter((species == "Aphyllon sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Aph.sp)

Aph.sp$species[Aph.sp$species == "Aphyllon sp."] <- "Aphyllon fasciculatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Aph.sp)

#Remove Astragalus sp., there are two other options already listed and it is unlikely there was a third species observed.

Astra.sp <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter((species == "Astragalus sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Astra.sp)

#Chaenactis douglasii var. alpina -> var.douglasii

chae.doug <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter((species == "Chaenactis douglasii var. alpina"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(chae.doug)

chae.doug$species[chae.doug$species == "Chaenactis douglasii var. alpina"] <- "Chaenactis douglasii var. douglasii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(chae.doug)

#Erigeron sp. -> Erigeron clokeyi var. pinzliae

eri.sp <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter(species == "Erigeron sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(eri.sp)

eri.sp$species[eri.sp$species == "Erigeron sp."] <- "Erigeron clokeyi var. pinzliae"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(eri.sp)

#Remove Eriogonum umbellatum var. unknown, it is either var. subaridum or var. versicolor

E.umb.drop <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter(species == "Eriogonum umbellatum var. unknown")

ggb_all_data <- ggb_all_data %>% 
  anti_join(E.umb.drop)

#Galium sp. -> Galium hilendiae ssp. carneum

Gali_sp <- ggb_all_data %>% 
  filter(peak == "low") %>% 
  filter(species == "Galium sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(Gali_sp)

Gali_sp$species[Gali_sp$species == "Galium sp."] <- "Galium hilendiae ssp. carneum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Gali_sp)

#mid

#Castilleja sp. -> Castilleja applegatei subsp. martinii (only species ever seen on this peak)

cast_mid_sp <- ggb_all_data %>% 
  filter(peak == "mid") %>% 
  filter(species == "Castilleja sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(cast_mid_sp)

cast_mid_sp$species[cast_mid_sp$species == "Castilleja sp."] <- "Castilleja applegatei subsp. martinii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cast_mid_sp)

#Chaenactis sp. -> Chaenactis douglasii var. douglasii

chae_mid_sp <- ggb_all_data %>% 
  filter(peak == "mid") %>% 
  filter(species == "Chaenactis sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(chae_mid_sp)

chae_mid_sp$species[chae_mid_sp$species == "Chaenactis sp."] <- "Chaenactis douglasii var. douglasii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(chae_mid_sp)

#Stipa sp. -> Stipa hymenoides

stipa_mid_sp <- ggb_all_data %>% 
  filter(peak == "mid") %>% 
  filter(species == "Keckiella sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_mid_sp)

stipa_mid_sp$species[stipa_mid_sp$species == "Stipa sp."] <- "Stipa hymenoides"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(stipa_mid_sp)

#ben

#Penstemon sp. -> Penstemon rostriflorus

pen_ben_sp <- ggb_all_data %>% 
  filter(peak == "ben") %>% 
  filter(species == "Penstemon sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pen_ben_sp)

pen_ben_sp$species[pen_ben_sp$species == "Penstemon sp."] <- "Penstemon rostriflorus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(pen_ben_sp)

#tel

#Ribes sp. -> Ribes cereum already made.

####################################################################################################################
#GRB

#No changes apply across the entire TR.

#bld

#Draba sp. -> remove

drab_bld_sp <- ggb_all_data %>% 
  filter(peak == "bld") %>% 
  filter(species == "Draba sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(drab_bld_sp)

#Poa cusickii ssp. cusickii -> Poa cusickii

poacus_bld_sp <- ggb_all_data %>% 
  filter(peak == "bld") %>% 
  filter(species == "Poa cusickii ssp. cusickii")

ggb_all_data <- ggb_all_data %>% 
  anti_join(poacus_bld_sp)

poacus_bld_sp$species[poacus_bld_sp$species == "Poa cusickii ssp. cusickii"] <- "Poa cusickii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(poacus_bld_sp)

#Poa fendleriana subsp. longiligua -> Poa fendleriana

pfend_bld <- ggb_all_data %>% 
  filter(peak == "bld") %>% 
  filter(species == "Poa fendleriana subsp. longiligua") #why doesn't this match?

#bck

#Remove Minuartia obtusiloba from bck_2018

minobt_bck <- ggb_all_data %>% 
  filter(peak == "bck") %>% 
  filter(species == "Minuartia obtusiloba")

ggb_all_data <- ggb_all_data %>% 
  anti_join(minobt_bck)

#pmd

#Minuartia obtusiloba -> Minuartia sp.

minob_sp <- ggb_all_data %>% 
  filter(peak == "pmd") %>% 
  filter(species == "Minuartia obtusiloba")

ggb_all_data <- ggb_all_data %>% 
  anti_join(minob_sp)

minob_sp$species[minob_sp$species == "Minuartia obtusiloba"] <- "Minuartia sp."

ggb_all_data <- ggb_all_data %>% 
  bind_rows(minob_sp)

#Silene sp. -> Silene acaulis already changed

#wlr

#Phlox sp. -> Phlox pulvinata

phlo_wlr_sp <- ggb_all_data %>% 
  filter(peak == "wlr") %>% 
  filter(species == "Phlox sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(phlo_wlr_sp)

phlo_wlr_sp$species[phlo_wlr_sp$species == "Phlox sp."] <- "Phlox pulvinata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(phlo_wlr_sp)

#Polemonium sp. -> Polemonium viscosum

pole_wlr_sp <- ggb_all_data %>% 
  filter(peak == "wlr") %>% 
  filter(species == "Polemonium sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pole_wlr_sp)

pole_wlr_sp$species[pole_wlr_sp$species == "Polemonium sp."] <- "Polemonium viscosum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(pole_wlr_sp)

#Primula sp. -> Primula parryi already changed

##############################################################################################################
#LAN

#Changes across the entire TR:

#Castilleja applegatei subsp. martinii -> subsp. pallida

casti_lan <- ggb_all_data %>% 
  filter(target_region == "lan") %>% 
  filter(species == "Castilleja applegatei subsp. martinii")

ggb_all_data <- ggb_all_data %>% 
  anti_join(casti_lan)

casti_lan$species[casti_lan$species == "Castilleja applegatei subsp. martinii"] <- "Castilleja applegatei subsp. pallida"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(casti_lan)

#Polemonium sp. -> Polemonium eximium

pole_sp <- ggb_all_data %>% 
  filter(target_region == "lan") %>% 
  filter(species == "Polemonium sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pole_sp)

pole_sp$species[pole_sp$species == "Polemonium sp."] <- "Polemonium eximium"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(pole_sp)

#cfk

#Antennaria rosea -> Antennaria rosea subsp. confinis

ant_cfk_sp <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Antennaria rosea")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ant_cfk_sp)

ant_cfk_sp$species[ant_cfk_sp$species == "Antennaria rosea"] <- "Antennaria rosea subsp. confinis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ant_cfk_sp)

#Remove Carex subnigricans:

carex_subn_cfk <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Carex subnigricans")

ggb_all_data <- ggb_all_data %>% 
  anti_join(carex_subn_cfk)

#Festuca sp. -> Festuca saximontana

Festu_cfk <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Festuca sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(Festu_cfk)

Festu_cfk$species[Festu_cfk$species == "Festuca sp."] <- "Festuca saximontana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Festu_cfk)

#Lewisia sp. -> Lewisia pygmaea

lew_cfk_sp <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Lewisia sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(lew_cfk_sp)

lew_cfk_sp$species[lew_cfk_sp$species == "Lewisia sp."] <- "Lewisia pygmaea"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(lew_cfk_sp)

#Remove Packera sp.

pack_cfk_sp <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Packera sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pack_cfk_sp)

#Add Penstemon davidsonii var. davidsonii to 2015 - completed on adds sheet! Added to n/e 10m contour.

#Remove Penstemon heterodoxus var. heterodoxus from 2021:

penh_cfk_sp <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Penstemon heterodoxus var. heterodoxus") %>% 
  filter(year == "2021")

ggb_all_data <- ggb_all_data %>% 
  anti_join(penh_cfk_sp)

#Remove Potentilla sp.

pot_cfk_sp <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Potentilla sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pot_cfk_sp)

#Solidago sp. -> Solidago multiradiata: completed above

#Stipa occidentalis var. californica -> Stipa occidentalis var. occidentalis

stipa_cfk_occ <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Stipa occidentalis var. californica")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_cfk_occ)

stipa_cfk_occ$species[stipa_cfk_occ$species == "Stipa occidentalis var. californica"] <- "Stipa occidentalis var. occidentalis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(stipa_cfk_occ)

#Stipa pinetorum remove in 2015 - I added these on the adds sheet!

stipa_pin_cfk <- ggb_all_data %>% 
  filter(peak == "cfk") %>% 
  filter(species == "Stipa pinetorum") %>% 
  filter(year == "2015")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_pin_cfk)

#Add Stipa sp. - this will be completed later, Stipa is a prime candidate to aggregate to "sp." across this entire peak.

#idr

#Remove Agoseris sp.

ago_sp_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Agoseris sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ago_sp_idr)

#Remove Agoseris x elata

agox_sp_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Agoseris x elata")

ggb_all_data <- ggb_all_data %>% 
  anti_join(agox_sp_idr)

#Lupinus sp. -> Lupinus lepidus var. lobii

lup_sp_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Lupinus sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(lup_sp_idr)

lup_sp_idr$species[lup_sp_idr$species == "Lupinus sp."] <- "Lupinus lepidus var. lobbii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(lup_sp_idr)

#Potentilla breweri -> Potentilla bruceae

potb_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Potentilla breweri")

ggb_all_data <- ggb_all_data %>% 
  anti_join(potb_idr)

potb_idr$species[potb_idr$species == "Potentilla breweri"] <- "Potentilla bruceae"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(potb_idr)

#Remove Potentilla sp. from 2015

pot_sp_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Potentilla sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(pot_sp_idr)

#Stipa occidentalis var. californica -> Stipa pinetorum

stipa_pin_idr <- ggb_all_data %>% 
  filter(peak == "idr") %>% 
  filter(species == "Stipa occidentalis var. californica")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_pin_idr)

stipa_pin_idr$species[stipa_pin_idr$species == "Stipa occidentalis var. californica"] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(stipa_pin_idr)

#lss

#Add Boechera sp. to 2010 and 2021 - e_10, n_5, w_5/10, s_10

boe_lss <- ggb_all_data %>% 
  filter(peak == "lss") %>% 
  filter(species == "Boechera lemmonii")

#Carex sp. -> Carex rossii

carex_sp_lss <- ggb_all_data %>% 
  filter(peak == "lss") %>% 
  filter(species == "Carex sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(carex_sp_lss)

carex_sp_lss$species[carex_sp_lss$species == "Carex sp."] <- "Carex rossii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(carex_sp_lss)

#Cryptantha sp. -> Oreocarya sp. -> Oreocarya nubigena

crypt_sp_lss <- ggb_all_data %>% 
  filter(peak == "lss") %>% 
  filter(species == "Oreocarya sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(crypt_sp_lss)

crypt_sp_lss$species[crypt_sp_lss$species == "Cryptantha sp."] <- "Oreocarya nubigena"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(crypt_sp_lss)

#Add Draba sp. to lss_2015 and 2021, W10.

#lws

#Add Draba breweri in 2021 to E10

#Oxyria sp. -> Oxyria digyna already changed.

##################################################################################################################################
#SND

#no changes across entire TR.

#grl

#Antennaria sp. -> Antennaria rosea subsp. confinis

ant_grl_sp <- ggb_all_data %>% 
  filter(peak == "grl") %>% 
  filter(species == "Antennaria sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ant_grl_sp)

ant_grl_sp$species[ant_grl_sp$species == "Antennaria sp."] <- "Antennaria rosea subsp. confinis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ant_grl_sp)

#Add Boechera sp. 2004_grl n_5/10, e_5/10_, s_5/10

boe_grl_sp <- ggb_all_data %>% 
  filter(peak == "grl") %>% 
  filter(species == "Boechera sp.")

#Calamagrostis sp. -> Calamagrostis purpurascens

cala_grl_sp <- ggb_all_data %>% 
  filter(peak == "grl") %>% 
  filter(species == "Calamagrostis sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(cala_grl_sp)

cala_grl_sp$species[cala_grl_sp$species == "Calamagrostis sp."] <- "Calamagrostis purpurascens"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cala_grl_sp)

#Carex sp. -> Carex rossii

carex_sp_grl <- ggb_all_data %>% 
  filter(peak == "grl") %>% 
  filter(species == "Carex sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(carex_sp_grl)

carex_sp_grl$species[carex_sp_grl$species == "Carex sp."] <- "Carex rossii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(carex_sp_grl)

#Eriogonum ovalifolium var. caelestinum -> Eriogonum ovalifolium for grl_2004

eriooval_2004_grl <- ggb_all_data %>% 
  filter(peak == "grl") %>% 
  filter(species == "Eriogonum ovalifolium var. caelestinum") %>% 
  filter(year == "2004")

ggb_all_data <- ggb_all_data %>% 
  anti_join(eriooval_2004_grl)

eriooval_2004_grl$species[eriooval_2004_grl$species == "Eriogonum ovalifolium var. caelestinum"] <- "Eriogonum ovalifolium"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(eriooval_2004_grl)

#add Erigeron algidus to n5, e5 and e10 contours in 2004 grl.

#332

#Remove Artemisia arbuscula

art_arb_332 <- ggb_all_data %>% 
  filter(peak == "332") %>% 
  filter(species == "Artemisia arbuscula")

ggb_all_data <- ggb_all_data %>% 
  anti_join(art_arb_332)

#Add Boechera sp. to 2004 n/s/e_5

boe_332_sp <- ggb_all_data %>% 
  filter(peak == "332") %>% 
  filter(species == "Boechera sp.")

#Eriogonum ovalifolium varieties? Jan says they are all E. oval. caelestinum for 2009 and all subsequent years, in all plots except W10.

#Add Gayophytum diffusum ssp. parviflorum to S10 and W10 in 2009.

#Gayophytum humile add on w_10 2009

gayohum_332 <- ggb_all_data %>% 
  filter(peak == "332") %>% 
  filter(species == "Gayophytum humile")

#remove Gayophytum sp. from 2009

ghum_2009_332 <- ggb_all_data %>% 
  filter(peak == "332") %>% 
  filter(species == "Gayophytum sp.") %>% 
  filter(year == "2009")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ghum_2009_332)

#Linanthus sp. -> Linanthus pungens

lin_sp_332 <- ggb_all_data %>% 
  filter(peak == "332") %>% 
  filter(species == "Linanthus sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(lin_sp_332)

lin_sp_332$species[lin_sp_332$species == "Linanthus sp."] <- "Linanthus pungens"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(lin_sp_332)

#357

#Add Castilleja sp. on 357 2019 s_10

cast_357_sp <- ggb_all_data %>% 
  filter(peak == "357") %>% 
  filter(species == "Castilleja nana")

#Remove Draba breweri from 2004

drabb_357_2004 <- ggb_all_data %>% 
  filter(peak == "357") %>% 
  filter(species == "Draba breweri") %>% 
  filter(year == "2004")

ggb_all_data <- ggb_all_data %>% 
  anti_join(drabb_357_2004)

#Change Oreocarya flavoculata -> Oreocarya humilis subsp. humilis

oreo_fl_357 <- ggb_all_data %>% 
  filter(peak == "357") %>% 
  filter(species == "Oreocarya flavoculata")

ggb_all_data <- ggb_all_data %>% 
  anti_join(oreo_fl_357)

oreo_fl_357$species[oreo_fl_357$species == "Oreocarya flavoculata"] <- "Oreocarya humilis subsp. humilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(oreo_fl_357)

#374

#Boechera platysperma -> Boechera sp.

bplat_374 <- ggb_all_data %>% 
  filter(peak == "374") %>% 
  filter(species == "Boechera platysperma")

ggb_all_data <- ggb_all_data %>% 
  anti_join(bplat_374)

bplat_374$species[bplat_374$species == "Boechera platysperma"] <- "Boechera sp."

ggb_all_data <- ggb_all_data %>% 
  bind_rows(bplat_374)

#Add Boechera sp. to 2004 and 2014 s_5/10

boe_374_sp <- ggb_all_data %>% 
  filter(peak == "374") %>% 
  filter(species == "Boechera sp.")

#Eriogonum ovalifolium var. nivale -> Eriogonum ovalifolium var. caelestinum

eriooval_374 <- ggb_all_data %>% 
  filter(peak == "374") %>% 
  filter(species == "Eriogonum ovalifolium var. nivale")

ggb_all_data <- ggb_all_data %>% 
  anti_join(eriooval_374)

eriooval_374$species[eriooval_374$species == "Eriogonum ovalifolium var. nivale"] <- "Eriogonum ovalifolium var. caelestinum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(eriooval_374)

#Phlox sp. -> Phlox condensata

phlo_374_sp <- ggb_all_data %>% 
  filter(peak == "374") %>% 
  filter(species == "Phlox sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(phlo_374_sp)

phlo_374_sp$species[phlo_374_sp$species == "Phlox sp."] <- "Phlox condensata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(phlo_374_sp)

#########################################################################################################################
#SWE

#Cleaning across the entire TR:

#Poa cusickii subsp. pallida -> Poa cusickii

poacus_fry_bel_sp <- ggb_all_data %>% 
  filter(peak == "fry" | peak == "bel") %>% 
  filter(species == "Poa cusickii subsp. pallida")

ggb_all_data <- ggb_all_data %>% 
  anti_join(poacus_fry_bel_sp)

poacus_fry_bel_sp$species[poacus_fry_bel_sp$species == "Poa cusickii subsp. pallida"] <- "Poa cusickii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(poacus_fry_bel_sp)

#fry

#Antennaria umbrinella -> Antennaria sp.

Ant_fry_sp <- ggb_all_data %>% 
  filter(peak == "fry") %>% 
  filter(species == "Antennaria umbrinella")

ggb_all_data <- ggb_all_data %>% 
  anti_join(Ant_fry_sp)

Ant_fry_sp$species[Ant_fry_sp$species == "Antennaria umbrinella"] <- "Antennaria sp."

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Ant_fry_sp)

#Draba sp. -> Draba oligosperma

draba_fry_sp <- ggb_all_data %>% 
  filter(peak == "fry") %>% 
  filter(species == "Draba sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(draba_fry_sp)

draba_fry_sp$species[draba_fry_sp$species == "Draba sp."] <- "Draba oligosperma"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(draba_fry_sp)

#Potentilla sp. -> Potentilla pseudosericea

potp_fry_sp <- ggb_all_data %>% 
  filter(peak == "fry") %>% 
  filter(species == "Potentilla sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(potp_fry_sp)

potp_fry_sp$species[potp_fry_sp$species == "Potentilla sp."] <- "Potentilla pseudosericea"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(potp_fry_sp)

#bel

#Festuca sp. -> Festuca kingii

festu_bel_sp <- ggb_all_data %>% 
  filter(peak == "bel") %>% 
  filter(species == "Festuca sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(festu_bel_sp)

festu_bel_sp$species[festu_bel_sp$species == "Festuca sp."] <- "Festuca kingii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(festu_bel_sp)

#whe - no changes needed.

###############################################################################################################
#WDS

#Target-region level changes:

#Elymus sp. -> Elymus elymoides

ely_wds_sp <- ggb_all_data %>% 
  filter(target_region == "wds") %>% 
  filter(species == "Elymus sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ely_wds_sp)

ely_wds_sp$species[ely_wds_sp$species == "Elymus sp."] <- "Elymus elymoides"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ely_wds_sp)


#Eriogonum sp. -> Eriogonum gracilipes

eriog_wds_sp <- ggb_all_data %>% 
  filter(target_region == "wds") %>% 
  filter(species == "Eriogonum sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(eriog_wds_sp)

eriog_wds_sp$species[eriog_wds_sp$species == "Eriogonum sp."] <- "Eriogonum gracilipes"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(eriog_wds_sp)

#Oreocarya sp. -> Oreocarya humilis subsp. humilis

oreo_sp_sme <- ggb_all_data %>% 
  filter(peak == "sme" | peak == "pgs") %>% 
  filter(species == "Oreocarya sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(oreo_sp_sme)

oreo_sp_sme$species[oreo_sp_sme$species == "Oreocarya sp."] <- "Oreocarya humilis subsp. humilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(oreo_sp_sme)

#Phlox sp. -> Phlox condensata

phlox_sp_sme <- ggb_all_data %>% 
  filter(peak == "sme" | peak == "cws") %>% 
  filter(species == "Phlox sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(phlox_sp_sme)

phlox_sp_sme$species[phlox_sp_sme$species == "Phlox sp."] <- "Phlox condensata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(phlox_sp_sme)

#sme

#Remove Artemisia arbuscula from 2005

artarb_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Artemisia arbuscula")

ggb_all_data <- ggb_all_data %>% 
  anti_join(artarb_sme)

#Add Artemisia rothrockii to sme 2005 s_10, n_10

artroth_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Artemisia rothrockii")

#Remove Artemisia tridentata subsp. vaseyana  and Artemisia sp.

arttri_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Artemisia tridentata subsp. vaseyana")

ggb_all_data <- ggb_all_data %>% 
  anti_join(arttri_sme)

arttri_sme2 <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Artemisia sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(arttri_sme2)

#Brassicaceae/Boechera sp. -> Boechera sp.

brassbo_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Brassicaceae/Boechera sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(brassbo_sme)

brassbo_sme$species[brassbo_sme$species == "Brassicaceae/Boechera sp."] <- "Boechera sp."

ggb_all_data <- ggb_all_data %>% 
  bind_rows(brassbo_sme)

#Remove Draba lemmonii

drablem_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Draba lemmonii")

ggb_all_data <- ggb_all_data %>% 
  anti_join(drablem_sme)

#Remove Ericameria discoidea

ericad_sme <- ggb_all_data %>% 
  filter(peak == "sme") %>% 
  filter(species == "Ericameria discoidea")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ericad_sme)

#Townsendia sp. -> Townsendia leptotes

town_sp_pgs <- ggb_all_data %>% 
  filter(peak == "pgs") %>% 
  filter(species == "Townsendia sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(town_sp_pgs)

town_sp_pgs$species[town_sp_pgs$species == "Townsendia sp."] <- "Townsendia leptotes"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(town_sp_pgs)

#cws

#Carex duriscula -> Carex rossii in 2005 even though duriscula was there in 2021?

#Remove 2010 Poa sp. -- keeping this because all these Poas are going to become Poa sp.!

poa_sp_cws <- ggb_all_data %>% 
  filter(peak == "cws") %>% 
  filter(species == "Poa sp.")

#####################################################################################################
#WIM

#Target region wide changes:

#Stipa sp. -> Stipa pinetorum

stipa_shfrna_sp <- ggb_all_data %>% 
  filter(peak == "shf" | peak == "rna") %>% 
  filter(species == "Stipa sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_shfrna_sp)

stipa_shfrna_sp$species[stipa_shfrna_sp$species == "Stipa sp."] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(stipa_shfrna_sp)

#Carex sp. -> Carex rossii

carex_sp_rnabar <- ggb_all_data %>% 
  filter(peak == "rna" | peak == "bar") %>% 
  filter(species == "Carex sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(carex_sp_rnabar)

carex_sp_rnabar$species[carex_sp_rnabar$species == "Carex sp."] <- "Carex rossii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(carex_sp_rnabar)

#shf

#Add Boechera sp. to 2004 w_10 (where the 2019 sp. was seen)

boe_sp_shf <- ggb_all_data %>% 
  filter(peak == "shf") %>% 
  filter(species == "Boechera sp.")

#Remove Boechera sp. from 2009 (in 1x1, leaving for now)

#Calamagrostis sp. -> Calamagrostis purpurascens
cala_shf_sp <- ggb_all_data %>% 
  filter(peak == "shf") %>% 
  filter(species == "Calamagrostis sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(cala_shf_sp)

cala_shf_sp$species[cala_shf_sp$species == "Calamagrostis sp."] <- "Calamagrostis purpurascens"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cala_shf_sp)

#Stipa sp. -> Stipa pinetorum

stipa_shf_sp <- ggb_all_data %>% 
  filter(peak == "shf") %>% 
  filter(species == "Stipa sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(stipa_shf_sp)

stipa_shf_sp$species[stipa_shf_sp$species == "Stipa sp."] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(stipa_shf_sp)

#rna

#Athyrium distentifolium var. americanum -> Cystopteris fragilis

ath_rna_cys <- ggb_all_data %>% 
  filter(peak == "rna") %>% 
  filter(species == "Athyrium distentifolium var. americanum")

ggb_all_data <- ggb_all_data %>% 
  anti_join(ath_rna_cys)

ath_rna_cys$species[ath_rna_cys$species == "Athyrium distentifolium var. americanum"] <- "Cystopteris fragilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ath_rna_cys)

#Calamagrostis sp. -> Calamagrostis purpurascens

cala_rna_sp <- ggb_all_data %>% 
  filter(peak == "rna") %>% 
  filter(species == "Calamagrostis sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(cala_rna_sp)

cala_rna_sp$species[cala_rna_sp$species == "Calamagrostis sp."] <- "Calamagrostis purpurascens"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cala_rna_sp)

#Add Draba sp. to all years -- this looks like an aggregation due to confusion over subumbellta vs. oligosperma issue!

#Add Oxytropis parryi to 2004 in n_5

oxy_rna_check <- ggb_all_data %>% 
  filter(peak == "rna") %>% 
  filter(species == "Oxytropis parryi")

#Remove Trifolium andersonii subsp. beatleyae

triander_rna_rm <- ggb_all_data %>% 
  filter(peak == "rna") %>% 
  filter(species == "Trifolium andersonii subsp. beatleyae")

ggb_all_data <- ggb_all_data %>% 
  anti_join(triander_rna_rm)

#bar

#Remove Astragalus sp.

astra_bar_rm <- ggb_all_data %>% 
  filter(peak == "bar") %>% 
  filter(species == "Astragalus sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(astra_bar_rm)

#Add Draba sp. to 2004/2014/2019 -- this again is due to confusion over subumbellata and oligosperma, will be aggregated in future script.

#Eriogonum sp. -> Eriogonum ovalifolium var. nivale

erio_bar_sp <- ggb_all_data %>% 
  filter(peak == "bar") %>% 
  filter(species == "Eriogonum sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(erio_bar_sp)

erio_bar_sp$species[erio_bar_sp$species == "Eriogonum sp."] <- "Eriogonum ovalifolium var. nivale"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(erio_bar_sp)

#Festuca sp. -> Festuca brachyphylla subsp. breviculmis

festu_bar_sp <- ggb_all_data %>% 
  filter(peak == "bar") %>% 
  filter(species == "Festuca sp.")

ggb_all_data <- ggb_all_data %>% 
  anti_join(festu_bar_sp)

festu_bar_sp$species[festu_bar_sp$species == "Festuca sp."] <- "Festuca brachyphylla subsp. breviculmis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(festu_bar_sp)

#wmt

#Polemonium sp. -> Polemonium chartaceum already changed
