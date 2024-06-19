# Title: 02E_omits
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################

Omit_janslist <- janslist_problems %>% 
  filter(problem_code == "Omit")

#Row 1-2: Athyrium distentifolium var. americanum on rna in 2014:
  
rna_2014_fern <- ggb_all_data %>% 
  filter((peak == "rna")) %>% 
  filter((year == "2014")) %>% 
  filter((species == "Athyrium distentifolium var. americanum"))

#abundance = "not seen" and "?id"... so these seem like easy omits. It also seems like checking for stuff like this written in abundance would help:

ggb_all_data <- ggb_all_data %>% 
  anti_join(rna_2014_fern)

table(ggb_all_data$abundance)

#so pull "did not find", "did not find/see", "did not see", "did not see 2014", "not found", "not found in 2014", "not seen", "not seen in 2014"

not_there <- ggb_all_data %>% 
  filter((abundance == "did not find" | 
            abundance == "did not find/see" | 
            abundance == "did not see" | 
            abundance == "did not see 2014" | 
            abundance == "not found" | 
            abundance == "not found in 2014" | 
            abundance == "not seen" | 
            abundance == "not seen" | 
            abundance == "not seen 2014" | 
            abundance == "x" | 
            abundance == "not in nnw" | 
            abundance == "just over into W10"))

#The larger question is, of course, why was this written, and why was this entered from the scans to the csv files? Your guess is as good as mine. Omit all of these records
  
ggb_all_data <- ggb_all_data %>% 
  anti_join(not_there)

#It seems like this could also be done with the 10x10 and 1x1:
  
table(ggb_all_data$cover)

#"not present" and "not seen"

not_cover <- ggb_all_data %>% 
  filter((cover == "not present" | cover == "not seen"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(not_cover)

table(ggb_all_data$pct_cover)

#"not noticed" and "not seen"

not_pctcover <- ggb_all_data %>% 
  filter((pct_cover == "not noticed" | pct_cover == "not seen"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(not_pctcover)

#Ok back to Jan's omit list...

#Row 3: grl_2014_e Boechera howellii

grl_2014_e <- ggb_all_data %>% 
  filter((peak == "grl")) %>% 
  filter((year == "2014")) %>% 
  filter((aspect == "e")) %>%
  filter((species == "Boechera howellii"))

#Changed to proper name previously.
  
#Row 4: already fixed this above.

#Row 5: bar_2009_s Draba densifolia

bar_2009_s <- ggb_all_data %>% 
  filter((peak == "bar")) %>% 
  filter((year == "2009")) %>% 
  filter((aspect == "s")) %>%
  filter((species == "Draba densifolia"))

#Looks like Draba oligosperma is on the original data sheet, and D. densifolia was misentered in its place.

ggb_all_data <- ggb_all_data %>% 
  anti_join(bar_2009_s)

bar_2009_s$species[bar_2009_s$species == "Draba densifolia"] <- "Draba oligosperma"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(bar_2009_s)

#Row 6: idr_2015_e, Lewisia nevadensis

idr_2015_e <- ggb_all_data %>% 
  filter((peak == "idr")) %>% 
  filter((year == "2015")) %>% 
  filter((aspect == "e"))

#Not there... what happened to it? It was probably part of the "not found" etc. group!

#Row 7: cfk_2015_w, Lupinus breweri var. bryoides

cfk_2015_w <- ggb_all_data %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2015")) %>% 
  filter((aspect == "w"))

#same issue as above. fixed.

#Rows 8 - 10: Mertensia ciliata on bck 2018 e/n/w

bck_2018 <- ggb_all_data %>% 
  filter((peak == "bck")) %>% 
  filter((year == "2018")) %>% 
  filter((species == "Mertensia ciliata"))

#so, go back to bck_2018_10x10 scans. Was Mertensia ciliata seen in any 10x10s that year? No.

ggb_all_data <- ggb_all_data %>% 
  anti_join(bck_2018)

#Row 11: bck_2013_w, Mertensia franciscana, but says M. ciliata in Jan's notes..

bck_2013 <- ggb_all_data %>% 
  filter((peak == "bck")) %>% 
  filter((year == "2013")) %>% 
  filter((species == "Mertensia ciliata"))

#Was M. franciscana seen in the w 10x10 in 2013 on bck. Yes. Was M. ciliata? No.

#Row 12: There is a strange NA in lan_2010_cfk_e, so pull all those rows and check on the scanned data sheets to see if a species is missing.

cfk_2010_e <- ggb_all_data %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "e")) %>% 
  filter((contour == "10"))

#it was something on the 10m sheet and was r so is an SAS type

#go back to scans and check: Noticed that there are two Carex filifolia entries, one s and one r, and the one that has an r should be Carex subnigricans. 

#it is tough to navigate what might be that NA because of the name changes, but Haplopappus (changed to Ericameria discoidea), Achillea lannulosa (changed to millefolium), and Antennaria rosea are not on the scan. So the NA should be Antennaria rosea.

#remove cfk_2010_e from entire data:

ggb_all_data <- ggb_all_data %>% 
  anti_join(cfk_2010_e)

#replace NA in species with Antennaria rosea:

cfk_2010_e[ 29 , 7] = "Antennaria rosea"

#replace Carex filifolia abundance r with Carex subnigricans:

cfk_2010_e_r <- cfk_2010_e %>% 
  filter((abundance == "r"))

cfk_2010_e <- cfk_2010_e %>% 
  anti_join(cfk_2010_e_r)

cfk_2010_e_r$species[cfk_2010_e_r$species == "Carex filifolia var. erostrata"] <- "Carex subnigricans"

cfk_2010_e <- cfk_2010_e %>% 
  bind_rows(cfk_2010_e_r)

#add cfk_2010_e back to entire data:

ggb_all_data <- ggb_all_data %>% 
  bind_rows(cfk_2010_e)

#Row 13: simple omit of Phlox dispersa on idr_2021_s. But the other question is, did this entry error replace something that should have been on there?

#check s_10x10 on idr in 2021:

idr_10x_s <- ggb_all_data %>% 
  filter((aspect == "s")) %>% 
  filter((year == "2021")) %>% 
  filter((peak == "idr")) %>% 
  filter(data_type == "10m-x-10m-plot")

#Looking back at the original scans -- Phlox dispersa IS on there! So Jan's mistake. Moving on...

#Row 14: Polemonium pulcherrimum not there but P. viscosum is. Simple name change? wlr_2008_e

pol_wlr <- ggb_all_data %>% 
  filter((aspect == "e")) %>% 
  filter((year == "2008")) %>% 
  filter((peak == "wlr")) %>% 
  filter(species == "Polemonium pulcherrimum var. pulcherrimum")

#seems like a simple change to P. viscosum, which was seen in the 1x1 and SAS that year.

ggb_all_data <- ggb_all_data %>% 
  anti_join(pol_wlr)

pol_wlr$species[pol_wlr$species == "Polemonium pulcherrimum var. pulcherrimum"] <- "Polemonium viscosum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(pol_wlr)

#row 15: lan_idr_2010_s, Potentilla breweri not on data sheet?

idr_2010_s <- ggb_all_data %>% 
  filter((aspect == "s")) %>% 
  filter((year == "2010")) %>% 
  filter((peak == "idr")) %>% 
  filter(species == "Potentilla breweri")

#looking at the 10x10 scans, Potentilla breweri is not on there but Potentilla drummondii ssp. breweri is. Looking at the Jepson eflora, P. drummondii ssp. breweri is now P. breweri, so I don't understand what the problem is here.

#Rows 16-19 concern Stipas on ben_2013:

ben_2013 <- ggb_all_data %>% 
  filter((peak == "ben")) %>% 
  filter((year == "2013"))

#we've got S. lettermanii, S. thurberiana, and S. sp. Jan is saying the only thing there should be is Stipa pinetorum? Looking at the original datasheets, it says "Stipa (lettermanii) TBD", so that seems like an easy change to Stipa pinetorum. Perhaps the data entry person entered letermanii and Stipa sp (it isn't otherwise on the scan)?

ben_2013_stipa1 <- ggb_all_data %>% 
  filter((peak == "ben")) %>% 
  filter((year == "2013")) %>% 
  filter((aspect == "w")) %>% 
  filter((species == "Stipa lettermanii" | species == "Stipa sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(ben_2013_stipa1)

ben_2013_stipa1$species[ben_2013_stipa1$species == "Stipa lettermanii"] <- "Stipa pinetorum"
ben_2013_stipa1$species[ben_2013_stipa1$species == "Stipa sp."] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ben_2013_stipa1)

#now on to Stipa thurberiana:

ben_2013_stipathurb <- ggb_all_data %>% 
  filter((peak == "ben")) %>% 
  filter((year == "2013")) %>% 
  filter((aspect == "e" | aspect == "s")) %>% 
  filter((species == "Stipa thurberiana" | species == "Stipa sp."))

ggb_all_data <- ggb_all_data %>% 
  anti_join(ben_2013_stipathurb)

ben_2013_stipathurb$species[ben_2013_stipathurb$species == "Stipa thurberiana"] <- "Stipa pinetorum"
ben_2013_stipathurb$species[ben_2013_stipathurb$species == "Stipa sp."] <- "Stipa pinetorum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(ben_2013_stipathurb)

#there are still some Stipa sp. on the n aspect for this year, should these be changed to Stipa pinetorum?

#Lastly, remove any 2015 white mountains data that might have been added on accident:

wm_2015 <- ggb_all_data %>% 
  filter((target_region == "wim" | target_region == "wds")) %>% 
  filter((year == "2015"))

#the only thing there are some Poa cusickii I added on the add list. Just remove them here.

ggb_all_data <- ggb_all_data %>% 
  anti_join(wm_2015)



