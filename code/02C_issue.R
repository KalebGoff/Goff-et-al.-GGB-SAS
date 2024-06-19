# Title: 02C_issue
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#This script is part of a suite of sourced sub-scripts that clean the GLORIA Great Basin data. See script "02_GGB_taxonomic_data_cleaning_janslist" for details.

###############################################################################################################
#Fixes the "issue" problem code, which is essentially a catch-all for problems that Jan couldn't readily work out on her own, and needed a second or collective decision on. There are still unresolved/open issues in this section, some of which we might not be able to fully resolve.

#As I delved into the issues, I realized that they followed the same pattern as the wider problem codes, meaning some were essentially just name changes, some were mis-identifications, some were additions and some were omissions. So, I subdivided this script to fit those cases.

################################################################################################################
#Issue: Name Change

#Create a data frame of issue codes:

Issue_janslist <- janslist_problems %>% 
  filter(problem_code == "Issue")

#Row 6 - 10: Astragalus platytropis -> A. calycosus var. calycosus on bld_2008. Rationale: It seems reasonable to conclude that they called this Astragalus platytropis in 2008, and calycosus in all years following.

A_plat_issue <- ggb_all_data %>% 
  filter((species == "Astragalus platytropis")) %>% 
  filter((peak == "bld")) %>% 
  filter((year == "2008"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_plat_issue)

A_plat_issue$species[A_plat_issue$species == "Astragalus platytropis"] <- "Astragalus calycosus var. calycosus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_plat_issue)

#Row 11 - 14: Athyrium distentifolium var. americanum -> Cystopteris fragilis on 374_2004_e/n/s and 374_2009. Rationale: Jan said this change was "likely" based on her knowledge of the issue.

A_distent <- ggb_all_data %>% 
  filter((species == "Athyrium distentifolium var. americanum")) %>% 
  filter((peak == "374")) %>% 
  filter((year == "2004" | year == "2009"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_distent)

A_distent$species[A_distent$species == "Athyrium distentifolium var. americanum"] <- "Cystopteris fragilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_distent)

#Row 20: Athyrium distentifolium var. americanum -> Woodsia scopulina subsp. scopulina on rna_2009_s. Rationale: Jan's determination that this species is more likely.

A_distent2 <- ggb_all_data %>% 
  filter((species == "Athyrium distentifolium var. americanum")) %>% 
  filter((peak == "rna")) %>% 
  filter((year == "2009")) %>% 
  filter((aspect == "s"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(A_distent2)

A_distent2$species[A_distent2$species == "Athyrium distentifolium var. americanum"] <- "Woodsia scopulina subsp. scopulina"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(A_distent2)

#Rows 21 - 30: Boechera depauperata -> Boechera lemmonii on idr_2010_e/s, idr_2021, lss_2010_w. Rationale: Jan labeled this as a "possible misident." However, B. depauperata was observed on lss_2021 on the e/w aspects. So is this a workable change?

B_depaup_issue1 <- ggb_all_data %>% 
  filter((species == "Boechera depauperata")) %>% 
  filter((peak == "idr"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_depaup_issue1)

B_depaup_issue1$species[B_depaup_issue1$species == "Boechera depauperata"] <- "Boechera lemmonii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_depaup_issue1)

#lss

B_depaup_issue2 <- ggb_all_data %>% 
  filter((species == "Boechera depauperata")) %>% 
  filter((peak == "lss")) %>% 
  filter((year == "2010")) %>% 
  filter((aspect == "w"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_depaup_issue2)

B_depaup_issue2$species[B_depaup_issue2$species == "Boechera depauperata"] <- "Boechera lemmonii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_depaup_issue2)

#Row 34 - 38. No issue.

#Row 41: Boechera inyoensis -> Boechera depauperata on 374_2014_s. Rationale: Jan noticed a note on the datasheet.

B_inyo_issue <- ggb_all_data %>% 
  filter((species == "Boechera inyoensis")) %>% 
  filter((peak == "374")) %>% 
  filter((year == "2014")) %>% 
  filter((aspect == "s"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_inyo_issue)

B_inyo_issue$species[B_inyo_issue$species == "Boechera inyoensis"] <- "Boechera depauperata"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_inyo_issue)

#Row 42: Boechera inyoensis -> Boechera pendulocarpa on bel_2017_s. Rationale: jan's determination "leans toward."

B_inyo_issue2 <- ggb_all_data %>% 
  filter((species == "Boechera inyoensis")) %>% 
  filter((peak == "bel")) %>% 
  filter((year == "2017")) %>% 
  filter((aspect == "s"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_inyo_issue2)

B_inyo_issue2$species[B_inyo_issue2$species == "Boechera inyoensis"] <- "Boechera pendulocarpa"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_inyo_issue2)

#Rows 43 - 50: No changes needed.

#Rows 68 - 70: Carex microptera -> Carex haydeniana on bar_e all years. Rationale: "probably misident" according to Jan.

C_micro_issue <- ggb_all_data %>% 
  filter((species == "Carex microptera")) %>% 
  filter((peak == "bar")) %>% 
  filter((aspect == "e"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(C_micro_issue)

C_micro_issue$species[C_micro_issue$species == "Carex microptera"] <- "Carex haydeniana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(C_micro_issue)

#Rows 223 - 224: Penstemon davidsonii var. davidsonii -> Penstemon newberryi var. newberryi on cfk_2015_n/s. Rationale: Jan's determination, though still in question.

Pen_issue <- ggb_all_data %>% 
  filter((species == "Penstemon davidsonii var. davidsonii")) %>% 
  filter((peak == "cfk")) %>% 
  filter((year == "2015"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Pen_issue)

Pen_issue$species[Pen_issue$species == "Penstemon davidsonii var. davidsonii"] <- "Penstemon newberryi var. newberryi"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Pen_issue)

#Rows 225 - 233: Poa cusickii -> "the typical" -- pallida on fry and bel. I already changed these.

#Row 234 - 235: Potentilla bruceae -> Potentilla pseudosericea on 374_2014_n and 374_2004_n

P_bruc_issue <- ggb_all_data %>% 
  filter((species == "Potentilla bruceae")) %>% 
  filter((peak == "374")) %>% 
  filter((year == "2014" | year == "2004"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(P_bruc_issue)

P_bruc_issue$species[P_bruc_issue$species == "Potentilla bruceae"] <- "Potentilla pseudosericea"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_bruc_issue)

#Rows 246 - 247: Stipa lettermanii -> Stipa occidentalis var. occidentalis on 332_2009_s/w. Rationale: Potentially confused, see Jan's notes.

S_lett_issue <- ggb_all_data %>% 
  filter((species == "Stipa lettermanii")) %>% 
  filter((peak == "332")) %>% 
  filter((year == "2009"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(S_lett_issue)

S_lett_issue$species[S_lett_issue$species == "Stipa lettermanii"] <- "Stipa occidentalis var. occidentalis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(S_lett_issue)

########################################################################################################################
#Issue: Suspected Mis-identification based on Jan's expert opinion

#Rows 261 - 270: Boechera platysperma -> Boechera howellii on cat in 2006 and 2011.

B_plat_issue <- ggb_all_data %>% 
  filter((species == "Boechera platysperma")) %>% 
  filter((target_region == "cat"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(B_plat_issue)

B_plat_issue$species[B_plat_issue$species == "Boechera platysperma"] <- "Boechera howellii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(B_plat_issue)

#Rows 271 - 275: Carex rossii -> Carex hellerii on fpk_2011

Carex_susmisident <- ggb_all_data %>% 
  filter((species == "Carex rossii")) %>% 
  filter((target_region == "cat"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Carex_susmisident)

Carex_susmisident$species[Carex_susmisident$species == "Carex rossii"] <- "Carex hellerii"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Carex_susmisident)

#Rows 276 - 277: Koeleria macrantha -> Trisetum spicatum on fes and fpk in 2011.

Koe_issue <- ggb_all_data %>% 
  filter((species == "Koeleria macrantha")) %>% 
  filter((target_region == "cat"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Koe_issue)

Koe_issue$species[Koe_issue$species == "Koeleria macrantha"] <- "Trisetum spicatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Koe_issue)

#Rows 278 - 288: Penstemon newberryi var. newberryi -> Penstemon speciosus on cat_2006. Rationale: not seen again, easily confused.

P_cat_issue <- ggb_all_data %>% 
  filter((species == "Penstemon newberryi var. newberryi")) %>% 
  filter((target_region == "cat"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(P_cat_issue)

P_cat_issue$species[P_cat_issue$species == "Penstemon newberryi var. newberryi"] <- "Penstemon speciosus"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(P_cat_issue)

##############################################################################################################################
#Issue: Collective Decision - These were issues that Jan could not work out on her own. A meeting with Kaleb, Jan and Seema in July 2022 at Crooked Creek was used to talk through some of these issues and come to conclusions. In other cases we were not able to reach a conclusion.

#Rows 1: Leave it as A. umbrinella; Jan says we need a list of things to pursue next time. Who put this on the data sheet - Mary Kline and Mark Darrach, and they added "?" next to this row. Seems like a good example of "sp." to me.

#Rows 2 - 3: Aspidotis is NOT there, pull list and see if one of the two species noted is present in the same aspect in another year. That might be a good guess.

#grl n and grl e, how many entries of either Cryptogramma acrosticoides or Cystopteris fragilis?

Aspidotis_issue <- ggb_all_data %>% 
  filter((peak == "grl")) %>% 
  filter((aspect == "n" | aspect == "e")) %>% 
  filter((species == "Cryptogramma acrosticoides" | species == "Cystopteris fragilis"))

#ONLY Cystopteris has been seen on these aspects, in 2009. So change Aspidotis -> Cystopteris fragilis on grl.

Aspidotis_iss_fix <- ggb_all_data %>% 
  filter((species == "Aspidotis densa")) %>% 
  filter((peak == "grl"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Aspidotis_iss_fix)

Aspidotis_iss_fix$species[Aspidotis_iss_fix$species == "Aspidotis densa"] <- "Cystopteris fragilis"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Aspidotis_iss_fix)

#Rows 15 - 19: Another tricky fern issue, with the added problem that they might both be present.

Athyrium_issue <- ggb_all_data %>% 
  filter((species == "Cystopteris fragilis" | species == "Woodsia scopulina subsp. scopulina")) %>% 
  filter((peak == "rna"))

#Both fern species were seen on the e/n/s... so what do we do here?

#Rows 31 - 34: These should be changed to "Boechera sp."

#Rows 39 - 40: Change to "Boechera sp."

#Rows 51 - 66 : Same issue as above

#Row 67: Omit from data. A very interesting case where a noxious weed was found on a summit and immediately pulled.

bromus_rubens <- ggb_all_data %>% 
  filter((species == "Bromus rubens"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(bromus_rubens)

#Row 71: Omit

rossii_issue <- ggb_all_data %>% 
  filter((species == "Carex rossii")) %>% 
  filter((peak == "cws"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(rossii_issue)

#Row 72 - 73: change to "Castilleja sp." for now.

#Row 74: no issue here...

#Row 75: no issue

#Rows 76 - 80: change to "Oreocarya sp"

#Rows 81 - 93 - Leave alone until we come back from Carson, we can say what we saw this year, but what about the previous year?
  
#Row 94: It is "Draba sp." for now, but we are waiting for an answer!
  
#Rows 95 - 99 - Same as row 81 - 93

#Rows 100 - 102: Change to "Draba sp."

#Rows 103 - 135: Draba oligospermma vs subumbellata in the White Mountains -- this might be a Draba sp. issue -- though there may be a way in future years to see which is on the summit. There is more oligospermma than subumbellata in those upper transects.

#Rows 136 - 146 - see above

#Rows 147 - 172: Check duplicates? See rows 4260 - 4346 from Jan's original file, I see what this is referring to (same peak and aspect, one var. monocephala and one not) -- but how can we figure out if these are duplicates? See section on duplicates.

#Rows 173 - 192: what are we going to do about these two varieties? Just drop the varieties all together (that causes other problems)? - See notes above

#Row 193 - Eriogonum umbellatum, dev_low_2018_w: remove intrarank.

erio_umb_vardrop <- ggb_all_data %>% 
  filter((aspect == "w")) %>% 
  filter((peak == "low")) %>% 
  filter((year == "2018")) %>% 
  filter((species == "Eriogonum umbellatum var. versicolor"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(erio_umb_vardrop)

erio_umb_vardrop$species[erio_umb_vardrop$species == "Eriogonum umbellatum var. versicolor"] <- "Eriogonum umbellatum"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(erio_umb_vardrop)

#Rows 195 - 200: Where when and how often do we add Festuca minutiflora?
  
#Row 201: Lupinus breweri -> Lupinus breweri var. bryoides on cfk_2010_w. Rationale: Jan's best idea of what variety this might be. I already changed this on Line 702.

#Row 202 - 203: Mertensia ciliata -> Mertensia franciscana on bck_2008/2018_s

M_cil_issue <- ggb_all_data %>% 
  filter((species == "Mertensia ciliata")) %>% 
  filter((peak == "bck")) %>% 
  filter((aspect == "s")) %>% 
  filter((year == "2008" | year == "2018"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(M_cil_issue)

M_cil_issue$species[M_cil_issue$species == "Mertensia ciliata"] <- "Mertensia franciscana"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(M_cil_issue)

#Rows 204 - 219: move to "sp."

#Rows 220 - 222: Might be as simple as dropping the var. everywhere.

#drop subsp. crinita and subsp. marginata on all Oenothera cespitosa

Oeno_cesp <- ggb_all_data %>% 
  filter((species == "Oenothera cespitosa subsp. crinita" | species == "Oenothera cespitosa subsp. marginata"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(Oeno_cesp)

Oeno_cesp$species[Oeno_cesp$species == "Oenothera cespitosa subsp. crinita"] <- "Oenothera cespitosa"

ggb_all_data <- ggb_all_data %>% 
  bind_rows(Oeno_cesp)

#Row 236: P. jepsonii only on BAR this year, change all prior morefieldii for BAR. Dylan's expert opinion.

#Row 237: Jan is going to check specimen at WMRC!
  
#Rows 239 - 244: simple as providing Jan with the aspects in question? - see above

#Row 245: needs validation from SEKI - Jan is confident that is what it is.

#Rows 248 - 260 - probably no good changes to do here, live with it.

dev_low_umb <- ggb_all_data %>% 
  filter((species == "Eriogonum umbellatum var. dichrocephalum"))

ggb_all_data <- ggb_all_data %>% 
  anti_join(dev_low_umb)

#Salvia not correcting, "pachypylla" record can be removed, it is doubled with the same record on the same peak

salvia_pachy_rem <- ggb_all_data %>% 
  filter(species == "Salvia pachypylla")

ggb_all_data <- ggb_all_data %>% 
  anti_join(salvia_pachy_rem)




