# Title: 03_SAS_data_cleaning_prep
# Author: Kaleb Goff
# Date: October 2022

##############################################################################################################
#The goal of this script is to clean the abundance and percent cover columns from the SAS data type. This script is used in cleaning the SAS data type as a whole.

##############################################################################################################
# SAS Cover Codes Cleaning:

SAS_qual <- SAS_data

#Check out the unique values:

table(SAS_qual$abundance)

# *possibly two plants -> r!

p_two_pls <- SAS_qual %>% 
  filter((abundance == "*possibly two plants"))

SAS_qual <- SAS_qual %>% 
  anti_join(p_two_pls)

p_two_pls$abundance[p_two_pls$abundance == "*possibly two plants"] <- "r!"

SAS_qual <- SAS_qual %>% 
  bind_rows(p_two_pls)

# r!! -> r!

r_double <- SAS_qual %>% 
  filter((abundance == "r!!"))

SAS_qual <- SAS_qual %>% 
  anti_join(r_double)

r_double$abundance[r_double$abundance == "r!!"] <- "r!"

SAS_qual <- SAS_qual %>% 
  bind_rows(r_double)

# r? -> r

r_question <- SAS_qual %>% 
  filter((abundance == "r?"))

SAS_qual <- SAS_qual %>% 
  anti_join(r_question)

r_question$abundance[r_question$abundance == "r?"] <- "r"

SAS_qual <- SAS_qual %>% 
  bind_rows(r_question)

#Round overlapping classes down:

#c-d -> c

o_1 <- SAS_qual %>% 
  filter((abundance == "c-d"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_1)

o_1$abundance[o_1$abundance == "c-d"] <- "c"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_1)

#c-s -> s

o_2 <- SAS_qual %>% 
  filter((abundance == "c-s"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_2)

o_2$abundance[o_2$abundance == "c-s"] <- "s"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_2)

#c/d -> c

o_3 <- SAS_qual %>% 
  filter((abundance == "c/d"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_3)

o_3$abundance[o_3$abundance == "c/d"] <- "c"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_3)

#r-s -> r

o_4 <- SAS_qual %>% 
  filter((abundance == "r-s"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_4)

o_4$abundance[o_4$abundance == "r-s"] <- "r"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_4)

#s-c -> s

o_5 <- SAS_qual %>% 
  filter((abundance == "s-c"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_5)

o_5$abundance[o_5$abundance == "s-c"] <- "s"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_5)

#s-r -> r

o_6 <- SAS_qual %>% 
  filter((abundance == "s-r"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_6)

o_6$abundance[o_6$abundance == "s-r"] <- "r"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_6)

#s/r -> r

o_7 <- SAS_qual %>% 
  filter((abundance == "s/r"))

SAS_qual <- SAS_qual %>% 
  anti_join(o_7)

o_7$abundance[o_7$abundance == "s/r"] <- "r"

SAS_qual <- SAS_qual %>% 
  bind_rows(o_7)

q31_check <- SAS_qual %>% 
  filter((abundance == "not present in 31 quad"))

SAS_qual <- SAS_qual %>% 
  anti_join(q31_check)

##################################################################################
#SAS Percent Cover Cleaning:

table(SAS_qual$pct_cover)

#The goal in this section is to make all of these interpretable as numeric. Ideally we have a range of values from 0.01 - 100.

#~90 -> 90.
c_1 <- SAS_qual %>% 
  filter((pct_cover == "~90"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_1)

c_1$pct_cover[c_1$pct_cover == "~90"] <- "90"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_1)

# >90 -> 95

c_90 <- SAS_qual %>% 
  filter((pct_cover == ">90"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_90)

c_90$pct_cover[c_90$pct_cover == ">90"] <- "95"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_90)

#<<1 -> 0.1

c_2 <- SAS_qual %>% 
  filter((pct_cover == "<<1"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_2)

c_2$pct_cover[c_2$pct_cover == "<<1"] <- "0.1"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_2)

#Change to the lowest acceptable cover class: 0.01:

one_tenth <- c("<0.01", "<0.1", "t", "tr", "trace", "trace "," trace ", "minimal", "miniscule", "0.001")

one_tenth_fix <- SAS_qual %>% 
  filter(pct_cover %in% one_tenth)

SAS_qual <- SAS_qual %>% 
  anti_join(one_tenth_fix)

one_tenth_fix$pct_cover <- "0.01"
one_tenth_fix$abundance <- NA #fix the case where there is a cover class with an abundance recorded

SAS_qual <- SAS_qual %>% 
  bind_rows(one_tenth_fix)

#actually, "1 clump", "2 individuals", "2 indivs." are species that need NA in this box.

add_NA_s <- c("1 clump", "2 individuals", "2 indivs.")

add_NA_sp <- SAS_qual %>% 
  filter(pct_cover %in% add_NA_s)

SAS_qual <- SAS_qual %>% 
  anti_join(add_NA_sp)

add_NA_sp$pct_cover <- NA

#What is up with "0"?

SAS_qual <- SAS_qual %>% 
  bind_rows(add_NA_sp)

zero_cov <- SAS_qual %>% 
  filter((pct_cover == "0"))

#remove

SAS_qual <- SAS_qual %>% 
  anti_join(zero_cov)

# <0.5 -> 0.1

c_3 <- SAS_qual %>% 
  filter((pct_cover == "<0.5"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_3)

c_3$pct_cover[c_3$pct_cover == "<0.5"] <- "0.1"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_3)

# <1 -> 0.5

c_4 <- SAS_qual %>% 
  filter((pct_cover == "<1"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_4)

c_4$pct_cover[c_4$pct_cover == "<1"] <- "0.5"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_4)

# <5 -> 2.5

c_5 <- SAS_qual %>% 
  filter((pct_cover == "<5"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_5)

c_5$pct_cover[c_5$pct_cover == "<5"] <- "2.5"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_5)

# >0.1 -> 0.5

c_6 <- SAS_qual %>% 
  filter((pct_cover == ">0.1"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_6)

c_6$pct_cover[c_6$pct_cover == ">0.1"] <- "0.5"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_6)

#What do we do about "present"?? This is mostly all bryophytes and lichens on soil, making me think that this most likely meant something closer to trace. Change to 0.1 for now.

# present -> 0.1

pres_cov <- SAS_qual %>% 
  filter((pct_cover == "present"))

SAS_qual <- SAS_qual %>% 
  anti_join(pres_cov)

pres_cov$pct_cover[pres_cov$pct_cover == "present"] <- "0.1"

SAS_qual <- SAS_qual %>% 
  bind_rows(pres_cov)

# >1 -> 2.5

c_7 <- SAS_qual %>% 
  filter((pct_cover == ">1"))

SAS_qual <- SAS_qual %>% 
  anti_join(c_7)

c_7$pct_cover[c_7$pct_cover == ">1"] <- "2.5"

SAS_qual <- SAS_qual %>% 
  bind_rows(c_7)

#There are species with abundances and percent covers. There are only 5 cases, and all are from grb. It looks like in the grb 2008 survey, they were midway between the absolute m2 cover and the qualitative system. Probably simple enough to just replace those percent covers with NA.

grb_add_na <- SAS_qual %>% 
  filter((pct_cover == "0.5")) %>% 
  filter((abundance == "r!"))

SAS_qual <- SAS_qual %>% 
  anti_join(grb_add_na)

grb_add_na$pct_cover <- NA

SAS_qual <- SAS_qual %>% 
  bind_rows(grb_add_na)

###########################################################################################################
#Clean up cover classes using: solid rock, bare ground, scree, litter, bryophytes, lichen.

#combine extras into lichen
comb_lichen <- c("lichen on litter", "lichen on Pinus albicaulis", "lichen on rock", "lichen on soil")

lichen <- SAS_qual %>% 
  filter(species %in% comb_lichen)

SAS_qual <- SAS_qual %>% 
  anti_join(lichen)

lichen$species <- "lichen"

SAS_qual <- SAS_qual %>% 
  bind_rows(lichen)

#combine into bryophytes

comb_bryo <- c("bryophytes on soil", "bryophytes on rock", "soil bryophytes")

bryo <- SAS_qual %>% 
  filter(species %in% comb_bryo)

SAS_qual <- SAS_qual %>% 
  anti_join(bryo)

bryo$species <- "bryophytes"

SAS_qual <- SAS_qual %>% 
  bind_rows(bryo)

#Remove unknowns:

non_species_problems <- c("unknown", "Unknown", "Unknown Apiaceae", "unknown dicot", "Unknown fern", "unknown grass", "Unknown grass", "unknown vascular dicot", "Unknown Brassicaceae", "unknown cotyledons")

SAS_qual <- SAS_qual %>% 
  filter(!species %in% non_species_problems)

################################################################################################################

SAS_check2 <- SAS_qual %>% 
  filter((abundance == "trace" | abundance == "<0.01"))

SAS_qual <- SAS_qual %>% 
  anti_join(SAS_check2)

SAS_check2$abundance[SAS_check2$abundance == "trace"] <- "0.01"
SAS_check2$abundance[SAS_check2$abundance == "<0.01"] <- "0.01"

SAS_qual <- SAS_qual %>% 
  bind_rows(SAS_check2)

###################################################################################################
#problems with the pct_cover data:

#lss_2015_e_5: the scan says 1 plants, 60 rock, 20 scree, 0.1 lichens, 0.1 bryos, 18.8 bare ground. The entered data has 100 for rock misentered.

lss_fix <- SAS_qual %>% 
  filter(peak == "lss") %>% 
  filter(year == "2015") %>% 
  filter(aspect == "e") %>% 
  filter(contour == "5") %>% 
  filter(species == "solid rock")

SAS_qual <- SAS_qual %>% 
  anti_join(lss_fix)

lss_fix$pct_cover[lss_fix$pct_cover == "100"] <- "60"

SAS_qual <- SAS_qual %>% 
  bind_rows(lss_fix)

#idr_2010_n_5: litter entered on the data sheet as 27. Should be 2.

idr_fix <- SAS_qual %>% 
  filter(peak == "idr") %>% 
  filter(year == "2010") %>% 
  filter(aspect == "n") %>% 
  filter(contour == "5") %>% 
  filter(species == "litter")

SAS_qual <- SAS_qual %>% 
  anti_join(idr_fix)

idr_fix$pct_cover[idr_fix$pct_cover == "27"] <- "2"

SAS_qual <- SAS_qual %>% 
  bind_rows(idr_fix)

#wmt_2009_se_10 change scree to 84 instead of 8.4:

wmt_scree_fix <- SAS_qual %>% 
  filter(peak == "wmt") %>% 
  filter(year == "2009") %>% 
  filter(aspect == "se") %>% 
  filter(contour == "10") %>% 
  filter(species == "scree")

SAS_qual <- SAS_qual %>% 
  anti_join(wmt_scree_fix)

wmt_scree_fix$pct_cover[wmt_scree_fix$pct_cover == "8.4"] <- "84"

SAS_qual <- SAS_qual %>% 
  bind_rows(wmt_scree_fix)

#cat_fpk_2011_s_5, change 66 bryophytes to 66 scree. This was done by changing the raw entered data.

#fix data entry for bld_2018_n_5. This was done by fixing the raw entered data.

#change 49.9 vascular plants to 49.9 solid rock at lws_2015_s_10 - changed on raw data.