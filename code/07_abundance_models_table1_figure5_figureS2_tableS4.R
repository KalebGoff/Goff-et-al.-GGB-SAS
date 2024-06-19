#Title: 07_abundance_models_table1_figure5_figureS2_tableS4
#Date: April 2024
#Author: Kaleb Goff

#############################################################################################
#Abundance models for each functional group to test whether abundance change over time depends on functional group.

#this script creates Table 1, Figure 5, Figure S2, and Table S4 in the manuscript.

###########################################################################################
# make vector of packages needed

packages_needed <- c("tidyverse", "ordinal", "ggeffects", "performance", "ggpubr")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

##############################################################################################################
#import data:
SAS_dat <- read_csv("./data-derived/SAS_clean_data.csv")

#remove country, data_type:
SAS_dat <- SAS_dat %>% 
  dplyr::select(-country, -data_type, -pct_cover, -cf)

#remove cover class "species":
cover_class <- c("solid rock", "bare ground", "scree", "litter", "bryophytes", "lichen")

abund_df <- SAS_dat %>% 
  filter(!species %in% cover_class)

#remove abundance data before 2006, when another method was used:
abund_df_trunc <- abund_df %>% 
  filter(year > 2006)

abund_df_trunc3 <- abund_df_trunc

abund_df_trunc3$abundance <- gsub("r!", "1", abund_df_trunc3$abundance)
abund_df_trunc3$abundance <- gsub("r", "2", abund_df_trunc3$abundance)
abund_df_trunc3$abundance <- gsub("s", "3", abund_df_trunc3$abundance)
abund_df_trunc3$abundance <- gsub("c", "4", abund_df_trunc3$abundance)
abund_df_trunc3$abundance <- gsub("d", "5", abund_df_trunc3$abundance)

abund_df_trunc3 <- abund_df_trunc3 %>% 
  mutate(abundance = ordered(abundance))

#remove na and rename abundance to response:
resp_df <- abund_df_trunc3 %>% 
  filter(!is.na(abundance))

d <- resp_df

#add in year_number:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

d2 <- left_join(d, year.df)

#add elevation:
#bring in elevation information:
elev_df <- read_csv("./data-raw/plot_elevation_information.csv")

d3 <- left_join(d2, elev_df, by = c("target_region", "peak"))

#create a new elevation column with elevation centered across all peaks:
d3 <- d3 %>% 
  ungroup() %>% 
  mutate(elev_m_cen = as.vector(scale(elevation_m, center = TRUE, scale = TRUE)))

#make sure data classes are correct:

d3 <- d3 %>% 
  mutate(contour = as.factor(contour)) %>% 
  mutate(aspect = as.factor(aspect)) %>% 
  mutate(target_region = as.factor(target_region)) %>% 
  mutate(peak = as.factor(peak)) %>% 
  dplyr::select(-elev_rank) %>% 
  dplyr::select(-elevation_m)

d4 <- d3

#Bring in functional group list:
func_gr <- read_csv("./data-derived/functional_group_sheet_V2.csv")

#join:
d5 <- left_join(x = d4, y = func_gr, by = "species")

sp_check <- d5 %>% 
  ungroup %>% 
  distinct(species)

#subset functional groups:

d_shrub_tree <- d5 %>% 
  filter(functional_group == "shrub" | functional_group == "tree")

d_graminoid <- d5 %>% 
  filter(functional_group == "graminoid")

d_cushion <- d5 %>% 
  filter(functional_group == "cushion")

d_forb <- d5 %>% 
  filter(functional_group == "forb")

######################################################################
#CLM MODELS:
####################################################################
#Forbs:

m_forb_clm <- clm(formula = abundance ~ year_num * elev_m_cen + year_num * target_region + elev_m_cen * target_region,
                   link = "probit", data = d_forb)

summary(m_forb_clm)

car::Anova(m_forb_clm, type = 3)
#sig eff target region
#sig eff elev * tr

r2(m_forb_clm)
#0.034

###############################################################
#Cushions:
m_cushion_clm <- clm(formula = abundance ~ year_num * elev_m_cen + year_num * target_region + elev_m_cen * target_region,
                      link = "probit", data = d_cushion)

summary(m_cushion_clm)

car::Anova(m_cushion_clm, type = 3)
#sig eff year
#sig eff target region

r2(m_cushion_clm)
#0.101

##############################
#graminoids:
m_graminoid_clm <- clm(formula = abundance ~ year_num * elev_m_cen + year_num * target_region + elev_m_cen * target_region,
                        link = "probit", data = d_graminoid)

summary(m_graminoid_clm)

car::Anova(m_graminoid_clm, type = 3)
#sig eff year
#sig eff target region

r2(m_graminoid_clm)
#0.074

##############################
#shrubs/trees:
m_shrub_tree_clm <- clm(formula = abundance ~ year_num * elev_m_cen + year_num * target_region + elev_m_cen * target_region,
                         link = "probit", data = d_shrub_tree)

summary(m_shrub_tree_clm)

car::Anova(m_shrub_tree_clm, type = 3)
#sig eff year
#sig eff target region
#sig eff year * target region
#sig eff elev * target region

r2(m_shrub_tree_clm)
#0.126

###########################################################
#Compute marginal effects for significant effects:
#Forb: target region, elev:tr
#Cushion: year, target region
#Graminoid: year, target_region
#Shrub/Tree: year, target region, year:tr, year:elev

################################################################################################################
#Visualize, create Figure 5:

#create model predictions from emmeans, bind and set up data frame:
em_shrubtree <- ggemmeans(m_shrub_tree_clm, terms = "year_num")
em_shrubtree$group <- "shrub/tree"

em_cushion <- ggemmeans(m_cushion_clm, terms = "year_num")
em_cushion$group <- "cushion"

em_graminoid <- ggemmeans(m_graminoid_clm, terms = "year_num")
em_graminoid$group <- "graminoid"

em_forb <- ggemmeans(m_forb_clm, terms = "year_num")
em_forb$group <- "forb"

em_fun_bind <- rbind(em_forb, em_graminoid, em_cushion, em_shrubtree)

em_df <- em_fun_bind %>% 
  rename(year_num = x,
         functional_group = group)

#add year numbers back in:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

em_df2 <- left_join(em_df, year.df, by = "year_num")

#recode response levels to reflect abundance categories:
em_df2$response.level <- recode_factor(em_df2$response.level,
                                       "1" = "r! - very rare",
                                       "2" = "r - rare",
                                       "3" = "s - scattered",
                                       "4" = "c - common",
                                       "5" = "d - dominant")

theme_set(theme_bw(base_size = 14))


pal <- c("forb" = "#333333",
          "cushion" = "#737373",
          "graminoid" = "#b4b4b4",
          "shrub/tree" = "#CCCCCC")

p2 <- ggplot(data = em_df2, aes(x = year, y = predicted, color = functional_group, group = functional_group, fill = functional_group)) +
  geom_line(linewidth = 2, aes(linetype = functional_group)) +
  facet_wrap(~ response.level, nrow = 1) +
  xlab("Survey Year") +
  ylab("Predicted probability") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 14, face = "bold"), legend.position = "top", legend.title = element_blank()) +
  scale_linetype_manual(values = c("forb" = "dashed", "cushion" = "solid", "graminoid" = "solid", "shrub/tree" = "solid"), guide = "none") +
  scale_color_manual(values = pal)

p2

ggsave(filename = "abundance_model_pred_07_16.jpg",
       path = "./figures/",
       plot = p2,
       units = "in",
       dpi = 200,
       width = 10, height = 6)

############################################################
#Create supplemental figure showing year * target region interaction for shrub/tree:

#5 panels (one for each abundance category), with 1 row of plots. Use same TR color palette.

theme_set(theme_bw(base_size = 24))

pal.tr <- c("cat" = "#999999", 
            "snd" = "#E69F00", 
            "swe" = "#56B4E9", 
            "grb" = "#009E73", 
            "lan" = "#F0E442",
            "wim" = "#0072B2", 
            "wds" = "#D55E00", 
            "dev" = "#CC79A7")

# New facet label names for supp variable
tr.labs <- c("Carson-Tahoe", "Death Valley NP", "Great Basin NP", "Mount Langley", "Sierra Nevada Dunderburg", "Sweetwater Mountains", "White Mountain Dolostone", "White Mountain Silicates")
names(tr.labs) <- c("cat", "dev", "grb", "lan", "snd", "swe", "wds", "wim")

#shrub/tree
em_int_st <- ggemmeans(m_shrub_tree_clm, terms = c("year_num", "target_region"))

em_int_st <- em_int_st %>% 
  rename(year_num = x,
         target_region = group)

#add year numbers back in:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

em_int_st <- left_join(em_int_st, year.df, by = "year_num")

#recode response levels to reflect abundance categories:
em_int_st$response.level <- recode_factor(em_int_st$response.level,
                                          "1" = "r!",
                                          "2" = "r",
                                          "3" = "s",
                                          "4" = "c",
                                          "5" = "d")

pS2 <- ggplot(data = em_int_st, aes(x = year, y = predicted, color = target_region, group = target_region, fill = target_region)) +
  geom_line(linewidth = 2) +
  geom_ribbon(aes(ymin = predicted - conf.low, ymax = predicted + conf.high), alpha = 0.1, color = NA) +
  facet_wrap(~ response.level, nrow = 1) +
  xlab("Survey Year") +
  ylab("Predicted probability") +
  ggtitle("functional group: shrub/tree") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 18, face = "bold"), legend.title = element_blank(), strip.text.x = element_text(size = 18)) +
  scale_color_manual(values = pal.tr) + 
  scale_fill_manual(values = pal.tr)

pS2

ggsave(filename = "abundance_tr-year_interaction_07_16.jpg",
       path = "./figures/",
       plot = pS2,
       units = "in",
       dpi = 200,
       width = 13, height = 9)

###########################################
#Table outputs for sig year effects for cushion, graminoid and shrub/tree

#year table:
em_shrubtree <- ggemmeans(m_shrub_tree_clm, terms = "year_num[5, 11, 19]")
em_shrubtree$group <- "shrub/tree"

em_cushion <- ggemmeans(m_cushion_clm, terms = "year_num[5, 11, 19]")
em_cushion$group <- "cushion"

em_graminoid <- ggemmeans(m_graminoid_clm, terms = "year_num[5, 11, 19]")
em_graminoid$group <- "graminoid"

em_fun_bind <- rbind(em_graminoid, em_cushion, em_shrubtree)

em_df <- em_fun_bind %>% 
  rename(year_num = x,
         functional_group = group)

#add year numbers back in:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

em_df2 <- left_join(em_df, year.df, by = "year_num")

#recode response levels to reflect abundance categories:
em_df2$response.level <- recode_factor(em_df2$response.level,
                                       "1" = "r!",
                                       "2" = "r",
                                       "3" = "s",
                                       "4" = "c",
                                       "5" = "d")

year_table <- em_df2

year_table$predicted <- round(year_table$predicted, digits = 3)
year_table$conf.low <- round(year_table$conf.low, digits = 3)
year_table$conf.high <- round(year_table$conf.high, digits = 3)

#year_table <- year_table %>% 
  #relocate(functional_group, response.level, year, predicted, conf.low, conf.high)

year_table <- year_table %>% 
  subset(select = -c(std.error, year_num))

year_table <- year_table %>% 
  group_by(functional_group, response.level) %>% 
  arrange(functional_group, response.level) %>% 
  unite(col = "conf-int", conf.low, conf.high, sep = " - ", remove = T)

#write_csv(year_table, "./data-derived/results_tables/07_16_year_table.csv")

######
#elevation tables not needed b/c there we no sig elev effects...

em_elev_shrubtree <- ggemmeans(m_shrub_tree_clm, terms = "elev_m_cen[-2, 0, 2.8]")
em_elev_shrubtree$group <- "shrub/tree"

em_elev_cushion <- ggemmeans(m_cushion_clm, terms = "elev_m_cen[-2, 0, 2.8]")
em_elev_cushion$group <- "cushion"

em_elev_graminoid <- ggemmeans(m_graminoid_clm, terms = "elev_m_cen[-2, 0, 2.8]")
em_elev_graminoid$group <- "graminoid"

em_elev_forb <- ggemmeans(m_forb_clm, terms = "elev_m_cen[-2, 0, 2.8]")
em_elev_forb$group <- "forb"

em_elev_fun_bind <- rbind(em_elev_forb, em_elev_graminoid, em_elev_cushion, em_elev_shrubtree)

em_elev_df <- em_elev_fun_bind %>% 
  rename(elev_m = x,
         functional_group = group)

#add elevation numbers back in, where -2 = dev_low = 2915 m, -0.03 = wds_pgs = 3479 m,  2.8 = wmt = 4299 m 
em_elev_df$elev_m <- as.factor(em_elev_df$elev_m)

em_elev_df$elev_m <- recode_factor(em_elev_df$elev_m,
                                   "-2" = "2915",
                                   "0" = "3479",
                                   "2.8" = "4299")

#recode response levels to reflect abundance categories:
em_elev_df$response.level <- recode_factor(em_elev_df$response.level,
                                           "1" = "r!",
                                           "2" = "r",
                                           "3" = "s",
                                           "4" = "c",
                                           "5" = "d")

elev_table <- em_elev_df

elev_table$predicted <- round(elev_table$predicted, digits = 3)
elev_table$conf.low <- round(elev_table$conf.low, digits = 3)
elev_table$conf.high <- round(elev_table$conf.high, digits = 3)

elev_table <- elev_table %>% 
  relocate(functional_group, elev_m, response.level, predicted, conf.low, conf.high) %>% 
  select(-std.error)

elev_table <- elev_table %>% 
  group_by(functional_group, response.level) %>% 
  arrange(functional_group, response.level) %>% 
  unite(col = "conf-int", conf.low, conf.high, sep = " - ", remove = T)

#write_csv(elev_table, "./data-derived/results_tables/07_16_elev_table.csv")