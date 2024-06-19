#Title: 06_func-gr_gain-loss_binomial_figure4
# Author: Kaleb Goff
# Date: June 2024

##############################################################################################################
#This is an analysis of gains and losses focused on functional groups, to test whether certain functional groups have greater numbers of gains vs. losses.

#This script will subset by functional group, run each functional group through a modified codyn::turnover, to calculate gain and loss, then bind these data frames together and build a logistic regression model for gain, and for loss, using target region, elevation and functional group as predictors.

#Finally, this script creates Figure 4 for the manuscript.

##################################################################################################
# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse", "codyn", "ggeffects", "performance", "marginaleffects", "lme4", "car")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

#############################################################################################
#Data wrangling:
SAS_dat <- read_csv("./data-derived/SAS_clean_data.csv")

# select relevant columns:
SAS_dat <- SAS_dat[,c("target_region", "peak", "year", "aspect", "contour", "species")]

#remove cover classes (solid rock, bare ground, scree, litter, bryophytes, lichen):

cover_class <- c("solid rock", "bare ground", "scree", "litter", "bryophytes", "lichen")

SAS_dat <- SAS_dat %>% 
  filter(!species %in% cover_class)

#filter down to the unique species on a peak in a given year:
df <- SAS_dat %>% 
  group_by(target_region, peak, year) %>% 
  distinct(species)

#unite target region and peak, rename to replicate. This will allow us to preserve the target region column:
df <- df %>% 
  unite(col = "id", target_region, peak, sep = "_", remove = F)

#Bring in functional group list:
#UPDATE: 10/3/2023: created a new functional group sheet, by taking species from the supplemental list and re-assigning functional groups. This serves as another check, and simplifies synonymizing the two lists.
func_gr <- read_csv("./data-derived/functional_group_sheet_V2.csv")

#277 sp in functional list
sp_check <- df %>% 
  ungroup %>% 
  distinct(species)
#277 in SAS data.

#join, match species in gains and losses data frame:
gl_func_df <- left_join(x = df, y = func_gr, by = "species")

#################################################################################################################
#Create a modified turnover function that does not divide by the total richness, and instead yields absolute numbers of gains and losses. Also included a modified version that just calculates total richness of two sequential surveys:

source("./code/05A_modify_codyn_turnover.R", local = knitr::knit_global())

##############################################################################################################
#Set up data for codyn::turnover --

#replicate = id (target region/peak)
#year = year
#species = species names
#abundance = 1 for present

dat_turnover <- gl_func_df %>% 
  rename(replicate = id)

dat_turnover$abundance <- 1
as.numeric(dat_turnover$abundance)

#subset into a data frame for each of the four groups:
dt_forb <- dat_turnover %>% 
  filter(functional_group == "forb")

dt_shrub_tree <- dat_turnover %>% 
  filter(functional_group == "shrub" | functional_group == "tree")

dt_cushion <- dat_turnover %>% 
  filter(functional_group == "cushion")

dt_graminoid <- dat_turnover %>% 
  filter(functional_group == "graminoid")

##############################################################################################################
#calculate turnover for each functional group:

forb_gain <- turnover_MOD(df = dt_forb, 
                          time.var = "year", 
                          species.var = "species", 
                          abundance.var = "abundance", 
                          replicate.var = "replicate",
                          metric = "appearance")

forb_loss <- turnover_MOD(df = dt_forb, 
                          time.var = "year", 
                          species.var = "species", 
                          abundance.var = "abundance", 
                          replicate.var = "replicate",
                          metric = "disappearance")

forb_totrich <- calc_totrich(df = dt_forb, 
                             time.var = "year", 
                             species.var = "species", 
                             abundance.var = "abundance", 
                             replicate.var = "replicate",
                             metric = "total")

forb_totrich <- forb_totrich %>% 
  rename(totrich = total)

forb_gain$metric <-"gain"
names(forb_gain)[1] = "turnover"

forb_loss$metric<-"loss"
names(forb_loss)[1]="turnover"

forb_totrich$metric<-"totrich"
names(forb_totrich)[1]="turnover"

forb_gain_loss <- rbind(forb_gain, forb_loss, forb_totrich)

#error, because grb_pmd only occurs once with one tree.

dt_shrub_tree <- dt_shrub_tree %>% 
  filter(!replicate == "grb_pmd")

shrub_tree_gain <- turnover_MOD(df = dt_shrub_tree, 
                                time.var = "year", 
                                species.var = "species", 
                                abundance.var = "abundance", 
                                replicate.var = "replicate",
                                metric = "appearance")

shrub_tree_loss <- turnover_MOD(df = dt_shrub_tree, 
                                time.var = "year", 
                                species.var = "species", 
                                abundance.var = "abundance", 
                                replicate.var = "replicate",
                                metric = "disappearance")

shrub_tree_totrich <- calc_totrich(df = dt_shrub_tree, 
                                   time.var = "year", 
                                   species.var = "species", 
                                   abundance.var = "abundance", 
                                   replicate.var = "replicate",
                                   metric = "total")

shrub_tree_totrich <- shrub_tree_totrich %>% 
  rename(totrich = total)

shrub_tree_gain$metric <-"gain"
names(shrub_tree_gain)[1] = "turnover"

shrub_tree_loss$metric<-"loss"
names(shrub_tree_loss)[1]="turnover"

shrub_tree_totrich$metric<-"totrich"
names(shrub_tree_totrich)[1]="turnover"

shrub_tree_gain_loss <- rbind(shrub_tree_gain, shrub_tree_loss, shrub_tree_totrich)

graminoid_gain <- turnover_MOD(df = dt_graminoid, 
                               time.var = "year", 
                               species.var = "species", 
                               abundance.var = "abundance", 
                               replicate.var = "replicate",
                               metric = "appearance")

graminoid_loss <- turnover_MOD(df = dt_graminoid, 
                               time.var = "year", 
                               species.var = "species", 
                               abundance.var = "abundance", 
                               replicate.var = "replicate",
                               metric = "disappearance")

graminoid_totrich <- calc_totrich(df = dt_graminoid, 
                                  time.var = "year", 
                                  species.var = "species", 
                                  abundance.var = "abundance", 
                                  replicate.var = "replicate",
                                  metric = "total")

graminoid_totrich <- graminoid_totrich %>% 
  rename(totrich = total)

graminoid_gain$metric <-"gain"
names(graminoid_gain)[1] = "turnover"

graminoid_loss$metric<-"loss"
names(graminoid_loss)[1]="turnover"

graminoid_totrich$metric<-"totrich"
names(graminoid_totrich)[1]="turnover"

graminoid_gain_loss <- rbind(graminoid_gain, graminoid_loss, graminoid_totrich)

#cushion and tree returning error that peaks and years are not replicated. Looks like snd_357 is causing the problem with only 1 cushion obs in 1 year (codyn needs pairs of years).

dt_cushion <- dt_cushion %>% 
  filter(!replicate == "snd_357")

cushion_gain <- turnover_MOD(df = dt_cushion, 
                             time.var = "year", 
                             species.var = "species", 
                             abundance.var = "abundance", 
                             replicate.var = "replicate",
                             metric = "appearance")

cushion_loss <- turnover_MOD(df = dt_cushion, 
                             time.var = "year", 
                             species.var = "species", 
                             abundance.var = "abundance", 
                             replicate.var = "replicate",
                             metric = "disappearance")

cushion_totrich <- calc_totrich(df = dt_cushion, 
                                time.var = "year", 
                                species.var = "species", 
                                abundance.var = "abundance", 
                                replicate.var = "replicate",
                                metric = "total")

cushion_totrich <- cushion_totrich %>% 
  rename(totrich = total)

cushion_gain$metric <-"gain"
names(cushion_gain)[1] = "turnover"

cushion_loss$metric<-"loss"
names(cushion_loss)[1]="turnover"

cushion_totrich$metric<-"totrich"
names(cushion_totrich)[1]="turnover"

cushion_gain_loss <- rbind(cushion_gain, cushion_loss, cushion_totrich)

#############################################################################################
#clean up and prepare turnover data frame for model:

#re-create functional group labels:
cushion_gain_loss$functional_group <- "cushion"
shrub_tree_gain_loss$functional_group <- "shrub/tree"
graminoid_gain_loss$functional_group <- "graminoid"
forb_gain_loss$functional_group <- "forb"

#bind data frames:
fungr_gainloss <- rbind(cushion_gain_loss, graminoid_gain_loss, shrub_tree_gain_loss, forb_gain_loss)

#add target region and elevation
fungr_gainloss <- fungr_gainloss %>% 
  separate(replicate, c("target_region", "peak"))

#join elevation information:
elev_df <- read_csv("./data-raw/plot_elevation_information.csv")

fungr_gainloss <- left_join(fungr_gainloss, elev_df, by = c("target_region", "peak"))

#create a new elevation column with elevation scaled and centered across all peaks.
fungr_gainloss <- fungr_gainloss %>% 
  ungroup() %>% 
  mutate(elev_m_cen = as.vector(scale(elevation_m, center = TRUE, scale = TRUE)))

#clean up data frame: 
fungr_gainloss <- fungr_gainloss %>% 
  dplyr::select(-elevation_m, -elev_rank) %>% 
  mutate(peak = factor(peak)) %>% 
  mutate(target_region = factor(target_region)) %>% 
  relocate(target_region, peak, elev_m_cen)

#pivot so you have gains, losses and total richness as columns:

fungr_gainloss2 <- fungr_gainloss %>% 
  group_by(functional_group, target_region) %>% 
  pivot_wider(names_from = metric,
              values_from = turnover)

###############################################################################################
#Implement models

#Logistic regression for number of gains, and number of losses. Use quasibinomial instead of binomial to estimate the amount of overdispersion in the model (which turns out to be small)

################Gain:

m_gain2 = glm(cbind(gain, totrich - (gain)) ~ functional_group + target_region + elev_m_cen, family = quasibinomial, data = fungr_gainloss2)

summary(m_gain2)

car::Anova(m_gain2, type = 3)
#sig eff of functional group - forbs most gains, shrub/tree least gains
#sig eff of target region - driven by DEV having more gains than other TRs

plot_predictions(m_gain2, condition = ("target_region"))
plot_predictions(m_gain2, condition = ("functional_group"))

#plot(m_gain2)

#############Loss:

m_loss2 = glm(cbind(loss, totrich - (loss)) ~ functional_group + target_region + elev_m_cen, family = quasibinomial, data = fungr_gainloss2)

summary(m_loss2)

car::Anova(m_loss2, type = 3)
#sig eff of functional group - driven by shrub/tree having few losses

plot_predictions(m_loss2, condition = ("functional_group"))

#plot(m_loss2)

############################################################################
#Create a plot with the turnover of each functional group:

theme_set(theme_bw(base_size = 14))

ge_gain <- ggemmeans(m_gain2, terms = c("functional_group"))

ge_loss <- ggemmeans(m_loss2, terms = c("functional_group"))

#clean it up for plotting:

#rename:
ge_gain <- ge_gain %>% 
  rename(functional_group = x,
         turnover = predicted,
         metric = group)

ge_gain$metric <- "gain"

ge_loss <- ge_loss %>% 
  rename(functional_group = x,
         turnover = predicted,
         metric = group)

ge_loss$metric <- "loss"

ge_gainloss <- rbind(ge_gain, ge_loss)

#need to transform the columns in the df to "upper" and "lower"

ge_gainloss2 <- ge_gainloss

#make a column called lower with predicted minus conf, make a column called upper with predicted plus conf for each row:

ge_gainloss2 <- ge_gainloss2 %>% 
  rowwise() %>% 
  mutate(lower = turnover - std.error) %>% 
  mutate(upper = turnover + std.error)

theme_set(theme_bw(base_size = 12))

pal <- c("gain" = "#b4b4b4",
         "loss" = "#333333")

p88 <- ggplot(data = ge_gainloss2, aes(x = reorder(functional_group, turnover), y = turnover, fill = metric)) +
  geom_bar(stat="identity", position = position_dodge(width = 1)) +
  geom_errorbar(data = ge_gainloss2, aes(ymin = conf.low, ymax = conf.high), linewidth = 0.4, width = 0.4,  position = position_dodge(width = 1)) +
  xlab("Functional Group") +
  ylab("Proportional Gain or Loss") +
  ylim(0.00, 0.23) +
  theme(text = element_text(size = 12, face = "bold"), legend.position = "top", legend.title = element_blank()) +
  scale_fill_manual(values = pal)

p88

ggsave(filename = "functional-group_gainloss_06_09_grey.jpeg",
       path = "./figures/",
       plot = p88,
       units = "in",
       dpi = 200,
       width = 7, height = 6)
