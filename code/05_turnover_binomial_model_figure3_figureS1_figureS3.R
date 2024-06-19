# Title: 05_turnover_binomial_model_figure3_figureS1_figureS3
# Author: Kaleb Goff
# Date: June 2024

##############################################################################################################
#This script is used for the analysis of turnover, using peaks as plots, and modifying the codyn::turnover function to output the raw number of gains, losses and total richness for each sequential pair of peaks.

#We then use a logistic regression approach to model turnover.

#creates figure 3, figure S1 and figure S3

##################################################################################################
# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse", "codyn", "marginaleffects", "ggpubr", "ggeffects", "lme4", "car", "performance", "MASS")

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

#import data:
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

#################################################################################################################
#Create a modified turnover function that does not divide by the total richness, and instead yields absolute numbers of gains and losses. Also included a modified version that just calculates total richness of two sequential surveys:

source("./code/05A_modify_codyn_turnover.R", local = knitr::knit_global())

##############################################################################################################
#Set up data for codyn::turnover --

#replicate = id (target region/peak)
#year = year
#species = species names
#abundance = 1 for present

dat_turnover <- df %>% 
  rename(replicate = id)

dat_turnover$abundance <- 1
as.numeric(dat_turnover$abundance)

#######################################################################
#Calculate gains, losses:

ggb_turnover_gain <- turnover_MOD(df = dat_turnover, 
                                  time.var = "year", 
                                  species.var = "species", 
                                  abundance.var = "abundance", 
                                  replicate.var = "replicate",
                                  metric = "appearance")

ggb_turnover_loss <- turnover_MOD(df = dat_turnover, 
                                  time.var = "year", 
                                  species.var = "species", 
                                  abundance.var = "abundance", 
                                  replicate.var = "replicate",
                                  metric = "disappearance")

totrich <- calc_totrich(df = dat_turnover, 
                        time.var = "year", 
                        species.var = "species", 
                        abundance.var = "abundance", 
                        replicate.var = "replicate",
                        metric = "total")

totrich <- totrich %>% 
  rename(totrich = total)

#join to one data frame and organize:
ggb_turnover <- left_join(ggb_turnover_gain, ggb_turnover_loss)
ggb_turnover <- left_join(ggb_turnover, totrich)

ggb_turnover <- ggb_turnover %>% 
  separate(replicate, c("target_region", "peak"))

#join elevation and year information:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

ggb_gainloss_yr <- left_join(ggb_turnover, year.df, by = "year")

elev_df <- read_csv("./data-raw/plot_elevation_information.csv")

ggb_gainloss_yr_elev <- left_join(ggb_gainloss_yr, elev_df, by = c("target_region", "peak"))

#create a new elevation column with elevation scaled and centered across all peaks.
ggb_gainloss_yr_elev <- ggb_gainloss_yr_elev %>% 
  ungroup() %>% 
  mutate(elev_m_cen = as.vector(scale(elevation_m, center = TRUE, scale = TRUE)))

#clean up data frame: 
dat_turnover <- ggb_gainloss_yr_elev %>% 
  dplyr::select(-elevation_m, -elev_rank) %>% 
  mutate(peak = factor(peak)) %>% 
  mutate(target_region = factor(target_region)) %>% 
  relocate(target_region, peak, year, year_num, elev_m_cen, appearance, disappearance, totrich) %>% 
  rename(gain = appearance,
         loss = disappearance)

##################################################################################
#Logistic regression for turnover. Use quasibinomial instead of binomial to estimate the amount of overdispersion in the model (which turns out to be small)

fm2 = glm(cbind(gain + loss, totrich - (gain + loss)) ~ elev_m_cen * target_region, family = quasibinomial, data = dat_turnover)

summary(fm2)

car::Anova(fm2, type = 3)
#only sig effect is overall model intercept.

#plot(fm2)
plot_predictions(fm2, condition = c("target_region"))
###################################################################################################
#Create a new figure 3:

#add column for proportional turnover (gain + loss)/total richness in the observed data:
dat_turnover$gl <- (dat_turnover$gain + dat_turnover$loss)/dat_turnover$totrich

theme_set(theme_bw(base_size = 16))

pl_turn_tr <- ggemmeans(fm2, terms = c("target_region"))
plot(pl_turn_tr, rawdata = T)

#rename:
pl_turn2 <- pl_turn_tr %>% 
  rename(target_region = x,
         turnover = predicted)

mean(pl_turn2$turnover)
#0.1870464 model predicted average turnover

# New facet label names for target_region
tr.labs <- c("Carson-Tahoe", "Death Valley NP", "Great Basin NP", "Mount Langley", "Sierra Nevada Dunderberg", "Sweetwater Mountains", "White Mountains Dolostone", "White Mountains Silicates")
names(tr.labs) <- c("cat", "dev", "grb", "lan", "snd", "swe", "wds", "wim")

pal <- c("cat" = "#999999", 
         "snd" = "#E69F00", 
         "swe" = "#56B4E9", 
         "grb" = "#009E73", 
         "lan" = "#F0E442",
         "wim" = "#0072B2", 
         "wds" = "#D55E00", 
         "dev" = "#CC79A7")


p4 <- ggplot(data = pl_turn2, aes(x = reorder(target_region, turnover), y = turnover, color = target_region, fill = target_region)) +
  geom_point(data = pl_turn2, aes(x = reorder(target_region, turnover), y = turnover, color = target_region, fill = target_region), show.legend = F, size = 4, inherit.aes = F) +
  geom_linerange(data = pl_turn2, aes(ymin = conf.low, ymax = conf.high), linewidth = 1) +
  geom_jitter(data = dat_turnover, aes(x = target_region, y = gl), show.legend = F, size = 1.5, alpha = 0.5, inherit.aes = F, width = 0.07) +
  xlab("Target Region") +
  ylab("Turnover Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_x_discrete(labels= tr.labs) + 
  geom_text(aes(label = tr.labs), angle = 90, vjust = 2, size = 6)  + 
  theme(axis.text.x=element_blank())

p4

ggsave(filename = "global_turnover_05_13_proportion.jpg",
       path = "./figures/",
       plot = p4,
       units = "in",
       dpi = 200,
       width = 8, height = 6)

########################################################################################################
#Figure S1: compute gain/loss for each peak in each target region:

theme_set(theme_bw(base_size = 15))

dat_turnover2 <- dat_turnover

#add column for proportional gain/loss (gain + loss)/total richness in the observed data:
dat_turnover2$gain <- (dat_turnover2$gain)/dat_turnover2$totrich
dat_turnover2$loss <- (dat_turnover2$loss)/dat_turnover2$totrich

#remove points that will fall directly on the 1:1 line (i.e. they have gain = loss)
dat_turnover2 <- dat_turnover2[dat_turnover2$gain != dat_turnover2$loss,]

#set up target region labels:
dat_turnover2$target_region <- factor(dat_turnover2$target_region, levels = c("cat", "dev", "grb", "lan", "snd", "swe", "wds", "wim"), labels = c("Carson-Tahoe", "Death Valley NP", "Great Basin NP", "Mount Langley", "Sierra Nevada Dunderberg", "Sweetwater Mountains", "White Mountains Dolostone", "White Mountains Silicates"))

s4 <- ggplot(data = dat_turnover2, aes(x = gain, y = loss, color = elev_m_cen)) +
  geom_point(size = 2.2) +
  geom_abline(intercept = 0, slope = 1, linewidth = 0.9, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 0.355), breaks = seq(0, 0.355, 0.1)) +
  scale_x_continuous(limits = c(0, 0.355), breaks = seq(0, 0.355, 0.1)) +
  facet_wrap(~ target_region, nrow = 2, labeller = labeller(target_region = label_wrap_gen(20))) +
  xlab("Proportional Gain") +
  ylab("Proportional Loss") +
  theme(legend.position = "top", text = element_text(size = 11, face = "bold")) +
  labs(color= "relative elevation") +
  scale_colour_gradient2(
    low = "orange",
    mid = "forestgreen",
    high = "blue",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

s4

ggsave(filename = "one-to-one_TR_gain_loss_05_14_.jpg",
  path = "./figures/",
  plot = s4,
  units = "in",
  dpi = 200,
  width = 7, height = 5)

####################################################################################################################
#create a new figure 3 including the results from the null turnover computations:

#the components of this figure are:

#A. observed turnover:
#observed mean turnover across all target regions as a dashed line (hline)
#colored dots showing mean obs turnover for each TR
#small jittered dots showing observed turnover for pairs of surveys within each target region

#B. Null turnover:
#grey transparent box showing 95% CI for null model across all target regions (rectangle annotation with ymin/ymax)
#black crossbars showing 95% ci for null turnover of each tr

#prepare null turnover data for plotting:
turnover_null <- read_csv("./data-derived/null_turnover.csv")

#add target region, compute 95% ci (2.5 and 97.5 quantiles), rename turnover = total.
guide <- dat_turnover[,1:2]
guide <- unique(guide)

turnover_null <- left_join(turnover_null, guide)

#calculate mean and 95% ci for null model by target region (vertical elements)
turnover_null2 <- turnover_null %>% 
  rename(tn = total) %>% 
  group_by(target_region) %>% 
  summarise_at(vars(tn),
               list(mean = mean, qlow = ~quantile(., probs = 0.025),
                    qhigh = ~quantile(., probs = 0.975)))

turnover_null3 <- turnover_null2 %>% 
  rename(turnover = mean)

#calculate mean and 95% ci for null model (horizontal element)
null_ci <- turnover_null %>% 
  rename(tn = total) %>%
  summarise_at(vars(tn),
               list(mean = mean, qlow = ~quantile(., probs = 0.025),
                    qhigh = ~quantile(., probs = 0.975)))

#mean: 0.29, lower ci: 0.129 ; upper ci: 0.588

#code this so that all raw (i.e. NOT model predicted data) is shown on this plot:
turnover_obs <- dat_turnover %>% 
  rename(ob_tn = gl) %>% 
  group_by(target_region) %>% 
  summarise_at(vars(ob_tn),
               list(obmean = mean, ob_qlow = ~quantile(., probs = 0.025),
                    ob_qhigh = ~quantile(., probs = 0.975)))


turnover_obs2 <- turnover_obs %>% 
  rename(turnover = obmean)

mean(turnover_obs2$turnover)
#0.2169242 at the target region scale

mean(dat_turnover$gl)
#0.2071914 at the peak scale

theme_set(theme_bw(base_size = 15))

#plot:
ps2 <- ggplot(data = turnover_obs2, aes(x = reorder(target_region, turnover), y = turnover, color = target_region, fill = target_region)) +
  
  #raw observed data (turnover estimates for sequential surveys)
  geom_jitter(data = dat_turnover, aes(x = reorder(target_region, gl), y = gl), show.legend = F, size = 1.5, alpha = 0.5, inherit.aes = F, width = 0.07) +
  
  #observed mean turnover for each target region
  geom_point(data = turnover_obs2, aes(x = reorder(target_region, turnover), y = turnover, color = target_region, fill = target_region), show.legend = F, size = 4, inherit.aes = F) +
  
  #null 95% ci for each target region
  geom_errorbar(data = turnover_null3, aes(ymin = qlow, ymax = qhigh), color = "black", width = 0.4) +
  
  #mean observed turnover across all target regions
  geom_hline(yintercept = 0.2169242, linetype = 2) +
  
  #null turnover model 95% ci
  annotate("rect", ymin = 0.1290323, ymax = 0.5882353, xmin = -Inf, xmax = Inf, alpha = .3,fill = "grey") +
  
  #labels, etc...
  xlab("Target Region") +
  ylab("Turnover") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 16, face = "bold"), legend.position = "none") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_x_discrete(labels= tr.labs) + 
  geom_text(aes(label = tr.labs), angle = 90, vjust = 2, size = 6)  + 
  theme(axis.text.x=element_blank())

ps2

ggsave(filename = "05_13_figure-S1_null_turnover.jpg",
       path = "./figures/",
       plot = ps2,
       units = "in",
       dpi = 200,
       width = 8, height = 6)