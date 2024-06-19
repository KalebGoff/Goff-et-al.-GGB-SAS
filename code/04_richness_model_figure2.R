# Title: 04_richness_model_figure2
# Author: Kaleb Goff
# Date: June 2024

##############################################################################################################
#The goal of this script is to create linear models for species richness change over time at the peak scale.
#1. set up data, elevation is scaled and centered across all peaks
#2. build species richness model
#3. Create Figure 2 for manuscript

#######################################################################################################
#load packages:

packages_needed <- c("tidyverse", "lme4", "car", "performance", "ggrepel", "marginaleffects")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

######################################################################################################
#Set up data for model:

#import data:
SAS_dat <- read_csv("./data-derived/SAS_clean_data.csv")

# select relevant columns:
SAS_dat <- SAS_dat[,c("target_region", "peak", "year", "aspect", "contour", "species")]

#remove cover classes (solid rock, bare ground, scree, litter, bryophytes, lichen):

cover_class <- c("solid rock", "bare ground", "scree", "litter", "bryophytes", "lichen")

SAS_dat <- SAS_dat %>% 
  filter(!species %in% cover_class)

#inspect species list:
table(SAS_dat$species)

#calculate species richness for each peak in each year (i.e. treating the entire peak as a plot): 
df <- SAS_dat %>% 
  group_by(target_region, peak, year) %>% 
  summarise(richness = n_distinct(species))

#Set up year_number column:
year <- c(2004:2022)
year_num <- c(1:19)
year.df <- data.frame(year, year_num)

rm(year)
rm(year_num)

df2 <- left_join(df, year.df, by = "year")

#bring in elevation information:
elev_df <- read_csv("./data-raw/plot_elevation_information.csv")

df3 <- left_join(df2, elev_df, by = c("target_region", "peak"))

df3_elev <- left_join(df2, elev_df, by = c("target_region", "peak"))

#create a new elevation column with elevation in meters centered and scaled across all peaks and target regions
df3 <- df3 %>% 
  ungroup() %>% 
  mutate(elev_m_cen = as.vector(scale(elevation_m, center = TRUE, scale = TRUE)))

#create final data sheet: be certain that factor variable are the correct data class, rename, remove elev_m (keep elev_rank for figure):
dat <- df3 %>% 
  ungroup() %>% 
  mutate(peak = factor(peak)) %>% 
  mutate(target_region = factor(target_region)) %>%
  dplyr::select(-elevation_m)

dat_elev <- df3_elev %>% 
  ungroup() %>% 
  mutate(peak = factor(peak)) %>% 
  mutate(target_region = factor(target_region))

#calculate mean number of years between surveys for all peaks:

dat2 <- dat %>% 
  group_by(target_region, peak)

dat2 <- dat2 %>% 
  summarise(mean(diff(year)))

dat3 <- dat2 %>% 
  ungroup() %>% 
  summarise(mean(`mean(diff(year))`))

#5.712644 years on average between surveys

##################################################################################################
#try poisson, check overdispersion:

m1 <- glmer(richness ~ year_num * elev_m_cen + (1 | target_region),
          family = poisson(link = "log"),
          data = dat)

check_overdispersion(m1) #overdispersion detected

#species richness model with over dispersed counts:

m5 <- glmer.nb(richness ~ year_num * elev_m_cen + (1 |target_region),
             data = dat)

#assess model fit:
summary(m5) #sig eff of elevation but not interaction
performance(m5)
plot(m5)

#assess model adequacy:
r2_nakagawa(m5)
#conditional 0.634
#marginal 0.387

#interpret:
car::Anova(m5, type = 3) #sig effect of elevation

#interpret elevation effect:
#elev coeff -0.3766143, but is scaled/centered. for every one unit increase in elevation, you see (exp(-0.3766)) = 0.686 fewer species?? But that doesn't tell us much. We'd rather know X species per 100m. To do that we need to unscale (divide by sd) and uncenter (subtract mean from every point).

# Calculate the standard deviation of the original elevation variable
sd_elevation <- sd(elev_df$elevation_m, na.rm = TRUE)

# Model estimate for the scaled and centered elevation variable
estimate_scaled <- -0.3766129

# Convert the estimate to the original scale (per 100 meters)
estimate_per_100m <- estimate_scaled * (100 / sd_elevation)

# Print the result
estimate_per_100m #-0.10754, or per 1000m is -1.07 species less.

#I tried a bunch of stuff and none of it made sense to me or worked to get an easy interpretation.

#extract random intercepts for target region:
tr_ran <- ranef(m5)[[1]]
tr_ran$target_region <- row.names(tr_ran)
tr_ran <- tr_ran %>% 
  rename(intercept = "(Intercept)")

#0.0086793 is the overall year slope

tr_ran$slope <- 0.0086793

plot_predictions(m5, condition = c("year_num", "target_region")) +
  facet_wrap(~target_region)

test <- predictions(m5) 

##########################################################################################
#CONDITIONAL PREDICTIONS: we are predicting richness values based on year and tr.

#recreate figure 2 using the slope from the model, and the random intercepts from target regions.

theme_set(theme_bw(base_size = 14))

pal_elev <- c("1" = "red",
              "2" = "orange",
              "3" = "yellow",
              "4" = "blue")

pal_elev <- c("1" = "#333333",
              "2" = "#737373",
              "3" = "#b4b4b4",
              "4" = "white")

pal_fill <- c("1" = "black",
              "2" = "black",
              "3" = "black",
              "4" = "black")

#calculate average elevation for ranks 1 - 4:
d_e1 <- df3 %>% 
  filter(elev_rank == 1)

mean(d_e1$elevation_m) #rank 1 = 3264m

d_e2 <- df3 %>% 
  filter(elev_rank == 2)

mean(d_e2$elevation_m) #rank 2 = 3474m

d_e3 <- df3 %>% 
  filter(elev_rank == 3)

mean(d_e3$elevation_m) #rank 3 = 3633m

d_e4 <- df3 %>% 
  filter(elev_rank == 4)

mean(d_e4$elevation_m) #rank 4 = 3960m

#Model predictions:
test <- plot_predictions(m5, condition = c("year_num", "target_region"), draw = F, vcov = F)

test$year_num <- round(test$year_num)

pl_sr2 <- left_join(test, year.df)

#now the years just need to be clipped for each TR:

#cat: 2006, 2011, 2022
pl_sr2_cat <- pl_sr2 %>% 
  filter(target_region == "cat") %>% 
  filter(year == "2006" | year == "2011" | year == "2022")

#dev: 2013 | 2018
pl_sr2_dev <- pl_sr2 %>% 
  filter(target_region == "dev") %>% 
  filter(year == "2013" | year == "2018")

#grb: 2008 | 2013 | 2018
pl_sr2_grb <- pl_sr2 %>% 
  filter(target_region == "grb") %>% 
  filter(year == "2008" | year == "2013" | year == "2018")

#lan: 2010 | 2015 | 2021
pl_sr2_lan <- pl_sr2 %>% 
  filter(target_region == "lan") %>% 
  filter(year == "2010" | year == "2015" | year == "2021")

#snd: 2004, 2009, 2014, 2019
pl_sr2_snd <- pl_sr2 %>% 
  filter(target_region == "snd") %>% 
  filter(year == "2004" | year == "2009" | year == "2014" | year == "2019")

#swe: 2012 | 2017
pl_sr2_swe <- pl_sr2 %>% 
  filter(target_region == "swe") %>% 
  filter(year == "2012" | year == "2017")

#wds: 2005, 2010, 2021
pl_sr2_wds <- pl_sr2 %>% 
  filter(target_region == "wds") %>% 
  filter(year == "2005" | year == "2010" | year == "2021")

#wim:
pl_sr2_wim <- pl_sr2 %>% 
  filter(target_region == "wim") %>% 
  filter(year == "2004" | year == "2009" | year == "2014" | year == "2019" | year == "2021")

#bind together:
pl_sr3 <- rbind(pl_sr2_cat, pl_sr2_dev, pl_sr2_grb, pl_sr2_lan, pl_sr2_snd, pl_sr2_swe, pl_sr2_wds, pl_sr2_wim)

dat$elev_rank <- as.character(dat$elev_rank)

#use symbol # 24, then you can control interior with fill and outline with color aes. Then swap legend to these filled/colored symbols.

# New facet label names for supp variable
tr.labs <- c("Carson-Tahoe", "Death Valley NP", "Great Basin NP", "Mount Langley", "Sierra Nevada Dunderberg", "Sweetwater Mountains", "White Mountain Dolostone", "White Mountain Silicates")
names(tr.labs) <- c("cat", "dev", "grb", "lan", "snd", "swe", "wds", "wim")

p2 <- ggplot() +
  geom_line(data = pl_sr3, aes(x = year, y = estimate, color = target_region), linewidth = 1.3, linetype = "dashed", show.legend = F) +
  geom_point(data = dat, aes(x = year, y = richness, color = elev_rank, fill = elev_rank), size = 3.3, alpha = 0.7, pch = 24, show.legend = T) +
  facet_wrap(~target_region, nrow = 2, labeller = labeller(target_region = tr.labs)) +
  xlab("Survey Year") +
  ylab("Species richness") +
  scale_x_continuous(breaks= c(2004, 2009, 2014, 2019, 2022)) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 12, face = "bold"), legend.position = "top") +
  scale_color_manual(name = "elevation rank (1 = lowest, 4 = highest)",
                     breaks = c("1", "2", "3", "4"),
                     values = pal_fill) +
  scale_fill_manual(name = "elevation rank (1 = lowest, 4 = highest)",
                    breaks = c("1", "2", "3", "4"),
                    values = pal_elev) + 
  scale_shape_manual(name = "elevation rank (1 = lowest, 4 = highest)",
                     breaks = c("1", "2", "3", "4"),
                     values = c(21:24))

p2

ggsave(filename = "richness_model_pred_04_10_greyscale.jpg",
       path = "./figures/",
       plot = p2,
       units = "in",
       dpi = 200,
       width = 8.5, height = 5)
