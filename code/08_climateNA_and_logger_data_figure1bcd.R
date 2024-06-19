# Title: 08_10_climateNA_and_logger_data_fig1bcd
# Author: Kaleb Goff
# Date: June 2024

##############################################################################################################
#in this script:

#1 calculate the rate of change for mat and map across all summits using 1980 - 2022.
#2 create figure 1 panels b (mat) and c (map), show present and historical period
#3 create a model for soil temp change over time from the logger data, make figure 1d, showing logger data over time for each tr.

################################################################################################################
# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

#make vector of packages needed

packages_needed <- c("tidyverse", "ggrepel", "ggpubr", "lme4", "performance", "broom", "data.table", "ggeffects", "lubridate", "timetk")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

#################################################################################################
#load ClimateNA data:
ggb_climNA <- read_csv("./data-derived/ggb_geo_elev_forClimNA_1901-2022MSY.csv")

#split out target_region

ggb_climNA <- ggb_climNA %>% 
  separate(col = id2, into = c("summit", "target_region"), sep = "_")

#change to Wheeler Peak - swe and Wheeler Peak - grb:

wheeler_grb <- ggb_climNA %>% 
  filter((target_region =='GRB' & summit =='Wheeler Peak'))

ggb_climNA <- ggb_climNA %>% 
  filter(!(target_region =='GRB' & summit =='Wheeler Peak'))

wheeler_grb$summit[wheeler_grb$summit == "Wheeler Peak"] <- "Wheeler Peak grb"

ggb_climNA <- ggb_climNA %>% 
  bind_rows(wheeler_grb)

#swe

wheeler_swe <- ggb_climNA %>% 
  filter((target_region =='SWE' & summit =='Wheeler Peak'))

ggb_climNA <- ggb_climNA %>% 
  filter(!(target_region =='SWE' & summit =='Wheeler Peak'))

wheeler_swe$summit[wheeler_swe$summit == "Wheeler Peak"] <- "Wheeler Peak swe"

ggb_climNA <- ggb_climNA %>% 
  bind_rows(wheeler_swe)

#############################################################
#create basic figures of relevant climate variables, use a longer list of annual variables this time:
#See help2 on ClimateNA page for variable descriptions

#chop date range to 1980-2022:

ggb_climNA <- ggb_climNA %>% 
  filter(Year > 1979)

#use mixed effects model for climatena data, with overall slope for year and group level random intercepts for peak nested within target region.

#MAT
m1 <- lmer(MAT ~ Year + (1 | summit) + (1 | target_region),
          data = ggb_climNA)

summary(m1)
car::Anova(m1, type = "3")
r2(m1)

#sig eff of year, 0.0465 C increase per year
#conditonal r2 0.962 /  marginal 0.041

#MAP
m2 <- lmer(MAP ~ Year + (1 | summit) + (1 | target_region),
           data = ggb_climNA)

summary(m2)
car::Anova(m2, type = "3")
r2(m2)

#sig effect of precip, decrease of 1.171 mm/year
#conditonal r2 0.583 /  marginal 0.002

df_mat <- ggb_climNA %>% 
  filter(Year >= "2004") %>% 
  select(summit, MAT, Year, target_region)

df_map <- ggb_climNA %>% 
  filter(Year >= "2004") %>% 
  select(summit, MAP, Year, target_region)

#descriptive stats:
dev_temp <- df_mat %>% 
  filter(target_region == "DEV")

dev_precip <- df_map %>% 
  filter(target_region == "DEV")

wim_temp <- df_mat %>% 
  filter(target_region == "WIM")

cat_precip <- df_map %>% 
  filter(target_region == "CAT")

#######################################################################################
#create lines for individual peaks within TRs:

ggb_clim_sum <- ggb_climNA %>% 
  group_by(target_region, Year) %>% 
  mutate(tr_map = mean(MAP)) %>% 
  mutate(tr_mat = mean(MAT))

#Create panels for Fig 1:
pal <- c("CAT" = "#999999", 
         "SND" = "#E69F00", 
         "SWE" = "#56B4E9", 
         "GRB" = "#009E73", 
         "LAN" = "#F0E442",
         "WIM" = "#0072B2", 
         "WDS" = "#D55E00", 
         "DEV" = "#CC79A7")

theme_set(theme_classic(base_size = 30))

mat_lo <- ggplot(data = ggb_climNA, aes(x = Year, y = MAT, color = target_region)) +
  geom_smooth(se = F, size = 3.5, method = "loess") +
  geom_line(data = ggb_clim_sum, aes(x = Year, y = tr_mat, color = target_region), alpha = 0.4, size = 1.5) +
  scale_color_manual(values= pal) +
  ylab("Mean Annual Air Temperature (°C)")+
  theme(legend.position = "none") +
  labs(color = "Target Region") +
  annotate("rect", xmin = 1980, xmax = 2003, ymin = -Inf, ymax = Inf,
           alpha = .3) +
  scale_y_continuous(breaks= c(-3, 0, 3, 6, 10), limits = c(-4, 11))

mat_lo

map_lo <- ggplot(data = ggb_climNA, aes(x = Year, y = MAP, color = target_region)) +
  geom_smooth(se = F, size = 3.5, method = "loess") +
  geom_line(data = ggb_clim_sum, aes(x = Year, y = tr_map, color = target_region), alpha = 0.4, size = 1.5) +
  scale_color_manual(values= pal) +
  ylab("Mean Annual Precipitation (mm)") +
  theme(legend.position = "none") +
  labs(color = "Target Region") +
  annotate("rect", xmin = 1980, xmax = 2003, ymin = -Inf, ymax = Inf,
           alpha = .3)

map_lo

#export MAT, MAP for a try an an updated Fig 1:

ggsave(filename = "05_fig1_MAP_1980.jpg",
       path = "./figures/",
       plot = map_lo,
       units = "in",
       width = 8.5, height = 8.5)

ggsave(filename = "04_fig1_MAT_1980.jpg",
       path = "./figures/",
       plot = mat_lo,
       units = "in",
       width = 8.5, height = 8.5)

#############################################################################
#Soil temperature logger data analysis and figure:

#NOTE: LOGGER DATA IS A VERY LARGE FILE, so it is not included on github. It will be included in the final data repository.

#bring in cleaned logger data:
us_merged_cleaned <- fread("./data-derived/02_us_merged_cleaned.csv")

#clip 2023 data:
us_merged_cleaned <- us_merged_cleaned  %>% 
  filter(!year == "2023")

temp <- us_merged_cleaned %>% 
  mutate(temp_c = ((Temp_F - 32) * 5/9)) %>% 
  select(-Temp_F, temp_diff)

temp_dt <- us_merged_cleaned %>% 
  mutate(temp_c = ((Temp_F - 32) * 5/9)) %>% 
  select(-Temp_F, temp_diff)

##############################

temp[, hours_elapsed := difftime(time1 = date_time, time2 = min(date_time), units = "hours")]

fm1 = lmer(temp_c ~ hours_elapsed + (1 | peak) + (1 | target_region), data = temp)

summary(fm1)

total_hours_elapsed = difftime(
  time1 = max(temp$date_time),
  time2 = min(temp$date_time),
  units = "hours"
) |>
  as.numeric()

# temperature change across the record
as.numeric(summary(fm1)$coefficients[, "Estimate"]["hours_elapsed"]) * 
  total_hours_elapsed

#change to C, you get -0.6730678 C across the study period.

car::Anova(fm1, type = 3)

coef(fm1)

r2(fm1)

###################################################################
#compute predictions for figure:

pred <- ggpredict(fm1, terms = c("hours_elapsed", "target_region"), type = "re", ci.lvl = NA)

temp_origin <- min(temp$date_time)

pred2 <- pred %>% 
  rename(hours_elapsed = x,
         target_region = group)

pred2$hours_elapsed <- as.numeric(pred2$hours_elapsed)

pred2$hours_elapsed <- as_datetime(x = 3600*(pred2$hours_elapsed), origin = temp_origin)

#I had to multiply the hours_elapsed column by the number of seconds in an hour so that lubridate::as_datetime would count up by hours and not by seconds!

#make 8 predictions data frames one for each target region, then chop each one to the observed data for each tr, then have 8 different geom_line calls.

#cat
cat_tp <- pred2 %>% 
  filter(target_region == "cat")

cat_ref <- temp %>% 
  filter(target_region == "cat")

min_cat <- min(cat_ref$date_time)
max_cat <- max(cat_ref$date_time)

cat_plot <- cat_tp %>% filter(between(x = hours_elapsed, lower = min_cat, upper = max_cat))

#dev
dev_tp <- pred2 %>% 
  filter(target_region == "dev")

dev_ref <- temp %>% 
  filter(target_region == "dev")

min_dev <- min(dev_ref$date_time)
max_dev <- max(dev_ref$date_time)

dev_plot <- dev_tp %>% filter(between(x = hours_elapsed, lower = min_dev, upper = max_dev))

#grb
grb_tp <- pred2 %>% 
  filter(target_region == "grb")

grb_ref <- temp %>% 
  filter(target_region == "grb")

min_grb <- min(grb_ref$date_time)
max_grb <- max(grb_ref$date_time)

grb_plot <- grb_tp %>% filter(between(x = hours_elapsed, lower = min_grb, upper = max_grb))

#lan
lan_tp <- pred2 %>% 
  filter(target_region == "lan")

lan_ref <- temp %>% 
  filter(target_region == "lan")

min_lan <- min(lan_ref$date_time)
max_lan <- max(lan_ref$date_time)

lan_plot <- lan_tp %>% filter(between(x = hours_elapsed, lower = min_lan, upper = max_lan))

#snd
snd_tp <- pred2 %>% 
  filter(target_region == "snd")

snd_ref <- temp %>% 
  filter(target_region == "snd")

min_snd <- min(snd_ref$date_time)
max_snd <- max(snd_ref$date_time)

snd_plot <- snd_tp %>% filter(between(x = hours_elapsed, lower = min_snd, upper = max_snd))

#swe
swe_tp <- pred2 %>% 
  filter(target_region == "swe")

swe_ref <- temp %>% 
  filter(target_region == "swe")

min_swe <- min(swe_ref$date_time)
max_swe <- max(swe_ref$date_time)

swe_plot <- swe_tp %>% filter(between(x = hours_elapsed, lower = min_swe, upper = max_swe))

#wds
wds_tp <- pred2 %>% 
  filter(target_region == "wds")

wds_ref <- temp %>% 
  filter(target_region == "wds")

min_wds <- min(wds_ref$date_time)
max_wds <- max(wds_ref$date_time)

wds_plot <- wds_tp %>% filter(between(x = hours_elapsed, lower = min_wds, upper = max_wds))

#wim
wim_tp <- pred2 %>% 
  filter(target_region == "wim")

wim_ref <- temp %>% 
  filter(target_region == "wim")

min_wim <- min(wim_ref$date_time)
max_wim <- max(wim_ref$date_time)

wim_plot <- wim_tp %>% filter(between(x = hours_elapsed, lower = min_wim, upper = max_wim))

# Calculate the monthly average temperature for each peak and year
temp_month_tr <- temp %>%
  group_by(year, target_region) %>%
  summarize(avg_temp = mean(temp_c))

as.Date.POSIXct(temp_month_tr$year, origin = "2009-01-01 00:00:00", tz = "UTC", "%y-%m-%d")

temp_month_tr <- temp %>%
  group_by(target_region) %>%
  summarise_by_time(.date_var = date_time,
                    .by       = "year",
                    value  = mean(temp_c))

#plot it!

pal <- c("cat" = "#999999", 
         "snd" = "#E69F00", 
         "swe" = "#56B4E9", 
         "grb" = "#009E73", 
         "lan" = "#F0E442",
         "wim" = "#0072B2", 
         "wds" = "#D55E00", 
         "dev" = "#CC79A7")

theme_set(theme_classic(base_size = 30))

logger_temp <- ggplot() +
  geom_line(data = temp_month_tr, aes(x = date_time, y = value, color = target_region), alpha = 0.5, linewidth = 1.7) +
  geom_line(data = cat_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = dev_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = grb_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = lan_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = snd_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = swe_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = wds_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  geom_line(data = wim_plot, aes(x = hours_elapsed, y = predicted, color = target_region), linewidth = 2.1) +
  scale_color_manual(values= pal) +
  ylab("Mean Annual Soil Temperature (°C)")+
  theme(legend.position = "none") +
  xlab("Year") +
  scale_y_continuous(breaks= c(-3, 0, 3, 6, 10), limits = c(-4, 11)) +
  scale_x_datetime(breaks= seq(min(min_wim), max(max_cat), length=5), 
                   date_labels="%Y")

logger_temp

#save the updated Figure 1b:
ggsave(filename = "06_fig1_loggerdata.jpg",
       path = "./figures/",
       plot = logger_temp,
       units = "in",
       width = 8.5, height = 8.5)
