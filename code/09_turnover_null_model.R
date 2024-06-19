# Title: 11_01_turnover_null_model
# Author: Kaleb Goff & Seema Sheth
# Date: January 2024
# Last modified: 20240201

##############################################################################################################
#This script is used to create a null turnover model to use as a basis for comparison for the turnover model created in script 05_.

#To do this, we need to:
#1 Create the neutral species pool - a list of all unique species observed across all surveys on a given peak.
#2 For each survey of each peak, randomly sample from the species pool on each peak without replacement until you reach the richness of a given survey year. (set the number of draws equal to richness of a given survey).
#3 Repeat steps 1 & 2 for all 29 peaks sorted into target regions.
#4 Take those randomly sampled lists and feed them into codyn::turnover to calculate turnover.

#5 Iterate steps 1-4 1000 times.

#Then this gets you a null distribution for each pair of surveys, and you aggregate that to the target region or global scale.

#From there, you can simply put the observed turnover value (for a target region or globally in this case) onto that null distribution and see if it falls outside the 97.5 percentiles. If it does, the observed is significantly different from random. If it doesn't, it is not different from random. You can then make plots showing the histograms of the null distributions plotted vertically. Those histograms or density plots can be added to figure 3.

#OR, you could feed these feed iterations into a linear model and estimate the turnover from there and compare. Or do one linear model per iteration and create a null distribution of modeled iterations.

##################################################################################################
# 1. INSTALL PACKAGES
##################################################################################################

# Easy code for installing packages in R (if not installed) and calling their libraries
# From: https://gist.github.com/DrK-Lo/a945a29d6606b899022d0f03109b9483

# make vector of packages needed

packages_needed <- c("tidyverse", "codyn","data.table")

# install packages needed (if not already installed)
for (i in 1:length(packages_needed)){
  if(!(packages_needed[i] %in% installed.packages())){install.packages(packages_needed[i])}
}

# load packages needed
for (i in 1:length(packages_needed)){
  library( packages_needed[i], character.only = TRUE)
}

##################################################################################################
# 2. IMPORT DATA
##################################################################################################

# Import and clean data
df <- read_csv("data-derived/SAS_clean_data.csv") |>
  select(target_region,peak,year,aspect,contour,species) |> # select relevant columns
  filter(!species %in% c("solid rock","bare ground","scree","litter","bryophytes","lichen")) |> # remove cover classes (solid rock, bare ground, scree, litter, bryophytes, lichen)
  group_by(target_region, peak, year) |> 
  distinct(species) # filter down to the unique species on a peak in a given year

# create a data frame with tr/peak/year/number of unique species:
df_ref <- df |> 
  group_by(target_region, peak, year) |>
  summarise(richness = n_distinct(species))

# create a vector of unique peaks, sorted alphabetically
peaks = sort(unique(df$peak))

##################################################################################################
# 3. INITIATE OUTER LOOP AND FILTER DATA TO PEAK i
##################################################################################################

# create empty list to store turnover values from all iterations of all peaks
turnover = list()

# create for loop for peak 
for (i in 1:length(peaks)) 
{
  
# reset data frames for each peak
test = c()
test_ref = c()
nsp = c()
  
# create empty list to be filled; this is list containing all turnover estimates for all iterations of peak i
turnover_peak_reps = list()
  
# subset all data for peak i
test <- df |> 
  filter(peak == peaks[i])
  
# create a vector of unique years for peak i, sorted chronologically
peak_years = sort(unique(test$year))
  
# count how many surveys there are for peak i
n_years = n_distinct(peak_years)
  
# subset the number of unique species in each year for peak i; this will be used to give the number of samples:
test_ref <- df_ref |> 
  filter(peak == peaks[i]) |>
  arrange(year) # make sure data frame is sorted by year
  
# obtain neutral species pool - list of all unique species seen across all years on peak i
nsp <- unique(test$species)
  
##################################################################################################
# 4.FOR PEAK i, INITIATE LOOP ACROSS N REPLICATES
##################################################################################################
  
# set number of sample replicates
n_reps = 1000

# Set seed for random sampling to obtain reproducible results
# WARNING!!! IS THIS THE RIGHT PLACEMENT FOR THIS!?!?!? VERIFY THAT ALL SAMPLES ARE UNIQUE!!!! moved first from k loop to j loop, that didnt work then moved to this pre-loop position
set.seed(122)
  
# create nested for loop to estimate turnover based on randomly sampled subsets of species pool
for (j in 1:n_reps) 
  {
    
# create empty list to be filled
sampled_peak_years = list()
    
#####################################################################################################
# 5.FOR PEAK i AND REPLICATE j, INITIATE DOUBLE-NESTED LOOP TO RANDOMLY SAMPLE SPECIES OVER k years
####################################################################################################
    
# create double-nested for loop to estimate randomly sampled subsets of species pool equal to the number of surveys that occurred on peak i
for (k in 1:n_years) 
{
      
#randomly sample from this species pool w/o replacement, number of draws is equal to the species richness of a given survey (peak/year):
sampled_peak_years[[k]] = data.frame("sample" = sample(nsp, test_ref$richness[k], replace = F)) |> # perform random sampling  from species pool
mutate(year = peak_years[k], peak = peaks[i]) # add columns for peak and year to data frame
    } # end double-nested loop
    
# convert list to data frame and add abundance column; get each of these randomly sampled species lists ready for codyn::turnover
samples = data.table::rbindlist(sampled_peak_years, fill = TRUE) |> # I found this solution for converting a list of unequal length to data frame here: https://www.robwiederstein.org/2021/04/04/convert-list-of-unequal-length-to-dataframe/
mutate(abundance = 1) # abundance = 1 for feeding to codyn package
    
# Now you have a single random sample from the pool of all possible species for each year, and each year's sample is equal to the richness in that year. We tied this up into a handy data frame of all the samples. 
# So a single one of these data frames is like recreating all the work that has been done at 332 by GLORIA field teams :)
# year = year
# species = species names
# abundance = 1 for present
    
##################################################################################################
# 6.FOR PEAK i AND REPLICATE j, ESTIMATE TURNOVER BETWEEN ALL CONSECUTIVE PAIRS OF SURVEY YEARS
##################################################################################################
    
#feed to codyn::turnover:
    
turnover_peak <- turnover(df = samples, 
                          time.var = "year", 
                          species.var = "sample",
                          abundance.var = "abundance",
                          metric = "total")|>
    mutate(replicate = j,peak = peaks[i]) # add iteration number to data frame
    
    
##################################################################################################
# 7.STORE TURNOVER ESTIMATES IN LIST, END LOOPS, AND WRITE CSV FILE
##################################################################################################
    
# store jth iteration of peak i into list
turnover_peak_reps[[j]] = turnover_peak
    
  } # end within-peak nested loop (completing all iterations of peak i)
  
# Convert list of all iterations of peak i into a data frame
turnover_reps <- do.call(rbind, turnover_peak_reps)
  
# save all iterations of peak i into list
turnover[[i]] = turnover_reps
  
} # end across-peak loop

# Convert list of bootstrapped lambdas to data frame and arrange by Site
turnover_all <- do.call(rbind, turnover) 

# write bootstrapped lambda estimates to .csv
#write_csv(turnover_all,"data-derived/null_turnover.csv") 

#plot observed mean and confidence intervals of null on histograms of null target region turnover. Just the 97.5 and 2.5% quantiles from null and v lines!

#plus ninth panel for global results, same as above

#would be super valuable to see how other papers work with null turnover models in the literature.

#another set of ideas building on the current figure 3 is to add a dashed horizontal line for observed mean turnover, then for each target region add confidence intervals of null turnover to each target region. Do this first and if it is too crazy circle back to histograms!

#check this script before you do things, do some eyeballing to make sure everything is making sense! Plotting the 59 null distributions would be a good check that we are creating good distributions each time. 

# 59 histograms, for every peak and date combination:
theme_set(theme_classic())

raw_hist_all <- ggplot(data = turnover_all) +
  geom_histogram(aes(x = total), binwidth = 0.025) + #2.5% turnover change from bin to bin
  facet_wrap(~ peak + year)

raw_hist_all

#so, do these look healthy or not? for some peaks these look really nice (rna 2009,332 2009), for others they look pretty bad (wlr, wmt). Thought for the peaks that look bad, those have few species and few surveys.

# 29 histograms, one for each peak:

raw_hist_peak <- ggplot(data = turnover_all) +
  geom_histogram(aes(x = total), binwidth = 0.05) + #5% turnover change from bin to bin
  facet_wrap(~ peak)

raw_hist_peak

################################################################################################################
#PREVIOUS CHECKS / notes (NOT RUN)
############################################################################################

#already some weird stuff like the same turnover estimate in multiple replicates-- this seems to be happening quite a bit and seems kind of strange? Look at the observed for a peak (try 332 2014) and think about how many different turnover values are possible! look 357. I suspect there are only just so many combinations possible of very similar species lists.

#one thing I quickly noticed-- mid has the same turnover for every replicate. Is that a symptom of there only being two surveys and thus one turnover measure?? Or is it because the same species were seen both times?

###########################################################################
#one way to check things out is to re-run the code for one peak at a time, and compare to raw turnover numbers from the other script:

#potential suspects:

#dev_mid
#wmt_2014
#wlr_2013
#bar_2014
#whe_2017

#all of these peaks and more have only one turnover value. Hopefully, this is because the same species are found in the two adjoining surveys and those represent the entire species pool?

###############################################
#check a single peak:

check <- df %>% 
  filter(peak == "mid")

#this shows the number of unique species in each year, and will be used to give the number of samples:
check_ref <- df_ref %>% 
  filter(peak == "mid")

nsp_check <- unique(check$species)

#same seed as above:
#set.seed(122)

size_mid <- check_ref$richness

s_2013 <- data.frame("sample" = sample(nsp_check, size_mid[1], replace = F)) %>% mutate(year = 2013, peak = "mid")
s_2018 <- data.frame("sample" = sample(nsp_check, size_mid[2], replace = F)) %>% mutate(year = 2018, peak = "mid")

samples_mid <- rbind(s_2013, s_2018)

samples_mid <- samples_mid %>% 
  mutate(abundance = 1)

#feed to codyn::turnover:

turn_mid <- turnover(df = samples_mid, 
                     time.var = "year", 
                     species.var = "sample",
                     abundance.var = "abundance",
                     metric = "total")

#indeed what is calculated here (0.351) is a different number from the iterations show above (0.305). This peak has different species assemblages and different species richness in the two years surveyed. I used the same seed as above, and if I use a different seed I get a different value. So we have a problem, I just don't know what it is!

#values from this (pasted here after each run): 0.3947368, 0.3513514, 0.3055556, 0.3947368, 0.3513514, 0.3513514, 0.3513514, 0.3947368

#########################################################################################
#WAIT actually when I run this script I just get the same turnover estimate for every replicate, which might mean that just that part is busted.

#when I remove set seed in the loop, this problem goes away. Yay, and that just means it wasn't quite in the correct part of the loop. looking back at mid though, there are only 3 unique turnover values:

mid_check2 <- turnover_all %>% 
  filter(peak == "mid")

unique(mid_check2$total)

#is this because only three outcomes are possible? Went back and re-ran the code above (lns 208-236), and pasted the outcome after each trial. In 8 runs I only saw the three values. Seems to check out!

###################################################################################################
#check a single peak:

check <- df %>% 
  filter(peak == "wlr")

#this shows the number of unique species in each year, and will be used to give the number of samples:
check_ref <- df_ref %>% 
  filter(peak == "wlr")

nsp_check <- unique(check$species)

#same seed as above:
#set.seed(122)

size_wlr <- check_ref$richness

s_2008 <- data.frame("sample" = sample(nsp_check, size_wlr[1], replace = F)) %>% mutate(year = 2008, peak = "wlr")
s_2013 <- data.frame("sample" = sample(nsp_check, size_wlr[2], replace = F)) %>% mutate(year = 2013, peak = "wlr")
s_2018 <- data.frame("sample" = sample(nsp_check, size_wlr[3], replace = F)) %>% mutate(year = 2018, peak = "wlr")

samples_wlr <- rbind(s_2008, s_2013, s_2018)

samples_wlr <- samples_wlr %>% 
  mutate(abundance = 1)

#feed to codyn::turnover:

turn_wlr <- turnover(df = samples_wlr, 
                     time.var = "year", 
                     species.var = "sample",
                     abundance.var = "abundance",
                     metric = "total")

# I think wlr has only one possible turnover value from year to year?
