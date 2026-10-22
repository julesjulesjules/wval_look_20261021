# wval look

library(tidyverse)
library(epidatr)

file_storage <- "C:/Users/juliegil/Documents/Delphi_Work/oracle/real_time_storage/"

### api key
code_loc1 <- "C:/Users/juliegil/Documents/Delphi_Work/"

key <- read.table(paste0(code_loc1, "delphi_key.txt"))[1, 1]
Sys.setenv(DELPHI_EPIDATA_KEY = key)

nwss_main_rsv <- epidata_snapshot(
  source = "nwss",
  signals = "rsv_avg_conc", 
  geo_type = "sewershed"
)

nwss_aux_rsv <- nwss_main_rsv %>%
  epidata_aux()

wval_download <- read.csv("C:/Users/juliegil/Documents/Delphi_Work/wval_look/CDC_Wastewater_Viral_Activity_Level_for_SARS-CoV-2,_Influenza_A_and_RSV_20261021.csv")

# want to compare the site ids in the wval set with the site ids in the 
# underlying nwss data set, to see if that might be the difference in site id 
# counts going into the wval calculations...

wval_download <- wval_download %>% mutate(site_id = gsub("ID:", "", Site))
rsv_set <- filter(wval_download, Pathogen_Target == "RSV")

setdiff(unique(rsv_set$site_id), unique(nwss_aux_rsv$geo_value))
# nothing in wval_set that is not in the raw data set

setdiff(unique(nwss_aux_rsv$geo_value), unique(rsv_set$site_id))
# there are 189 site ids that are in the raw data set that are not in the wval set

# some of these might be because the sites are no longer existing, so let's check when 
# these sites last had datapoints

outsiders <- filter(nwss_aux_rsv, geo_value %in% setdiff(unique(nwss_aux_rsv$geo_value), unique(rsv_set$site_id)))


length(unique(outsiders$geo_value))


max_dates <- outsiders %>% group_by(geo_value, state_territory) %>% summarize(min_date = min(reference_time), 
                                                                              max_date = max(reference_time))

# the weird week is 10-10, so let's pull everything relevant to that time
# 10-4 to 10-10

relevant <- filter(max_dates, max_date >= "2026-10-04")
# how many are from minnesota?

filter(relevant, state_territory == "mn")


# Minnesota has 21 sites in the site level wval dataset for 10-10, but on the cdc website 
# for state wval, has 27 sites counted. 
# there are 7 sites that are in the raw nwss data that are not in the site level wval dataset

# 2773 - mn - 2026-09-07 2026-10-07
# my assumption is that this site is not counted at all, because it's new, so not
# enough history to provide an individual wval stream

# leaving six sites that are in the raw nwss data, NOT in the site level wval data, 
# but ARE counted in the state level wval roll-up

# let's see if there is anything in the data to infer why these guys are a little weird

mn_invest_sites <- filter(nwss_aux_rsv, geo_value %in% c(1000, 1006, 1007, 
                                                         1010, 1014, 1022))
table(mn_invest_sites$geo_value)

table(mn_invest_sites$geo_value, mn_invest_sites$nwss_source)
# all six are state_territory sources


### looked at the data manually
# i think it might be a timing thing - the data might not have been available at the
# time that the different data files/reports were made available?

# wval site level data was published on 2025-10-16
# state level wval report was updated 2026-10-15, 7:14am

only_1010 <- filter(mn_invest_sites, reference_time >= "2026-10-04")
# all of these sites that i believe were counted in the 10/10 state wval data
# have raw nwss data for the relevant week that wasn't available until 10/16


ggplot(mn_invest_sites, aes(x = reference_time, y = value)) + 
  geom_point() + 
  facet_wrap(.~geo_value)

# raw data is available, site level wval data is NOT available, counted in the state
# level wval data as a site counted, but likely not in the wval calculation
# looking at the raw data, mostly zeros, probably wval doens't have enough information 
# in these sites to be useful as a separate calculation

# compare with
mn_sites <- filter(nwss_aux_rsv, !geo_value %in% c(1000, 1006, 1007, 
                                                         1010, 1014, 1022) & state_territory == "mn")
table(mn_sites$geo_value)

# yes, so my guess is that they're doing some "historical" culling - i.e. you need 
# to have enough history in order for your site level wval to be reported

ggplot(mn_sites, aes(x = reference_time, y = value)) + 
  geom_point() + 
  facet_wrap(.~geo_value)

# and they're doing some sort of minimal level of variation necessary / some number of 
# values above zero that need to be accepted for reporting site level wval

all_mn_sites <- filter(nwss_aux_rsv, state_territory == "mn" )
length(unique(all_mn_sites$geo_value)) # 30 unique sites with rsv data reporting
# 27 are contributing to state wval (or otherwise still being counted)
# 21 have site level wval reported

# the theory is that some number don't have enough history to be included in either
test1 <- all_mn_sites %>% group_by(geo_value) %>% summarize(min_reference = min(reference_time))

ggplot(test1, aes(y = geo_value, x = min_reference)) + 
  geom_point()
# so that holds out, that 1008, 2713, 2773, are all excluded from BOTH state level
# and site level wval because they all have limited data with which to make wval calls from

# so that gives us 30 sites in the raw nwss wastewater data
# and accounts for the 27 reportedly used and/or able to be considered for the 
# statewide wval calculation

# only 21 of the total 30 possible sites have site level wval reported though
# and in theory, that's because of the six that have basically all zeros
# 1000, 1006, 1007, 1010, 1014, 1022

# calculate what percent of each site values are zero

test2 <- all_mn_sites %>% mutate(zero_value = case_when(value == 0 ~ 1, 
                                                        T ~ 0))

test2 <- test2 %>% group_by(geo_value) %>% summarize(count_zero = sum(zero_value), 
                                                     total_samples = length(sample_index))

test2 <- test2 %>% mutate(percent_zero = round((count_zero / total_samples)*100, 1))
# okay, that is not it. way more sites would be cut out if there was a 
# cutoff on that overall

# color the ones that are in the site level wval set
all_mn_sites <- filter(all_mn_sites, !geo_value %in%c(1008, 2713, 2773)) %>% mutate(site_level_wval_avail = case_when(geo_value %in% rsv_set$site_id ~ "Has Site Level Wval", 
                                                                          T ~ "Does not"))


ggplot(all_mn_sites, aes(x = reference_time, y = value, color = geo_value)) + 
  geom_point() + 
  facet_wrap(.~site_level_wval_avail)
# okay, guessing that it's still based on how much history is available?
# all the sites that are NOT in the site level wval reporting only have data from 
# after July 2025 

all_mn_sites <- all_mn_sites %>% group_by(geo_value) %>% mutate(min_reference = min(reference_time), 
                                                                number_of_samples = length(sample_index))
all_mn_sites <- all_mn_sites %>% mutate(check_range = case_when(as_date(min_reference) < as_date("2025-07-01") ~ "1", 
                                                                T ~ "0"))


ggplot(all_mn_sites, aes(x = reference_time, y = value, color = check_range)) + 
  geom_point() + 
  facet_wrap(.~site_level_wval_avail)


ggplot(all_mn_sites, aes(x = number_of_samples, y = site_level_wval_avail, color = check_range)) + 
  geom_point(position = "jitter")
# possibly a combination of date cutoff and total number of samples
# nope





ggplot(all_mn_sites, aes(x = reference_time, y = value, color = site_level_wval_avail)) + 
  geom_point() + 
  facet_wrap(.~geo_value)

ggplot(all_mn_sites, aes(x = reference_time, y = value, color = site_level_wval_avail)) + 
  geom_point() + 
  facet_wrap(.~nwss_source)
# not a source thing

ggplot(all_mn_sites, aes(x = reference_time, y = value, color = site_level_wval_avail)) + 
  geom_point() + 
  facet_wrap(.~counties_served)
# not a double county/other thing


ggplot(all_mn_sites, aes(x = number_of_samples, y = as.numeric(population_served), color = check_range)) + 
  geom_point() + 
  facet_wrap(.~site_level_wval_avail)
# feels like it might be a combination of things? but i just haven't landed on the right combo


