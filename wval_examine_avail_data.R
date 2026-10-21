# wval look

library(tidyverse)

wval_download <- read.csv("C:/Users/juliegil/Documents/Delphi_Work/wval_look/CDC_Wastewater_Viral_Activity_Level_for_SARS-CoV-2,_Influenza_A_and_RSV_20261021.csv")
# file was downloaded on 2026-10-21
# website updated 2025-10-16
# https://data.cdc.gov/Public-Health-Surveillance/CDC-Wastewater-Viral-Activity-Level-for-SARS-CoV-2/atcp-73re/about_data

# today is 2026-10-21
# want to look at what it has for current wval per state if i calculate median, 
# then compare to websites

colnames(wval_download)

state_estimate <- wval_download %>% 
  group_by(State.Territory, Week_End, Pathogen_Target) %>% 
  summarize(median = median(as.numeric(Site_WVAL), na.rm = TRUE), 
            count_sites = length(unique(Site)))

state_estimate2 <- filter(state_estimate, Week_End == "2026-10-10")
# website https://www.cdc.gov/wastewater/respiratory-viruses/national.html
# has update date of 2026-10-15, 7:14am
# flu A matches exactly, except for Minnesota site count
# website has 27, this has 24 unique sites, median wval still matches

# covid - Minnesota has 27 on website and here, so something about flu A

# rsv - Kentucky here has 14, on website has 19, but median wval still matches
# maine here has 3, on website has 8, wval still matches
# minnesota here has 21, website has 27, wval still matches
# ohio here 54, website has 57

# possibly the website is number of sites that exist, not number of sites
# with available data/reporting? this aligns with it being the same 
# across all three datasets from the website

check_range <- state_estimate %>% group_by(State.Territory, Pathogen_Target) %>%
  summarize(min_site_count = min(count_sites), 
            max_site_count = max(count_sites))
# not enough granularity

check_range <- filter(state_estimate, State.Territory == "Minnesota" & year(Week_End) == 2026) %>% select(State.Territory, Pathogen_Target, Week_End, count_sites) %>% distinct()


ggplot(check_range, aes(x = as_date(Week_End), y = count_sites)) + 
  geom_col() + 
  theme_bw() + 
  labs(title = "Minnesota 2026", 
       x = "Week", 
       y = "Count of Unique Sites Reporting") + 
  facet_wrap(.~Pathogen_Target)
### okay, not that? there is pretty significant range here, and 27 is 
# not a number shared by all three pathogens as the number of sites reporting ever
# sars also has more, so it's likely not just a "this is how many sites we have, 
# and not necessarily everyone is always reporting"



minnesota <- filter(wval_download, State.Territory == "Minnesota" & Week_End == "2026-10-10") %>% 
  group_by(State.Territory, Week_End, Pathogen_Target) %>% 
  mutate(median = median(as.numeric(Site_WVAL), na.rm = TRUE), 
            count_sites = length(unique(Site)))

# could be a site size thing

wval_download <- wval_download %>% mutate(pop = as.numeric(gsub(",", "", Population_Served)))

under_30 <- filter(wval_download, Week_End == "2026-10-10") %>% select(State.Territory, Site, pop, Pathogen_Target) %>% distinct()
table(filter(under_30, Pathogen_Target == "Influenza A virus")$pop < 30000)

# not that, too many sites have less than 30000 across the board, more
# state counts would not match up



