
library(tidyverse)
library(epidatr)

# wval data from cdc website (not delphi)
wval_download <- read.csv("C:/Users/juliegil/Documents/Delphi_Work/wval_look/CDC_Wastewater_Viral_Activity_Level_for_SARS-CoV-2,_Influenza_A_and_RSV_20261026.csv")

### raw nwss data (in delphi)
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

### state wval roll-up data
loc1 <- "C:/Users/juliegil/Documents/Delphi_Work/wval_look/"

state_wval_rsv <- read.csv(paste0(loc1, "NWSSWVALStateMap_20261017_rsv.csv"))
state_wval_cov <- read.csv(paste0(loc1, "NWSSWVALStateMap_20261017_cov.csv"))
state_wval_flu <- read.csv(paste0(loc1, "NWSSWVALStateMap_20261017_flua.csv"))

## Q1. How many sites are in the nwss delphi data that would be eligible for
## wval calculations for week ending 10/17/2026

nwss_eligible <- filter(nwss_aux_rsv, reference_time >= as_date("2026-10-11") & 
                          reference_time <= as_date("2026-10-17")) #& 
                          #report_time <= as_date("2026-10-17"))

nwss_eligible2 <- nwss_eligible %>% 
  group_by(state_territory) %>% 
  summarize(count_sites = length(unique(geo_value)))

nwss_eligible2$source <- "NWSS Raw"

# compare it with the downloaded cdc site level wval file

wval_site_eligible <- filter(wval_download, Pathogen_Target == "RSV" & 
                               Week_End == "2026-10-17")

wval_site_eligible2 <- wval_site_eligible %>% group_by(State.Territory) %>% 
  summarize(count_sites = length(unique(Site)))

wval_site_eligible2 <- wval_site_eligible2 %>% mutate(state_territory = tolower(state.abb[match(State.Territory,state.name)]))

wval_site_eligible2 <- wval_site_eligible2 %>% mutate(state_territory = case_when(State.Territory == "U.S. Virgin Islands" ~ "vi", 
          T ~ state_territory))

wval_site_eligible2 <- wval_site_eligible2 %>% select(state_territory, count_sites)

wval_site_eligible2$source <- "WVAL Site Level"


# wval state reported by cdc

wval_state_eligible <- state_wval_rsv %>% select(State.Territory, Number.of.Sites)

wval_state_eligible <- wval_state_eligible %>% mutate(state_territory = tolower(state.abb[match(State.Territory,state.name)]))

wval_state_eligible <- wval_state_eligible %>% mutate(state_territory = case_when(State.Territory == "U.S. Virgin Islands" ~ "vi", 
                                                                                 State.Territory == "District of Columbia" ~ "dc", 
                                                                                  T ~ state_territory))


wval_state_eligible2 <- wval_state_eligible %>% select(state_territory, Number.of.Sites)
colnames(wval_state_eligible2) <- c("state_territory", "count_sites")
wval_state_eligible2$source <- "WVAL State Level"

all_eligible <- rbind(wval_site_eligible2, nwss_eligible2, wval_state_eligible2)

ggplot(all_eligible, aes(y = count_sites, x = state_territory, fill = source)) + 
  geom_bar(stat = "identity", position = "dodge")
# this relationship changes significantly with limiting report date on the 
# raw nwss data


all_eligible2 <- reshape2::dcast(all_eligible, state_territory ~ source, value.var = "count_sites")

all_eligible2 <- all_eligible2 %>% mutate(same_count = case_when(`NWSS Raw` == `WVAL Site Level` & `NWSS Raw` == `WVAL State Level` & `WVAL Site Level` == `WVAL State Level` ~ 0, 
                                                    T ~ 1))

all_eligible2 <- filter(all_eligible2, same_count == 1)

all_eligible2 <- reshape2::melt(all_eligible2, id.vars = c("state_territory", "same_count"))

colnames(all_eligible2) <- c("state_territory", "same_count", "source", "count_sites")

ggplot(all_eligible2, aes(y = count_sites, x = state_territory, fill = source)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_bw() + 
  labs(x= "", 
       y = "Count of Sites", 
       fill = "Source", 
       title = "States with Different Number of Sites Between Sources", 
       subtitle = "2026-10-17")


### guess: some of these mis-matches are because sites are new and don't have
### enough history to have a wval calculated


# so let's pull the data from the nwss raw set from the states here, and look at the 
# list of site differences

mismatches <- filter(nwss_aux_rsv, state_territory %in% unique(all_eligible2$state_territory))

mismatches2 <- filter(nwss_eligible, state_territory %in% unique(all_eligible2$state_territory))

look_at_history <- filter(mismatches, geo_value %in% unique(mismatches2$geo_value))
# need just the sites from the states that we looked at in the above graph, but
# need their full history

look_at_history <- look_at_history %>% group_by(geo_value) %>% mutate(min_ref = min(reference_time))

wval_site_eligible <- wval_site_eligible %>% mutate(site2 = gsub("ID:", "", Site))

look_at_history <- look_at_history %>% mutate(in_site_wval = case_when(geo_value %in% unique(wval_site_eligible$site2) ~ "In Both NWSS Raw and WVAL Site Level", 
                                                                       T ~ "Not in WVAL Site"))

look_at_history2 <- look_at_history %>% select(geo_value, state_territory, min_ref, in_site_wval) %>% distinct()

look_at_history2 <- look_at_history2 %>% group_by(state_territory) %>% mutate(count_sites_total = length(unique(geo_value)))

ggplot(look_at_history2, aes(x = min_ref, y = geo_value, color = in_site_wval)) + 
  geom_point() + 
  geom_vline(xintercept = as_date("2025-01-01")) + 
  theme_bw() + 
  facet_wrap(.~state_territory+count_sites_total, scales = "free_y")

filter(wval_state_eligible2, state_territory %in% unique(look_at_history2$state_territory))






