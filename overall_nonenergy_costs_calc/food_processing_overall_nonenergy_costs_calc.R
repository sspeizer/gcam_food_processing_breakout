# Script to estimate the non-energy costs for the overall food processing sector.
# Obtains the current energy costs from electricity and process heat food processing
# (which incorporates already the non-energy costs for the technologies within process heat food processing)
# and calculates the difference in those costs from the total food processing sector costs, excluding 
# food inputs, from GTAP

# Set up and load data ------------------------

library(tidyr)
library(dplyr)
library(readr)

# load data
# GTAP data
CostShare_FoodProcessing_GTAP <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_exploration/data/CostShare_FoodProcessing_GTAP.csv")
# current food processing costs data and food demand data - output from GCAM run
food_demand <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_demand.csv")
food_pro_costs_tech <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_pro_costs_tech.csv")
food_pro_costs_tech_input <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_pro_costs_tech_input.csv")

# constants
Pcal_to_Mcal <- 10^9
to_bil <- 10^-9

# Calculate current cost values -----------------

# all we want from the current cost values are the current energy costs associated with the overall food processing sector.
# these energy costs will include costs of electricity and process heat food processing, the latter of which already incorporates
# the non-energy costs associated with the process heat food processing technologies.
# we want to exclude the current non-energy cost values for the overall food processing sector, since those are currently
# placeholder assumptions that we are aiming to replace.
food_pro_costs_overall_excl_overall_nonenergy <- food_pro_costs_tech %>%
  filter(sector == "food processing") %>% # just want overall sector costs
  # remove the placeholder assumptions for non-energy costs
  left_join(food_pro_costs_tech_input %>%
              filter(sector == "food processing" & input == "non-energy") %>%
              dplyr::select(scenario, region, sector, subsector, technology, year, Units, assumed_nonenergy_cost = value)) %>%
  mutate(energy_cost = value - assumed_nonenergy_cost) 

# calculate total food demand
food_demand_total <- food_demand %>%
  group_by(scenario, Units, region, year) %>%
  summarize(value = sum(value)) %>%
  ungroup()

# obtain current total food processing costs by multiplying costs per calorie by food consumption
food_pro_costs_overall_excl_overall_nonenergy_total <- food_pro_costs_overall_excl_overall_nonenergy %>%
  left_join(food_demand_total %>%
              dplyr::select(scenario, region, year, Pcal = value)) %>%
  mutate(energy_cost_total_1975USD = Pcal * Pcal_to_Mcal * energy_cost, # convert Pcal to Mcal
         energy_cost_total_bil_2015USD = energy_cost_total_1975USD * gcamdata::gdp_deflator(2015, 1975) * to_bil) # convert to billion 2015$

# Aggregate GTAP costs ----------------------
# aggregate to summarized cost types
CostShare_FoodProcessing_GTAP_agg <- CostShare_FoodProcessing_GTAP %>%
  mutate(cost_bil = value / 1000) %>%
  group_by(year, output, region_GCAM, region_GCAM_abb, input_AGG) %>%
  summarize(cost_bil = sum(cost_bil)) %>%
  group_by(year, output, region_GCAM) %>%
  mutate(share = cost_bil / sum(cost_bil)) %>%
  ungroup()

# get total costs excluding PrimaryAg and Proc_Food
FoodProcessing_GTAP_total_costs_excl_food_inputs <- CostShare_FoodProcessing_GTAP_agg %>%
  filter(input_AGG != "PrimaryAg" & input_AGG != "Proc_Food") %>%
  group_by(year, output, region_GCAM) %>%
  summarize(cost_bil = sum(cost_bil)) %>%
  ungroup()

# also get just capital, energy, and labor costs
FoodProcessing_GTAP_total_costs_cap_en_labor <- CostShare_FoodProcessing_GTAP_agg %>%
  filter(input_AGG %in% c("Capital", "Energy", "Labor")) %>%
  group_by(year, output, region_GCAM) %>%
  summarize(cost_bil = sum(cost_bil)) %>%
  ungroup()

# combine
FoodProcessing_GTAP_total_costs_sel <- FoodProcessing_GTAP_total_costs_excl_food_inputs %>%
  rename(all_non_food_input_cost_bil = cost_bil) %>%
  left_join(FoodProcessing_GTAP_total_costs_cap_en_labor %>%
              rename(cap_en_labor_cost_bil = cost_bil))

# Compare current food processing costs to GTAP totals ---------------------
# we will just compare 2015 in GCAM to 2014 in GTAP, for a rough estimate
comp_costs <- food_pro_costs_overall_excl_overall_nonenergy_total %>%
  dplyr::select(region, scenario, year, energy_cost_total_bil_2015USD, Pcal) %>%
  filter(year == 2015) %>%
  left_join(FoodProcessing_GTAP_total_costs_sel %>%
              # just want 2014 and the overall FoodProduct output
              filter(year == 2014 & output == "FoodProduct") %>%
              dplyr::select(region = region_GCAM, all_non_food_input_cost_bil, cap_en_labor_cost_bil)) %>%
  mutate(dif_all_non_food_input_cost_bil = all_non_food_input_cost_bil - energy_cost_total_bil_2015USD,
         dif_cap_en_labor_cost_bil = cap_en_labor_cost_bil - energy_cost_total_bil_2015USD,
         dif_all_non_food_input_cost_1975USD = dif_all_non_food_input_cost_bil * gcamdata::gdp_deflator(1975, 2015) / to_bil,
         dif_cap_en_labor_cost_1975_USD = dif_cap_en_labor_cost_bil * gcamdata::gdp_deflator(1975, 2015) / to_bil,
         dif_all_non_food_input_cost_1975USD_per_Mcal = dif_all_non_food_input_cost_1975USD / (Pcal * Pcal_to_Mcal),
         dif_cap_en_labor_cost_1975USD_per_Mcal = dif_cap_en_labor_cost_1975_USD / (Pcal * Pcal_to_Mcal))

# calculate globally summarized values
comp_costs_global <- comp_costs %>%
  summarize(dif_all_non_food_input_cost_1975USD = sum(dif_all_non_food_input_cost_1975USD),
            dif_cap_en_labor_cost_1975_USD = sum(dif_cap_en_labor_cost_1975_USD),
            Pcal = sum(Pcal)) %>%
  mutate(dif_all_non_food_input_cost_1975USD_per_Mcal = dif_all_non_food_input_cost_1975USD / (Pcal * Pcal_to_Mcal),
         dif_cap_en_labor_cost_1975USD_per_Mcal = dif_cap_en_labor_cost_1975_USD / (Pcal * Pcal_to_Mcal))
