# Script to estimate the non-energy costs for the overall food processing sector.
# Obtains the current energy costs from electricity and process heat food processing
# (which incorporates already the non-energy costs for the technologies within process heat food processing)
# and calculates the difference in those costs from the total food processing sector costs, excluding 
# food inputs, from GTAP

# Set up and load data ------------------------

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# load functions
source("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/parse_PIC_data.R")

# load data
# GTAP data
CostShare_FoodProcessing_GTAP <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_exploration/data/CostShare_FoodProcessing_GTAP.csv")
HHDGOV_Ag <- readRDS("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_exploration/data/HHDGOV_Ag.rds")
GCAM_reg_abb_map <- CostShare_FoodProcessing_GTAP %>%
  dplyr::select(region_GCAM, region_GCAM_abb) %>%
  unique()
# current food processing costs data, food demand data, and agriculture costs data - output from GCAM run

# food_demand <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_demand.csv")
# food_pro_costs_tech <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_pro_costs_tech.csv")
# food_pro_costs_tech_input <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_run_8-3-23/food_pro_costs_tech_input.csv")

prices_all_markets <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_prices_all_markets.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
costs_tech <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_costs_tech.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
costs_tech_input <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_costs_tech_input.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
food_demand <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_food_demand.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
demand_balances_crop_commodity <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_demand_balances_crop_commodity.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
demand_balances_meat_dairy_commodity <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_demand_balances_meat_dairy_commodity.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
ag_regional_prices <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_ag_regional_prices.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch
GDP_MER_region <- read_csv("C:/Users/spei632/Documents/GCAM_industry/food_processing/initial_breakout_results/data/food_processing_validation_run_8-21-23/queryoutall_GDP_MER_region.csv", skip = 1) %>%
  parse_PIC_data() %>% filter(scenario == "GCAM" & grepl("2023-21-8", date)) # just get reference scenario for food branch

# constants
Pcal_to_Mcal <- 10^9
to_bil <- 10^-9


# Calculate current cost values -----------------

# all we want from the current cost values are the current energy costs associated with the overall food processing sector.
# these energy costs will include costs of electricity and process heat food processing, the latter of which already incorporates
# the non-energy costs associated with the process heat food processing technologies.
# we want to exclude the current non-energy cost values for the overall food processing sector, since those are currently
# placeholder assumptions that we are aiming to replace.

# food_pro_costs_overall_excl_overall_nonenergy <- food_pro_costs_tech %>%
#   filter(sector == "food processing") %>% # just want overall sector costs
#   # remove the placeholder assumptions for non-energy costs
#   left_join(food_pro_costs_tech_input %>%
#               filter(sector == "food processing" & input == "non-energy") %>%
#               dplyr::select(scenario, region, sector, subsector, technology, year, Units, assumed_nonenergy_cost = value)) %>%
#   mutate(energy_cost = value - assumed_nonenergy_cost) 
food_pro_costs_overall_excl_overall_nonenergy <- costs_tech %>%
  filter(sector == "food processing") %>% # just want overall sector costs
  # remove the placeholder assumptions for non-energy costs
  left_join(costs_tech_input %>%
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
         energy_cost_total_bil_2014USD = energy_cost_total_1975USD * gcamdata::gdp_deflator(2014, 1975) * to_bil) # convert to billion 2014$

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
  dplyr::select(region, scenario, year, energy_cost_total_bil_2014USD, Pcal) %>%
  filter(year == 2015) %>%
  left_join(FoodProcessing_GTAP_total_costs_sel %>%
              # just want 2014 and the overall FoodProduct output
              filter(year == 2014 & output == "FoodProduct") %>%
              dplyr::select(region = region_GCAM, all_non_food_input_cost_bil, cap_en_labor_cost_bil)) %>%
  mutate(dif_all_non_food_input_cost_bil = all_non_food_input_cost_bil - energy_cost_total_bil_2014USD,
         dif_cap_en_labor_cost_bil = cap_en_labor_cost_bil - energy_cost_total_bil_2014USD,
         dif_all_non_food_input_cost_1975USD = dif_all_non_food_input_cost_bil * gcamdata::gdp_deflator(1975, 2014) / to_bil,
         dif_cap_en_labor_cost_1975_USD = dif_cap_en_labor_cost_bil * gcamdata::gdp_deflator(1975, 2014) / to_bil,
         dif_all_non_food_input_cost_1975USD_per_Mcal = dif_all_non_food_input_cost_1975USD / (Pcal * Pcal_to_Mcal),
         dif_cap_en_labor_cost_1975USD_per_Mcal = dif_cap_en_labor_cost_1975_USD / (Pcal * Pcal_to_Mcal))

# calculate globally summarized values
comp_costs_global <- comp_costs %>%
  summarize(dif_all_non_food_input_cost_1975USD = sum(dif_all_non_food_input_cost_1975USD),
            dif_cap_en_labor_cost_1975_USD = sum(dif_cap_en_labor_cost_1975_USD),
            Pcal = sum(Pcal)) %>%
  mutate(dif_all_non_food_input_cost_1975USD_per_Mcal = dif_all_non_food_input_cost_1975USD / (Pcal * Pcal_to_Mcal),
         dif_cap_en_labor_cost_1975USD_per_Mcal = dif_cap_en_labor_cost_1975_USD / (Pcal * Pcal_to_Mcal))

# Compare to sum of household and government consumption from GTAP -------------
# obtain the total cost for all consumed crops for food
OtherMeat_Fish_prices <- prices_all_markets %>%
  filter(market == "globalOtherMeat_Fish")

ag_regional_total_costs_food <- rbind(demand_balances_crop_commodity,
                                      demand_balances_meat_dairy_commodity) %>%
  filter(sector == "FoodDemand_NonStaples" | sector == "FoodDemand_Staples") %>%
  left_join(ag_regional_prices %>% 
              bind_rows(OtherMeat_Fish_prices %>% 
                          dplyr::select(-c(market, region)) %>% 
                          mutate(sector = "OtherMeat_Fish") %>%
                          gcamdata::repeat_add_columns(tibble(region = unique(ag_regional_prices$region)))) %>%
              rename(input = sector,
                     Units_cost = Units,
                     value_cost = value)) %>%
  mutate(cost_per_Mt = value_cost * 10^9,
         total_cost = cost_per_Mt * value,
         total_cost_2005USD = total_cost * gcamdata::gdp_deflator(2005, 1975),
         total_cost_bil_2014USD = total_cost * gcamdata::gdp_deflator(2014, 1975) * 10^-9)

# aggregate to total
ag_regional_total_costs_food_agg <- ag_regional_total_costs_food %>%
  group_by(scenario, year, region) %>%
  summarize(total_cost_2005USD = sum(total_cost_2005USD),
            total_cost_bil_2014USD = sum(total_cost_bil_2014USD)) %>%
  ungroup()

# aggregate the household and government food data from GTAP
HHDGOV_Ag_agg <- HHDGOV_Ag %>%
  filter(year == 2014, !input %in% c("Wool", "Forest")) %>% 
  filter(input_agg %in% c("PrimaryAg", "Proc_Food")) %>%
  mutate(sector = if_else(input %in% c("FoodProduct", "BeverageTobacco"), "processed_GTAP", "primary_GTAP")) %>%
  # group_by(region_GCAM_abb, sector = input_agg, year) %>%
  group_by(region_GCAM_abb, sector, year) %>%
  summarise(value = sum(value), .groups = "drop")

# compare costs
costs_comp_test <- food_pro_costs_overall_excl_overall_nonenergy_total %>%
  filter(year == 2015) %>%
  dplyr::select(scenario, region, Pcal, food_pro_cost_bil_2014USD = energy_cost_total_bil_2014USD) %>%
  left_join(ag_regional_total_costs_food_agg %>%
              filter(year == 2015) %>%
              dplyr::select(scenario, region, food_cost_bil_2014USD = total_cost_bil_2014USD)) %>%
  dplyr::select(region, food_pro_GCAM = food_pro_cost_bil_2014USD, food_GCAM = food_cost_bil_2014USD) %>%
  pivot_longer(-c(region), names_to = "cost_type", values_to = "value") %>%
  mutate(source = "GCAM") %>%
  rbind(HHDGOV_Ag_agg %>%
          left_join(GCAM_reg_abb_map %>% rename(region = region_GCAM)) %>%
          dplyr::select(region, cost_type = sector, value) %>%
          mutate(source = "GTAP")) %>%
  # mutate(cost_type = factor(cost_type, levels = c("food_pro_GCAM", "food_GCAM", "PrimaryAg", "Proc_Food")))
  mutate(cost_type = factor(cost_type, levels = c("food_pro_GCAM", "food_GCAM", "processed_GTAP", "primary_GTAP"))) %>%
  group_by(source, region) %>%
  mutate(share = value / sum(value)) %>%
  ungroup()

ggplot(costs_comp_test, aes(x = source, y = value, fill = cost_type)) +
  geom_col() + 
  facet_wrap(~region, ncol = 8, scales = "free_y") + 
  #scale_fill_manual(values = c("food_pro_GCAM" = "darkblue", "food_GCAM" = "lightblue", "PrimaryAg" = "lightgreen", "Proc_Food" = "darkgreen")) +
  scale_fill_manual(values = c("food_pro_GCAM" = "darkblue", "food_GCAM" = "lightblue", "processed_GTAP" = "darkgreen", "primary_GTAP" = "lightgreen")) +
  labs(x = "", y = "bil 2014$", title = "GCAM food and food processing (excluding non-energy) costs (2015) vs GTAP food consumption costs (HHD/GOV) (2014)") +
  theme_bw() +
  theme(text=element_text(size=12),
        strip.background = element_blank())
ggsave("C:/Users/spei632/Documents/GCAM_industry/food_processing/food_food_pro_costs_comp_HHDGOV_Ag.png", width = 12, height = 8)

# plot shares of costs from GTAP data
ggplot(costs_comp_test %>% filter(cost_type == "processed_GTAP") %>% 
         mutate(region = factor(region, levels = costs_comp_test %>% filter(cost_type == "processed_GTAP") %>% arrange(share) %>% pull(region))), 
       aes(x = region, y = share)) +
  geom_col() + 
  labs(x = "", y = "Share", title = "GTAP processed food share of total HHD/GOV consumption (2014)") +
  theme_bw() +
  theme(text=element_text(size=14),
        strip.background = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 60))
ggsave("C:/Users/spei632/Documents/GCAM_industry/food_processing/HHDGOV_Ag_processed_share.png", width = 12, height = 8)

# calculate the difference between the costs from GTAP and from GCAM
costs_comp_test_wider <- costs_comp_test %>%
  dplyr::select(-c(source, share)) %>%
  pivot_wider(names_from = cost_type, values_from = value) %>%
  mutate(dif_primary_costs_bil = primary_GTAP - food_GCAM,
         dif_processed_costs_bil = processed_GTAP - food_pro_GCAM,
         dif_total_costs_bil = primary_GTAP + processed_GTAP - (food_GCAM + food_pro_GCAM),
         dif_processed_costs_1975USD = dif_processed_costs_bil * gcamdata::gdp_deflator(1975, 2014)/ to_bil,
         dif_total_costs_1975USD = dif_total_costs_bil * gcamdata::gdp_deflator(1975, 2014)/ to_bil) %>%
  left_join(food_demand_total %>% filter(year == 2015) %>% dplyr::select(region, Pcal = value)) %>%
  mutate(dif_processed_costs_1975USD_per_Mcal = dif_processed_costs_1975USD / (Pcal * Pcal_to_Mcal),
         dif_total_costs_1975USD_per_Mcal = dif_total_costs_1975USD / (Pcal * Pcal_to_Mcal))

## Final values to use as gcamdata inputs ---------
# we will use the resulting differences in total processed costs per Mcal as the food processing sector non-energy costs
# pull just these values, adding a default value for regions not specified calculated as the global weighted average (weighted by calorie consumption)
final_foodpro_nonenergy_cost_adder_region <- costs_comp_test_wider %>%
  dplyr::select(region, cost = dif_processed_costs_1975USD_per_Mcal) %>%
  rbind(costs_comp_test_wider %>%
          summarize(dif_processed_costs_1975USD = sum(dif_processed_costs_1975USD),
                    Pcal = sum(Pcal)) %>%
          mutate(cost = dif_processed_costs_1975USD / (Pcal * Pcal_to_Mcal),
                 region = "default") %>%
          dplyr::select(region, cost)) %>%
  mutate(units = "1975$ per Mcal",
         supplysector = "food processing",
         subsector = "food processing",
         technology = "food processing",
         minicam.non.energy.input = "non-energy") %>%
  arrange(region, supplysector, subsector, technology, minicam.non.energy.input, cost, units)

ggplot(final_foodpro_nonenergy_cost_adder_region %>%
         mutate(region = factor(region, levels = final_foodpro_nonenergy_cost_adder_region %>% arrange(cost) %>% pull(region)),
                color_label = if_else(region == "default", "a", "b")), 
       aes(x = region, y = cost, fill = color_label)) +
  geom_col() + 
  scale_fill_manual(values = c("blue4", "blue"), guide = "none") + 
  labs(x = "", y = "1975$/Mcal", title = "Food processing non-energy cost adder") +
  theme_bw() +
  theme(text=element_text(size=14),
        strip.background = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 60))
ggsave("C:/Users/spei632/Documents/GCAM_industry/food_processing/food_pro_non_energy_adder_regional.png", width = 12, height = 8)

# Look at resulting food expenditures from GCAM data ---------

# calculate total food expenditures as the sum of agricultural costs plus food processing costs, for each source
total_food_expenditures <- costs_comp_test_wider %>%
  mutate(total_GTAP_bil = primary_GTAP + processed_GTAP, 
         total_GCAM_adjusted_bil = food_pro_GCAM + food_GCAM + dif_processed_costs_bil,
         total_GTAP_bil_per_Pcal = total_GTAP_bil / Pcal,
         total_GCAM_adjusted_bil_per_Pcal = total_GCAM_adjusted_bil / Pcal)

total_food_expenditures_Pcal <- total_food_expenditures %>%
  dplyr::select(region, total_GTAP_bil_per_Pcal, total_GCAM_adjusted_bil_per_Pcal) %>%
  pivot_longer(cols = c(total_GTAP_bil_per_Pcal, total_GCAM_adjusted_bil_per_Pcal),
               names_to = "type", values_to = "cost_bil2014USD_per_Pcal") %>%
  mutate(source = if_else(grepl("GCAM", type), "GCAM", "GTAP"))

ggplot(total_food_expenditures_Pcal %>%
         mutate(region = factor(region, levels = total_food_expenditures_Pcal %>% filter(source == "GCAM") %>% arrange(cost_bil2014USD_per_Pcal) %>% pull(region))), 
       aes(x = region, y = cost_bil2014USD_per_Pcal, fill = source)) +
  geom_col(position = "dodge") + 
  labs(x = "", y = "bil 2014$ per Pcal", title = "GCAM adjusted total food costs (2015) vs GTAP food consumption costs (HHD/GOV) (2014)") +
  theme_bw() +
  theme(text=element_text(size=14),
        strip.background = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 1, angle = 60))
ggsave("C:/Users/spei632/Documents/GCAM_industry/food_processing/total_costs_comp_per_Pcal.png", width = 12, height = 8)

# as a share of GDP

