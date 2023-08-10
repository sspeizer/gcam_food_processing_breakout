# function to edit raw output data downloaded from PIC
# removes extra headers and column names and puts data in longer form and 
# splits date from scenario name
# requires tidyr and dplyr
parse_PIC_data <- function(results_df) {
  require(dplyr)
  require(tidyr)
  
  # remove extra headers and column names and separate date
  results_df <- results_df %>% 
    filter(scenario != "scenario" & ! (is.na(region))) %>%
    separate(scenario, into = c("scenario", "date"), sep = c(","))
  
  # remove last column, which is NAs
  results_df <- results_df[, !apply(is.na(results_df), 2, all)]
  
  # put in longer form
  # first get year columns
  year_vals <- as.character(intersect(colnames(results_df), c(1975:2100)))
  results_df <- results_df %>% 
    pivot_longer(cols = all_of(year_vals), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(year))
  
  return(results_df)
}