library(tidyverse)
library(readxl)
library(haven)
library(janitor)

# setup -------

## read relevant datasets and cleanup (0 to 1 for values)
internet_penetration <- read_csv('individuals-using-the-internet_1741283929073.csv') %>%
  rename(country = entityName) %>%
  group_by(country) %>%
  summarize(
    int_pen = mean(dataValue, na.rm = TRUE) / 100
  )


mobile_connectivity_index <- read_csv('MCI_Data_2024.csv') %>%
  rename(country = Country) %>%
  group_by(country) %>%
  summarize(
    mob_con = mean(Index,  na.rm = TRUE) / 100
  )


trade_to_GDP <- read_csv('API_NE.TRD.GNFS.ZS_DS2_en_csv_v2_75991.csv') %>%
  rename(country = `Country Name`) %>%
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "year",
    values_to = "trad_GDP"
  ) %>%
  group_by(country) %>%
  summarize(
    trad_GDP = mean(trad_GDP,  na.rm = TRUE) / 100
  )

###+ Note: Above was converted to be a value from 0 to 1 based on the 
###+ understanding that the original numbers represent % of GDP, where if their
###+ trade is producing all their GDP they would get a 1. We might need to adjust,
###+ given that some countries trade seems to be higher than their GDP (e.g.:
###+ Singapore has a score of 3.3 )

file_path <- "International_LPI_from_2007_to_2023_0.xlsx"
sheet_names <- excel_sheets("International_LPI_from_2007_to_2023_0.xlsx")
logistics_performance_index <- map_dfr(sheet_names, ~ read_excel(file_path, sheet = .x) %>%
                                         clean_names() %>%
                                         mutate(year = .x)
                                       ) %>%
  relocate(year, .after = country) %>%
  group_by(country) %>%
  summarize(
    lpi_score = mean(lpi_score, na.rm = TRUE) / 5
  )

###+ Note: the LPI score is 1 to 5. This calculation might not be the best, as low
###+ performing countries would not get anything below a 0.2 in a scale from 0 to 1.
###+ Might need to reconsider approach.


modern_renewables <- read_excel('Share of modern renewables database.xlsx') %>%
  rename(country = `Country/Region`) %>%
  pivot_longer(
    cols = `1990`:`2021`,
    names_to = "year",
    values_to = "ren_energy"
  ) %>%
  mutate (
    ren_energy = na_if(ren_energy, "..")
  ) %>%
  group_by(country) %>%
  summarize(
    ren_energy = mean(as.numeric(ren_energy),  na.rm = TRUE) / 100
  )
  
  
carbon_intensity_exports <- read_excel('CBAM-exposure-index-webpage-final.xlsx', sheet = 'aggregate') %>%
  rename(country = Country,
         carb_intensity_e = `Aggregate relative CBAM exposure index`) %>%
  select(country, carb_intensity_e)
  

political_stability_index <- read_dta('wgidataset.dta') %>%
  rename(country = countryname) %>%
  mutate (
    pol_stability = estimate + 2.5
  ) %>%
  group_by(country)%>%
  summarize(
    pol_stability = mean(pol_stability, na.rm = TRUE) / 5
  )

###+ Note: political stability is supposed to be scored from -2.5 to 2.5 (approximately). 
###+ Given that, I am adding 2.5 and dividing by 5 to produce a scale from 0 to 1.
###+ However, there are some scores that go lower than -2.5. We might need to rescale.

# combining into one table -------

df_list <- list(internet_penetration, mobile_connectivity_index, trade_to_GDP, logistics_performance_index, 
                modern_renewables, carbon_intensity_exports, political_stability_index)

merged_df <- reduce(df_list, full_join, by = "country") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

