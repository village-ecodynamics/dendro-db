library(tidyverse)
library(magrittr)
library(robinson2020)
library(villager)

readr::read_csv(here::here("data-derived/TR_2022_04_01.csv")) 
  
# Add Sinensky Data: https://doi.org/10.15184/aqy.2021.19
sinensky <- 
  readr::read_csv(here::here("data-raw/sinensky_2021_AD200_800_tr.csv")) %>% 
  dplyr::rename(
    sinensky_Site_Name = Site_Name, #Specify Boc Site Name
    sinensky_Site_Number = Site_Number #Specify Boc Site Number
  )  %>% 
  dplyr::left_join(
    ., 
    readr::read_csv(
      here::here("data-raw/VEP_name_dictionary.csv") #Read in Dictionary
    ) %>%
      dplyr::mutate(Canon_Site_Number =  # Make a new column with site number (Both Trinomial and LA numbers)
                      ifelse(is.na(Canon_Trinomial), #Trinomials are prioritized over LA numbers
                             Canon_LA_Number, # Only supply LA number if there's no trinomial
                             Canon_Trinomial)) %>% # supply trinomial if there is one
      dplyr::select(Canon_Site_Number, 
                    Canon_Site_Name),  #Only keep the site name and site number from the dictionary
    by =  c(
      "sinensky_Site_Number" = "Canon_Site_Number")
  ) %>% 
  dplyr::select(-sinensky_Site_Name) %>% 
  dplyr::rename(Canon_Site_Number = sinensky_Site_Number,
                `Year AD` = Outer_Date_AD) %>% 
  dplyr::mutate(
    Type = factor(C_Level, # Adjust C-level (AKA cutting level) to have appropriate names
                  levels = c(1,2,3),
                  labels = c("Non-cutting", "Near-cutting", "Cutting"),
                  ordered = T)) %>% 
  dplyr::select(
    Canon_Site_Name,
    Canon_Site_Number, 
    Lab_Number, 
    Type,
    `Year AD`,
    Refs
  ) %>% 
  dplyr::mutate(
    Source = "Sinensky 2021"
  )



