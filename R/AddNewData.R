library(tidyverse)
library(magrittr)
library(robinson2020)
library(villager)

# Add Sinensky Data: https://doi.org/10.15184/aqy.2021.19
sinensky <- 
  readr::read_csv(here::here("data-raw/sinensky_2021_AD200_800_tr.csv")) %>% 
  dplyr::rename(
    sinensky_Site_Name = Site_Name, #Specify Site Name
    sinensky_Site_Number = Site_Number #Specify Site Number
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
  ) %>% 
  dplyr::filter(
    !(Refs == "Diederichs 2020:503-504")
  )

# add Diederichs 

diederichs <- 
  readr::read_csv(here::here("data-raw/diederichs_2020_ccac.csv")) %>% 
  dplyr::rename(
    diederichs_Site_Number = Site_Number #Specify Site Number
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
      "diederichs_Site_Number" = "Canon_Site_Number")
  ) %>% 
  dplyr::rename(Canon_Site_Number = diederichs_Site_Number,
                `Year AD` = `Outside Date`) %>% 
  dplyr::mutate(`Year AD` = as.integer(`Year AD`),
                `Inside Date` = as.integer(`Inside Date`),
                Type = factor(C_level, # Adjust C-level (AKA cutting level) to have appropriate names
                              levels = c(1,2,3),
                              labels = c("Non-cutting", "Near-cutting", "Cutting"),
                              ordered = T),
                `Tree Age` = ifelse(is.na(`Inside Date`),
                                    NA,
                                    `Year AD` - `Inside Date`)) %>%  
  dplyr::select(
    -treespecies,
    -`Symbol Inside Date`,
    -`Symbol Outside Date`,
    -Provenience
  ) %>% 
  dplyr::left_join(
    read_csv(here::here("data-raw/tlkptreeringspecies.csv")) %>% 
      dplyr::rename(Species = treespecies ), # read in species csv
    by = "treespeciescode") %>% # 
  dplyr::select(
    Canon_Site_Name,
    Canon_Site_Number, 
    Lab_Number, 
    Type,
    `Year AD`,
    Species,
    -`Inside Date`
  ) %>% 
  dplyr::mutate(
    Refs = "Diederichs 2020",
    Source = "Diederichs 2020"
  ) 

##merge dataframes

dendro <- 
  readr::read_csv(
  here::here("data-derived/TR_2022_04_01.csv") #read in the Bocinsky dataset
) %>% 
  tibble::as_tibble() %>% 
  tibble::add_row(sinensky) %>% 
  tibble::add_row(diederichs)

write_csv(dendro,
          here::here("data-derived/TR_2022_04_02.csv"))





