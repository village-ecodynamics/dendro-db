library(tidyverse)
library(magrittr)
library(robinson2020)
library(villager)


dendro <- readr::read_csv(
  here::here("data-raw/BOCINSKY_ET_AL_2015_TREE_RINGS.csv") #read in the Bocinsky dataset
)  %>% 
  dplyr::rename(
    Boc_Site_Name = Site_Name, #Specify Boc Site Name
    Boc_Site_Number = Site_Number, #Specify Boc Site Number
    Boc_SiteID = Seq_Number #Specify Boc sequence number is actually Boc site Id 
  )  %>% 
  # Join the tree ring dataset to the Boc/VEP naming dictionary
  dplyr::left_join(
    readr::read_csv(
      here::here("data-raw/VEP_name_dictionary.csv") #Read in Dictionary
    ) %>%
      dplyr::mutate(Canon_Site_Number =  # Make a new column with site number (Both Trinomial and LA numbers)
                      ifelse(is.na(Canon_Trinomial), #Trinomials are prioritized over LA numbers
                             Canon_LA_Number, # Only supply LA number if there's no trinomial
                             Canon_Trinomial)) %>% # supply trinomial if there is one
      dplyr::select(Boc_SiteID,  
                    Canon_Site_Number, 
                    Canon_Site_Name),  #Only keep the site ID and site number from the dictionary
    by = "Boc_SiteID")  %>% # join the boncinsky tree ring dataset by Boc_Site_ID
  dplyr::rename(`Year AD` = Outer_Date_AD) %>% # Rename to be better
  # Join with the VEP Master Tree Ring dataset, which has Species, inside dates, and provenience
  left_join(readr::read_csv(
    here::here("data-raw/VEP II Master Tree-Ring Date Database Final.csv")) %>% 
      # Adjust the lab numbers to match Bocinsky Dataset's numbers
      tidyr::separate(`TRL Spec. No.`, into = c("Alpha", "Numeric"), sep = "-") %>% # separate by hyphen
      dplyr::mutate(Numeric = str_remove(Numeric, "^0+"))  %>% #cleanup weird spaces
      dplyr::mutate(Lab_Number = paste(.$Alpha, .$Numeric, sep='-')) %>% 
      dplyr::filter(!is.na(`Outside Date`)), # make a new column with better lab number
    by = "Lab_Number") %>% # join according to Lab_number
  # Join a dataset that translates the tree species codes into the actual tree species names
  dplyr::left_join(
    read_csv(here::here("data-raw/tlkptreeringspecies.csv")) %>% # read in species csv
      rename(Species = treespeciescode), #match the tree nicknames to join
    by = "Species") %>% # Join by matched species nickname
  # sf::st_as_sf(  # Convert to spatial object
  #   coords = c("LON", "LAT"),
  #   crs = 4326
  # ) %>% 
  # # join with VEP shapefile to identify VEP region NOTE THAT ONLY VEP REGIONS HAVE CANON SITE NAMES
  # sf::st_join(sf::read_sf(here::here("data/derived_data/VEPshp.shp"))) %>%   
  # as.data.frame() %>% # turn back into a dataframe, shapefile no longer needed
  # dplyr::rename(`Study Area` = Study.Area) %>% # fix errors caused by the df-sf-df transformation
  dplyr::mutate(
    Type = factor(C_Level, # Adjust C-level (AKA cutting level) to have appropriate names
                  levels = c(1,2,3),
                  labels = c("Non-cutting", "Near-cutting", "Cutting"),
                  ordered = T),
    Boc_Site_Name = stringr::str_to_title(Boc_Site_Name), # Keep because only VEP sites have canon names
    `Tree Age` = ifelse(is.na(`Inside Date`),
                        NA,
                        `Year AD` - `Inside Date`)) %>% 
  dplyr::select(-Species) %>% # Get rid of the species nickname
  dplyr::rename(Species = treespecies ) %>%  #,
                # Subregion = Stratum) %>% #rename species fullname column
  # dplyr::filter(`Study Area` %in% c("CMV")) %>% 
  # dplyr::filter(Type %in% c("Cutting", "Near-cutting"),
  #               `Year AD` %in% 604:1280) %>% 
  # dplyr::mutate(Type = c("Cutting and Near-cutting")) %>% 
  dplyr::select( # clean up the dataset to only have relevant things.
    # `Study Area`, # VEP region, if it exists
    # Subregion, # VEP subregion, if it exists
    Canon_Site_Name, # Canon site name, for VEP sites
    Canon_Site_Number, # Canon site number, for VEP sites
    Boc_Site_Name, # Bocinsky Site Name, for non-VEP sites
    Boc_Site_Number, # Bocinsky Site Number, for non-VEP sites
    # Provenience, # Tree Ring provenience, if available
    Lab_Number, # Lab number for the dendrochronological date
    Species, # Tree species of dendro date
    Type, # Type of cutting date, non-, near-, or cutting
    `Year AD` , # Year of cutting date
    `Inside Date`, # inside date (useful later to identify tree age at cutting, which may reveal deforestation info)
    `Tree Age`,
    Refs
    # geometry
  ) %>% 
  dplyr::mutate(
    Source = "Bocinsky 2016"
  )


write_csv(dendro,
          here::here("data-derived/TR_2022_04_01.csv"))
