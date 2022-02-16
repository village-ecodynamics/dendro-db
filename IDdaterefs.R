library(tidyverse)

db <- readr::read_csv(
  here::here("data-raw/BOCINSKY_ET_AL_2015_TREE_RINGS.csv")
)

refs <- readr::read_csv(
  here::here("data-raw/ref-tracker.csv")
)

refs %>% 
  dplyr::group_by(`Got it?`) %>% 
  dplyr::summarise(n = n())

db %>% 
  dplyr::right_join(
    refs %>% 
      dplyr::filter(
        `Got it?` == "yes"
      ),
    by = c("Refs")
  ) %>%
  readr::write_csv(
    here::here("data-derived/dates_wRefs.csv")
  )

db %>% 
  dplyr::right_join(
    refs %>% 
      dplyr::filter(
        `Got it?` == "no"
      ),
    by = c("Refs")
  ) %>%
  readr::write_csv(
    here::here("data-derived/dates_noRefs.csv")
  )


