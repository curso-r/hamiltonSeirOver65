## code to prepare `latest_covid` dataset goes here
library(dplyr)
latest = utils::read.csv("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.csv")
latest = latest %>%
  dplyr::mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date, TotalCovidDeaths, Aged1, 
                Aged1to4, Aged5to14, Aged15to24, Aged25to34, Aged35to44, 
                Aged45to54, Aged55to64, Aged65up) %>% 
  dplyr::mutate(`Under 65s` = Aged1+ 
                  Aged1to4+ Aged5to14+ Aged15to24+ Aged25to34+ Aged35to44+ 
                  Aged45to54+ Aged55to64,
                `Over 65s` = Aged65up,
                `Total` = TotalCovidDeaths) %>% 
  dplyr::select(Date, `Under 65s`, `Over 65s`, `Total`) %>% 
  dplyr::mutate(
    dplyr::across( is.numeric, ~(.x - dplyr::lag(.x)))
  ) %>%
  tidyr::pivot_longer(names_to = 'Age group', values_to = 'Value', -Date) 

if(identical(latest, latest_covid)) {
  usethis::ui_done("Nothing to update")
} else{
  latest_covid <- latest
  usethis::use_data(latest_covid, overwrite = TRUE)
  usethis::ui_done("dataset updated!")
  deploy_app <- 1
}
