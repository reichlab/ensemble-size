library(tidyverse)
# devtools::install_github("jarad/FluSight")
library(FluSight) 
library(MMWRweek)

get_single_forecast_entry <- function(file_path){
  # browser()
  entry <- read_entry(file_path)
  file_parts <- strsplit(file_path, split = "/")[[1]]
  file_only <- file_parts[length(file_parts)]
  entry %>% mutate(year = as.numeric(substr(file_only, 6, 9)),
                   model_name = substr(file_only, 11, gregexpr("\\.", file_only)[[1]]-1))
}

score_multi_entry <- function(entry, truth){
  # browser()
  names(entry) <- tolower(names(entry))
  names(truth) <- tolower(names(truth))
  if (!("forecast_week" %in% names(entry))) 
    stop("Column forecast_week needed in entry - \n         use read_entry() with your submission CSV")
  seasonal <- entry %>% 
    filter(type == "Bin", 
           target %in% c("Season onset", 
                         "Season peak week", 
                         "Season peak percentage", 
                         "Season peak rate")) %>% 
    right_join(truth, by = c("location", "target", "bin_start_incl")) %>% 
    filter(target %in% c("Season onset", "Season peak week", 
                         "Season peak percentage", "Season peak rate")) %>% 
    select(-forecast_week.y) %>% 
    rename(forecast_week = forecast_week.x)
  weekly <- entry %>% 
    filter(type == "Bin", target %in% c("1 wk ahead", 
                                        "2 wk ahead", 
                                        "3 wk ahead", 
                                        "4 wk ahead")) %>% 
    right_join(truth, by = c("location", "target", 
                             "bin_start_incl", "forecast_week")) %>% 
    filter(target %in% c("1 wk ahead", 
                         "2 wk ahead", 
                         "3 wk ahead", 
                         "4 wk ahead"))
  scores <- bind_rows(seasonal, 
                      weekly) %>% 
    group_by(model_name,
             year,
             location, 
             target, 
             forecast_week) %>% filter(n()!=1)
    summarize(score = log(sum(value))) %>% 
    ungroup() %>% 
    mutate(score = ifelse(score < -10 | is.na(score), -10, score))
  return(scores)
}


files <- list.files('raw-data/FluSightNetwork-cdc-flusight-ensemble-e7d16c9/model-forecasts/component-models/ReichLab_kde/', 
                    full.names = T)
files <- files[grepl('2010', files)]

files %>% 
  map(get_single_forecast_entry) %>% 
  bind_rows() -> forecast_df

truth_df <- create_truth(year = 2010)

score_multi_entry(forecast_df, truth_df) %>% 
  as_tibble() -> temp
temp