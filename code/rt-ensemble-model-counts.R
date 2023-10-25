# This file is used to calculate mean number of models included in real-time
# ensembles, used in the caption of Figure 1 in the main text of the manuscript.

library(tidyverse)

# calculations for covid targets
test_dates <- list(
  inc_case = seq.Date(as.Date("2021-11-15"), as.Date("2022-07-25"), by=7),
  inc_hosp = seq.Date(as.Date("2022-05-30"), as.Date("2022-11-07"), by=7),
  inc_death = seq.Date(as.Date("2021-11-15"), as.Date("2022-11-07"), by=7)
)

get_num_incl_models_one_target_date_covid <- function(target, date) {
  model_weights <- readr::read_csv(paste0(
    "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/4_week_ensemble-metadata/",
    date, "-", target, "-model-weights.csv"))
  
  num_models <- model_weights |>
    dplyr::filter(nchar(locations) == 2, locations != "US") |>
    dplyr::select(-locations) |>
    apply(2, sum) |>
    as.data.frame() |>
    `colnames<-`("total") |>
    dplyr::filter(total > 0) |>
    nrow()
  
  return(num_models)
}

get_num_incl_models_one_target_covid <- function(target) {
  dates <- test_dates[[target]]
  
  num_models <- purrr::map(
    dates,
    \(date) get_num_incl_models_one_target_date_covid(target, date)
  ) |>
    unlist()
  
  return(
    data.frame(
      target = target,
      date = dates,
      num_models = num_models
    )
  )
}

num_models_by_target_date_covid <- purrr::map(
  names(test_dates),
  get_num_incl_models_one_target_covid
) |>
  purrr::list_rbind()

num_models_by_target_date_covid |>
  dplyr::group_by(target) |>
  dplyr::summarize(mean_num_models = mean(num_models))

# # A tibble: 3 × 2
#   target    mean_num_models
#   <chr>               <dbl>
# 1 inc_case             15  
# 2 inc_death            18.9
# 3 inc_hosp             17.0


# calculations for influenza admissions
get_num_incl_models_one_date_flu <- function(date) {
  models_incl <- readr::read_csv(paste0(
    "https://raw.githubusercontent.com/cdcepi/Flusight-ensemble/main/Archive_2223/",
    date, "/models-to-include-in-ensemble-", date, ".csv"
  ))
  
  num_models <- nrow(models_incl)
  
  return(num_models)
}

test_dates <- seq.Date(as.Date("2022-10-17"), as.Date("2023-04-03"), by=7)

num_models_by_date_flu <- data.frame(
  date = test_dates,
  num_models = purrr::map_int(test_dates, get_num_incl_models_one_date_flu)
)

num_models_by_date_flu |>
  dplyr::summarize(mean_num_models = mean(num_models))
# 21.4


# maximum number of included models -- used in discussion
max(num_models_by_date_flu$num_models)
# 25

max(num_models_by_target_date_covid$num_models)
# 26
