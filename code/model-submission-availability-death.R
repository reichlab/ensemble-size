library(tidyverse)
library(covidHubUtils)
library(covidData)

# query parameters -- targets and locations
death_targets <- paste(1:5, "wk ahead inc death")
incl_locations <- covidData::fips_codes %>%
    dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
    dplyr::pull(location)

# forecast due dates, reference dates for death forecasts; all are Mondays
dates <- seq.Date(from = as.Date("2020-11-09"),
                  to = as.Date("2022-10-24"),
                  by = 7) %>%
    as.character()


# assemble data frame with availability of death forecasts by model
# and reference date
death_forecast_avail <- purrr::map_dfr(
    dates,
    function(date) {
        message(date)
        load_forecasts(
            dates = date,
            date_window_size = 6,
            locations = incl_locations,
            types = "quantile",
            targets = death_targets,
            source = "zoltar",
            verbose = FALSE,
            as_of = NULL,
            hub = c("US")
        ) %>%
            # align forecasts that may have been submitted on different dates
            # around a common reference date, keep only up to relative horizon
            # of 28 dayes (relative to the reference date)
            align_forecasts() %>%
            dplyr::filter(relative_horizon <= 4) %>%
            # keep only model/location/date/horizon combos with all quantiles
            dplyr::group_by(model, location, reference_date,
                            relative_horizon) %>%
            dplyr::summarize(n_quantiles = dplyr::n(), .groups = "drop") %>%
            dplyr::filter(n_quantiles == 23L) %>%
            # for each model/location/reference date, track how many horizons
            # are available
            dplyr::group_by(model, location, reference_date) %>%
            dplyr::summarize(
                d_1wk_avail = (all(seq_len(1) %in% relative_horizon)),
                d_2wk_avail = (all(seq_len(2) %in% relative_horizon)),
                d_3wk_avail = (all(seq_len(3) %in% relative_horizon)),
                d_4wk_avail = (all(seq_len(4) %in% relative_horizon)),
                .groups = "drop"
            ) %>%
            # for each model/reference date, count how many locations have all
            # of horizons 1, horizons 1 through 2, or horizons 1 through 3, 
            # all of horizons 1 through 4
            dplyr::group_by(model, reference_date) %>%
            dplyr::summarize(
                n_loc_d1wk_avail = sum(d_1wk_avail),
                n_loc_d2wk_avail = sum(d_2wk_avail),
                n_loc_d3wk_avail = sum(d_3wk_avail),
                n_loc_d4wk_avail = sum(d_4wk_avail)
            )
    })



saveRDS(death_forecast_avail, "data-death/death_forecast_avail.rds")



## plot for death_n_loc_d1wk_available_all

death_forecast_avail <- readRDS("data-death/death_forecast_avail_all.rds")

pdf("plots-death/death_n_loc_d4wk_avail_all.pdf", width = 12, height = 8)
ggplot(data = death_forecast_avail) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_d4wk_avail)) +
    theme_bw()
dev.off()

d4wk_models <- c("USC-SI_kJalpha", "UCSD_NEU-DeepGLEAM", "RobertWalraven-ESG",
                 "PSI-DRAFT", "MOBS-GLEAM_COVID", "MIT_ISOLAT-Mixtures",
                 "Microsoft-DeepSTIA", "Karlen-pypm", "JHUAPL-Bucky", 
                 "JHU_IDD-CovidSP", "GT-DeepCOVID", "CU-select", 
                 "CU-scenario_mid", "CU-scenario_low",
                 "CU-nochange", "COVIDhub-baseline", "CovidAnalytics-DELPHI",
                 "BPagano-RtDriven")
# the list of d4wk_models is the same as those for other weeks. 

pdf("plots-death/death_n_loc_d4wk_avail_subset.pdf", width = 12, height = 8)
ggplot(data = death_forecast_avail %>% 
               dplyr::filter(model %in% d4wk_models)) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_d4wk_avail)) +
    theme_bw()
dev.off()




d4wk_models_2021 <- c("USC-SI_kJalpha", "UCSD_NEU-DeepGLEAM", 
                      "RobertWalraven-ESG", "PSI-DRAFT", "MOBS-GLEAM_COVID", 
                      "MIT_ISOLAT-Mixtures", "Microsoft-DeepSTIA", 
                      "Karlen-pypm", "JHUAPL-Bucky", "JHU_IDD-CovidSP",  
                      "GT-DeepCOVID", "CU-select", "CU-scenario_mid", 
                      "CU-scenario_low", "CU-nochange", "COVIDhub-baseline", 
                      "CovidAnalytics-DELPHI", "BPagano-RtDriven",
                      "UMich-RidgeTfReg", "UMass-MechBayes",
                      "SteveMcConnell-CovidComplete", "MIT_CritData-GBCF", 
                      "JHU_CSSE-DECOM", "DDS-NBDS"
                      )

pdf("plots-death/death_n_loc_d4wk_avail_subset_2021.pdf", width = 12, height = 8)
ggplot(data = death_forecast_avail %>% 
               dplyr::filter(model %in% d4wk_models_2021) %>% 
               dplyr::filter(reference_date >= as.Date("2021-01-02"), 
                             reference_date <= as.Date("2022-01-01"))) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_d4wk_avail)) +
    theme_bw()
dev.off()

