library(tidyverse)
library(covidHubUtils)
library(covidData)

# query parameters -- targets and locations
inc_hosp_targets <- paste(1:34, "day ahead inc hosp")
incl_locations <- covidData::fips_codes %>%
    dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
    dplyr::pull(location)

# forecast due dates, reference dates for hosp forecasts; all are Mondays
dates <- seq.Date(from = as.Date("2020-11-09"),
                  to = as.Date("2022-10-24"),
                  by = 7) %>%
    as.character()

# assemble data frame with availability of hospitalization forecasts by model
# and reference date
hosp_forecast_avail <- purrr::map_dfr(
    dates,
    function(date) {
        message(date)
        load_forecasts(
            dates = date,
            date_window_size = 6,
            locations = incl_locations,
            types = "quantile",
            targets = inc_hosp_targets,
            source = "zoltar",
            verbose = FALSE,
            as_of = NULL,
            hub = c("US")
        ) %>%
            # align forecasts that may have been submitted on different dates
            # around a common reference date, keep only up to relative horizon
            # of 28 dayes (relative to the reference date)
            align_forecasts() %>%
            dplyr::filter(relative_horizon <= 28) %>%
            # keep only model/location/date/horizon combos with all quantiles
            dplyr::group_by(model, location, reference_date,
                            relative_horizon) %>%
            dplyr::summarize(n_quantiles = dplyr::n(), .groups = "drop") %>%
            dplyr::filter(n_quantiles == 23L) %>%
            # for each model/location/reference date, track how many horizons
            # are available
            dplyr::group_by(model, location, reference_date) %>%
            dplyr::summarize(
                h_14_avail = (all(seq_len(14) %in% relative_horizon)),
                h_28_avail = (all(seq_len(28) %in% relative_horizon)),
                .groups = "drop"
            ) %>%
            # for each model/reference date, count how many locations have all
            # of horizons 1 through 14 or all of horizons 1 through 28
            dplyr::group_by(model, reference_date) %>%
            dplyr::summarize(
                n_loc_h14_avail = sum(h_14_avail),
                n_loc_h28_avail = sum(h_28_avail)
            )
    })

saveRDS(hosp_forecast_avail, "data/hosp_forecast_avail.rds")

pdf("plots/hosp_n_loc_h28_avail_all.pdf", width = 12, height = 8)
ggplot(data = hosp_forecast_avail) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_h28_avail)) +
    theme_bw()
dev.off()

h28_models <- c("BPagano-RtDriven", "CMU-TimeSeries", "Covid19Sim-Simulator",
    "COVIDhub-baseline", "CU-select", "CUB_PopCouncil-SLSTM",
    "Google_Harvard-CPF", "GT-DeepCOVID", "IHME-CurveFit",
    "JHU_IDD-CovidSP", "JHUAPL-Bucky", "JHUAPL-Gecko",
    "Karlen-pypm", "LANL-GrowthRate",
    "LUcompUncertLab-VAR_3streams", "MOBS-GLEAM_COVID", "MUNI-ARIMA",
    "PSI-DICE", "UMass-sarix",
    "UMass-trends_ensemble", "USC-SI_kJalpha", "UT-Osiris",
    "UVA-Ensemble")

pdf("plots/hosp_n_loc_h28_avail_subset.pdf", width = 12, height = 8)
ggplot(data = hosp_forecast_avail %>% dplyr::filter(model %in% h28_models)) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_h28_avail)) +
    theme_bw()
dev.off()


h28_models_2021 <- c(
    "COVIDhub-baseline", "CU-select", "JHUAPL-Bucky", "JHUAPL-Gecko",
    "Karlen-pypm", "MOBS-GLEAM_COVID", "USC-SI_kJalpha")

pdf("plots/hosp_n_loc_h28_avail_subset_2021.pdf", width = 12, height = 8)
ggplot(data = hosp_forecast_avail %>% dplyr::filter(model %in% h28_models_2021)) +
    geom_tile(mapping = aes(x = reference_date,
                            y = model,
                            fill = n_loc_h28_avail)) +
    theme_bw()
dev.off()

