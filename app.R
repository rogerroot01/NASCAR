library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)
library(f1dataR)

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
formula1_dir <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(formula1_dir, "data")
bundled_data_dir <- file.path(app_dir, "data")
keep_data_dir <- file.path(data_dir, "keep these files")
keep_bundled_data_dir <- file.path(bundled_data_dir, "keep these files")
vignettes_dir <- file.path(formula1_dir, "vignettes")
static_track_dir <- file.path(app_dir, "www", "track_plots")
first_existing_path <- function(paths) {
  hits <- paths[file.exists(paths)]
  if (length(hits) > 0) hits[[1]] else paths[[1]]
}
bundled_track_profile_inventory_csv <- file.path(bundled_data_dir, "f1_stage5_track_profile_inventory_2018_2026.csv")
project_track_profile_inventory_csv <- file.path(data_dir, "f1_stage5_track_profile_inventory_2018_2026.csv")
track_profile_inventory_csv <- if (file.exists(bundled_track_profile_inventory_csv)) bundled_track_profile_inventory_csv else project_track_profile_inventory_csv

bundled_stage1_csv <- file.path(bundled_data_dir, "f1_stage1_driver_race_backbone_2018_2026.csv")
project_stage1_csv <- file.path(data_dir, "f1_stage1_driver_race_backbone_2018_2026.csv")
bundled_wet_races_csv <- file.path(bundled_data_dir, "f1_dry_training_wet_race_exclusions_2018_2026.csv")
project_wet_races_csv <- file.path(data_dir, "f1_dry_training_wet_race_exclusions_2018_2026.csv")
wet_races_csv <- first_existing_path(c(bundled_wet_races_csv, project_wet_races_csv))
speed_supplement_csv <- first_existing_path(c(
  file.path(keep_bundled_data_dir, "f1_speed_trap_supplement.csv"),
  file.path(bundled_data_dir, "f1_speed_trap_supplement.csv")
))
schedule_csv <- file.path(bundled_data_dir, "f1_schedule_2018_2026.csv")
future_prerace_estimates_csv <- file.path(bundled_data_dir, "f1_stage6_future_prerace_estimates_2018_2026.csv")
bundled_qualifying_predictions_csv <- file.path(bundled_data_dir, "f1_stage7_qualifying_app_predictions_2018_2026.csv")
project_qualifying_predictions_csv <- file.path(data_dir, "f1_stage7_qualifying_app_predictions_2018_2026.csv")
bundled_qualifying_metrics_csv <- file.path(bundled_data_dir, "f1_stage7_qualifying_metrics_2018_2026_train2024_test2025.csv")
project_qualifying_metrics_csv <- file.path(data_dir, "f1_stage7_qualifying_metrics_2018_2026_train2024_test2025.csv")
bundled_rf_predictions_with_odds_csv <- file.path(bundled_data_dir, "f1_stage9_rf_app_predictions_with_market_odds_2018_2026.csv")
project_rf_predictions_with_odds_csv <- file.path(data_dir, "f1_stage9_rf_app_predictions_with_market_odds_2018_2026.csv")
bundled_rf_probability_predictions_csv <- file.path(bundled_data_dir, "f1_stage10_rf_probability_app_predictions_2018_2026.csv")
project_rf_probability_predictions_csv <- file.path(data_dir, "f1_stage10_rf_probability_app_predictions_2018_2026.csv")
bundled_xgb_finish_predictions_csv <- file.path(bundled_data_dir, "f1_stage11_xgb_finish_app_predictions_2018_2026.csv")
project_xgb_finish_predictions_csv <- file.path(data_dir, "f1_stage11_xgb_finish_app_predictions_2018_2026.csv")
bundled_xgb_probability_predictions_csv <- file.path(bundled_data_dir, "f1_stage12_xgb_probability_app_predictions_2018_2026.csv")
project_xgb_probability_predictions_csv <- file.path(data_dir, "f1_stage12_xgb_probability_app_predictions_2018_2026.csv")
bundled_xgb_points_predictions_csv <- file.path(bundled_data_dir, "f1_stage13_xgb_points_app_predictions_2018_2026.csv")
project_xgb_points_predictions_csv <- file.path(data_dir, "f1_stage13_xgb_points_app_predictions_2018_2026.csv")
bundled_xgb_winner_without_predictions_csv <- file.path(bundled_data_dir, "f1_stage17_xgb_winner_without_app_predictions_2018_2026.csv")
project_xgb_winner_without_predictions_csv <- file.path(data_dir, "f1_stage17_xgb_winner_without_app_predictions_2018_2026.csv")
bundled_fastest_lap_predictions_csv <- file.path(bundled_data_dir, "f1_stage19_fastest_lap_app_predictions_2018_2026.csv")
project_fastest_lap_predictions_csv <- file.path(data_dir, "f1_stage19_fastest_lap_app_predictions_2018_2026.csv")
bundled_fastest_lap_odds_csv <- file.path(bundled_data_dir, "f1_fastest_lap_odds_current_2025_2026.csv")
project_fastest_lap_odds_csv <- file.path(data_dir, "f1_fastest_lap_odds_current_2025_2026.csv")
bundled_draftkings_salaries_csv <- file.path(bundled_data_dir, "f1_draftkings_salaries_current_2026.csv")
project_draftkings_salaries_csv <- file.path(data_dir, "f1_draftkings_salaries_current_2026.csv")
bundled_chatter_team_features_csv <- file.path(bundled_data_dir, "f1_stage18_chatter_team_features_2018_2026.csv")
project_chatter_team_features_csv <- file.path(data_dir, "f1_stage18_chatter_team_features_2018_2026.csv")
bundled_chatter_coefficients_csv <- file.path(bundled_data_dir, "f1_stage18_chatter_overlay_coefficients_2018_2026.csv")
project_chatter_coefficients_csv <- file.path(data_dir, "f1_stage18_chatter_overlay_coefficients_2018_2026.csv")
bundled_chatter_qualifying_overlay_csv <- file.path(bundled_data_dir, "f1_stage18_qualifying_chatter_overlay_2018_2026.csv")
project_chatter_qualifying_overlay_csv <- file.path(data_dir, "f1_stage18_qualifying_chatter_overlay_2018_2026.csv")
bundled_chatter_finish_overlay_csv <- file.path(bundled_data_dir, "f1_stage18_xgb_finish_chatter_overlay_2018_2026.csv")
project_chatter_finish_overlay_csv <- file.path(data_dir, "f1_stage18_xgb_finish_chatter_overlay_2018_2026.csv")
bundled_chatter_probability_overlay_csv <- file.path(bundled_data_dir, "f1_stage18_xgb_probability_chatter_overlay_2018_2026.csv")
project_chatter_probability_overlay_csv <- file.path(data_dir, "f1_stage18_xgb_probability_chatter_overlay_2018_2026.csv")
bundled_chatter_points_overlay_csv <- file.path(bundled_data_dir, "f1_stage18_xgb_points_chatter_overlay_2018_2026.csv")
project_chatter_points_overlay_csv <- file.path(data_dir, "f1_stage18_xgb_points_chatter_overlay_2018_2026.csv")
bundled_chatter_winner_without_overlay_csv <- file.path(bundled_data_dir, "f1_stage18_xgb_winner_without_chatter_overlay_2018_2026.csv")
project_chatter_winner_without_overlay_csv <- file.path(data_dir, "f1_stage18_xgb_winner_without_chatter_overlay_2018_2026.csv")
bundled_consensus_bets_csv <- file.path(bundled_data_dir, "f1_stage9_consensus_bets_2018_2026.csv")
project_consensus_bets_csv <- file.path(data_dir, "f1_stage9_consensus_bets_2018_2026.csv")
bundled_consensus_season_summary_csv <- file.path(bundled_data_dir, "f1_stage9_consensus_betting_season_summary_2018_2026.csv")
project_consensus_season_summary_csv <- file.path(data_dir, "f1_stage9_consensus_betting_season_summary_2018_2026.csv")
market_odds_summary_candidates <- c(
  file.path(keep_bundled_data_dir, "f1_market_odds_current_driver_summary_2025_2026.csv"),
  file.path(keep_data_dir, "f1_market_odds_current_driver_summary_2025_2026.csv"),
  file.path(keep_bundled_data_dir, "f1_market_odds_driver_summary_supplied_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_market_odds_driver_summary_supplied_2025_2026.csv"),
  file.path(keep_data_dir, "f1_market_odds_driver_summary_supplied_2025_2026.csv"),
  file.path(data_dir, "f1_market_odds_driver_summary_supplied_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_market_odds_driver_summary_2025_2026.csv"),
  file.path(data_dir, "f1_market_odds_driver_summary_2025_2026.csv")
)
market_odds_summary_csv <- market_odds_summary_candidates[file.exists(market_odds_summary_candidates)][1]
opening_market_odds_summary_candidates <- c(
  file.path(keep_bundled_data_dir, "f1_market_odds_opening_driver_summary_2025_2026.csv"),
  file.path(keep_data_dir, "f1_market_odds_opening_driver_summary_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_market_odds_opening_driver_summary_2025_2026.csv"),
  file.path(data_dir, "f1_market_odds_opening_driver_summary_2025_2026.csv")
)
opening_market_odds_summary_csv <- opening_market_odds_summary_candidates[file.exists(opening_market_odds_summary_candidates)][1]
grid_market_odds_summary_candidates <- c(
  file.path(keep_bundled_data_dir, "f1_market_odds_grid_driver_summary_2025_2026.csv"),
  file.path(keep_data_dir, "f1_market_odds_grid_driver_summary_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_market_odds_grid_driver_summary_2025_2026.csv"),
  file.path(data_dir, "f1_market_odds_grid_driver_summary_2025_2026.csv")
)
grid_market_odds_summary_csv <- grid_market_odds_summary_candidates[file.exists(grid_market_odds_summary_candidates)][1]
qualifying_market_odds_candidates <- c(
  file.path(keep_bundled_data_dir, "f1_qualifying_odds_current_2025_2026.csv"),
  file.path(keep_data_dir, "f1_qualifying_odds_current_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_qualifying_odds_current_2025_2026.csv"),
  file.path(data_dir, "f1_qualifying_odds_current_2025_2026.csv")
)
qualifying_market_odds_csv <- qualifying_market_odds_candidates[file.exists(qualifying_market_odds_candidates)][1]
winner_without_market_odds_candidates <- c(
  file.path(keep_bundled_data_dir, "f1_winner_without_odds_current_2025_2026.csv"),
  file.path(keep_data_dir, "f1_winner_without_odds_current_2025_2026.csv"),
  file.path(bundled_data_dir, "f1_winner_without_odds_current_2025_2026.csv"),
  file.path(data_dir, "f1_winner_without_odds_current_2025_2026.csv")
)
winner_without_market_odds_csv <- winner_without_market_odds_candidates[file.exists(winner_without_market_odds_candidates)][1]
rf_predictions_csv <- dplyr::case_when(
  file.exists(bundled_rf_predictions_with_odds_csv) ~ bundled_rf_predictions_with_odds_csv,
  file.exists(project_rf_predictions_with_odds_csv) ~ project_rf_predictions_with_odds_csv,
  TRUE ~ bundled_rf_predictions_with_odds_csv
)
rf_probability_predictions_csv <- dplyr::case_when(
  file.exists(bundled_rf_probability_predictions_csv) ~ bundled_rf_probability_predictions_csv,
  file.exists(project_rf_probability_predictions_csv) ~ project_rf_probability_predictions_csv,
  TRUE ~ bundled_rf_probability_predictions_csv
)
xgb_finish_predictions_csv <- dplyr::case_when(
  file.exists(bundled_xgb_finish_predictions_csv) ~ bundled_xgb_finish_predictions_csv,
  file.exists(project_xgb_finish_predictions_csv) ~ project_xgb_finish_predictions_csv,
  TRUE ~ bundled_xgb_finish_predictions_csv
)
xgb_probability_predictions_csv <- dplyr::case_when(
  file.exists(bundled_xgb_probability_predictions_csv) ~ bundled_xgb_probability_predictions_csv,
  file.exists(project_xgb_probability_predictions_csv) ~ project_xgb_probability_predictions_csv,
  TRUE ~ bundled_xgb_probability_predictions_csv
)
xgb_points_predictions_csv <- dplyr::case_when(
  file.exists(bundled_xgb_points_predictions_csv) ~ bundled_xgb_points_predictions_csv,
  file.exists(project_xgb_points_predictions_csv) ~ project_xgb_points_predictions_csv,
  TRUE ~ bundled_xgb_points_predictions_csv
)
qualifying_predictions_csv <- dplyr::case_when(
  file.exists(bundled_qualifying_predictions_csv) ~ bundled_qualifying_predictions_csv,
  file.exists(project_qualifying_predictions_csv) ~ project_qualifying_predictions_csv,
  TRUE ~ bundled_qualifying_predictions_csv
)
qualifying_metrics_csv <- dplyr::case_when(
  file.exists(bundled_qualifying_metrics_csv) ~ bundled_qualifying_metrics_csv,
  file.exists(project_qualifying_metrics_csv) ~ project_qualifying_metrics_csv,
  TRUE ~ bundled_qualifying_metrics_csv
)
xgb_winner_without_predictions_csv <- dplyr::case_when(
  file.exists(bundled_xgb_winner_without_predictions_csv) ~ bundled_xgb_winner_without_predictions_csv,
  file.exists(project_xgb_winner_without_predictions_csv) ~ project_xgb_winner_without_predictions_csv,
  TRUE ~ bundled_xgb_winner_without_predictions_csv
)
fastest_lap_predictions_csv <- dplyr::case_when(
  file.exists(bundled_fastest_lap_predictions_csv) ~ bundled_fastest_lap_predictions_csv,
  file.exists(project_fastest_lap_predictions_csv) ~ project_fastest_lap_predictions_csv,
  TRUE ~ bundled_fastest_lap_predictions_csv
)
fastest_lap_odds_csv <- first_existing_path(c(bundled_fastest_lap_odds_csv, project_fastest_lap_odds_csv))
draftkings_salaries_csv <- first_existing_path(c(bundled_draftkings_salaries_csv, project_draftkings_salaries_csv))
chatter_team_features_csv <- first_existing_path(c(bundled_chatter_team_features_csv, project_chatter_team_features_csv))
chatter_coefficients_csv <- first_existing_path(c(bundled_chatter_coefficients_csv, project_chatter_coefficients_csv))
chatter_qualifying_overlay_csv <- first_existing_path(c(bundled_chatter_qualifying_overlay_csv, project_chatter_qualifying_overlay_csv))
chatter_finish_overlay_csv <- first_existing_path(c(bundled_chatter_finish_overlay_csv, project_chatter_finish_overlay_csv))
chatter_probability_overlay_csv <- first_existing_path(c(bundled_chatter_probability_overlay_csv, project_chatter_probability_overlay_csv))
chatter_points_overlay_csv <- first_existing_path(c(bundled_chatter_points_overlay_csv, project_chatter_points_overlay_csv))
chatter_winner_without_overlay_csv <- first_existing_path(c(bundled_chatter_winner_without_overlay_csv, project_chatter_winner_without_overlay_csv))
consensus_bets_csv <- dplyr::case_when(
  file.exists(bundled_consensus_bets_csv) ~ bundled_consensus_bets_csv,
  file.exists(project_consensus_bets_csv) ~ project_consensus_bets_csv,
  TRUE ~ bundled_consensus_bets_csv
)
consensus_season_summary_csv <- dplyr::case_when(
  file.exists(bundled_consensus_season_summary_csv) ~ bundled_consensus_season_summary_csv,
  file.exists(project_consensus_season_summary_csv) ~ project_consensus_season_summary_csv,
  TRUE ~ bundled_consensus_season_summary_csv
)
stage1_csv <- if (file.exists(bundled_stage1_csv)) bundled_stage1_csv else project_stage1_csv
if (!file.exists(stage1_csv)) {
  stop("Stage 1 data not found: ", stage1_csv)
}

if (dir.exists(vignettes_dir)) {
  addResourcePath("vignettes", vignettes_dir)
}

if (dir.exists(static_track_dir)) {
  addResourcePath("track_plots", static_track_dir)
}

if (!file.exists(schedule_csv)) {
  stop("Schedule data not found: ", schedule_csv)
}

stage1 <- read_csv(stage1_csv, show_col_types = FALSE) %>%
  mutate(
    season = as.integer(season),
    round = as.integer(round),
    race_date = as.Date(race_date),
    date_of_birth = as.Date(date_of_birth),
    finish_position = as.numeric(finish_position),
    grid = as.numeric(grid),
    quali_position = as.numeric(quali_position),
    points = as.numeric(points),
    across(starts_with("is_"), ~ as.integer(.x))
  )

if (file.exists(speed_supplement_csv)) {
  speed_supplement <- read_csv(speed_supplement_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round)
    )

  stage1 <- stage1 %>%
    left_join(
      speed_supplement,
      by = c("season", "round", "driver_code")
    )
} else {
  stage1 <- stage1 %>%
    mutate(
      max_speed_st_kph = NA_real_,
      race_max_speed_st_kph = NA_real_,
      speed_st_delta_kph = NA_real_
    )
}

meaningful_wet_races <- if (file.exists(wet_races_csv)) {
  read_csv(wet_races_csv, show_col_types = FALSE) %>%
    transmute(
      season = as.integer(season),
      round = as.integer(round),
      race_name = as.character(race_name),
      race_date = as.Date(race_date),
      wet_exclusion_reason = as.character(wet_exclusion_reason)
    ) %>%
    distinct(season, round, .keep_all = TRUE)
} else {
  tibble(
    season = integer(), round = integer(), race_name = character(),
    race_date = as.Date(character()), wet_exclusion_reason = character()
  )
}

historical_prerace_lookup <- stage1 %>%
  group_by(season, round) %>%
  mutate(
    non_pit_grid_max = suppressWarnings(max(grid[grid > 0], na.rm = TRUE)),
    non_pit_grid_max = if_else(is.infinite(non_pit_grid_max), n(), non_pit_grid_max),
    pit_start_order = cumsum(coalesce(as.logical(grid_started_from_pit), FALSE)),
    display_start_position = if_else(
      coalesce(as.logical(grid_started_from_pit), FALSE) & (is.na(grid) | grid <= 0),
      non_pit_grid_max + pit_start_order,
      grid
    )
  ) %>%
  ungroup() %>%
  transmute(
    season,
    round,
    driver_code,
    display_start_position = as.numeric(display_start_position),
    display_quali_position = as.numeric(quali_position),
    display_quali_delta_sec = as.numeric(best_quali_delta_sec),
    display_grid_started_from_pit = as.integer(grid_started_from_pit),
    display_prerace_source = if_else(!is.na(finish_position), "actual", "scheduled"),
    has_explicit_grid_override = !is.na(display_start_position),
    has_explicit_quali_override = !is.na(display_quali_position),
    has_grid_penalty_override = FALSE,
    grid_penalty_places = 0,
    grid_penalty_back_of_grid = 0
  )

future_prerace_lookup <- if (file.exists(future_prerace_estimates_csv)) {
  read_csv(future_prerace_estimates_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      current_grid = as.numeric(current_grid),
      current_quali_position = as.numeric(current_quali_position),
      current_best_quali_delta_sec = as.numeric(current_best_quali_delta_sec),
      current_grid_started_from_pit = as.integer(current_grid_started_from_pit),
      has_explicit_grid_override = coalesce(as.logical(has_explicit_grid_override), FALSE),
      has_explicit_quali_override = coalesce(as.logical(has_explicit_quali_override), FALSE),
      has_grid_penalty_override = coalesce(as.logical(has_grid_penalty_override), FALSE),
      grid_penalty_places = pmax(0, coalesce(as.numeric(grid_penalty_places), 0)),
      grid_penalty_back_of_grid = as.integer(coalesce(as.numeric(grid_penalty_back_of_grid), 0) >= 1)
    ) %>%
    group_by(season, round) %>%
    mutate(
      non_pit_grid_max = suppressWarnings(max(current_grid[current_grid > 0], na.rm = TRUE)),
      non_pit_grid_max = if_else(is.infinite(non_pit_grid_max), n(), non_pit_grid_max),
      pit_start_order = cumsum(coalesce(as.logical(current_grid_started_from_pit), FALSE)),
      display_start_position = if_else(
        coalesce(as.logical(current_grid_started_from_pit), FALSE) & (is.na(current_grid) | current_grid <= 0),
        non_pit_grid_max + pit_start_order,
        current_grid
      )
    ) %>%
    ungroup() %>%
    transmute(
      season,
      round,
      driver_code,
      display_start_position = as.numeric(display_start_position),
      display_quali_position = as.numeric(current_quali_position),
      display_quali_delta_sec = as.numeric(current_best_quali_delta_sec),
      display_grid_started_from_pit = as.integer(current_grid_started_from_pit),
      display_prerace_source = if_else(coalesce(has_prerace_override, FALSE), "override", "estimate"),
      has_explicit_grid_override,
      has_explicit_quali_override,
      has_grid_penalty_override,
      grid_penalty_places,
      grid_penalty_back_of_grid
    )
} else {
  tibble(
    season = integer(),
    round = integer(),
    driver_code = character(),
    display_start_position = numeric(),
    display_quali_position = numeric(),
    display_quali_delta_sec = numeric(),
    display_grid_started_from_pit = integer(),
    display_prerace_source = character(),
    has_explicit_grid_override = logical(),
    has_explicit_quali_override = logical(),
    has_grid_penalty_override = logical(),
    grid_penalty_places = numeric(),
    grid_penalty_back_of_grid = integer()
  )
}

prerace_display_lookup <- bind_rows(
  future_prerace_lookup,
  historical_prerace_lookup
) %>%
  arrange(
    desc(display_prerace_source %in% c("override", "estimate")),
    season,
    round,
    driver_code
  ) %>%
  distinct(season, round, driver_code, .keep_all = TRUE)

schedule <- read_csv(schedule_csv, show_col_types = FALSE) %>%
  mutate(
    season = as.integer(season),
    round = as.integer(round),
    race_date = as.Date(date)
  ) %>%
  select(season, round, race_name, circuit_id, circuit_name, locality, country, race_date)

empty_draftkings_salary_lookup <- tibble(
  season = integer(),
  round = integer(),
  salary_name = character(),
  roster_position = character(),
  draftkings_salary = numeric(),
  draftkings_id = character(),
  draftkings_avg_points = numeric()
)

draftkings_salary_lookup <- if (file.exists(draftkings_salaries_csv)) {
  read_csv(draftkings_salaries_csv, show_col_types = FALSE) %>%
    transmute(
      dk_season = as.integer(str_extract(as.character(`Game Info`), "\\d{4}$")),
      dk_race_name = str_remove(as.character(`Game Info`), "\\s+\\d{4}$"),
      salary_name = as.character(Name),
      roster_position = as.character(`Roster Position`),
      draftkings_salary = as.numeric(Salary),
      draftkings_id = as.character(ID),
      draftkings_avg_points = as.numeric(AvgPointsPerGame)
    ) %>%
    left_join(
      schedule %>% select(season, round, race_name),
      by = c("dk_season" = "season", "dk_race_name" = "race_name")
    ) %>%
    transmute(
      season = dk_season,
      round,
      salary_name,
      roster_position,
      draftkings_salary,
      draftkings_id,
      draftkings_avg_points
    ) %>%
    filter(!is.na(season), !is.na(round), !is.na(draftkings_salary))
} else {
  empty_draftkings_salary_lookup
}

draftkings_driver_salary_lookup <- draftkings_salary_lookup %>%
  filter(roster_position == "D") %>%
  mutate(
    driver_name = case_when(
      salary_name == "Carlos Sainz Jr." ~ "Carlos Sainz",
      salary_name == "Nico Hulkenberg" ~ "Nico Hülkenberg",
      salary_name == "Sergio Perez" ~ "Sergio Pérez",
      TRUE ~ salary_name
    )
  ) %>%
  select(season, round, driver_name, draftkings_salary, draftkings_id, draftkings_avg_points) %>%
  distinct(season, round, driver_name, .keep_all = TRUE)

draftkings_constructor_salary_lookup <- draftkings_salary_lookup %>%
  filter(roster_position == "CNSTR") %>%
  mutate(
    constructor_name = recode(
      salary_name,
      "Red Bull Racing" = "Red Bull",
      "Racing Bulls F1 Team" = "RB F1 Team",
      "Audi F1 Team" = "Audi",
      "Cadillac" = "Cadillac F1 Team",
      "Aston Martin F1 Team" = "Aston Martin",
      .default = salary_name
    )
  ) %>%
  select(season, round, constructor_name, draftkings_salary, draftkings_id, draftkings_avg_points) %>%
  distinct(season, round, constructor_name, .keep_all = TRUE)

family_flags <- c(
  "is_street",
  "is_permanent_road_course",
  "is_high_speed",
  "is_stop_start",
  "is_flowing_high_downforce",
  "is_low_overtake",
  "is_tyre_deg_heavy"
)

family_labels <- c(
  is_street = "Street",
  is_permanent_road_course = "Permanent",
  is_high_speed = "High speed",
  is_stop_start = "Stop-start",
  is_flowing_high_downforce = "Flowing aero",
  is_low_overtake = "Low overtake",
  is_tyre_deg_heavy = "Tyre deg"
)

family_descriptions <- c(
  is_street = "Temporary or street-style circuit. Track evolution, walls, and mistake avoidance matter.",
  is_permanent_road_course = "Purpose-built road circuit. Usually more conventional run-off and rhythm.",
  is_high_speed = "Long straights or sustained fast sections. Power and aero efficiency matter.",
  is_stop_start = "Repeated braking and traction zones. Braking stability and exits matter.",
  is_flowing_high_downforce = "Linked corners and aero load. Balance and downforce confidence matter.",
  is_low_overtake = "Track position is unusually valuable. Qualifying and pit timing loom larger.",
  is_tyre_deg_heavy = "Tyre wear matters. Stint length, compound choice, and race pace are central."
)

cluster_nicknames <- c(
  cluster_1 = "Fast Flowing Power Tracks",
  cluster_2 = "Fast Street / Low-Overtake Hybrids",
  cluster_3 = "Power Stop-Start Permanents",
  cluster_4 = "Stop-Start Street Circuits",
  cluster_5 = "Technical Flowing Permanents"
)

cluster_display_number <- function(cluster_id) {
  str_replace(coalesce(as.character(cluster_id), ""), "^cluster_", "")
}

cluster_display_name <- function(cluster_id, fallback = NA_character_) {
  cluster_id <- as.character(cluster_id)
  nickname <- unname(cluster_nicknames[cluster_id])
  coalesce(nickname, fallback, cluster_id)
}

cluster_display_label <- function(cluster_id, fallback = NA_character_) {
  number <- cluster_display_number(cluster_id)
  name <- cluster_display_name(cluster_id, fallback)
  if_else(
    is.na(number) | number == "",
    coalesce(name, "Unclustered"),
    paste0("Cluster ", number, " - ", coalesce(name, "Unlabeled"))
  )
}

result_events_from_prediction_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble(season = integer(), round = integer(), has_results = logical()))
  }

  rows <- suppressMessages(read_csv(path, show_col_types = FALSE))
  actual_cols <- if ("actual_rank_in_race" %in% names(rows)) {
    "actual_rank_in_race"
  } else {
    intersect(c("finish_position", "actual_finish_position"), names(rows))
  }

  if (!all(c("season", "round") %in% names(rows)) || length(actual_cols) == 0) {
    return(tibble(season = integer(), round = integer(), has_results = logical()))
  }

  rows %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round)
    ) %>%
    filter(if_any(all_of(actual_cols), ~ !is.na(.x))) %>%
    distinct(season, round) %>%
    mutate(has_results = TRUE)
}

result_events <- bind_rows(
  stage1 %>%
    filter(!is.na(finish_position)) %>%
    distinct(season, round) %>%
    mutate(has_results = TRUE),
  bind_rows(lapply(
    c(
      rf_predictions_csv,
      rf_probability_predictions_csv,
      qualifying_predictions_csv,
      xgb_finish_predictions_csv,
      xgb_probability_predictions_csv,
      xgb_points_predictions_csv,
      xgb_winner_without_predictions_csv
    ),
    result_events_from_prediction_csv
  ))
) %>%
  filter(!is.na(season), !is.na(round)) %>%
  distinct(season, round, .keep_all = TRUE)

circuit_family_lookup <- stage1 %>%
  filter(!is.na(circuit_id)) %>%
  arrange(desc(season), desc(round)) %>%
  group_by(circuit_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(circuit_id, all_of(family_flags))

circuit_track_cluster_template <- tibble(
  circuit_id = character(),
  track_cluster_id = character(),
  track_cluster_label = character(),
  cluster_peer_circuits = character(),
  track_profile_id = character(),
  track_profile_label = character()
)

circuit_track_cluster_lookup <- if (file.exists(track_profile_inventory_csv)) {
  track_profile_inventory_rows <- read_csv(track_profile_inventory_csv, show_col_types = FALSE)

  if ("circuit_id" %in% names(track_profile_inventory_rows)) {
    track_profile_inventory_rows %>%
      select(any_of(c("circuit_id", "track_cluster_id", "track_cluster_label", "cluster_peer_circuits", "track_profile_id", "track_profile_label"))) %>%
      filter(!is.na(circuit_id)) %>%
      arrange(circuit_id) %>%
      distinct(circuit_id, .keep_all = TRUE)
  } else {
    circuit_track_cluster_template
  }
} else {
  circuit_track_cluster_template
}

race_choices <- schedule %>%
  left_join(result_events, by = c("season", "round")) %>%
  left_join(circuit_family_lookup, by = "circuit_id") %>%
  left_join(circuit_track_cluster_lookup, by = "circuit_id") %>%
  mutate(has_results = coalesce(has_results, FALSE)) %>%
  arrange(desc(season), round) %>%
  mutate(
    status_label = if_else(has_results, "", " - scheduled"),
    label = paste0("R", sprintf("%02d", round), " - ", race_name, " (", circuit_name, ")", status_label)
  )

default_race_round <- function(choices) {
  if (nrow(choices) == 0) return(NULL)

  choices <- choices %>%
    mutate(
      round = as.integer(round),
      race_date = as.Date(if ("race_date" %in% names(.)) race_date else NA),
      has_results = if ("has_results" %in% names(.)) coalesce(as.logical(has_results), FALSE) else FALSE
    )

  upcoming <- choices %>%
    filter(!has_results) %>%
    arrange(race_date, round)

  if (nrow(upcoming) > 0) return(upcoming$round[[1]])

  completed <- choices %>%
    filter(has_results) %>%
    arrange(desc(race_date), desc(round))

  if (nrow(completed) > 0) return(completed$round[[1]])

  min(choices$round, na.rm = TRUE)
}

empty_rf_predictions <- tibble(
  model = character(),
  model_label = character(),
  selected_model = logical(),
  maxnodes = numeric(),
  mean_terminal_nodes = numeric(),
  data_split = character(),
  season = integer(),
  round = integer(),
  is_wet_race = logical(),
  wet_exclusion_reason = character(),
  driver_id = character(),
  driver_code = character(),
  race_date = as.Date(character()),
  race_name = character(),
  driver_name = character(),
  constructor_name = character(),
  finish_position = numeric(),
  predicted_finish_position = numeric(),
  predicted_rank_in_race = integer(),
  actual_rank_in_race = integer(),
  predicted_winner = logical(),
  actual_winner = logical(),
  winner_pick_correct = logical(),
  predicted_podium = logical(),
  actual_podium = logical(),
  podium_pick_correct = logical(),
  win_avg_american_odds = numeric(),
  win_avg_american_odds_label = character(),
  win_market_no_vig_probability = numeric(),
  podium_avg_american_odds = numeric(),
  podium_avg_american_odds_label = character(),
  podium_market_no_vig_probability = numeric(),
  podium_effective_avg_american_odds = numeric(),
  podium_effective_avg_american_odds_label = character(),
  podium_effective_no_vig_probability = numeric(),
  podium_odds_source = character()
)

prepare_prediction_rows <- function(rows, empty_template) {
  if (!all(c("season", "round") %in% names(rows))) {
    return(empty_template)
  }

  for (col in setdiff(names(empty_template), names(rows))) {
    template_col <- empty_template[[col]]
    rows[[col]] <- if (inherits(template_col, "Date")) {
      as.Date(rep(NA_character_, nrow(rows)))
    } else if (is.logical(template_col)) {
      rep(NA, nrow(rows))
    } else if (is.integer(template_col)) {
      rep(NA_integer_, nrow(rows))
    } else if (is.numeric(template_col)) {
      rep(NA_real_, nrow(rows))
    } else {
      rep(NA_character_, nrow(rows))
    }
  }

  for (col in intersect(names(empty_template), names(rows))) {
    template_col <- empty_template[[col]]
    rows[[col]] <- if (inherits(template_col, "Date")) {
      as.Date(rows[[col]])
    } else if (is.logical(template_col)) {
      as.logical(rows[[col]])
    } else if (is.integer(template_col)) {
      as.integer(rows[[col]])
    } else if (is.numeric(template_col)) {
      as.numeric(rows[[col]])
    } else {
      as.character(rows[[col]])
    }
  }

  if (!"selected_model" %in% names(rows) || all(is.na(rows$selected_model))) {
    rows$selected_model <- TRUE
  }
  rows
}

fmt_american_label <- function(x) {
  if_else(is.na(x), "", if_else(x > 0, paste0("+", as.integer(round(x))), as.character(as.integer(round(x)))))
}

american_to_decimal_numeric <- function(x) {
  odds <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(odds) ~ NA_real_,
    odds > 0 ~ 1 + odds / 100,
    odds < 0 ~ 1 + 100 / abs(odds),
    TRUE ~ NA_real_
  )
}

decimal_to_american_numeric <- function(x) {
  decimal <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(decimal) | decimal <= 1 ~ NA_real_,
    decimal >= 2 ~ (decimal - 1) * 100,
    TRUE ~ -100 / (decimal - 1)
  )
}

implied_prob_from_decimal <- function(x) {
  if_else(!is.na(x) & x > 1, 1 / x, NA_real_)
}

load_market_odds_lookup <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(tibble(season = integer(), round = integer(), driver_code = character()))
  }

  odds_raw <- read_csv(path, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      driver_code = str_to_upper(as.character(driver_code)),
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      win_avg_decimal_odds = if ("win_avg_decimal_odds" %in% names(.)) as.numeric(win_avg_decimal_odds) else NA_real_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_avg_decimal_odds = if ("podium_avg_decimal_odds" %in% names(.)) as.numeric(podium_avg_decimal_odds) else NA_real_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      win_avg_decimal_odds = coalesce(win_avg_decimal_odds, american_to_decimal_numeric(win_avg_american_odds)),
      podium_avg_decimal_odds = coalesce(podium_avg_decimal_odds, american_to_decimal_numeric(podium_avg_american_odds)),
      podium_estimated_probability = if ("podium_estimated_probability" %in% names(.)) as.numeric(podium_estimated_probability) else NA_real_,
      podium_estimated_decimal_odds = if ("podium_estimated_decimal_odds" %in% names(.)) as.numeric(podium_estimated_decimal_odds) else NA_real_,
      podium_estimated_american_odds = if ("podium_estimated_american_odds" %in% names(.)) as.numeric(podium_estimated_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else NA_real_,
      podium_effective_avg_decimal_odds = if ("podium_effective_avg_decimal_odds" %in% names(.)) as.numeric(podium_effective_avg_decimal_odds) else NA_real_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else NA_real_,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) na_if(as.character(podium_odds_source), "") else NA_character_
    ) %>%
    group_by(season, round) %>%
    mutate(
      podium_market_row_count = sum(!is.na(podium_avg_decimal_odds)),
      podium_market_probability_total = sum(podium_market_no_vig_probability, na.rm = TRUE),
      podium_market_place_scale = if_else(
        podium_market_row_count >= 10L &
          podium_market_probability_total >= 0.75 &
          podium_market_probability_total <= 1.25,
        3,
        1
      ),
      podium_market_no_vig_probability = pmin(1, podium_market_no_vig_probability * podium_market_place_scale),
      podium_effective_no_vig_probability = if_else(
        !is.na(podium_avg_decimal_odds),
        pmin(1, podium_effective_no_vig_probability * podium_market_place_scale),
        podium_effective_no_vig_probability
      )
    ) %>%
    ungroup() %>%
    select(-podium_market_row_count, -podium_market_probability_total, -podium_market_place_scale)

  podium_estimation_rows <- odds_raw %>%
    filter(
      !is.na(win_avg_decimal_odds),
      !is.na(podium_avg_decimal_odds),
      win_avg_decimal_odds > 1,
      podium_avg_decimal_odds > 1
    ) %>%
    mutate(
      win_logit = qlogis(pmin(pmax(implied_prob_from_decimal(win_avg_decimal_odds), 0.001), 0.999)),
      podium_logit = qlogis(pmin(pmax(implied_prob_from_decimal(podium_avg_decimal_odds), 0.001), 0.999))
    )

  podium_estimation_model <- if (nrow(podium_estimation_rows) >= 25) {
    lm(podium_logit ~ win_logit, data = podium_estimation_rows)
  } else {
    NULL
  }

  if (!is.null(podium_estimation_model)) {
    odds_raw <- odds_raw %>%
      mutate(
        win_logit_for_podium = qlogis(pmin(pmax(implied_prob_from_decimal(win_avg_decimal_odds), 0.001), 0.999)),
        podium_estimated_probability = coalesce(
          podium_estimated_probability,
          if_else(
            is.na(podium_avg_decimal_odds) & !is.na(win_logit_for_podium),
            plogis(predict(podium_estimation_model, newdata = tibble(win_logit = win_logit_for_podium))),
            NA_real_
          )
        ),
        podium_estimated_decimal_odds = coalesce(
          podium_estimated_decimal_odds,
          if_else(!is.na(podium_estimated_probability) & podium_estimated_probability > 0, 1 / podium_estimated_probability, NA_real_)
        ),
        podium_estimated_american_odds = coalesce(podium_estimated_american_odds, decimal_to_american_numeric(podium_estimated_decimal_odds))
      ) %>%
      select(-win_logit_for_podium)
  }

  odds_raw %>%
    mutate(
      podium_effective_avg_american_odds = coalesce(podium_effective_avg_american_odds, podium_avg_american_odds, podium_estimated_american_odds),
      podium_effective_avg_decimal_odds = coalesce(podium_effective_avg_decimal_odds, podium_avg_decimal_odds, podium_estimated_decimal_odds),
      podium_effective_no_vig_probability = coalesce(podium_effective_no_vig_probability, podium_market_no_vig_probability, podium_estimated_probability),
      podium_odds_source = case_when(
        !is.na(podium_avg_american_odds) ~ "market",
        !is.na(podium_estimated_american_odds) | !is.na(podium_effective_avg_american_odds) ~ coalesce(podium_odds_source, "estimated_from_win"),
        TRUE ~ coalesce(podium_odds_source, "missing")
      ),
      win_avg_american_odds_label = fmt_american_label(win_avg_american_odds),
      podium_avg_american_odds_label = fmt_american_label(podium_avg_american_odds),
      podium_effective_avg_american_odds_label = paste0(
        if_else(podium_odds_source == "estimated_from_win", "est ", ""),
        fmt_american_label(podium_effective_avg_american_odds)
      ),
      podium_effective_avg_american_odds_label = if_else(
        is.na(podium_effective_avg_american_odds),
        "",
        podium_effective_avg_american_odds_label
      )
    ) %>%
    select(
      season, round, driver_code,
      win_avg_american_odds, win_avg_decimal_odds, win_avg_american_odds_label, win_market_no_vig_probability,
      podium_avg_american_odds, podium_avg_decimal_odds, podium_avg_american_odds_label, podium_market_no_vig_probability,
      podium_effective_avg_american_odds, podium_effective_avg_decimal_odds, podium_effective_avg_american_odds_label,
      podium_effective_no_vig_probability, podium_odds_source
    ) %>%
    distinct(season, round, driver_code, .keep_all = TRUE)
}

empty_opening_market_odds_lookup <- tibble(
  season = integer(),
  round = integer(),
  driver_code = character(),
  opening_win_avg_american_odds = numeric(),
  opening_win_avg_decimal_odds = numeric(),
  opening_win_market_no_vig_probability = numeric()
)

empty_grid_market_odds_lookup <- tibble(
  season = integer(),
  round = integer(),
  driver_code = character(),
  grid_win_avg_american_odds = numeric(),
  grid_win_avg_decimal_odds = numeric(),
  grid_win_market_no_vig_probability = numeric()
)

market_odds_lookup <- load_market_odds_lookup(market_odds_summary_csv)
opening_market_odds_base <- load_market_odds_lookup(opening_market_odds_summary_csv)
opening_market_odds_lookup <- if (
  nrow(opening_market_odds_base) == 0 ||
    !all(c("win_avg_american_odds", "win_avg_decimal_odds", "win_market_no_vig_probability") %in% names(opening_market_odds_base))
) {
  empty_opening_market_odds_lookup
} else {
  opening_market_odds_base %>%
    transmute(
      season, round, driver_code,
      opening_win_avg_american_odds = win_avg_american_odds,
      opening_win_avg_decimal_odds = win_avg_decimal_odds,
      opening_win_market_no_vig_probability = win_market_no_vig_probability
    )
}

grid_market_odds_base <- load_market_odds_lookup(grid_market_odds_summary_csv)
grid_market_odds_lookup <- if (
  nrow(grid_market_odds_base) == 0 ||
    !all(c("win_avg_american_odds", "win_avg_decimal_odds", "win_market_no_vig_probability") %in% names(grid_market_odds_base))
) {
  empty_grid_market_odds_lookup
} else {
  grid_market_odds_base %>%
    transmute(
      season, round, driver_code,
      grid_win_avg_american_odds = win_avg_american_odds,
      grid_win_avg_decimal_odds = win_avg_decimal_odds,
      grid_win_market_no_vig_probability = win_market_no_vig_probability
    )
}


latest_market_odds_lookup <- bind_rows(
  grid_market_odds_base %>% mutate(.odds_priority = 1L),
  market_odds_lookup %>% mutate(.odds_priority = 2L)
) %>%
  arrange(.odds_priority) %>%
  select(-.odds_priority) %>%
  distinct(season, round, driver_code, .keep_all = TRUE)

overlay_latest_market_odds <- function(rows) {
  overlay_market_odds(rows, latest_market_odds_lookup)
}

empty_qualifying_market_odds_lookup <- tibble(
  season = integer(),
  round = integer(),
  driver_code = character(),
  pole_current_american_odds = numeric(),
  pole_current_decimal_odds = numeric(),
  pole_current_no_vig_probability = numeric(),
  pole_opening_american_odds = numeric(),
  pole_opening_decimal_odds = numeric(),
  pole_opening_no_vig_probability = numeric()
)

load_qualifying_market_odds_lookup <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(empty_qualifying_market_odds_lookup)
  }

  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      driver_code = str_to_upper(as.character(driver_code)),
      pole_current_american_odds = if ("pole_current_american_odds" %in% names(.)) as.numeric(pole_current_american_odds) else NA_real_,
      pole_current_decimal_odds = if ("pole_current_decimal_odds" %in% names(.)) as.numeric(pole_current_decimal_odds) else american_to_decimal_numeric(pole_current_american_odds),
      pole_current_no_vig_probability = if ("pole_current_no_vig_probability" %in% names(.)) as.numeric(pole_current_no_vig_probability) else implied_prob_from_decimal(pole_current_decimal_odds),
      pole_opening_american_odds = if ("pole_opening_american_odds" %in% names(.)) as.numeric(pole_opening_american_odds) else NA_real_,
      pole_opening_decimal_odds = if ("pole_opening_decimal_odds" %in% names(.)) as.numeric(pole_opening_decimal_odds) else american_to_decimal_numeric(pole_opening_american_odds),
      pole_opening_no_vig_probability = if ("pole_opening_no_vig_probability" %in% names(.)) as.numeric(pole_opening_no_vig_probability) else implied_prob_from_decimal(pole_opening_decimal_odds)
    ) %>%
    select(
      season, round, driver_code,
      pole_current_american_odds, pole_current_decimal_odds, pole_current_no_vig_probability,
      pole_opening_american_odds, pole_opening_decimal_odds, pole_opening_no_vig_probability
    ) %>%
    distinct(season, round, driver_code, .keep_all = TRUE) %>%
    bind_rows(empty_qualifying_market_odds_lookup) %>%
    distinct(season, round, driver_code, .keep_all = TRUE)
}

qualifying_market_odds_lookup <- load_qualifying_market_odds_lookup(qualifying_market_odds_csv)

empty_winner_without_market_odds_lookup <- tibble(
  season = integer(),
  round = integer(),
  driver_code = character(),
  ww_current_american_odds = numeric(),
  ww_current_decimal_odds = numeric(),
  ww_current_no_vig_probability = numeric()
)

load_winner_without_market_odds_lookup <- function(path) {
  if (is.na(path) || !file.exists(path)) {
    return(empty_winner_without_market_odds_lookup)
  }

  read_csv(path, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      driver_code = str_to_upper(as.character(driver_code)),
      ww_current_american_odds = if ("american_odds" %in% names(.)) as.numeric(american_odds) else NA_real_,
      ww_current_decimal_odds = if ("decimal_odds" %in% names(.)) as.numeric(decimal_odds) else american_to_decimal_numeric(ww_current_american_odds),
      ww_current_no_vig_probability = if ("market_no_vig_probability" %in% names(.)) as.numeric(market_no_vig_probability) else implied_prob_from_decimal(ww_current_decimal_odds)
    ) %>%
    select(season, round, driver_code, ww_current_american_odds, ww_current_decimal_odds, ww_current_no_vig_probability) %>%
    distinct(season, round, driver_code, .keep_all = TRUE) %>%
    bind_rows(empty_winner_without_market_odds_lookup) %>%
    distinct(season, round, driver_code, .keep_all = TRUE)
}

winner_without_market_odds_lookup <- load_winner_without_market_odds_lookup(winner_without_market_odds_csv)

ensure_columns <- function(rows, cols, default = NA_real_) {
  for (col in cols) {
    if (!col %in% names(rows)) {
      rows[[col]] <- default
    }
  }
  rows
}

overlay_market_odds <- function(rows, odds_lookup = market_odds_lookup) {
  if (nrow(rows) == 0 || nrow(odds_lookup) == 0 || !all(c("season", "round", "driver_code") %in% names(rows))) {
    return(rows)
  }

  odds_cols <- setdiff(names(odds_lookup), c("season", "round", "driver_code"))
  joined <- rows %>%
    mutate(driver_code = str_to_upper(as.character(driver_code))) %>%
    left_join(odds_lookup, by = c("season", "round", "driver_code"), suffix = c("", "_lookup"))

  for (col in odds_cols) {
    lookup_col <- paste0(col, "_lookup")
    if (lookup_col %in% names(joined)) {
      joined[[col]] <- dplyr::coalesce(joined[[lookup_col]], joined[[col]])
    } else if (!col %in% names(joined)) {
      joined[[col]] <- NA
    }
  }

  joined %>% select(-any_of(paste0(odds_cols, "_lookup")))
}

add_expected_win_moneyline_impact <- function(rows, base_probability_col = "base_win_probability", adjusted_probability_col = "adjusted_win_probability") {
  if (nrow(rows) == 0 || !all(c(base_probability_col, adjusted_probability_col) %in% names(rows))) {
    return(rows)
  }

  opening_cols <- c(
    "opening_win_avg_american_odds",
    "opening_win_avg_decimal_odds",
    "opening_win_market_no_vig_probability"
  )

  grid_cols <- c(
    "grid_win_avg_american_odds",
    "grid_win_avg_decimal_odds",
    "grid_win_market_no_vig_probability"
  )

  rows %>%
    overlay_market_odds() %>%
    select(-any_of(c(opening_cols, grid_cols))) %>%
    left_join(opening_market_odds_lookup, by = c("season", "round", "driver_code")) %>%
    left_join(grid_market_odds_lookup, by = c("season", "round", "driver_code")) %>%
    ensure_columns(c(
      "win_avg_american_odds",
      "win_avg_decimal_odds",
      "win_market_no_vig_probability",
      opening_cols,
      grid_cols
    )) %>%
    mutate(
      win_current_american_odds = coalesce(win_avg_american_odds, decimal_to_american_numeric(win_avg_decimal_odds)),
      win_current_market_probability = coalesce(win_market_no_vig_probability, implied_prob_from_decimal(win_avg_decimal_odds)),
      win_chatter_anchor_probability = coalesce(opening_win_market_no_vig_probability, win_current_market_probability),
      chatter_win_probability_nudge = .data[[adjusted_probability_col]] - .data[[base_probability_col]],
      expected_win_market_probability = if_else(
        !is.na(win_chatter_anchor_probability) & !is.na(chatter_win_probability_nudge),
        clamp_probability_numeric(win_chatter_anchor_probability + chatter_win_probability_nudge),
        NA_real_
      ),
      expected_win_american_odds = probability_to_american_numeric(expected_win_market_probability),
      expected_win_moneyline_delta = expected_win_american_odds - opening_win_avg_american_odds,
      actual_win_moneyline_delta = win_current_american_odds - opening_win_avg_american_odds,
      grid_win_moneyline_delta = grid_win_avg_american_odds - opening_win_avg_american_odds,
      grid_vs_chatter_moneyline_delta = grid_win_avg_american_odds - win_current_american_odds,
      win_current_american_odds_label = fmt_american_label(win_current_american_odds),
      opening_win_avg_american_odds_label = fmt_american_label(opening_win_avg_american_odds),
      grid_win_avg_american_odds_label = fmt_american_label(grid_win_avg_american_odds),
      expected_win_american_odds_label = fmt_american_label(expected_win_american_odds),
      expected_win_moneyline_delta_label = format_signed_moneyline_delta(expected_win_moneyline_delta),
      actual_win_moneyline_delta_label = format_signed_moneyline_delta(actual_win_moneyline_delta),
      grid_win_moneyline_delta_label = format_signed_moneyline_delta(grid_win_moneyline_delta),
      grid_vs_chatter_moneyline_delta_label = format_signed_moneyline_delta(grid_vs_chatter_moneyline_delta)
    )
}

add_expected_pole_moneyline_impact <- function(rows) {
  if (nrow(rows) == 0 || !all(c("base_quali_rank", "adjusted_quali_rank") %in% names(rows))) {
    return(rows)
  }

  pole_cols <- c(
    "pole_current_american_odds",
    "pole_current_decimal_odds",
    "pole_current_no_vig_probability",
    "pole_opening_american_odds",
    "pole_opening_decimal_odds",
    "pole_opening_no_vig_probability"
  )

  rows %>%
    group_by(season, round) %>%
    mutate(
      qualifying_pole_rank_score = 1 / pmax(base_quali_rank, 1),
      qualifying_implied_pole_probability = qualifying_pole_rank_score / sum(qualifying_pole_rank_score, na.rm = TRUE),
      base_pole_share = exp(-0.75 * (base_quali_rank - 1)),
      adjusted_pole_share = exp(-0.75 * (adjusted_quali_rank - 1)),
      base_pole_share = base_pole_share / sum(base_pole_share, na.rm = TRUE),
      adjusted_pole_share = adjusted_pole_share / sum(adjusted_pole_share, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(-any_of(pole_cols)) %>%
    left_join(qualifying_market_odds_lookup, by = c("season", "round", "driver_code")) %>%
    ensure_columns(pole_cols) %>%
    mutate(
      chatter_pole_probability_nudge = adjusted_pole_share - base_pole_share,
      expected_pole_market_probability = if_else(
        !is.na(pole_current_no_vig_probability) & !is.na(chatter_pole_probability_nudge),
        clamp_probability_numeric(pole_current_no_vig_probability + chatter_pole_probability_nudge),
        NA_real_
      ),
      expected_pole_american_odds = probability_to_american_numeric(expected_pole_market_probability),
      expected_pole_moneyline_baseline = coalesce(pole_opening_american_odds, pole_current_american_odds),
      expected_pole_moneyline_delta = expected_pole_american_odds - expected_pole_moneyline_baseline,
      actual_pole_moneyline_delta = pole_current_american_odds - pole_opening_american_odds,
      qualifying_implied_pole_american_odds = probability_to_american_numeric(qualifying_implied_pole_probability),
      pole_model_implied_base_american_odds = probability_to_american_numeric(base_pole_share),
      pole_model_implied_adjusted_american_odds = probability_to_american_numeric(adjusted_pole_share),
      pole_current_american_odds_label = fmt_american_label(pole_current_american_odds),
      pole_opening_american_odds_label = fmt_american_label(pole_opening_american_odds),
      qualifying_implied_pole_american_odds_label = fmt_american_label(qualifying_implied_pole_american_odds),
      expected_pole_american_odds_label = fmt_american_label(expected_pole_american_odds),
      expected_pole_moneyline_delta_label = format_signed_moneyline_delta(expected_pole_moneyline_delta),
      actual_pole_moneyline_delta_label = format_signed_moneyline_delta(actual_pole_moneyline_delta),
      pole_model_implied_base_american_odds_label = fmt_american_label(pole_model_implied_base_american_odds),
      pole_model_implied_adjusted_american_odds_label = fmt_american_label(pole_model_implied_adjusted_american_odds)
    )
}

rf_predictions <- if (file.exists(rf_predictions_csv)) {
  read_csv(rf_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions) %>%
    mutate(
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else podium_avg_american_odds,
      win_avg_american_odds_label = if ("win_avg_american_odds_label" %in% names(.)) win_avg_american_odds_label else NA_character_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds_label = if ("podium_avg_american_odds_label" %in% names(.)) podium_avg_american_odds_label else NA_character_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      podium_effective_avg_american_odds_label = if ("podium_effective_avg_american_odds_label" %in% names(.)) podium_effective_avg_american_odds_label else NA_character_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else podium_market_no_vig_probability,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) as.character(podium_odds_source) else if_else(!is.na(podium_avg_american_odds), "market", "missing"),
      podium_odds_source = coalesce(podium_odds_source, if_else(!is.na(podium_avg_american_odds), "market", "missing")),
      win_avg_american_odds_label = if_else(
        !is.na(win_avg_american_odds),
        if_else(win_avg_american_odds > 0, paste0("+", as.integer(round(win_avg_american_odds))), as.character(as.integer(round(win_avg_american_odds)))),
        coalesce(as.character(win_avg_american_odds_label), "")
      ),
      podium_avg_american_odds_label = if_else(
        !is.na(podium_avg_american_odds),
        if_else(podium_avg_american_odds > 0, paste0("+", as.integer(round(podium_avg_american_odds))), as.character(as.integer(round(podium_avg_american_odds)))),
        coalesce(as.character(podium_avg_american_odds_label), "")
      ),
      podium_effective_avg_american_odds_label = if_else(
        !is.na(podium_effective_avg_american_odds),
        paste0(
          if_else(podium_odds_source == "estimated_from_win", "est ", ""),
          if_else(
            podium_effective_avg_american_odds > 0,
            paste0("+", as.integer(round(podium_effective_avg_american_odds))),
            as.character(as.integer(round(podium_effective_avg_american_odds)))
          )
        ),
        coalesce(as.character(podium_effective_avg_american_odds_label), "")
      )
    ) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      predicted_finish_position = as.numeric(predicted_finish_position),
      predicted_rank_in_race = as.integer(predicted_rank_in_race),
      actual_rank_in_race = as.integer(actual_rank_in_race)
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_rf_predictions
}

empty_rf_probability_predictions <- empty_rf_predictions %>%
  mutate(
    predicted_win_probability = numeric(),
    predicted_podium_probability = numeric(),
    predicted_win_rank = integer(),
    predicted_podium_rank = integer()
  )

rf_probability_predictions <- if (file.exists(rf_probability_predictions_csv)) {
  read_csv(rf_probability_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_probability_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      predicted_win_probability = as.numeric(predicted_win_probability),
      predicted_podium_probability = as.numeric(predicted_podium_probability),
      predicted_win_rank = as.integer(predicted_win_rank),
      predicted_podium_rank = as.integer(predicted_podium_rank),
      predicted_rank_in_race = predicted_win_rank,
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else podium_avg_american_odds,
      win_avg_american_odds_label = if ("win_avg_american_odds_label" %in% names(.)) as.character(win_avg_american_odds_label) else NA_character_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds_label = if ("podium_avg_american_odds_label" %in% names(.)) as.character(podium_avg_american_odds_label) else NA_character_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      podium_effective_avg_american_odds_label = if ("podium_effective_avg_american_odds_label" %in% names(.)) as.character(podium_effective_avg_american_odds_label) else NA_character_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else podium_market_no_vig_probability,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) as.character(podium_odds_source) else if_else(!is.na(podium_avg_american_odds), "market", "missing"),
      podium_odds_source = coalesce(podium_odds_source, if_else(!is.na(podium_avg_american_odds), "market", "missing")),
      win_avg_american_odds_label = if_else(
        !is.na(win_avg_american_odds),
        if_else(win_avg_american_odds > 0, paste0("+", as.integer(round(win_avg_american_odds))), as.character(as.integer(round(win_avg_american_odds)))),
        coalesce(as.character(win_avg_american_odds_label), "")
      ),
      podium_avg_american_odds_label = if_else(
        !is.na(podium_avg_american_odds),
        if_else(podium_avg_american_odds > 0, paste0("+", as.integer(round(podium_avg_american_odds))), as.character(as.integer(round(podium_avg_american_odds)))),
        coalesce(as.character(podium_avg_american_odds_label), "")
      ),
      podium_effective_avg_american_odds_label = if_else(
        !is.na(podium_effective_avg_american_odds),
        paste0(
          if_else(podium_odds_source == "estimated_from_win", "est ", ""),
          if_else(
            podium_effective_avg_american_odds > 0,
            paste0("+", as.integer(round(podium_effective_avg_american_odds))),
            as.character(as.integer(round(podium_effective_avg_american_odds)))
          )
        ),
        coalesce(podium_effective_avg_american_odds_label, "")
      )
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_rf_probability_predictions
}

xgb_finish_predictions <- if (file.exists(xgb_finish_predictions_csv)) {
  read_csv(xgb_finish_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      predicted_finish_position = as.numeric(predicted_finish_position),
      predicted_rank_in_race = as.integer(predicted_rank_in_race),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else podium_avg_american_odds,
      win_avg_american_odds_label = if ("win_avg_american_odds_label" %in% names(.)) as.character(win_avg_american_odds_label) else NA_character_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds_label = if ("podium_avg_american_odds_label" %in% names(.)) as.character(podium_avg_american_odds_label) else NA_character_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      podium_effective_avg_american_odds_label = if ("podium_effective_avg_american_odds_label" %in% names(.)) as.character(podium_effective_avg_american_odds_label) else NA_character_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else podium_market_no_vig_probability,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) as.character(podium_odds_source) else if_else(!is.na(podium_avg_american_odds), "market", "missing"),
      podium_odds_source = coalesce(podium_odds_source, if_else(!is.na(podium_avg_american_odds), "market", "missing")),
      win_avg_american_odds_label = if_else(
        !is.na(win_avg_american_odds),
        if_else(win_avg_american_odds > 0, paste0("+", as.integer(round(win_avg_american_odds))), as.character(as.integer(round(win_avg_american_odds)))),
        coalesce(as.character(win_avg_american_odds_label), "")
      ),
      podium_avg_american_odds_label = if_else(
        !is.na(podium_avg_american_odds),
        if_else(podium_avg_american_odds > 0, paste0("+", as.integer(round(podium_avg_american_odds))), as.character(as.integer(round(podium_avg_american_odds)))),
        coalesce(as.character(podium_avg_american_odds_label), "")
      ),
      podium_effective_avg_american_odds_label = if_else(
        !is.na(podium_effective_avg_american_odds),
        paste0(
          if_else(podium_odds_source == "estimated_from_win", "est ", ""),
          if_else(
            podium_effective_avg_american_odds > 0,
            paste0("+", as.integer(round(podium_effective_avg_american_odds))),
            as.character(as.integer(round(podium_effective_avg_american_odds)))
          )
        ),
        coalesce(podium_effective_avg_american_odds_label, "")
      )
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_rf_predictions
}

wet_dry_prediction_history <- if (file.exists(xgb_finish_predictions_csv)) {
  read_csv(xgb_finish_predictions_csv, show_col_types = FALSE) %>%
    transmute(
      model = as.character(model),
      season = as.integer(season),
      round = as.integer(round),
      race_name = as.character(race_name),
      driver_code = as.character(driver_code),
      driver_name = as.character(driver_name),
      finish_position = as.numeric(finish_position),
      predicted_finish_position = as.numeric(predicted_finish_position)
    )
} else {
  tibble(
    model = character(), season = integer(), round = integer(), race_name = character(),
    driver_code = character(), driver_name = character(), finish_position = numeric(),
    predicted_finish_position = numeric()
  )
}

xgb_probability_predictions <- if (file.exists(xgb_probability_predictions_csv)) {
  read_csv(xgb_probability_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_probability_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      predicted_win_probability = as.numeric(predicted_win_probability),
      predicted_podium_probability = as.numeric(predicted_podium_probability),
      predicted_win_rank = as.integer(predicted_win_rank),
      predicted_podium_rank = as.integer(predicted_podium_rank),
      predicted_rank_in_race = predicted_win_rank,
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else podium_avg_american_odds,
      win_avg_american_odds_label = if ("win_avg_american_odds_label" %in% names(.)) as.character(win_avg_american_odds_label) else NA_character_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds_label = if ("podium_avg_american_odds_label" %in% names(.)) as.character(podium_avg_american_odds_label) else NA_character_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      podium_effective_avg_american_odds_label = if ("podium_effective_avg_american_odds_label" %in% names(.)) as.character(podium_effective_avg_american_odds_label) else NA_character_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else podium_market_no_vig_probability,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) as.character(podium_odds_source) else if_else(!is.na(podium_avg_american_odds), "market", "missing"),
      podium_odds_source = coalesce(podium_odds_source, if_else(!is.na(podium_avg_american_odds), "market", "missing")),
      win_avg_american_odds_label = coalesce(win_avg_american_odds_label, ""),
      podium_avg_american_odds_label = coalesce(podium_avg_american_odds_label, ""),
      podium_effective_avg_american_odds_label = if_else(
        !is.na(podium_effective_avg_american_odds),
        paste0(
          if_else(podium_odds_source == "estimated_from_win", "est ", ""),
          if_else(
            podium_effective_avg_american_odds > 0,
            paste0("+", as.integer(round(podium_effective_avg_american_odds))),
            as.character(as.integer(round(podium_effective_avg_american_odds)))
          )
        ),
        coalesce(podium_effective_avg_american_odds_label, "")
      )
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_rf_probability_predictions
}

xgb_points_predictions <- if (file.exists(xgb_points_predictions_csv)) {
  read_csv(xgb_points_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      points = if ("points" %in% names(.)) as.numeric(points) else NA_real_,
      predicted_points = as.numeric(predicted_points),
      predicted_rank_in_race = as.integer(predicted_rank_in_race),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      win_avg_american_odds = if ("win_avg_american_odds" %in% names(.)) as.numeric(win_avg_american_odds) else NA_real_,
      podium_avg_american_odds = if ("podium_avg_american_odds" %in% names(.)) as.numeric(podium_avg_american_odds) else NA_real_,
      podium_effective_avg_american_odds = if ("podium_effective_avg_american_odds" %in% names(.)) as.numeric(podium_effective_avg_american_odds) else podium_avg_american_odds,
      win_avg_american_odds_label = if ("win_avg_american_odds_label" %in% names(.)) as.character(win_avg_american_odds_label) else NA_character_,
      win_market_no_vig_probability = if ("win_market_no_vig_probability" %in% names(.)) as.numeric(win_market_no_vig_probability) else NA_real_,
      podium_avg_american_odds_label = if ("podium_avg_american_odds_label" %in% names(.)) as.character(podium_avg_american_odds_label) else NA_character_,
      podium_market_no_vig_probability = if ("podium_market_no_vig_probability" %in% names(.)) as.numeric(podium_market_no_vig_probability) else NA_real_,
      podium_effective_avg_american_odds_label = if ("podium_effective_avg_american_odds_label" %in% names(.)) as.character(podium_effective_avg_american_odds_label) else NA_character_,
      podium_effective_no_vig_probability = if ("podium_effective_no_vig_probability" %in% names(.)) as.numeric(podium_effective_no_vig_probability) else podium_market_no_vig_probability,
      podium_odds_source = if ("podium_odds_source" %in% names(.)) as.character(podium_odds_source) else if_else(!is.na(podium_avg_american_odds), "market", "missing"),
      podium_odds_source = coalesce(podium_odds_source, if_else(!is.na(podium_avg_american_odds), "market", "missing")),
      win_avg_american_odds_label = coalesce(win_avg_american_odds_label, ""),
      podium_avg_american_odds_label = coalesce(podium_avg_american_odds_label, ""),
      podium_effective_avg_american_odds_label = if_else(!is.na(podium_effective_avg_american_odds), paste0(if_else(podium_odds_source == "estimated_from_win", "est ", ""), if_else(podium_effective_avg_american_odds > 0, paste0("+", as.integer(round(podium_effective_avg_american_odds))), as.character(as.integer(round(podium_effective_avg_american_odds))))), coalesce(podium_effective_avg_american_odds_label, ""))
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_rf_predictions %>% mutate(points = numeric(), predicted_points = numeric())
}

load_finish_predictions <- function(path) {
  if (!file.exists(path)) return(empty_rf_predictions)
  read_csv(path, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      predicted_finish_position = as.numeric(predicted_finish_position),
      predicted_rank_in_race = as.integer(predicted_rank_in_race),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      win_avg_american_odds_label = coalesce(as.character(win_avg_american_odds_label), ""),
      win_market_no_vig_probability = as.numeric(win_market_no_vig_probability),
      podium_avg_american_odds = as.numeric(podium_avg_american_odds),
      podium_avg_american_odds_label = coalesce(as.character(podium_avg_american_odds_label), ""),
      podium_market_no_vig_probability = as.numeric(podium_market_no_vig_probability),
      podium_effective_avg_american_odds_label = coalesce(as.character(podium_effective_avg_american_odds_label), ""),
      podium_effective_no_vig_probability = as.numeric(podium_effective_no_vig_probability),
      podium_odds_source = coalesce(as.character(podium_odds_source), if_else(!is.na(podium_avg_american_odds), "market", "missing"))
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
}

load_probability_predictions <- function(path) {
  if (!file.exists(path)) return(empty_rf_probability_predictions)
  read_csv(path, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_probability_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      predicted_win_probability = as.numeric(predicted_win_probability),
      predicted_podium_probability = as.numeric(predicted_podium_probability),
      predicted_win_rank = as.integer(predicted_win_rank),
      predicted_podium_rank = as.integer(predicted_podium_rank),
      predicted_rank_in_race = predicted_win_rank,
      win_avg_american_odds_label = coalesce(as.character(win_avg_american_odds_label), ""),
      win_market_no_vig_probability = as.numeric(win_market_no_vig_probability),
      podium_avg_american_odds = as.numeric(podium_avg_american_odds),
      podium_avg_american_odds_label = coalesce(as.character(podium_avg_american_odds_label), ""),
      podium_market_no_vig_probability = as.numeric(podium_market_no_vig_probability),
      podium_effective_avg_american_odds_label = coalesce(as.character(podium_effective_avg_american_odds_label), ""),
      podium_effective_no_vig_probability = as.numeric(podium_effective_no_vig_probability),
      podium_odds_source = coalesce(as.character(podium_odds_source), if_else(!is.na(podium_avg_american_odds), "market", "missing"))
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
}

load_points_predictions <- function(path) {
  if (!file.exists(path)) return(empty_rf_predictions %>% mutate(points = numeric(), predicted_points = numeric()))
  read_csv(path, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions %>% mutate(points = numeric(), predicted_points = numeric())) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      finish_position = as.numeric(finish_position),
      points = if ("points" %in% names(.)) as.numeric(points) else NA_real_,
      predicted_points = as.numeric(predicted_points),
      predicted_rank_in_race = as.integer(predicted_rank_in_race),
      actual_rank_in_race = as.integer(actual_rank_in_race),
      win_avg_american_odds_label = coalesce(as.character(win_avg_american_odds_label), ""),
      win_market_no_vig_probability = as.numeric(win_market_no_vig_probability),
      podium_avg_american_odds = as.numeric(podium_avg_american_odds),
      podium_avg_american_odds_label = coalesce(as.character(podium_avg_american_odds_label), ""),
      podium_market_no_vig_probability = as.numeric(podium_market_no_vig_probability),
      podium_effective_avg_american_odds_label = coalesce(as.character(podium_effective_avg_american_odds_label), ""),
      podium_effective_no_vig_probability = as.numeric(podium_effective_no_vig_probability),
      podium_odds_source = coalesce(as.character(podium_odds_source), if_else(!is.na(podium_avg_american_odds), "market", "missing"))
    ) %>%
    overlay_latest_market_odds() %>%
    filter(season %in% c(2025L, 2026L))
}

empty_winner_without_predictions <- tibble(
  model = character(), model_label = character(), selected_model = logical(),
  data_split = character(), season = integer(), round = integer(), race_date = as.Date(character()),
  is_wet_race = logical(), wet_exclusion_reason = character(),
  race_name = character(), driver_id = character(), driver_code = character(),
  driver_name = character(), constructor_name = character(), finish_position = numeric(),
  without_market_candidate = logical(), winner_without_target = integer(),
  predicted_winner_without_probability = numeric(), predicted_without_rank = integer(),
  actual_without_rank = integer(), predicted_winner_without = logical(),
  actual_winner_without = logical(), winner_without_pick_correct = logical()
)

winner_without_predictions <- if (file.exists(xgb_winner_without_predictions_csv)) {
  read_csv(xgb_winner_without_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_winner_without_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      selected_model = as.logical(selected_model),
      is_wet_race = as.logical(is_wet_race),
      wet_exclusion_reason = as.character(wet_exclusion_reason),
      finish_position = as.numeric(finish_position),
      without_market_candidate = as.logical(without_market_candidate),
      winner_without_target = as.integer(winner_without_target),
      predicted_winner_without_probability = as.numeric(predicted_winner_without_probability),
      predicted_without_rank = as.integer(predicted_without_rank),
      predicted_winner_without = as.logical(predicted_winner_without),
      actual_without_rank = as.integer(actual_without_rank),
      actual_winner_without = as.logical(actual_winner_without),
      winner_without_pick_correct = as.logical(winner_without_pick_correct)
    ) %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_winner_without_predictions
}

empty_fastest_lap_predictions <- tibble(
  model = character(), model_label = character(), model_family = character(),
  routed_type = character(), selected_model = logical(), data_split = character(),
  season = integer(), round = integer(), race_date = as.Date(character()),
  race_name = character(), driver_id = character(), driver_code = character(),
  driver_name = character(), constructor_name = character(), track_profile_id = character(),
  is_wet_race = logical(), wet_exclusion_reason = character(), finish_position = numeric(),
  fastest_lap_sec = numeric(), race_best_fastest_lap_sec = numeric(),
  fastest_lap_delta_sec = numeric(), fastest_rank = numeric(), fastest_lap_winner = integer(),
  prediction_score = numeric(), predicted_fastest_lap_probability = numeric(),
  predicted_fastest_rank = numeric(), predicted_fastest_lap_delta_sec = numeric(),
  predicted_fastest_lap_rank = integer(), predicted_fastest_lap_winner = logical(),
  fastest_lap_pick_correct = logical()
)

fastest_lap_predictions <- if (file.exists(fastest_lap_predictions_csv)) {
  read_csv(fastest_lap_predictions_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season), round = as.integer(round), race_date = as.Date(race_date),
      selected_model = as.logical(selected_model), is_wet_race = as.logical(is_wet_race),
      finish_position = as.numeric(finish_position), fastest_lap_sec = as.numeric(fastest_lap_sec),
      race_best_fastest_lap_sec = as.numeric(race_best_fastest_lap_sec),
      fastest_lap_delta_sec = as.numeric(fastest_lap_delta_sec), fastest_rank = as.numeric(fastest_rank),
      fastest_lap_winner = as.integer(fastest_lap_winner), prediction_score = as.numeric(prediction_score),
      predicted_fastest_lap_probability = as.numeric(predicted_fastest_lap_probability),
      predicted_fastest_rank = as.numeric(predicted_fastest_rank),
      predicted_fastest_lap_delta_sec = as.numeric(predicted_fastest_lap_delta_sec),
      predicted_fastest_lap_rank = as.integer(predicted_fastest_lap_rank),
      predicted_fastest_lap_winner = as.logical(predicted_fastest_lap_winner),
      fastest_lap_pick_correct = as.logical(fastest_lap_pick_correct)
    ) %>%
    filter(season %in% c(2025L, 2026L))
} else {
  empty_fastest_lap_predictions
}

fastest_lap_odds <- if (file.exists(fastest_lap_odds_csv)) {
  read_csv(fastest_lap_odds_csv, show_col_types = FALSE) %>%
    transmute(
      season = as.integer(season), round = as.integer(round),
      driver_code = as.character(driver_code),
      fastest_lap_american_odds = as.numeric(fastest_lap_american_odds),
      fastest_lap_decimal_odds = as.numeric(fastest_lap_decimal_odds),
      fastest_lap_implied_probability = as.numeric(fastest_lap_implied_probability),
      fastest_lap_no_vig_probability = as.numeric(fastest_lap_no_vig_probability),
      fastest_lap_american_odds_label = fmt_american_label(fastest_lap_american_odds),
      fastest_lap_odds_source = as.character(source_note),
      fastest_lap_odds_timestamp = as.character(odds_timestamp),
      fastest_lap_odds_url = as.character(source_url)
    ) %>%
    distinct(season, round, driver_code, .keep_all = TRUE)
} else {
  tibble(
    season = integer(), round = integer(), driver_code = character(),
    fastest_lap_american_odds = numeric(), fastest_lap_decimal_odds = numeric(),
    fastest_lap_implied_probability = numeric(), fastest_lap_no_vig_probability = numeric(),
    fastest_lap_american_odds_label = character(), fastest_lap_odds_source = character(),
    fastest_lap_odds_timestamp = character(), fastest_lap_odds_url = character()
  )
}

empty_chatter_team_features <- tibble(
  season = integer(), round = integer(), Race_Key = character(), Grand_Prix = character(),
  Race_Date = as.Date(character()), Team = character(), join_team = character(),
  Momentum_Raw = numeric(), Upgrade_Raw = numeric(), Context_Raw = numeric(),
  Source_Confidence = numeric(), Composite_Chatter_Score = numeric(),
  race_average_chatter = numeric(), race_centered_chatter = numeric()
)

chatter_team_features <- if (file.exists(chatter_team_features_csv)) {
  read_csv(chatter_team_features_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      Race_Date = as.Date(Race_Date),
      Momentum_Raw = as.numeric(Momentum_Raw),
      Upgrade_Raw = as.numeric(Upgrade_Raw),
      Context_Raw = as.numeric(Context_Raw),
      Source_Confidence = as.numeric(Source_Confidence),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      race_average_chatter = as.numeric(race_average_chatter),
      race_centered_chatter = as.numeric(race_centered_chatter)
    )
} else {
  empty_chatter_team_features
}

chatter_coefficients <- if (file.exists(chatter_coefficients_csv)) {
  read_csv(chatter_coefficients_csv, show_col_types = FALSE) %>%
    mutate(
      beta = as.numeric(beta),
      train_end_season = as.integer(train_end_season),
      max_nudge = as.numeric(max_nudge)
    )
} else {
  tibble(
    overlay_family = character(), target = character(), coefficient_type = character(),
    beta = numeric(), train_end_season = integer(), max_nudge = numeric(), fitted_at = character()
  )
}

empty_chatter_qualifying_overlay <- tibble(
  season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
  driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
  finish_position = numeric(), current_grid = numeric(), current_quali_position = numeric(),
  current_best_quali_delta_sec = numeric(), actual_quali_position = numeric(),
  actual_quali_delta_sec = numeric(), actual_grid = numeric(),
  base_predicted_quali_position = numeric(), adjusted_predicted_quali_position = numeric(),
  base_predicted_quali_delta_sec = numeric(), chatter_beta = numeric(), chatter_quali_nudge = numeric(),
  race_centered_chatter = numeric(), Composite_Chatter_Score = numeric(),
  base_quali_rank = integer(), adjusted_quali_rank = integer(),
  base_predicted_pole = logical(), adjusted_predicted_pole = logical(), actual_pole_bool = logical(),
  base_pole_pick_correct = logical(), adjusted_pole_pick_correct = logical(),
  base_predicted_top3_quali = logical(), adjusted_predicted_top3_quali = logical(),
  actual_top3_quali_bool = logical(), base_top3_quali_pick_correct = logical(),
  adjusted_top3_quali_pick_correct = logical()
)

chatter_qualifying_overlay <- if (file.exists(chatter_qualifying_overlay_csv)) {
  read_csv(chatter_qualifying_overlay_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      finish_position = as.numeric(finish_position),
      current_grid = as.numeric(current_grid),
      current_quali_position = as.numeric(current_quali_position),
      current_best_quali_delta_sec = as.numeric(current_best_quali_delta_sec),
      actual_quali_position = as.numeric(actual_quali_position),
      actual_quali_delta_sec = as.numeric(actual_quali_delta_sec),
      actual_grid = as.numeric(actual_grid),
      base_predicted_quali_position = as.numeric(base_predicted_quali_position),
      adjusted_predicted_quali_position = as.numeric(adjusted_predicted_quali_position),
      base_predicted_quali_delta_sec = as.numeric(base_predicted_quali_delta_sec),
      chatter_quali_nudge = as.numeric(chatter_quali_nudge),
      race_centered_chatter = as.numeric(race_centered_chatter),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      base_quali_rank = as.integer(base_quali_rank),
      adjusted_quali_rank = as.integer(adjusted_quali_rank),
      base_predicted_pole = as.logical(base_predicted_pole),
      adjusted_predicted_pole = as.logical(adjusted_predicted_pole),
      actual_pole_bool = as.logical(actual_pole_bool),
      base_pole_pick_correct = as.logical(base_pole_pick_correct),
      adjusted_pole_pick_correct = as.logical(adjusted_pole_pick_correct),
      base_predicted_top3_quali = as.logical(base_predicted_top3_quali),
      adjusted_predicted_top3_quali = as.logical(adjusted_predicted_top3_quali),
      actual_top3_quali_bool = as.logical(actual_top3_quali_bool),
      base_top3_quali_pick_correct = as.logical(base_top3_quali_pick_correct),
      adjusted_top3_quali_pick_correct = as.logical(adjusted_top3_quali_pick_correct)
    )
} else {
  empty_chatter_qualifying_overlay
}

chatter_quali_display_lookup <- chatter_qualifying_overlay %>%
  transmute(
    season = as.integer(season),
    round = as.integer(round),
    driver_code = as.character(driver_code),
    chatter_base_quali_rank = as.integer(base_quali_rank),
    chatter_adjusted_quali_rank = as.integer(adjusted_quali_rank),
    chatter_base_predicted_quali_position = as.numeric(base_predicted_quali_position),
    chatter_adjusted_predicted_quali_position = as.numeric(adjusted_predicted_quali_position)
  ) %>%
  filter(!is.na(season), !is.na(round), !is.na(driver_code)) %>%
  arrange(season, round, chatter_adjusted_quali_rank, driver_code) %>%
  distinct(season, round, driver_code, .keep_all = TRUE)

# Resolve the display grid only after the chatter-adjusted qualifying order is
# known. Grid penalties are field-wide: moving one penalized driver rearward
# changes the displayed starting position of other drivers too.
prerace_display_lookup <- prerace_display_lookup %>%
  left_join(chatter_quali_display_lookup, by = c("season", "round", "driver_code")) %>%
  mutate(
    use_chatter_quali_display =
      (is.na(display_prerace_source) | display_prerace_source %in% c("estimate", "scheduled")) &
      !is.na(chatter_adjusted_quali_rank),
    resolved_display_quali = if_else(
      use_chatter_quali_display,
      as.numeric(chatter_adjusted_quali_rank),
      as.numeric(display_quali_position)
    ),
    grid_penalty_places = pmax(0, coalesce(as.numeric(grid_penalty_places), 0)),
    grid_penalty_back_of_grid = coalesce(as.numeric(grid_penalty_back_of_grid), 0) >= 1,
    provisional_grid_score = resolved_display_quali + grid_penalty_places,
    numeric_grid_penalty = grid_penalty_places > 0
  ) %>%
  group_by(season, round) %>%
  mutate(
    race_has_complete_explicit_grid = all(coalesce(has_explicit_grid_override, FALSE)),
    race_uses_chatter_quali = any(use_chatter_quali_display)
  ) %>%
  arrange(
    grid_penalty_back_of_grid,
    provisional_grid_score,
    numeric_grid_penalty,
    resolved_display_quali,
    driver_code,
    .by_group = TRUE
  ) %>%
  mutate(resolved_penalty_grid = as.numeric(row_number())) %>%
  ungroup() %>%
  mutate(
    display_quali_position = resolved_display_quali,
    display_start_position = if_else(
      race_uses_chatter_quali & !race_has_complete_explicit_grid,
      resolved_penalty_grid,
      as.numeric(display_start_position)
    )
  ) %>%
  select(
    -starts_with("chatter_"), -use_chatter_quali_display,
    -resolved_display_quali, -provisional_grid_score, -numeric_grid_penalty,
    -race_has_complete_explicit_grid, -race_uses_chatter_quali,
    -resolved_penalty_grid
  )

empty_chatter_finish_overlay <- tibble(
  season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
  driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
  finish_position = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
  base_predicted_finish = numeric(), adjusted_predicted_finish = numeric(), chatter_finish_nudge = numeric(),
  race_centered_chatter = numeric(), Composite_Chatter_Score = numeric(),
  base_finish_rank = integer(), adjusted_finish_rank = integer()
)

empty_chatter_probability_overlay <- tibble(
  season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
  driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
  finish_position = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
  base_win_probability = numeric(), adjusted_win_probability = numeric(),
  base_podium_probability = numeric(), adjusted_podium_probability = numeric(),
  chatter_win_logit_nudge = numeric(), chatter_podium_logit_nudge = numeric(),
  race_centered_chatter = numeric(), Composite_Chatter_Score = numeric(),
  base_win_rank = integer(), adjusted_win_rank = integer(), base_podium_rank = integer(), adjusted_podium_rank = integer()
)

empty_chatter_points_overlay <- tibble(
  season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
  driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
  finish_position = numeric(), points = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
  base_predicted_points = numeric(), adjusted_predicted_points = numeric(), chatter_points_nudge = numeric(),
  race_centered_chatter = numeric(), Composite_Chatter_Score = numeric(),
  base_points_rank = integer(), adjusted_points_rank = integer()
)

chatter_finish_overlay <- if (file.exists(chatter_finish_overlay_csv)) {
  read_csv(chatter_finish_overlay_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      finish_position = as.numeric(finish_position),
      base_predicted_finish = as.numeric(base_predicted_finish),
      adjusted_predicted_finish = as.numeric(adjusted_predicted_finish),
      chatter_finish_nudge = as.numeric(chatter_finish_nudge),
      race_centered_chatter = as.numeric(race_centered_chatter),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      base_finish_rank = as.integer(base_finish_rank),
      adjusted_finish_rank = as.integer(adjusted_finish_rank)
    )
} else {
  empty_chatter_finish_overlay
}

chatter_probability_overlay <- if (file.exists(chatter_probability_overlay_csv)) {
  read_csv(chatter_probability_overlay_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      finish_position = as.numeric(finish_position),
      base_win_probability = as.numeric(base_win_probability),
      adjusted_win_probability = as.numeric(adjusted_win_probability),
      base_podium_probability = as.numeric(base_podium_probability),
      adjusted_podium_probability = as.numeric(adjusted_podium_probability),
      chatter_win_logit_nudge = as.numeric(chatter_win_logit_nudge),
      chatter_podium_logit_nudge = as.numeric(chatter_podium_logit_nudge),
      race_centered_chatter = as.numeric(race_centered_chatter),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      base_win_rank = as.integer(base_win_rank),
      adjusted_win_rank = as.integer(adjusted_win_rank),
      base_podium_rank = as.integer(base_podium_rank),
      adjusted_podium_rank = as.integer(adjusted_podium_rank)
    )
} else {
  empty_chatter_probability_overlay
}

chatter_points_overlay <- if (file.exists(chatter_points_overlay_csv)) {
  read_csv(chatter_points_overlay_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      finish_position = as.numeric(finish_position),
      points = as.numeric(points),
      base_predicted_points = as.numeric(base_predicted_points),
      adjusted_predicted_points = as.numeric(adjusted_predicted_points),
      chatter_points_nudge = as.numeric(chatter_points_nudge),
      race_centered_chatter = as.numeric(race_centered_chatter),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      base_points_rank = as.integer(base_points_rank),
      adjusted_points_rank = as.integer(adjusted_points_rank)
    )
} else {
  empty_chatter_points_overlay
}

chatter_winner_without_overlay <- if (file.exists(chatter_winner_without_overlay_csv)) {
  read_csv(chatter_winner_without_overlay_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      finish_position = as.numeric(finish_position),
      base_winner_without_probability = as.numeric(base_winner_without_probability),
      adjusted_winner_without_probability = as.numeric(adjusted_winner_without_probability),
      chatter_winner_without_logit_nudge = as.numeric(chatter_winner_without_logit_nudge),
      race_centered_chatter = as.numeric(race_centered_chatter),
      Composite_Chatter_Score = as.numeric(Composite_Chatter_Score),
      base_winner_without_rank = as.integer(base_winner_without_rank),
      adjusted_winner_without_rank = as.integer(adjusted_winner_without_rank)
    )
} else {
  tibble()
}

qualifying_predictions <- if (file.exists(qualifying_predictions_csv)) {
  read_csv(qualifying_predictions_csv, show_col_types = FALSE) %>%
    prepare_prediction_rows(empty_rf_predictions) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      is_wet_race = as.logical(is_wet_race),
      wet_exclusion_reason = as.character(wet_exclusion_reason),
      current_grid = as.numeric(current_grid),
      current_quali_position = as.numeric(current_quali_position),
      current_best_quali_delta_sec = as.numeric(current_best_quali_delta_sec),
      actual_quali_position = as.numeric(actual_quali_position),
      actual_quali_delta_sec = as.numeric(actual_quali_delta_sec),
      actual_grid = as.numeric(actual_grid),
      predicted_quali_position = as.numeric(predicted_quali_position),
      predicted_quali_delta_sec = as.numeric(predicted_quali_delta_sec),
      predicted_grid = as.numeric(predicted_grid),
      predicted_quali_rank = as.integer(predicted_quali_rank),
      actual_quali_rank = as.numeric(actual_quali_rank),
      predicted_pole = as.logical(predicted_pole),
      actual_pole = as.logical(actual_pole),
      predicted_top3_quali = as.logical(predicted_top3_quali),
      actual_top3_quali = as.logical(actual_top3_quali),
      pole_pick_correct = as.logical(pole_pick_correct),
      top3_quali_pick_correct = as.logical(top3_quali_pick_correct)
    ) %>%
    filter(season %in% c(2025L, 2026L))
} else {
  tibble(
    season = integer(), round = integer(), race_date = as.Date(character()),
    is_wet_race = logical(), wet_exclusion_reason = character(),
    race_name = character(), driver_code = character(), driver_name = character(),
    constructor_name = character(), model = character(), model_label = character(),
    predicted_quali_position = numeric(), predicted_quali_delta_sec = numeric(),
    predicted_grid = numeric(), predicted_quali_rank = integer(),
    actual_quali_position = numeric(), actual_quali_delta_sec = numeric(),
    actual_grid = numeric(), pole_pick_correct = logical(),
    top3_quali_pick_correct = logical()
  )
}

qualifying_metrics <- if (file.exists(qualifying_metrics_csv)) {
  read_csv(qualifying_metrics_csv, show_col_types = FALSE)
} else {
  tibble()
}

normalize_race_condition_rows <- function(tbl) {
  if (nrow(tbl) == 0 || !all(c("season", "round") %in% names(tbl))) {
    return(tibble(season = integer(), round = integer(), is_wet_race = logical(), wet_exclusion_reason = character()))
  }

  if (!"is_wet_race" %in% names(tbl)) tbl$is_wet_race <- FALSE
  if (!"wet_exclusion_reason" %in% names(tbl)) tbl$wet_exclusion_reason <- NA_character_

  tbl %>%
    transmute(
      season = as.integer(season),
      round = as.integer(round),
      is_wet_race = as.logical(is_wet_race),
      wet_exclusion_reason = as.character(wet_exclusion_reason)
    ) %>%
    filter(!is.na(season), !is.na(round)) %>%
    arrange(desc(is_wet_race)) %>%
    distinct(season, round, .keep_all = TRUE)
}

race_condition_lookup <- bind_rows(
  normalize_race_condition_rows(xgb_finish_predictions),
  normalize_race_condition_rows(xgb_probability_predictions),
  normalize_race_condition_rows(xgb_points_predictions),
  normalize_race_condition_rows(winner_without_predictions),
  normalize_race_condition_rows(qualifying_predictions)
) %>%
  arrange(season, round, desc(is_wet_race)) %>%
  distinct(season, round, .keep_all = TRUE)

add_bet_race_condition <- function(bets) {
  if (nrow(bets) == 0) {
    return(bets %>% mutate(race_condition = character()))
  }

  if (!"is_wet_race" %in% names(bets) && all(c("season", "round") %in% names(bets))) {
    bets <- bets %>% left_join(race_condition_lookup, by = c("season", "round"))
  }
  if (!"is_wet_race" %in% names(bets)) bets$is_wet_race <- FALSE
  if (!"wet_exclusion_reason" %in% names(bets)) bets$wet_exclusion_reason <- NA_character_

  bets %>%
    mutate(
      is_wet_race = coalesce(as.logical(is_wet_race), FALSE),
      wet_exclusion_reason = as.character(wet_exclusion_reason),
      race_condition = if_else(is_wet_race, "Wet", "Dry")
    )
}

empty_consensus_bets <- tibble(
  season = integer(),
  round = integer(),
  race_date = as.Date(character()),
  race_name = character(),
  bet_market = character(),
  consensus_rank = integer(),
  driver_code = character(),
  driver_name = character(),
  constructor_name = character(),
  predicted_finish = numeric(),
  actual_finish = numeric(),
  bet_won = logical(),
  odds_american_label = character(),
  market_no_vig_probability = numeric(),
  stake = numeric(),
  profit = numeric(),
  roi = numeric(),
  bet_status = character()
)

consensus_bets <- if (file.exists(consensus_bets_csv)) {
  read_csv(consensus_bets_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_date = as.Date(race_date),
      consensus_rank = as.integer(consensus_rank),
      actual_finish = as.numeric(actual_finish),
      predicted_finish = as.numeric(predicted_finish),
      market_no_vig_probability = as.numeric(market_no_vig_probability),
      stake = as.numeric(stake),
      profit = as.numeric(profit),
      roi = as.numeric(roi),
      bet_won = as.logical(bet_won),
      odds_american_label = coalesce(as.character(odds_american_label), "")
    )
} else {
  empty_consensus_bets
}

empty_consensus_season_summary <- tibble(
  season = integer(),
  bet_market = character(),
  races_with_available_bets = numeric(),
  bets_available = numeric(),
  bets_won = numeric(),
  hit_rate = numeric(),
  stake = numeric(),
  profit = numeric(),
  roi = numeric()
)

consensus_season_summary <- if (file.exists(consensus_season_summary_csv)) {
  read_csv(consensus_season_summary_csv, show_col_types = FALSE) %>%
    mutate(
      season = as.integer(season),
      across(c(races_with_available_bets, bets_available, bets_won, hit_rate, stake, profit, roi), as.numeric)
    )
} else {
  empty_consensus_season_summary
}

rf_main_predictions <- rf_predictions %>%
  filter(!str_starts(model, "rf_profile_")) %>%
  mutate(
    model_label = if_else(
      model == "rf_14_profile_routed",
      "Track-specific RF (14 profiles)",
      model_label
    )
  )

rf_model_lookup <- rf_main_predictions %>%
  distinct(model, model_label, selected_model) %>%
  mutate(
    model_order = case_when(
      model == "rf_with_constructor" ~ 1L,
      model == "rf_no_constructor" ~ 2L,
      model == "rf_street_specialist" ~ 3L,
      model == "rf_permanent_specialist" ~ 4L,
      model == "rf_high_speed_specialist" ~ 5L,
      model == "rf_14_profile_routed" ~ 6L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(model_order, model_label)

rf_model_choices <- setNames(rf_model_lookup$model, rf_model_lookup$model_label)
rf_model_selected <- rf_model_lookup$model

projection_race_source <- bind_rows(
  schedule %>% select(any_of(c("season", "round", "race_name"))),
  rf_main_predictions %>% select(any_of(c("season", "round", "race_name"))),
  rf_probability_predictions %>% select(any_of(c("season", "round", "race_name"))),
  xgb_finish_predictions %>% select(any_of(c("season", "round", "race_name"))),
  xgb_probability_predictions %>% select(any_of(c("season", "round", "race_name"))),
  xgb_points_predictions %>% select(any_of(c("season", "round", "race_name"))),
  winner_without_predictions %>% select(any_of(c("season", "round", "race_name"))),
  fastest_lap_predictions %>% select(any_of(c("season", "round", "race_name"))),
  qualifying_predictions %>% select(any_of(c("season", "round", "race_name"))),
  chatter_qualifying_overlay %>% select(any_of(c("season", "round", "race_name"))),
  chatter_finish_overlay %>% select(any_of(c("season", "round", "race_name"))),
  chatter_probability_overlay %>% select(any_of(c("season", "round", "race_name"))),
  chatter_points_overlay %>% select(any_of(c("season", "round", "race_name")))
) %>%
  filter(!is.na(season), !is.na(round), !is.na(race_name))

rf_race_choices <- projection_race_source %>%
  distinct(season, round, race_name) %>%
  left_join(
    race_choices %>%
      select(season, round, race_date, has_results, circuit_id, circuit_name, all_of(family_flags)) %>%
      distinct(),
    by = c("season", "round")
  ) %>%
  arrange(desc(season), round) %>%
  mutate(label = paste0("R", sprintf("%02d", round), " - ", race_name))

rf_probability_model_lookup <- rf_probability_predictions %>%
  distinct(model, model_label, selected_model) %>%
  mutate(
    model_order = case_when(
      model == "rf_prob_with_constructor" ~ 1L,
      model == "rf_prob_no_constructor" ~ 2L,
      model == "rf_prob_street_specialist" ~ 3L,
      model == "rf_prob_permanent_specialist" ~ 4L,
      model == "rf_prob_high_speed_specialist" ~ 5L,
      model == "rf_prob_14_profile_routed" ~ 6L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(model_order, model_label)

rf_probability_model_choices <- setNames(rf_probability_model_lookup$model, rf_probability_model_lookup$model_label)

xgb_finish_model_lookup <- xgb_finish_predictions %>%
  filter(!str_starts(model, "xgb_finish_profile_")) %>%
  distinct(model, model_label, selected_model) %>%
  mutate(
    model_order = case_when(
      model == "xgb_finish_with_constructor" ~ 1L,
      model == "xgb_finish_no_constructor" ~ 2L,
      model == "xgb_finish_street_specialist" ~ 3L,
      model == "xgb_finish_permanent_specialist" ~ 4L,
      model == "xgb_finish_high_speed_specialist" ~ 5L,
      model == "xgb_finish_technical_specialist" ~ 6L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(model_order, model_label)

xgb_finish_model_choices <- setNames(xgb_finish_model_lookup$model, xgb_finish_model_lookup$model_label)
xgb_finish_default_models <- intersect(
  c(
    "xgb_finish_with_constructor",
    "xgb_finish_no_constructor"
  ),
  xgb_finish_model_lookup$model
)

xgb_probability_model_lookup <- xgb_probability_predictions %>%
  filter(!str_starts(model, "xgb_prob_profile_")) %>%
  distinct(model, model_label, selected_model) %>%
  mutate(
    model_order = case_when(
      model == "xgb_prob_with_constructor" ~ 1L,
      model == "xgb_prob_no_constructor" ~ 2L,
      model == "xgb_prob_street_specialist" ~ 3L,
      model == "xgb_prob_permanent_specialist" ~ 4L,
      model == "xgb_prob_high_speed_specialist" ~ 5L,
      model == "xgb_prob_technical_specialist" ~ 6L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(model_order, model_label)

xgb_probability_model_choices <- setNames(xgb_probability_model_lookup$model, xgb_probability_model_lookup$model_label)
xgb_probability_default_models <- intersect(
  c(
    "xgb_prob_with_constructor",
    "xgb_prob_no_constructor"
  ),
  xgb_probability_model_lookup$model
)

xgb_points_model_lookup <- xgb_points_predictions %>%
  filter(!str_starts(model, "xgb_points_profile_")) %>%
  distinct(model, model_label, selected_model) %>%
  mutate(
    model_order = case_when(
      model == "xgb_points_with_constructor" ~ 1L,
      model == "xgb_points_no_constructor" ~ 2L,
      model == "xgb_points_street_specialist" ~ 3L,
      model == "xgb_points_permanent_specialist" ~ 4L,
      model == "xgb_points_high_speed_specialist" ~ 5L,
      model == "xgb_points_technical_specialist" ~ 6L,
      TRUE ~ 99L
    )
  ) %>%
  arrange(model_order, model_label)

xgb_points_model_choices <- setNames(xgb_points_model_lookup$model, xgb_points_model_lookup$model_label)
xgb_points_default_models <- intersect(
  c(
    "xgb_points_with_constructor",
    "xgb_points_no_constructor"
  ),
  xgb_points_model_lookup$model
)

routed_specialist_model_lookup <- bind_rows(
  xgb_finish_model_lookup %>%
    filter(str_detect(model, "_(street|permanent|high_speed|technical)_specialist$")) %>%
    mutate(routed_family = "finish"),
  xgb_probability_model_lookup %>%
    filter(str_detect(model, "_(street|permanent|high_speed|technical)_specialist$")) %>%
    mutate(routed_family = "probability"),
  xgb_points_model_lookup %>%
    filter(str_detect(model, "_(street|permanent|high_speed|technical)_specialist$")) %>%
    mutate(routed_family = "points")
) %>%
  mutate(
    routed_type = case_when(
      str_detect(model, "street_specialist$") ~ "street",
      str_detect(model, "permanent_specialist$") ~ "permanent",
      str_detect(model, "high_speed_specialist$") ~ "high_speed",
      str_detect(model, "technical_specialist$") ~ "technical",
      TRUE ~ NA_character_
    ),
    routed_type_label = recode(
      routed_type,
      street = "Street",
      permanent = "Permanent",
      high_speed = "High speed",
      technical = "Technical",
      .default = "Specialist"
    ),
    model_label = paste0(routed_type_label, " - ", recode(
      routed_family,
      finish = "Finish",
      probability = "Probability",
      points = "Points",
      .default = routed_family
    ))
  ) %>%
  filter(!is.na(routed_type)) %>%
  arrange(match(routed_type, c("street", "permanent", "high_speed", "technical")), match(routed_family, c("finish", "probability", "points")))

routed_specialist_model_choices <- setNames(routed_specialist_model_lookup$model, routed_specialist_model_lookup$model_label)
routed_specialist_default_models <- character(0)

race_route_types <- function(selected_season, selected_round) {
  if (is.null(selected_season) || is.null(selected_round)) return(character(0))

  race_flags <- race_choices %>%
    filter(season == as.integer(selected_season), round == as.integer(selected_round)) %>%
    slice(1)

  if (nrow(race_flags) == 0) return(character(0))

  types <- c(
    if (coalesce(as.integer(race_flags$is_street[[1]]), 0L) == 1L) "street",
    if (coalesce(as.integer(race_flags$is_permanent_road_course[[1]]), 0L) == 1L) "permanent",
    if (coalesce(as.integer(race_flags$is_high_speed[[1]]), 0L) == 1L) "high_speed",
    if (
      coalesce(as.integer(race_flags$is_high_speed[[1]]), 0L) == 0L &&
        (
          coalesce(as.integer(race_flags$is_stop_start[[1]]), 0L) == 1L ||
            coalesce(as.integer(race_flags$is_flowing_high_downforce[[1]]), 0L) == 1L ||
            coalesce(as.integer(race_flags$is_low_overtake[[1]]), 0L) == 1L
        )
    ) "technical"
  )

  unique(types)
}

default_xgb_models_for_race <- function(selected_season, selected_round, prefix, available_models, base_models) {
  route_types <- race_route_types(selected_season, selected_round)
  specialist_models <- paste0(prefix, "_", route_types, "_specialist")

  defaults <- intersect(c(base_models, specialist_models), available_models)
  if (length(defaults) == 0) intersect(base_models, available_models) else defaults
}

default_xgb_finish_models_for_race <- function(selected_season, selected_round) {
  default_xgb_models_for_race(
    selected_season,
    selected_round,
    "xgb_finish",
    xgb_finish_model_lookup$model,
    c("xgb_finish_with_constructor", "xgb_finish_no_constructor")
  )
}

default_xgb_probability_models_for_race <- function(selected_season, selected_round) {
  default_xgb_models_for_race(
    selected_season,
    selected_round,
    "xgb_prob",
    xgb_probability_model_lookup$model,
    c("xgb_prob_with_constructor", "xgb_prob_no_constructor")
  )
}

default_xgb_points_models_for_race <- function(selected_season, selected_round) {
  default_xgb_models_for_race(
    selected_season,
    selected_round,
    "xgb_points",
    xgb_points_model_lookup$model,
    c("xgb_points_with_constructor", "xgb_points_no_constructor")
  )
}

default_routed_specialist_models_for_race <- function(selected_season, selected_round) {
  route_types <- race_route_types(selected_season, selected_round)
  defaults <- routed_specialist_model_lookup %>%
    filter(routed_type %in% route_types) %>%
    pull(model)

  intersect(defaults, routed_specialist_model_lookup$model)
}

fastest_lap_model_lookup <- fastest_lap_predictions %>%
  distinct(model, model_label, model_family, routed_type) %>%
  mutate(
    routed_type_label = recode(routed_type, street = "Street", permanent = "Permanent", high_speed = "High speed", technical = "Technical", .default = routed_type),
    family_label = recode(model_family, rank = "Rank", probability = "Probability", delta = "Delta seconds", .default = model_family),
    choice_label = paste0(routed_type_label, " - ", family_label)
  ) %>%
  arrange(match(routed_type, c("street", "permanent", "high_speed", "technical")), match(model_family, c("rank", "probability", "delta")))

fastest_lap_model_choices <- setNames(fastest_lap_model_lookup$model, fastest_lap_model_lookup$choice_label)

default_fastest_lap_models_for_race <- function(selected_season, selected_round) {
  routes <- race_route_types(selected_season, selected_round)
  fastest_lap_model_lookup %>% filter(routed_type %in% routes) %>% pull(model)
}

filter_fastest_lap_route_rows <- function(rows) {
  if (nrow(rows) == 0) return(rows)

  route_flags <- race_choices %>%
    select(season, round, all_of(family_flags)) %>%
    distinct(season, round, .keep_all = TRUE)

  rows %>%
    left_join(route_flags, by = c("season", "round")) %>%
    mutate(
      route_match = case_when(
        routed_type == "street" ~ coalesce(as.integer(is_street), 0L) == 1L,
        routed_type == "permanent" ~ coalesce(as.integer(is_permanent_road_course), 0L) == 1L,
        routed_type == "high_speed" ~ coalesce(as.integer(is_high_speed), 0L) == 1L,
        routed_type == "technical" ~ coalesce(as.integer(is_high_speed), 0L) == 0L &
          (coalesce(as.integer(is_stop_start), 0L) == 1L |
             coalesce(as.integer(is_flowing_high_downforce), 0L) == 1L |
             coalesce(as.integer(is_low_overtake), 0L) == 1L),
        TRUE ~ FALSE
      )
    ) %>%
    filter(route_match) %>%
    select(-all_of(family_flags), -route_match)
}


qualifying_model_lookup <- qualifying_predictions %>%
  distinct(model, model_label) %>%
  mutate(model_order = case_when(
    model == "linear_quali_position" ~ 1L,
    model == "linear_quali_constructor" ~ 2L,
    model == "xgb_quali_constructor_track" ~ 3L,
    model == "xgb_quali_no_constructor" ~ 4L,
    model == "xgb_quali_high_speed_specialist" ~ 5L,
    model == "xgb_quali_technical_specialist" ~ 6L,
    model == "xgb_quali_position" ~ 7L,
    model == "xgb_quali_delta" ~ 8L,
    TRUE ~ 99L
  )) %>%
  arrange(model_order, model_label)

qualifying_model_choices <- setNames(qualifying_model_lookup$model, qualifying_model_lookup$model_label)

winner_without_model_lookup <- winner_without_predictions %>%
  distinct(model, model_label) %>%
  mutate(model_order = case_when(
    model == "xgb_winner_without_probability" ~ 1L,
    model == "xgb_winner_without_high_speed_specialist" ~ 2L,
    model == "xgb_winner_without_non_high_speed_specialist" ~ 3L,
    TRUE ~ 99L
  )) %>%
  arrange(model_order, model_label) %>%
  select(-model_order)

winner_without_model_choices <- setNames(winner_without_model_lookup$model, winner_without_model_lookup$model_label)

selected_or_default_models <- function(selected_models, default_models) {
  if (is.null(selected_models)) as.character(default_models) else as.character(selected_models)
}

driver_choices <- stage1 %>%
  filter(!is.na(driver_code), driver_code != "") %>%
  arrange(desc(season), driver_code) %>%
  distinct(driver_code, driver_name) %>%
  mutate(label = paste0(driver_code, " - ", driver_name)) %>%
  arrange(label)

constructor_choices <- stage1 %>%
  filter(!is.na(constructor_id), constructor_id != "") %>%
  group_by(constructor_id, constructor_name) %>%
  summarise(
    first_season = min(season, na.rm = TRUE),
    last_season = max(season, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(last_season), constructor_name) %>%
  distinct(constructor_id, .keep_all = TRUE) %>%
  mutate(label = paste0(constructor_name, " (", last_season, ")")) %>%
  arrange(label)

profile_default_start_season <- if (2024L %in% stage1$season) 2024L else min(stage1$season, na.rm = TRUE)

constructor_choices_for_window <- function(start_season, end_season) {
  constructor_choices %>%
    filter(first_season <= end_season, last_season >= start_season) %>%
    arrange(label)
}

default_constructor_profile_choices <- constructor_choices_for_window(
  profile_default_start_season,
  max(stage1$season, na.rm = TRUE)
)

format_num <- function(x, digits = 2) {
  ifelse(is.na(x) | is.infinite(x), "", format(round(x, digits), nsmall = digits, trim = TRUE))
}

format_pct <- function(x, digits = 1) {
  ifelse(is.na(x) | is.infinite(x), "", percent(x, accuracy = 10^-digits))
}

format_int <- function(x) {
  ifelse(is.na(x) | is.infinite(x), "", format(round(x, 0), nsmall = 0, trim = TRUE))
}

format_predicted_position <- function(x) {
  value <- format_int(x)
  ifelse(value == "", "", paste0("P", value))
}

format_sec <- function(x, digits = 3) {
  if (is.na(x) || is.infinite(x)) "Unavailable" else paste0(format_num(x, digits), " sec")
}

format_delta_sec <- function(x, digits = 3) {
  if (is.na(x) || is.infinite(x)) "Unavailable" else paste0("+", format_num(x, digits), " sec")
}

add_prerace_display <- function(rows) {
  rows %>%
    left_join(prerace_display_lookup, by = c("season", "round", "driver_code")) %>%
    mutate(
      display_start_position_label = if_else(
        !is.na(display_start_position),
        paste0(
          if_else(display_prerace_source == "estimate", "P", ""),
          format_int(display_start_position),
          if_else(coalesce(display_grid_started_from_pit, 0L) == 1L, " pit", "")
        ),
        ""
      ),
      display_quali_position_label = if_else(
        !is.na(display_quali_position),
        paste0(if_else(display_prerace_source == "estimate", "P", ""), format_int(display_quali_position)),
        ""
      ),
      display_quali_delta_label = if_else(
        !is.na(display_quali_delta_sec),
        format_num(display_quali_delta_sec, 3),
        ""
      )
    )
}

format_speed <- function(x, digits = 1) {
  if (is.na(x) || is.infinite(x)) "Unavailable" else paste0(format_num(x, digits), " kph")
}

american_label_to_decimal <- function(x) {
  x <- str_replace_all(as.character(x), "[^0-9+\\-.]", "")
  odds <- suppressWarnings(as.numeric(x))
  case_when(
    is.na(odds) ~ NA_real_,
    odds > 0 ~ 1 + odds / 100,
    odds < 0 ~ 1 + 100 / abs(odds),
    TRUE ~ NA_real_
  )
}

probability_to_american_label <- function(p) {
  odds <- case_when(
    is.na(p) | is.infinite(p) | p <= 0 | p >= 1 ~ NA_real_,
    p >= 0.5 ~ -100 * p / (1 - p),
    TRUE ~ 100 * (1 - p) / p
  )

  ifelse(
    is.na(odds),
    "",
    ifelse(odds > 0, paste0("+", as.integer(round(odds))), as.character(as.integer(round(odds))))
  )
}

probability_to_american_numeric <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  case_when(
    is.na(p) | is.infinite(p) | p <= 0 | p >= 1 ~ NA_real_,
    p >= 0.5 ~ -100 * p / (1 - p),
    TRUE ~ 100 * (1 - p) / p
  )
}

clamp_probability_numeric <- function(p, eps = 0.001) {
  pmin(pmax(suppressWarnings(as.numeric(p)), eps), 1 - eps)
}

format_signed_moneyline_delta <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(x) | is.infinite(x),
    "",
    paste0(ifelse(x > 0, "+", ""), format(round(x, 0), nsmall = 0, trim = TRUE))
  )
}

bet_profit <- function(decimal_odds, won, stake = 1) {
  case_when(
    is.na(decimal_odds) ~ NA_real_,
    is.na(won) ~ NA_real_,
    won ~ stake * (decimal_odds - 1),
    TRUE ~ -stake
  )
}

driver_speed_label <- function(row) {
  if (!is.na(row$max_speed_st_kph[[1]])) "Peak speed" else "Fastest-lap avg speed"
}

driver_speed_value <- function(row) {
  if (!is.na(row$max_speed_st_kph[[1]])) row$max_speed_st_kph[[1]] else row$top_speed_kph[[1]]
}

driver_speed_delta_label <- function(row) {
  if (!is.na(row$max_speed_st_kph[[1]])) "Peak speed delta" else "Avg speed delta"
}

driver_speed_delta_value <- function(row) {
  if (!is.na(row$max_speed_st_kph[[1]])) row$speed_st_delta_kph[[1]] else row$top_speed_delta_kph[[1]]
}

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

podium_odds_allowed <- function(odds_label, favorite_limit) {
  limit <- suppressWarnings(as.numeric(favorite_limit))
  if (length(limit) == 0 || is.na(limit) || is.infinite(limit) || limit <= 0) {
    return(rep(TRUE, length(odds_label)))
  }

  odds <- suppressWarnings(as.numeric(str_remove(as.character(odds_label), "^est\\s+")))
  is.na(odds) | odds >= -abs(limit)
}

model_edge_allowed <- function(model_edge, minimum_edge_pct) {
  minimum_edge <- suppressWarnings(as.numeric(minimum_edge_pct))
  if (length(minimum_edge) == 0 || is.na(minimum_edge) || is.infinite(minimum_edge) || minimum_edge <= -100) {
    return(rep(TRUE, length(model_edge)))
  }

  !is.na(model_edge) & model_edge >= minimum_edge / 100
}

apply_podium_display_odds <- function(rows, use_estimated) {
  if (!"podium_avg_american_odds_label" %in% names(rows)) rows$podium_avg_american_odds_label <- NA_character_
  if (!"podium_effective_avg_american_odds_label" %in% names(rows)) rows$podium_effective_avg_american_odds_label <- NA_character_
  if (!"podium_market_no_vig_probability" %in% names(rows)) rows$podium_market_no_vig_probability <- NA_real_
  if (!"podium_effective_no_vig_probability" %in% names(rows)) rows$podium_effective_no_vig_probability <- NA_real_
  if (!"podium_odds_source" %in% names(rows)) rows$podium_odds_source <- NA_character_
  if (!"podium_avg_american_odds" %in% names(rows)) rows$podium_avg_american_odds <- NA_real_
  if (!"podium_effective_avg_american_odds" %in% names(rows)) rows$podium_effective_avg_american_odds <- NA_real_

  rows %>%
    mutate(
      podium_avg_american_odds_label = na_if(as.character(podium_avg_american_odds_label), ""),
      podium_effective_avg_american_odds_label = na_if(as.character(podium_effective_avg_american_odds_label), ""),
      podium_display_american_odds_label = if (isTRUE(use_estimated)) {
        coalesce(podium_effective_avg_american_odds_label, podium_avg_american_odds_label)
      } else {
        podium_avg_american_odds_label
      },
      podium_display_no_vig_probability = if (isTRUE(use_estimated)) {
        coalesce(podium_effective_no_vig_probability, podium_market_no_vig_probability)
      } else {
        podium_market_no_vig_probability
      },
      podium_display_odds_source = if (isTRUE(use_estimated)) {
        coalesce(
          podium_odds_source,
          case_when(
            !is.na(podium_avg_american_odds) ~ "market",
            !is.na(podium_effective_avg_american_odds) ~ "estimated_from_win",
            TRUE ~ "missing"
          )
        )
      } else {
        if_else(!is.na(podium_avg_american_odds), "market", "missing")
      },
      podium_display_american_odds_label = if_else(
        podium_display_odds_source == "estimated_from_win" &
          !is.na(podium_display_american_odds_label) &
          podium_display_american_odds_label != "" &
          !str_detect(str_to_lower(podium_display_american_odds_label), "^est\\s+"),
        paste0("est ", podium_display_american_odds_label),
        podium_display_american_odds_label
      )
    )
}

format_pct <- function(x, accuracy = 0.1) {
  ifelse(is.na(x) | is.infinite(x), "Unavailable", percent(x, accuracy = accuracy))
}

age_years <- function(date_of_birth, today = Sys.Date()) {
  date_of_birth <- date_of_birth[!is.na(date_of_birth)]
  if (length(date_of_birth) == 0) return(NA_integer_)

  date_of_birth <- date_of_birth[[1]]
  age <- as.integer(format(today, "%Y")) - as.integer(format(date_of_birth, "%Y"))
  had_birthday <- format(today, "%m%d") >= format(date_of_birth, "%m%d")
  if (!had_birthday) age <- age - 1L
  age
}

selected_event_data <- function(season, round_id) {
  stage1 %>%
    filter(season == !!season, round == !!round_id)
}

selected_event_meta <- function(season, round_id) {
  race_choices %>%
    filter(season == !!season, round == !!round_id) %>%
    slice(1)
}

static_track_image <- function(event_rows) {
  circuit_id <- event_rows$circuit_id[[1]]
  candidates <- c(
    file.path(static_track_dir, paste0(circuit_id, ".png")),
    file.path(static_track_dir, paste0(event_rows$season[[1]], "_", sprintf("%02d", event_rows$round[[1]]), "_", circuit_id, ".png"))
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) NULL else found[[1]]
}

driver_family_rows <- function(driver_code, start_season, end_season) {
  driver_data <- stage1 %>%
    filter(
      driver_code == !!driver_code,
      season >= !!start_season,
      season <= !!end_season
    )

  bind_rows(lapply(family_flags, function(flag) {
    driver_data %>%
      filter(.data[[flag]] == 1) %>%
      mutate(
        family_flag = flag,
        family = family_labels[[flag]]
      )
  }))
}

constructor_family_rows <- function(constructor_id, start_season, end_season) {
  constructor_data <- stage1 %>%
    filter(
      constructor_id == !!constructor_id,
      season >= !!start_season,
      season <= !!end_season
    )

  bind_rows(lapply(family_flags, function(flag) {
    constructor_data %>%
      filter(.data[[flag]] == 1) %>%
      mutate(
        family_flag = flag,
        family = family_labels[[flag]]
      )
  }))
}

summarise_driver_family <- function(profile_rows) {
  if (nrow(profile_rows) == 0) return(tibble())

  profile_rows %>%
    group_by(family_flag, family) %>%
    summarise(
      starts = n(),
      avg_finish = safe_mean(finish_position),
      avg_points = safe_mean(points),
      points_per_start = sum(points, na.rm = TRUE) / n(),
      podium_rate = mean(podium, na.rm = TRUE),
      top10_rate = mean(top10_finish, na.rm = TRUE),
      dnf_rate = mean(dnf_flag, na.rm = TRUE),
      avg_quali_delta = safe_mean(best_quali_delta_sec),
      avg_fastest_lap_delta = safe_mean(fastest_lap_delta_sec),
      .groups = "drop"
    ) %>%
    arrange(avg_finish)
}

summarise_driver_cluster <- function(profile_rows) {
  if (nrow(profile_rows) == 0) return(tibble())

  profile_rows %>%
    filter(!is.na(track_cluster_id), track_cluster_id != "") %>%
    group_by(track_cluster_id, track_cluster_label, cluster_peer_circuits) %>%
    summarise(
      starts = n(),
      avg_finish = safe_mean(finish_position),
      avg_points = safe_mean(points),
      points_per_start = sum(points, na.rm = TRUE) / n(),
      podium_rate = mean(podium, na.rm = TRUE),
      top10_rate = mean(top10_finish, na.rm = TRUE),
      dnf_rate = mean(dnf_flag, na.rm = TRUE),
      avg_quali_delta = safe_mean(best_quali_delta_sec),
      avg_fastest_lap_delta = safe_mean(fastest_lap_delta_sec),
      .groups = "drop"
    ) %>%
    mutate(
      cluster_number = as.integer(cluster_display_number(track_cluster_id)),
      cluster = cluster_display_label(track_cluster_id, track_cluster_label),
      similar_tracks = coalesce(cluster_peer_circuits, "")
    ) %>%
    arrange(avg_finish)
}

plot_track_layout <- function(event_rows) {
  if (!requireNamespace("f1dataR", quietly = TRUE)) {
    return(NULL)
  }

  season <- event_rows$season[[1]]
  round_id <- event_rows$round[[1]]

  if (!all(c("finish_position", "driver_code") %in% names(event_rows))) {
    return(NULL)
  }

  driver <- event_rows %>%
    arrange(finish_position) %>%
    filter(!is.na(driver_code), driver_code != "") %>%
    slice(1) %>%
    pull(driver_code)

  if (length(driver) == 0 || is.na(driver[[1]])) return(NULL)

  telemetry <- tryCatch(
    f1dataR::load_driver_telemetry(
      season = season,
      round = round_id,
      session = "Q",
      driver = driver[[1]],
      laps = "fastest"
    ),
    error = function(e) NULL
  )

  if (is.null(telemetry) || nrow(telemetry) == 0) return(NULL)

  circuit <- tryCatch(
    f1dataR::load_circuit_details(season = season, round = round_id),
    error = function(e) NULL
  )

  p <- ggplot(telemetry, aes(x = x, y = y, color = speed, group = 1)) +
    geom_path(linewidth = 2.4, lineend = "round") +
    scale_color_gradient(low = "#39A0ED", high = "#E10600", na.value = "#9AA0A6") +
    coord_equal() +
    theme_void(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.background = element_rect(fill = "#101418", color = NA),
      panel.background = element_rect(fill = "#101418", color = NA),
      legend.text = element_text(color = "#E8EAED"),
      legend.title = element_text(color = "#E8EAED")
    ) +
    labs(color = "Speed")

  corners <- circuit$corners
  if (!is.null(corners) && nrow(corners) > 0) {
    labelx <- function(x, angle, distance = 650) cos(angle * pi / 180) * distance + x
    labely <- function(y, angle, distance = 650) sin(angle * pi / 180) * distance + y

    corners <- corners %>%
      mutate(
        labx = labelx(x, angle),
        laby = labely(y, angle),
        corner_label = paste0(number, letter)
      )

    p <- p +
      geom_label(
        data = corners,
        aes(x = labx, y = laby, label = corner_label),
        inherit.aes = FALSE,
        size = 3,
        label.size = 0,
        fill = "#F8F9FA",
        color = "#202124"
      )
  }

  p
}

profile_metric_limits <- function(reference_summary, metric, padding = 0.05) {
  if (nrow(reference_summary) == 0 || !metric %in% names(reference_summary)) return(NULL)

  values <- suppressWarnings(as.numeric(reference_summary[[metric]]))
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NULL)

  lower <- min(0, min(values))
  upper <- max(0, max(values))
  span <- upper - lower
  if (!is.finite(span) || span <= 0) span <- max(abs(c(lower, upper)), 1)

  c(
    if (lower < 0) lower - padding * span else 0,
    upper + padding * span
  )
}

theme_f1_dark <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(color = "#DCE3ED"),
      axis.text = element_text(color = "#AEB8C7"),
      axis.title = element_text(color = "#AEB8C7"),
      plot.background = element_rect(fill = "#151A22", color = NA),
      panel.background = element_rect(fill = "#151A22", color = NA),
      panel.grid.major = element_line(color = "#29313E"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(color = "#F7F9FC", face = "bold"),
      plot.subtitle = element_text(color = "#9DA8B8"),
      plot.caption = element_text(color = "#8994A5"),
      legend.background = element_rect(fill = "#151A22", color = NA),
      legend.key = element_rect(fill = "#151A22", color = NA),
      legend.text = element_text(color = "#AEB8C7"),
      legend.title = element_text(color = "#DCE3ED")
    )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=f1-dark-3"),
    tags$link(rel = "stylesheet", type = "text/css", href = "dark-theme.css?v=f1-dark-3"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", href = "f1-logo.png"),
    tags$link(rel = "apple-touch-icon", href = "apple-touch-icon.png"),
    tags$style(HTML("
      #app-busy-overlay { position: fixed; inset: 0; z-index: 99999; display: none; align-items: center; justify-content: center; background: rgba(8, 11, 17, 0.68); backdrop-filter: blur(2px); }
      #app-busy-overlay.is-active { display: flex; }
      .app-busy-card { min-width: 240px; padding: 24px 30px; border: 1px solid rgba(255,255,255,.18); border-radius: 14px; background: #151a22; color: #edf2f8; text-align: center; box-shadow: 0 18px 50px rgba(0,0,0,.45); }
      .app-busy-hourglass { display: block; margin-bottom: 8px; font-size: 34px; animation: busy-pulse 1s ease-in-out infinite alternate; }
      .app-busy-label { font-size: 16px; font-weight: 700; }
      .app-busy-detail { margin-top: 5px; color: #aeb8c7; font-size: 13px; }
      @keyframes busy-pulse { from { opacity: .55; transform: scale(.96); } to { opacity: 1; transform: scale(1.04); } }
    ")),
    tags$script(HTML("
      (function () {
        var busyTimer = null;
        var busyEnabled = false;
        function fantasyTabIsActive() {
          var tabLinks = document.querySelectorAll('#main_tabs a[data-toggle=tab]');
          var fantasyLink = Array.prototype.slice.call(tabLinks).find(function (link) {
            return link.textContent.trim() === 'Fantasy Lineup';
          });
          return Boolean(fantasyLink && fantasyLink.parentElement && fantasyLink.parentElement.classList.contains('active'));
        }
        function showBusy() {
          if (!busyEnabled || !fantasyTabIsActive()) {
            hideBusy();
            return;
          }
          window.clearTimeout(busyTimer);
          busyTimer = window.setTimeout(function () {
            var overlay = document.getElementById('app-busy-overlay');
            if (overlay && fantasyTabIsActive()) overlay.classList.add('is-active');
          }, 180);
        }
        function hideBusy() {
          window.clearTimeout(busyTimer);
          var overlay = document.getElementById('app-busy-overlay');
          if (overlay) overlay.classList.remove('is-active');
        }
        window.enableAppBusyOverlay = function () {
          hideBusy();
          busyEnabled = true;
        };
        document.addEventListener('DOMContentLoaded', function () {
          if (window.jQuery) {
            window.jQuery(document)
              .on('shiny:busy', showBusy)
              .on('shiny:idle', hideBusy)
              .on('shown.bs.tab', '#main_tabs a[data-toggle=tab]', function () {
                if (!fantasyTabIsActive()) hideBusy();
              });
          }
        });
      })();
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        var enterButton = document.getElementById('enter-dashboard');
        var splash = document.getElementById('splash-screen');
        var hiddenTabs = [];
        var mainTabs = document.getElementById('main_tabs');

        function findDirectTabItem(nav, label) {
          return Array.prototype.slice.call(nav.children).find(function (item) {
            var link = item.firstElementChild;
            return link && link.matches('a[data-toggle=tab]') && link.textContent.trim() === label;
          });
        }

        function createTabGroup(nav, label, childLabels) {
          var firstItem = findDirectTabItem(nav, childLabels[0]);
          if (!firstItem) return;

          var group = document.createElement('li');
          group.className = 'dropdown nav-group';
          group.setAttribute('data-nav-group', label);

          var toggle = document.createElement('a');
          toggle.href = '#';
          toggle.className = 'dropdown-toggle';
          toggle.setAttribute('data-toggle', 'dropdown');
          toggle.setAttribute('data-bs-toggle', 'dropdown');
          toggle.setAttribute('role', 'button');
          toggle.setAttribute('aria-haspopup', 'true');
          toggle.setAttribute('aria-expanded', 'false');

          var groupLabel = document.createElement('span');
          groupLabel.className = 'nav-group-label';
          groupLabel.textContent = label;
          toggle.appendChild(groupLabel);
          toggle.appendChild(document.createTextNode(' '));

          var caret = document.createElement('span');
          caret.className = 'caret';
          toggle.appendChild(caret);

          var menu = document.createElement('ul');
          menu.className = 'dropdown-menu';
          menu.setAttribute('role', 'menu');

          group.appendChild(toggle);
          group.appendChild(menu);
          nav.insertBefore(group, firstItem);

          childLabels.forEach(function (childLabel) {
            var item = findDirectTabItem(nav, childLabel);
            if (item) menu.appendChild(item);
          });
        }

        function syncTabGroups() {
          if (!mainTabs) return;
          Array.prototype.slice.call(mainTabs.children).forEach(function (group) {
            if (!group.classList.contains('nav-group')) return;
            group.classList.toggle('active', Boolean(group.querySelector('.dropdown-menu > li.active')));
          });
        }

        if (mainTabs) {
          createTabGroup(mainTabs, 'Drivers and Tracks', ['Tracks', 'Driver Profiles', 'Constructor Profiles']);
          createTabGroup(mainTabs, 'Qualifying', ['Qualifying', 'Qualifying With Chatter']);
          createTabGroup(mainTabs, 'Other', ['Winner Without', 'Fastest Lap Model', 'Wet Weather']);
          syncTabGroups();

          if (window.jQuery) {
            window.jQuery(mainTabs).on('shown.bs.tab', 'a[data-toggle=tab]', syncTabGroups);
          }
          mainTabs.addEventListener('click', function () {
            window.setTimeout(syncTabGroups, 0);
          });
        }
        document.querySelectorAll('a[data-toggle=\"tab\"]').forEach(function (tabLink) {
          if (hiddenTabs.indexOf(tabLink.textContent.trim()) !== -1 && tabLink.parentElement) {
            tabLink.parentElement.style.display = 'none';
          }
        });
        if (enterButton && splash) {
          enterButton.addEventListener('click', function () {
            splash.classList.add('splash-hidden');
            window.setTimeout(function () {
              splash.style.display = 'none';
              if (window.enableAppBusyOverlay) window.enableAppBusyOverlay();
            }, 450);
          });
        }
      });
    "))
  ),
  div(
    id = "app-busy-overlay",
    role = "status",
    `aria-live` = "polite",
    div(
      class = "app-busy-card",
      span(class = "app-busy-hourglass", "⌛"),
      div(class = "app-busy-label", "Generating fantasy lineups"),
      div(class = "app-busy-detail", "Please wait while projections and portfolio safeguards are applied.")
    )
  ),
  div(
    id = "splash-screen",
    class = "splash-screen",
    div(
      class = "splash-visual full",
      div(class = "splash-bg", role = "img", `aria-label` = "Las Vegas Grand Prix grandstands at night"),
      div(class = "splash-overlay"),
      div(
        class = "splash-center",
        img(src = "f1-logo.png", class = "splash-logo", alt = "Formula 1 logo"),
        tags$button(id = "enter-dashboard", type = "button", class = "enter-button", "Enter dashboard")
      )
    )
  ),
  div(
    class = "app-shell",
    tags$main(
      class = "main-content",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Tracks",
          div(
            class = "track-tab-layout",
            div(
              class = "track-controls",
              h1("F1 Track Dashboard"),
              selectInput(
                "season",
                "Season",
                choices = sort(unique(race_choices$season), decreasing = TRUE),
                selected = max(race_choices$season, na.rm = TRUE)
              ),
              uiOutput("race_selector"),
              selectInput(
                "driver",
                "Driver",
                choices = c("All drivers" = "all"),
                selected = "all"
              )
            ),
            div(
              class = "track-tab-content",
              uiOutput("event_header"),
              fluidRow(
                column(
                  width = 6,
                  div(
                    class = "panel metrics-panel",
                    uiOutput("snapshot_title"),
                    uiOutput("race_metrics")
                  )
                ),
                column(
                  width = 6,
                  div(
                    class = "panel",
                    h2("Track Families"),
                    uiOutput("family_chips"),
                    uiOutput("family_notes")
                  )
                )
              ),
              div(
                class = "panel track-panel",
                h2("Circuit View"),
                uiOutput("track_visual")
              ),
              fluidRow(
                column(
                  width = 12,
                  div(
                    class = "panel",
                    h2("Recent Circuit Winners"),
                    div(class = "recent-winners-table", tableOutput("recent_winners"))
                  )
                ),
                column(
                  width = 12,
                  div(
                    class = "panel",
                    h2("Selected Race Results"),
                    div(class = "race-results-table", tableOutput("race_results"))
                  )
                )
              ),
              div(
                class = "panel",
                h2("Grid vs Finish"),
                plotOutput("grid_finish_plot", height = "340px")
              )
            )
          )
        ),
        tabPanel(
          "Driver Profiles",
          div(
            class = "profile-tab-layout",
            div(
              class = "profile-controls",
              h1("Driver Profiles"),
              selectInput(
                "profile_driver",
                "Driver",
                choices = setNames(driver_choices$driver_code, driver_choices$label),
                selected = if ("ANT" %in% driver_choices$driver_code) "ANT" else driver_choices$driver_code[[1]]
              ),
              selectInput(
                "profile_start_season",
                "From",
                choices = sort(unique(stage1$season)),
                selected = profile_default_start_season
              ),
              selectInput(
                "profile_end_season",
                "To",
                choices = sort(unique(stage1$season)),
                selected = max(stage1$season, na.rm = TRUE)
              ),
              checkboxInput(
                "profile_exclude_dnf",
                "Exclude did not finishers",
                value = FALSE
              ),
              uiOutput("profile_constructor_selector"),
              selectInput(
                "profile_metric",
                "Chart metric",
                choices = c(
                  "Average finish" = "avg_finish",
                  "Points per start" = "points_per_start",
                  "Top-10 rate" = "top10_rate",
                  "Podium rate" = "podium_rate",
                  "Average quali delta" = "avg_quali_delta",
                  "Average fastest-lap delta" = "avg_fastest_lap_delta"
                ),
                selected = "avg_finish"
              )
            ),
            div(
              class = "profile-tab-content",
              uiOutput("profile_summary_cards"),
              div(
                class = "panel",
                h2("Performance by Track Family"),
                plotOutput("driver_family_plot", height = "420px")
              ),
              div(
                class = "panel",
                h2("Track-Family Detail"),
                div(class = "driver-family-table", tableOutput("driver_family_table"))
              ),
              div(
                class = "panel",
                h2("Performance by Track Cluster"),
                plotOutput("driver_cluster_plot", height = "380px")
              ),
              div(
                class = "panel",
                h2("Track-Cluster Detail"),
                div(class = "driver-family-table", tableOutput("driver_cluster_table"))
              ),
              div(
                class = "panel",
                h2("Best Circuits for Selected Driver"),
                div(class = "driver-circuit-table", tableOutput("driver_circuit_table"))
              ),
              div(
                class = "panel",
                h2("Recent Driver Results"),
                div(class = "driver-recent-table", tableOutput("driver_recent_results"))
              )
            )
          )
        ),
        tabPanel(
          "Constructor Profiles",
          div(
            class = "profile-tab-layout",
            div(
              class = "profile-controls",
              h1("Constructor Profiles"),
              selectInput(
                "constructor_profile_constructor",
                "Constructor",
                choices = setNames(default_constructor_profile_choices$constructor_id, default_constructor_profile_choices$label),
                selected = if ("mercedes" %in% default_constructor_profile_choices$constructor_id) {
                  "mercedes"
                } else {
                  default_constructor_profile_choices$constructor_id[[1]]
                }
              ),
              selectInput(
                "constructor_profile_start_season",
                "From",
                choices = sort(unique(stage1$season)),
                selected = profile_default_start_season
              ),
              selectInput(
                "constructor_profile_end_season",
                "To",
                choices = sort(unique(stage1$season)),
                selected = max(stage1$season, na.rm = TRUE)
              ),
              checkboxInput(
                "constructor_profile_exclude_dnf",
                "Exclude did not finishers",
                value = FALSE
              ),
              selectInput(
                "constructor_profile_metric",
                "Chart metric",
                choices = c(
                  "Average finish" = "avg_finish",
                  "Points per start" = "points_per_start",
                  "Top-10 rate" = "top10_rate",
                  "Podium rate" = "podium_rate",
                  "Average quali delta" = "avg_quali_delta",
                  "Average fastest-lap delta" = "avg_fastest_lap_delta"
                ),
                selected = "avg_finish"
              )
            ),
            div(
              class = "profile-tab-content",
              uiOutput("constructor_profile_summary_cards"),
              div(
                class = "panel",
                h2("Performance by Track Family"),
                plotOutput("constructor_family_plot", height = "420px")
              ),
              div(
                class = "panel",
                h2("Track-Family Detail"),
                div(class = "driver-family-table", tableOutput("constructor_family_table"))
              ),
              div(
                class = "panel",
                h2("Performance by Track Cluster"),
                plotOutput("constructor_cluster_plot", height = "380px")
              ),
              div(
                class = "panel",
                h2("Track-Cluster Detail"),
                div(class = "driver-family-table", tableOutput("constructor_cluster_table"))
              ),
              div(
                class = "panel",
                h2("Best Circuits for Selected Constructor"),
                div(class = "driver-circuit-table", tableOutput("constructor_circuit_table"))
              ),
              div(
                class = "panel",
                h2("Recent Constructor Results"),
                div(class = "driver-recent-table", tableOutput("constructor_recent_results"))
              )
            )
          )
        )
        ,
#         tabPanel(
#           "Random Forests",
#           div(
#             class = "tree-tab-layout",
#             div(
#               class = "tree-controls",
#               h1("Random Forests"),
#               selectInput(
#                 "tree_roi_start_season",
#                 "ROI start season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "tree_roi_end_season",
#                 "ROI end season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "tree_season",
#                 "Season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               uiOutput("tree_race_selector"),
#               checkboxGroupInput(
#                 "tree_models",
#                 "RF models",
#                 choices = rf_model_choices,
#                 selected = rf_model_lookup$model
#               ),
#               checkboxInput(
#                 "use_estimated_podium_odds",
#                 "Use estimated podium odds",
#                 value = TRUE
#               ),
#               numericInput(
#                 "podium_favorite_limit",
#                 "Podium favorite cutoff",
#                 value = 300,
#                 min = 0,
#                 step = 25
#               )
#             ),
#             div(
#               class = "tree-tab-content",
#               uiOutput("tree_model_header"),
#               div(
#                 class = "panel",
#                 h2("Consensus Season Betting ROI"),
#                 tableOutput("rf_betting_season_summary")
#               ),
#               div(
#                 class = "panel",
#                 h2("Selected Race Consensus Bets"),
#                 tableOutput("rf_consensus_bets_table")
#               ),
#               div(
#                 class = "panel",
#                 h2("Predicted Winner"),
#                 tableOutput("tree_winner_table")
#               ),
#               div(
#                 class = "panel",
#                 h2("Full Predicted Order"),
#                 div(class = "tree-prediction-table", tableOutput("tree_prediction_table"))
#               )
#             )
#           )
#         ),
#         tabPanel(
#           "RF Probabilities",
#           div(
#             class = "tree-tab-layout",
#             div(
#               class = "tree-controls",
#               h1("RF Probabilities"),
#               selectInput(
#                 "prob_roi_start_season",
#                 "ROI start season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "prob_roi_end_season",
#                 "ROI end season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "prob_season",
#                 "Season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               uiOutput("prob_race_selector"),
#               checkboxGroupInput(
#                 "prob_models",
#                 "RF probability models",
#                 choices = rf_probability_model_choices,
#                 selected = rf_probability_model_lookup$model
#               ),
#               checkboxInput(
#                 "prob_use_estimated_podium_odds",
#                 "Use estimated podium odds",
#                 value = TRUE
#               ),
#               numericInput(
#                 "prob_podium_favorite_limit",
#                 "Podium favorite cutoff",
#                 value = 300,
#                 min = 0,
#                 step = 25
#               ),
#               numericInput(
#                 "prob_min_edge_pct",
#                 "Minimum edge (%)",
#                 value = -100,
#                 min = -100,
#                 max = 100,
#                 step = 1
#               )
#             ),
#             div(
#               class = "tree-tab-content",
#               uiOutput("prob_model_header"),
#               div(
#                 class = "panel",
#                 h2("Consensus Season Betting ROI"),
#                 tableOutput("prob_betting_season_summary")
#               ),
#               div(
#                 class = "panel",
#                 h2("Selected Race Consensus Bets"),
#                 tableOutput("prob_consensus_bets_table")
#               ),
#               div(
#                 class = "panel",
#                 h2("Predicted Winner"),
#                 tableOutput("prob_winner_table")
#               ),
#               div(
#                 class = "panel",
#                 h2("Full Predicted Order"),
#                 div(class = "tree-prediction-table", tableOutput("prob_prediction_table"))
#               )
#             )
#           )
#         ),
#         tabPanel(
#           "RF Consensus",
#           div(
#             class = "tree-tab-layout",
#             div(
#               class = "tree-controls",
#               h1("RF Consensus"),
#               selectInput(
#                 "allrf_roi_start_season",
#                 "ROI start season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "allrf_roi_end_season",
#                 "ROI end season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               selectInput(
#                 "allrf_season",
#                 "Season",
#                 choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
#                 selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
#               ),
#               uiOutput("allrf_race_selector"),
#               checkboxInput("allrf_use_finish", "Include finish RF family", value = TRUE),
#               checkboxInput("allrf_use_probability", "Include probability RF family", value = TRUE),
#               checkboxInput("allrf_force_consensus", "Force consensus", value = FALSE),
#               checkboxInput("allrf_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
#               numericInput(
#                 "allrf_podium_favorite_limit",
#                 "Podium favorite cutoff",
#                 value = 300,
#                 min = 0,
#                 step = 25
#               ),
#               numericInput(
#                 "allrf_min_edge_pct",
#                 "Minimum edge (%)",
#                 value = -100,
#                 min = -100,
#                 max = 100,
#                 step = 1
#               )
#             ),
#             div(
#               class = "tree-tab-content",
#               uiOutput("allrf_model_header"),
#               div(class = "panel", h2("All-Family Consensus Season Betting ROI"), tableOutput("allrf_betting_season_summary")),
#               div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("allrf_consensus_bets_table")),
#               div(class = "panel", h2("Predicted Winner"), tableOutput("allrf_winner_table")),
#               div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("allrf_prediction_table")))
#             )
#           )
#         ),
        tabPanel(
          "Qualifying",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Qualifying"),
              selectInput(
                "qualifying_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("qualifying_race_selector"),
              checkboxGroupInput(
                "qualifying_models",
                "Qualifying model",
                choices = qualifying_model_choices,
                selected = qualifying_model_lookup$model
              )
            ),
            div(
              class = "tree-tab-content",
              uiOutput("qualifying_model_header"),
              div(class = "panel", h2("Historical Model Success"), tableOutput("qualifying_metrics_table")),
              div(class = "panel", h2("Predicted Pole"), tableOutput("qualifying_pole_table")),
              div(class = "panel", h2("Full Predicted Qualifying Order"), div(class = "tree-prediction-table", tableOutput("qualifying_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Qualifying With Chatter",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Qualifying With Chatter"),
              selectInput(
                "chatter_quali_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("chatter_quali_race_selector"),
              checkboxGroupInput(
                "chatter_quali_models",
                "Qualifying model",
                choices = qualifying_model_choices,
                selected = qualifying_model_lookup$model
              )
            ),
            div(
              class = "tree-tab-content",
              uiOutput("chatter_quali_header"),
              div(class = "panel", h2("Historical Qualifying Impact"), tableOutput("chatter_quali_history_table")),
              div(class = "panel", h2("Predicted Pole With Chatter"), tableOutput("chatter_quali_pole_table")),
              div(class = "panel", h2("Full Qualifying Order With Chatter"), div(class = "tree-prediction-table", tableOutput("chatter_quali_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Winner Without",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Winner Without"),
              selectInput(
                "winner_without_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("winner_without_race_selector"),
              checkboxGroupInput(
                "winner_without_models",
                "Winner without model",
                choices = winner_without_model_choices,
                selected = winner_without_model_lookup$model
              )
            ),
            div(
              class = "tree-tab-content",
              uiOutput("winner_without_model_header"),
              div(class = "panel", h2("Historical Model Success"), tableOutput("winner_without_metrics_table")),
              div(class = "panel", h2("Predicted Winner Without"), tableOutput("winner_without_pick_table")),
              div(class = "panel", h2("Eligible Field Order"), div(class = "tree-prediction-table", tableOutput("winner_without_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Finish Model",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Finish Model"),
              selectInput(
                "xgb_roi_start_season",
                "ROI start season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
              ),
              selectInput(
                "xgb_roi_end_season",
                "ROI end season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              selectInput(
                "xgb_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("xgb_race_selector"),
              checkboxGroupInput(
                "xgb_models",
                "XGBoost models",
                choices = xgb_finish_model_choices,
                selected = xgb_finish_default_models
              ),
              checkboxInput("xgb_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("xgb_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("xgb_use_edge_filter", "Use probability value-edge filters", value = FALSE),
              numericInput("xgb_min_win_edge_pct", "Minimum win value edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("xgb_min_podium_edge_pct", "Minimum podium value edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("xgb_model_header"),
              div(class = "panel", h2("Consensus Season Betting ROI"), tableOutput("xgb_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("xgb_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("xgb_winner_table")),
              div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("xgb_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Probabilities Model",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Probabilities Model"),
              selectInput(
                "xgb_prob_roi_start_season",
                "ROI start season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
              ),
              selectInput(
                "xgb_prob_roi_end_season",
                "ROI end season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              selectInput(
                "xgb_prob_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("xgb_prob_race_selector"),
              checkboxGroupInput(
                "xgb_prob_models",
                "Probability models",
                choices = xgb_probability_model_choices,
                selected = xgb_probability_default_models
              ),
              checkboxInput("xgb_prob_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("xgb_prob_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("xgb_prob_use_edge_filter", "Use edge filters", value = FALSE),
              numericInput("xgb_prob_min_win_edge_pct", "Minimum win edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("xgb_prob_min_podium_edge_pct", "Minimum podium edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("xgb_prob_model_header"),
              div(class = "panel", h2("Consensus Season Betting ROI"), tableOutput("xgb_prob_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("xgb_prob_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("xgb_prob_winner_table")),
              div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("xgb_prob_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Points Model",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Points Model"),
              selectInput("xgb_points_roi_start_season", "ROI start season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL),
              selectInput("xgb_points_roi_end_season", "ROI end season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL),
              selectInput("xgb_points_season", "Season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL),
              uiOutput("xgb_points_race_selector"),
              checkboxGroupInput("xgb_points_models", "Points models", choices = xgb_points_model_choices, selected = xgb_points_default_models),
              checkboxInput("xgb_points_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("xgb_points_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("xgb_points_use_edge_filter", "Use probability value-edge filters", value = FALSE),
              numericInput("xgb_points_min_win_edge_pct", "Minimum win value edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("xgb_points_min_podium_edge_pct", "Minimum podium value edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("xgb_points_model_header"),
              div(class = "panel", h2("Consensus Season Betting ROI"), tableOutput("xgb_points_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("xgb_points_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("xgb_points_winner_table")),
              div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("xgb_points_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Routed Specialists",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Routed Specialists"),
              selectInput("routed_roi_start_season", "ROI start season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL),
              selectInput("routed_roi_end_season", "ROI end season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL),
              selectInput("routed_season", "Season", choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE), selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL),
              uiOutput("routed_race_selector"),
              checkboxInput("routed_use_defaults", "Use defaults", value = FALSE),
              checkboxInput("routed_select_all", "Select all", value = FALSE),
              checkboxInput("routed_clear_all", "Clear all", value = FALSE),
              checkboxGroupInput("routed_models", "Routed specialist models", choices = routed_specialist_model_choices, selected = routed_specialist_default_models),
              checkboxInput("routed_force_consensus", "Force consensus", value = FALSE),
              checkboxInput("routed_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("routed_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("routed_use_edge_filter", "Use edge filters", value = FALSE),
              numericInput("routed_min_win_edge_pct", "Minimum win edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("routed_min_podium_edge_pct", "Minimum podium edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("routed_model_header"),
              div(class = "panel", h2("Routed Specialist Season Betting ROI"), tableOutput("routed_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("routed_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("routed_winner_table")),
              div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("routed_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Model Consensus",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Model Consensus"),
              selectInput(
                "allmodel_roi_start_season",
                "ROI start season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2025L)) 2025L else if (nrow(rf_race_choices) > 0) min(rf_race_choices$season) else NULL
              ),
              selectInput(
                "allmodel_roi_end_season",
                "ROI end season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              selectInput(
                "allmodel_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("allmodel_race_selector"),
              checkboxInput("allmodel_use_xgb_finish", "Include finish model family", value = TRUE),
              checkboxInput("allmodel_use_xgb_probability", "Include probability model family", value = TRUE),
              checkboxInput("allmodel_use_xgb_points", "Include points model family", value = TRUE),
              checkboxInput("allmodel_use_routed_specialists", "Include routed specialists", value = TRUE),
              radioButtons(
                "allmodel_consensus_mode",
                "Consensus method",
                choices = c("Family weighted" = "family", "Selected model weighted" = "model"),
                selected = "family"
              ),
              checkboxInput("allmodel_force_consensus", "Force consensus", value = FALSE),
              checkboxInput("allmodel_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("allmodel_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("allmodel_use_edge_filter", "Use edge filters", value = FALSE),
              numericInput("allmodel_min_win_edge_pct", "Minimum win edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("allmodel_min_podium_edge_pct", "Minimum podium edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("allmodel_model_header"),
              div(class = "panel", h2("All-Model Consensus Season Betting ROI"), tableOutput("allmodel_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("allmodel_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("allmodel_winner_table")),
              div(class = "panel", h2("Full Predicted Order"), div(class = "tree-prediction-table", tableOutput("allmodel_prediction_table")))
            )
          )
        ),
        tabPanel(
          "Chatter Overlay",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Chatter Overlay"),
              selectInput(
                "chatter_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("chatter_race_selector"),
              selectInput(
                "chatter_projection",
                "Projection view",
                choices = c(
                  "Combined driver view" = "combined",
                  "Finish adjustment" = "finish",
                  "Probability adjustment" = "probability",
                  "Points adjustment" = "points",
                  "Winner without adjustment" = "winner_without"
                ),
                selected = "combined"
              ),
              checkboxInput("chatter_use_estimated_podium_odds", "Use estimated podium odds", value = TRUE),
              numericInput("chatter_podium_favorite_limit", "Podium favorite cutoff", value = 350, min = 0, step = 25),
              checkboxInput("chatter_use_edge_filter", "Use edge filters", value = FALSE),
              numericInput("chatter_min_win_edge_pct", "Minimum win edge (%)", value = -5, min = -100, max = 100, step = 1),
              numericInput("chatter_min_podium_edge_pct", "Minimum podium edge (%)", value = -10, min = -100, max = 100, step = 1)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("chatter_header"),
              div(class = "panel", h2("All-Model Consensus Season Betting ROI"), tableOutput("chatter_allmodel_betting_season_summary")),
              div(class = "panel", h2("Selected Race Consensus Bets"), tableOutput("chatter_allmodel_consensus_bets_table")),
              div(class = "panel", h2("Predicted Winner"), tableOutput("chatter_allmodel_winner_table")),
              div(class = "panel", h2("Driver Projection Overlay"), div(class = "tree-prediction-table", tableOutput("chatter_projection_table"))),
              div(class = "panel", h2("Team Chatter Leaders"), tableOutput("chatter_team_table")),
              div(
                class = "panel",
                h2("Historical Impact Check - Training Data"),
                tableOutput("chatter_history_training_table")
              ),
              div(
                class = "panel",
                h2("Historical Impact Check - 2025-2026 Results"),
                tableOutput("chatter_history_results_table")
              ),
              div(class = "panel", h2("Fitted Overlay Coefficients"), tableOutput("chatter_coefficients_table"))
            )
          )
        ),
        tabPanel(
          "Fastest Lap Model",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Fastest Lap Model"),
              selectInput(
                "fastest_lap_season", "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("fastest_lap_race_selector"),
              selectInput(
                "fastest_lap_history_start_season", "History start season",
                choices = sort(unique(fastest_lap_predictions$season), decreasing = TRUE),
                selected = if (any(fastest_lap_predictions$season == 2025L)) 2025L else min(fastest_lap_predictions$season, na.rm = TRUE)
              ),
              selectInput(
                "fastest_lap_history_end_season", "History end season",
                choices = sort(unique(fastest_lap_predictions$season), decreasing = TRUE),
                selected = if (any(fastest_lap_predictions$season == 2026L)) 2026L else max(fastest_lap_predictions$season, na.rm = TRUE)
              ),
              checkboxGroupInput(
                "fastest_lap_models", "Routed fastest-lap models",
                choices = fastest_lap_model_choices, selected = character(0)
              ),
              checkboxInput("fastest_lap_use_defaults", "Use race defaults", value = FALSE),
              checkboxInput("fastest_lap_select_all", "Select all", value = FALSE),
              checkboxInput("fastest_lap_clear_all", "Clear all", value = FALSE)
            ),
            div(
              class = "tree-tab-content",
              uiOutput("fastest_lap_header"),
              div(class = "panel", h2("Historical Fastest-Lap Fit - Dry vs Wet"), p("Pick accuracy and rank quality use completed races; betting ROI is withheld where archived fastest-lap odds are unavailable."), tableOutput("fastest_lap_history_table")),
              div(class = "panel", h2("Predicted Fastest Lap"), tableOutput("fastest_lap_winner_table")),
              div(class = "panel", h2("Consensus Driver Board"), div(class = "tree-prediction-table", tableOutput("fastest_lap_prediction_table"))),
              div(class = "panel", h2("Active Specialist Models"), tableOutput("fastest_lap_model_table"))
            )
          )
        ),
        tabPanel(
          "Wet Weather",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Wet Weather"),
              selectInput(
                "wet_weather_season", "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("wet_weather_race_selector"),
              selectInput(
                "wet_weather_intensity", "Wet scenario",
                choices = c(
                  "Sustained wet / intermediates" = "1.00",
                  "Light or intermittent rain" = "0.25"
                ),
                selected = "1.00"
              ),
              selectInput(
                "wet_weather_roi_start_season", "ROI start season",
                choices = c(2025L, 2026L), selected = 2025L
              ),
              selectInput(
                "wet_weather_roi_end_season", "ROI end season",
                choices = c(2025L, 2026L), selected = 2026L
              ),
              div(
                class = "panel wet-scenario-card",
                h3("Experimental wet residual"),
                uiOutput("wet_weather_blend_note"),
                p("The adjustment is shrunk for limited history and capped at one finishing position.")
              )
            ),
            div(
              class = "tree-tab-content",
              uiOutput("wet_weather_header"),
              div(
                class = "panel",
                h2("Wet-Weather Consensus Betting ROI"),
                p("Dry consensus versus the residual model on completed wet races. Historical races use their recorded rain intensity. Flat one-unit bets: one winner and the top three podium selections per race."),
                div(class = "tree-prediction-table", tableOutput("wet_weather_roi_table"))
              ),
              div(class = "panel", h2("Wet-Weather Projected Finish"), div(class = "tree-prediction-table", tableOutput("wet_weather_projection_table"))),
              div(class = "panel", h2("Wet-Weather Specialist Evidence"), p("Up to 10 prior wet starts, recency-weighted and measured relative to the dry model's expected rank."), div(class = "tree-prediction-table", tableOutput("wet_weather_specialist_table"))),
              div(class = "panel", h2("Wet Races Used"), tableOutput("wet_weather_races_table"))
            )
          )
        ),
        tabPanel(
          "Fantasy Lineup",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Fantasy Predictions"),
              selectInput(
                "fantasy_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("fantasy_race_selector"),
              checkboxInput("fantasy_use_chatter", "Preview chatter candidates (comparison only)", value = TRUE),
              p(class = "hint", "This switch changes only the A–H preview tables. The recommended portfolio automatically evaluates both Baseline and Chatter, so this switch does not change its results."),
              numericInput("fantasy_salary_cap", "Salary cap", value = 50000, min = 10000, step = 500),
              numericInput("fantasy_flex_count", "Flex drivers", value = 4, min = 1, max = 6, step = 1),
              numericInput("fantasy_driver_exposure", "Max driver exposure (%)", value = 75, min = 25, max = 100, step = 5),
              numericInput("fantasy_constructor_exposure", "Max constructor exposure (%)", value = 50, min = 10, max = 100, step = 10),
              numericInput("fantasy_min_major_changes", "Minimum major changes", value = 2, min = 2, max = 4, step = 1),
              numericInput("fantasy_chatter_strength", "Chatter strength (%)", value = 50, min = 0, max = 100, step = 10),
              p(class = "hint", "Every lineup must replace at least one driver, differ in at least two major ways, and share no more than four of six roster selections. Chatter is centered and bounded before it nudges projections.")
            ),
            div(
              class = "tree-tab-content",
              uiOutput("fantasy_header"),
                div(class = "panel", h2("Candidate Preview — Highest-Projected Lineup"), tableOutput("fantasy_single_lineup_table"), tableOutput("fantasy_single_lineup_summary")),
                div(class = "panel", h2("Five Strategy Philosophies"), tableOutput("fantasy_strategy_table")),
                div(class = "panel", h2("Candidate Preview Portfolio Summary"), uiOutput("fantasy_portfolio_mode_note"), tableOutput("fantasy_portfolio_summary_table")),
                div(class = "panel", h2("Constructor Board"), div(class = "tree-prediction-table", tableOutput("fantasy_constructor_table"))),
              div(
                class = "panel",
                div(
                  style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap;",
                  h2(style = "margin-bottom:0;", "Driver Fantasy Board"),
                  downloadButton("fantasy_driver_download", "Download driver projections (CSV)")
                ),
                p(class = "hint", "Exports every driver, the selected model-family ranks, and each projected DraftKings scoring component. Completed races also include actual category points and projection errors."),
                div(class = "tree-prediction-table", tableOutput("fantasy_driver_table"))
              ),
                div(
                  class = "panel",
                  div(
                    style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap;",
                    h2(style = "margin-bottom:0;", "Best 1 / Best 3 / Best 5 / Full 8"),
                    downloadButton("fantasy_combined_download", "Download all eight (CSV)")
                  ),
                  p(class = "hint", "Use Entry 1 for single-entry, Entries 1–3 for a three-lineup set, Entries 1–5 for a five-lineup set, or all eight for the complete portfolio. Entries are ordered by robust projection after portfolio safeguards. Fantasy uses a locked model recipe, so changes on the Model Consensus tab cannot silently replace these rosters."),
                  tableOutput("fantasy_combined_summary_table"),
                  tableOutput("fantasy_combined_table")
                ),
                div(class = "panel", h2("Portfolio Diversification Audit"), p(class = "hint", "Checks exact-lineup uniqueness, driver-pool overlap, and constructor concentration after all safeguards are applied."), tableOutput("fantasy_portfolio_audit_table")),
                div(class = "panel", h2("Portfolio Exposure"), p(class = "hint", "Driver, captain, and constructor usage across the generated portfolio."), tableOutput("fantasy_portfolio_exposure_table")),
                div(class = "panel", h2("Pairwise Lineup Overlap"), p(class = "hint", "No pair may share more than four of six selections: five drivers plus one constructor."), tableOutput("fantasy_portfolio_overlap_table")),
                div(class = "panel", h2("Candidate Preview — A through H"), p(class = "hint", "These eight candidates show the currently selected Chatter or Baseline mode; use the recommended reduced portfolio for entries."), downloadButton("fantasy_portfolio_download", "Download preview A–H (CSV)"), tableOutput("fantasy_portfolio_table"))
            )
          )
        ),
        tabPanel(
          "Fantasy Projections 2",
          div(
            class = "tree-tab-layout",
            div(
              class = "tree-controls",
              h1("Fantasy Projections 2"),
              selectInput(
                "fantasy2_season",
                "Season",
                choices = if (nrow(rf_race_choices) == 0) character(0) else sort(unique(rf_race_choices$season), decreasing = TRUE),
                selected = if (any(rf_race_choices$season == 2026L)) 2026L else if (nrow(rf_race_choices) > 0) max(rf_race_choices$season) else NULL
              ),
              uiOutput("fantasy2_race_selector"),
              checkboxInput("fantasy2_use_chatter", "Preview chatter candidates (comparison only)", value = TRUE),
              p(class = "hint", "This switch changes only the A–H preview. The recommended portfolio evaluates both Baseline and Chatter."),
              numericInput("fantasy2_salary_cap", "Salary cap", value = 50000, min = 10000, step = 500),
              numericInput("fantasy2_flex_count", "Flex drivers", value = 4, min = 1, max = 6, step = 1),
              numericInput("fantasy2_driver_exposure", "Max driver exposure (%)", value = 75, min = 25, max = 100, step = 5),
              numericInput("fantasy2_constructor_exposure", "Max constructor exposure (%)", value = 50, min = 10, max = 100, step = 10),
              numericInput("fantasy2_min_major_changes", "Minimum major changes", value = 2, min = 2, max = 4, step = 1),
              numericInput("fantasy2_chatter_strength", "Chatter strength (%)", value = 50, min = 0, max = 100, step = 10),
              p(class = "hint", "Experimental sequence: choose a projection-and-value constructor, choose a value captain outside the two highest salaries, require an elite flex driver, then optimize the remaining roster.")
            ),
            div(
              class = "tree-tab-content",
              uiOutput("fantasy2_header"),
              div(class = "panel", h2("Candidate Preview — Highest-Projected Lineup"), tableOutput("fantasy2_single_lineup_table"), tableOutput("fantasy2_single_lineup_summary")),
              div(class = "panel", h2("Five Strategy Philosophies"), tableOutput("fantasy2_strategy_table")),
              div(class = "panel", h2("Candidate Preview Portfolio Summary"), uiOutput("fantasy2_portfolio_mode_note"), tableOutput("fantasy2_portfolio_summary_table")),
              div(class = "panel", h2("Constructor Board"), p(class = "hint", "The eligible pool excludes low-ceiling constructors; the fit score balances projected points and value per $1,000."), div(class = "tree-prediction-table", tableOutput("fantasy2_constructor_table"))),
              div(
                class = "panel",
                div(
                  style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap;",
                  h2(style = "margin-bottom:0;", "Driver Fantasy Board"),
                  downloadButton("fantasy2_driver_download", "Download driver projections (CSV)")
                ),
                p(class = "hint", "The two highest-salaried drivers are flex-only. Captain fit is calculated among the strongest remaining projected drivers using projection and value."),
                div(class = "tree-prediction-table", tableOutput("fantasy2_driver_table"))
              ),
              div(
                class = "panel",
                div(
                  style = "display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:wrap;",
                  h2(style = "margin-bottom:0;", "Best 1 / Best 3 / Best 5 / Full 8"),
                  downloadButton("fantasy2_combined_download", "Download all eight (CSV)")
                ),
                p(class = "hint", "Entry 1 is the strongest robust lineup. The full portfolio preserves the constructor-first and captain-value rules while evaluating both Baseline and Chatter."),
                tableOutput("fantasy2_combined_summary_table"),
                tableOutput("fantasy2_combined_table")
              ),
              div(class = "panel", h2("Portfolio Diversification Audit"), tableOutput("fantasy2_portfolio_audit_table")),
              div(class = "panel", h2("Portfolio Exposure"), tableOutput("fantasy2_portfolio_exposure_table")),
              div(class = "panel", h2("Pairwise Lineup Overlap"), tableOutput("fantasy2_portfolio_overlap_table")),
              div(class = "panel", h2("Candidate Preview — A through H"), downloadButton("fantasy2_portfolio_download", "Download preview A–H (CSV)"), tableOutput("fantasy2_portfolio_table"))
            )
          )
        ),
      )
    )
  )
)

server <- function(input, output, session) {
  bounded_centered_chatter_nudge <- function(values, cap, weight = 1) {
    values <- as.numeric(values)
    finite_values <- values[is.finite(values)]
    center <- if (length(finite_values) == 0L) 0 else mean(finite_values)
    adjusted <- (values - center) * pmin(1, pmax(0, as.numeric(weight %||% 1)))
    pmin(as.numeric(cap), pmax(-as.numeric(cap), coalesce(adjusted, 0)))
  }
  observeEvent(
    list(input$constructor_profile_start_season, input$constructor_profile_end_season),
    {
      req(input$constructor_profile_start_season, input$constructor_profile_end_season)
      start_season <- min(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))
      end_season <- max(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))
      choices <- constructor_choices_for_window(start_season, end_season)
      selected <- input$constructor_profile_constructor
      if (is.null(selected) || !selected %in% choices$constructor_id) {
        selected <- choices$constructor_id[[1]]
      }
      updateSelectInput(
        session,
        "constructor_profile_constructor",
        choices = setNames(choices$constructor_id, choices$label),
        selected = selected
      )
    },
    ignoreInit = TRUE
  )

  output$tree_race_selector <- renderUI({
    validate(need(nrow(rf_race_choices) > 0, "Run Stage 7 random forest modeling to create RF predictions."))
    req(input$tree_season)

    choices <- rf_race_choices %>%
      filter(season == as.integer(input$tree_season))

    selectInput(
      "tree_round",
      "Race",
      choices = setNames(choices$round, choices$label),
      selected = default_race_round(choices)
    )
  })

  selected_tree_predictions <- reactive({
    validate(need(nrow(rf_main_predictions) > 0, "Run Stage 7 random forest modeling to create RF predictions."))
    req(input$tree_season, input$tree_round, input$tree_models)

    selected_rows <- rf_main_predictions %>%
      filter(
        season == as.integer(input$tree_season),
        round == as.integer(input$tree_round),
        model %in% input$tree_models
      )

    validate(need(nrow(selected_rows) > 0, "No RF predictions found for this selection."))

    use_estimated <- input$tree_use_estimated_podium_odds %||% input$use_estimated_podium_odds %||% TRUE

    selected_rows <- selected_rows %>%
      apply_podium_display_odds(use_estimated)

    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          finish_position, actual_rank_in_race, actual_winner, actual_podium
        ) %>%
        summarise(
          predicted_finish_position = mean(predicted_finish_position, na.rm = TRUE),
          model_count = n_distinct(model),
          .groups = "drop"
        ) %>%
        arrange(predicted_finish_position, driver_name) %>%
        mutate(
          model = "rf_consensus_selected",
          model_label = paste0("Consensus of selected RFs (", max(model_count, na.rm = TRUE), ")"),
          selected_model = TRUE,
          maxnodes = NA_real_,
          mean_terminal_nodes = NA_real_,
          predicted_rank_in_race = row_number(),
          predicted_winner = predicted_rank_in_race == 1,
          winner_pick_correct = predicted_winner & actual_winner,
          predicted_podium = predicted_rank_in_race <= 3,
          podium_pick_correct = predicted_podium & actual_podium
        ) %>%
        left_join(
          selected_rows %>%
            distinct(
              season, round, driver_code,
              win_avg_american_odds_label,
              win_market_no_vig_probability,
              podium_display_american_odds_label,
              podium_display_no_vig_probability,
              podium_display_odds_source
            ),
          by = c("season", "round", "driver_code")
        ) %>%
        select(
          model, model_label, selected_model, maxnodes, mean_terminal_nodes,
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          finish_position, predicted_finish_position,
          predicted_rank_in_race, actual_rank_in_race,
          predicted_winner, actual_winner, winner_pick_correct,
          predicted_podium, actual_podium, podium_pick_correct,
          win_avg_american_odds_label, win_market_no_vig_probability,
          podium_display_american_odds_label, podium_display_no_vig_probability,
          podium_display_odds_source
        )

      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      mutate(model_order = if_else(model == "rf_consensus_selected", 0L, match(model, rf_model_lookup$model))) %>%
      arrange(model_order, predicted_rank_in_race) %>%
      select(-model_order)
  })

  output$tree_model_header <- renderUI({
    rows <- selected_tree_predictions()
    validate(need(nrow(rows) > 0, "No RF predictions found for this selection."))
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)

    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p("Random forest predictions use the Stage 6 overall and matched-track rolling feature table.")
      )
    )
  })

  build_consensus_bets <- function(selected_rows) {
    validate(need(nrow(selected_rows) > 0, "No RF predictions found for this model selection."))

    selected_rows <- selected_rows %>%
      apply_podium_display_odds(input$tree_use_estimated_podium_odds %||% input$use_estimated_podium_odds %||% TRUE) %>%
      mutate(
        podium_bet_american_odds_label = podium_display_american_odds_label,
        podium_bet_no_vig_probability = podium_display_no_vig_probability,
        podium_bet_odds_source = podium_display_odds_source
      )

    consensus_rows <- selected_rows %>%
      group_by(
        data_split, season, round, race_date, race_name,
        driver_id, driver_code, driver_name, constructor_name,
        finish_position, actual_rank_in_race, actual_winner, actual_podium
      ) %>%
      summarise(
        predicted_finish = mean(predicted_finish_position, na.rm = TRUE),
        selected_model_count = n_distinct(model),
        .groups = "drop"
      ) %>%
      arrange(season, round, predicted_finish, driver_name) %>%
      group_by(season, round) %>%
      mutate(consensus_rank = row_number()) %>%
      ungroup() %>%
      left_join(
        selected_rows %>%
          distinct(
            season, round, driver_code,
            win_avg_american_odds_label,
            win_market_no_vig_probability,
            podium_bet_american_odds_label,
            podium_bet_no_vig_probability,
            podium_bet_odds_source
          ),
        by = c("season", "round", "driver_code")
      ) %>%
      group_by(season, round) %>%
      mutate(
        field_size = n(),
        rank_weight = pmax(field_size - consensus_rank + 1, 0),
        model_win_probability = rank_weight / sum(rank_weight, na.rm = TRUE),
        model_podium_probability = pmax(0, (4 - consensus_rank) / 3)
      ) %>%
      ungroup()

    winner_bets <- consensus_rows %>%
      filter(consensus_rank == 1) %>%
      transmute(
        season, round, race_date, race_name,
        selected_model_count,
        bet_market = "win",
        consensus_rank,
        driver_code, driver_name, constructor_name,
        predicted_finish,
        actual_finish = finish_position,
        bet_won = actual_winner,
        odds_american_label = win_avg_american_odds_label,
        market_no_vig_probability = win_market_no_vig_probability,
        model_edge = model_win_probability - market_no_vig_probability,
        odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")
      )

    podium_bets <- consensus_rows %>%
      filter(consensus_rank <= 3) %>%
      transmute(
        season, round, race_date, race_name,
        selected_model_count,
        bet_market = "podium",
        consensus_rank,
        driver_code, driver_name, constructor_name,
        predicted_finish,
        actual_finish = finish_position,
        bet_won = actual_podium,
        odds_american_label = podium_bet_american_odds_label,
        market_no_vig_probability = podium_bet_no_vig_probability,
        model_edge = model_podium_probability - market_no_vig_probability,
        odds_source = podium_bet_odds_source
      ) %>%
      filter(podium_odds_allowed(odds_american_label, input$podium_favorite_limit %||% 300))

    bind_rows(winner_bets, podium_bets) %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(
          is.na(actual_finish) ~ "No result",
          is.na(odds_decimal) ~ "No odds",
          bet_won ~ "Won",
          TRUE ~ "Lost"
        )
      )
  }

  selected_consensus_bets <- reactive({
    validate(need(nrow(rf_main_predictions) > 0, "Run Stage 9 to create RF predictions with market odds."))
    req(input$tree_roi_start_season, input$tree_roi_end_season, input$tree_models)
    bounds <- roi_window_bounds(input$tree_roi_start_season, input$tree_roi_end_season)

    selected_rows <- rf_main_predictions %>%
      filter(
        season >= bounds[["start"]],
        season <= bounds[["end"]],
        model %in% input$tree_models
      )

    build_consensus_bets(selected_rows)
  })

  summarise_consensus_bets <- function(bets) {
    if (!"model_edge" %in% names(bets)) {
      bets <- bets %>% mutate(model_edge = NA_real_)
    }
    bets <- add_bet_race_condition(bets)

    market_summary <- bets %>%
      filter(stake > 0) %>%
      group_by(season, race_condition, bet_market) %>%
      summarise(
        races_with_available_bets = n_distinct(paste(season, round)),
        bets_available = n(),
        bets_won = sum(bet_won, na.rm = TRUE),
        hit_rate = bets_won / bets_available,
        stake = sum(stake, na.rm = TRUE),
        profit = sum(profit, na.rm = TRUE),
        roi = profit / stake,
        avg_model_edge = mean(model_edge, na.rm = TRUE),
        .groups = "drop"
      )

    combined_summary <- bets %>%
      filter(stake > 0) %>%
      group_by(season, race_condition) %>%
      summarise(
        bet_market = "combined",
        races_with_available_bets = n_distinct(paste(season, round)),
        bets_available = n(),
        bets_won = sum(bet_won, na.rm = TRUE),
        hit_rate = bets_won / bets_available,
        stake = sum(stake, na.rm = TRUE),
        profit = sum(profit, na.rm = TRUE),
        roi = profit / stake,
        avg_model_edge = mean(model_edge, na.rm = TRUE),
        .groups = "drop"
      )

    bind_rows(market_summary, combined_summary) %>%
      arrange(season, match(race_condition, c("Dry", "Wet")), match(bet_market, c("win", "podium", "combined")))
  }

  roi_window_bounds <- function(start_season, end_season) {
    start_season <- suppressWarnings(as.integer(start_season))
    end_season <- suppressWarnings(as.integer(end_season))
    if (is.na(start_season)) start_season <- end_season
    if (is.na(end_season)) end_season <- start_season
    c(start = min(start_season, end_season, na.rm = TRUE), end = max(start_season, end_season, na.rm = TRUE))
  }

  summarise_consensus_bets_window <- function(bets, start_season, end_season) {
    if (!"model_edge" %in% names(bets)) {
      bets <- bets %>% mutate(model_edge = NA_real_)
    }

    bounds <- roi_window_bounds(start_season, end_season)
    filtered_bets <- bets %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]]) %>%
      add_bet_race_condition()

    market_summary <- filtered_bets %>%
      filter(stake > 0) %>%
      group_by(race_condition, bet_market) %>%
      summarise(
        races_with_available_bets = n_distinct(paste(season, round)),
        bets_available = n(),
        bets_won = sum(bet_won, na.rm = TRUE),
        hit_rate = bets_won / bets_available,
        stake = sum(stake, na.rm = TRUE),
        profit = sum(profit, na.rm = TRUE),
        roi = profit / stake,
        avg_model_edge = mean(model_edge, na.rm = TRUE),
        .groups = "drop"
      )

    combined_summary <- filtered_bets %>%
      filter(stake > 0) %>%
      group_by(race_condition) %>%
      summarise(
        bet_market = "combined",
        races_with_available_bets = n_distinct(paste(season, round)),
        bets_available = n(),
        bets_won = sum(bet_won, na.rm = TRUE),
        hit_rate = bets_won / bets_available,
        stake = sum(stake, na.rm = TRUE),
        profit = sum(profit, na.rm = TRUE),
        roi = profit / stake,
        avg_model_edge = mean(model_edge, na.rm = TRUE),
        .groups = "drop"
      )

    bind_rows(market_summary, combined_summary) %>%
      filter(!is.na(bet_market), bets_available > 0) %>%
      mutate(period = if_else(bounds[["start"]] == bounds[["end"]], as.character(bounds[["end"]]), paste0(bounds[["start"]], "-", bounds[["end"]]))) %>%
      arrange(match(race_condition, c("Dry", "Wet")), match(bet_market, c("win", "podium", "combined")))
  }

  selected_consensus_season_summary <- reactive({
    summarise_consensus_bets(selected_consensus_bets())
  })

  output$rf_betting_season_summary <- renderTable({
    req(input$tree_roi_start_season, input$tree_roi_end_season)

    summary_rows <- summarise_consensus_bets_window(selected_consensus_bets(), input$tree_roi_start_season, input$tree_roi_end_season)

    validate(need(nrow(summary_rows) > 0, "No completed bets with odds found for this selected model consensus."))

    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$rf_consensus_bets_table <- renderTable({
    req(input$tree_season, input$tree_round)

    bet_rows <- selected_consensus_bets() %>%
      filter(
        season == as.integer(input$tree_season),
        round == as.integer(input$tree_round)
      )

    validate(need(nrow(bet_rows) > 0, "No selected consensus bets found for this race."))

    bet_rows %>%
      mutate(
        bet_market = recode(bet_market, win = "Winner", podium = "Podium")
      ) %>%
      transmute(
        Market = bet_market,
        Rank = consensus_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Odds = odds_american_label,
        Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"),
        `Market %` = format_pct(market_no_vig_probability, 1),
        Edge = format_pct(model_edge, 0.1),
        Result = bet_status,
        `Actual finish` = format_int(actual_finish),
        Stake = format_num(stake, 0),
        Profit = format_num(profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$tree_winner_table <- renderTable({
    rows <- selected_tree_predictions()

    rows %>%
      filter(predicted_rank_in_race == 1) %>%
      transmute(
        Model = model_label,
        Pick = driver_name,
        Constructor = constructor_name,
        `Pred finish` = format_num(predicted_finish_position, 2),
        `Win odds` = win_avg_american_odds_label,
        `Mkt win %` = format_pct(win_market_no_vig_probability, 1),
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(winner_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$tree_podium_table <- renderTable({
    rows <- selected_tree_predictions()

    podium_rows <- if (any(rows$model == "rf_consensus_selected")) {
      rows %>% filter(model == "rf_consensus_selected")
    } else {
      rows
    }

    podium_rows %>%
      filter(predicted_rank_in_race <= 3) %>%
      transmute(
        Model = model_label,
        Rank = predicted_rank_in_race,
        Driver = driver_name,
        Constructor = constructor_name,
        `Pred finish` = format_num(predicted_finish_position, 2),
        `Podium odds` = podium_display_american_odds_label,
        `Podium %` = format_pct(podium_display_no_vig_probability, 1),
        `Actual finish` = format_int(finish_position),
        `Actual podium` = ifelse(actual_podium, "Yes", "No"),
        Hit = ifelse(podium_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$tree_prediction_table <- renderTable({
    rows <- selected_tree_predictions()

    rows %>%
      transmute(
        Model = model_label,
        Rank = predicted_rank_in_race,
        Driver = driver_name,
        Constructor = constructor_name,
        `Pred finish` = format_num(predicted_finish_position, 2),
        `Win odds` = win_avg_american_odds_label,
        `Mkt win %` = format_pct(win_market_no_vig_probability, 1),
        `Podium odds` = podium_display_american_odds_label,
        `Podium %` = format_pct(podium_display_no_vig_probability, 1),
        `Actual finish` = format_int(finish_position),
        `Actual rank` = actual_rank_in_race
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$prob_race_selector <- renderUI({
    validate(need(nrow(rf_race_choices) > 0, "Run Stage 10 probability RF modeling to create probability predictions."))
    req(input$prob_season)

    choices <- rf_race_choices %>%
      filter(season == as.integer(input$prob_season))

    selectInput(
      "prob_round",
      "Race",
      choices = setNames(choices$round, choices$label),
      selected = default_race_round(choices)
    )
  })

  add_probability_display_odds <- function(rows, use_estimated) {
    apply_podium_display_odds(rows, use_estimated)
  }

  selected_probability_predictions <- reactive({
    validate(need(nrow(rf_probability_predictions) > 0, "Run Stage 10 probability RF modeling to create probability predictions."))
    req(input$prob_season, input$prob_round, input$prob_models)

    selected_rows <- rf_probability_predictions %>%
      filter(
        season == as.integer(input$prob_season),
        round == as.integer(input$prob_round),
        model %in% input$prob_models
      ) %>%
      add_probability_display_odds(input$prob_use_estimated_podium_odds)

    validate(need(nrow(selected_rows) > 0, "No RF probability predictions found for this selection."))

    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          finish_position, actual_rank_in_race, actual_winner, actual_podium
        ) %>%
        summarise(
          predicted_win_probability = mean(predicted_win_probability, na.rm = TRUE),
          predicted_podium_probability = mean(predicted_podium_probability, na.rm = TRUE),
          model_count = n_distinct(model),
          .groups = "drop"
        ) %>%
        arrange(desc(predicted_win_probability), driver_name) %>%
        mutate(
          model = "rf_prob_consensus_selected",
          model_label = paste0("Consensus of selected probability RFs (", max(model_count, na.rm = TRUE), ")"),
          selected_model = TRUE,
          maxnodes = NA_real_,
          mean_terminal_nodes = NA_real_,
          predicted_win_rank = row_number(),
          predicted_winner = predicted_win_rank == 1,
          winner_pick_correct = predicted_winner & actual_winner
        ) %>%
        arrange(desc(predicted_podium_probability), driver_name) %>%
        mutate(
          predicted_podium_rank = row_number(),
          predicted_podium = predicted_podium_rank <= 3,
          podium_pick_correct = predicted_podium & actual_podium,
          predicted_rank_in_race = predicted_win_rank
        ) %>%
        left_join(
          selected_rows %>%
            distinct(
              season, round, driver_code,
              win_avg_american_odds_label,
              win_market_no_vig_probability,
              podium_display_american_odds_label,
              podium_display_no_vig_probability,
              podium_display_odds_source
            ),
          by = c("season", "round", "driver_code")
        )

      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      mutate(model_order = if_else(model == "rf_prob_consensus_selected", 0L, match(model, rf_probability_model_lookup$model))) %>%
      arrange(model_order, predicted_win_rank) %>%
      select(-model_order)
  })

  output$prob_model_header <- renderUI({
    rows <- selected_probability_predictions()
    validate(need(nrow(rows) > 0, "No RF probability predictions found for this selection."))
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)

    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p("Probability random forests rank winner picks by predicted win probability and podium picks by predicted podium probability.")
      )
    )
  })

  build_probability_consensus_bets <- function(selected_rows, use_estimated, favorite_limit = NULL, min_win_edge_pct = -100, min_podium_edge_pct = min_win_edge_pct) {
    validate(need(nrow(selected_rows) > 0, "No RF probability predictions found for this model selection."))

    selected_rows <- selected_rows %>%
      add_probability_display_odds(use_estimated)

    consensus_rows <- selected_rows %>%
      group_by(
        data_split, season, round, race_date, race_name,
        driver_id, driver_code, driver_name, constructor_name,
        finish_position, actual_rank_in_race, actual_winner, actual_podium
      ) %>%
      summarise(
        predicted_win_probability = mean(predicted_win_probability, na.rm = TRUE),
        predicted_podium_probability = mean(predicted_podium_probability, na.rm = TRUE),
        selected_model_count = n_distinct(model),
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(desc(predicted_win_probability), driver_name, .by_group = TRUE) %>%
      mutate(win_rank = row_number()) %>%
      arrange(desc(predicted_podium_probability), driver_name, .by_group = TRUE) %>%
      mutate(podium_rank = row_number()) %>%
      ungroup() %>%
      left_join(
        selected_rows %>%
          distinct(
            season, round, driver_code,
            win_avg_american_odds_label,
            win_market_no_vig_probability,
            podium_display_american_odds_label,
            podium_display_no_vig_probability,
            podium_display_odds_source
          ),
        by = c("season", "round", "driver_code")
      )

    winner_bets <- consensus_rows %>%
      filter(win_rank == 1) %>%
      transmute(
        season, round, race_date, race_name,
        selected_model_count,
        bet_market = "win",
        consensus_rank = win_rank,
        driver_code, driver_name, constructor_name,
        predicted_probability = predicted_win_probability,
        actual_finish = finish_position,
        bet_won = actual_winner,
        odds_american_label = win_avg_american_odds_label,
        market_no_vig_probability = win_market_no_vig_probability,
        model_edge = predicted_probability - market_no_vig_probability,
        odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")
      ) %>%
      filter(model_edge_allowed(model_edge, min_win_edge_pct))

    podium_bets <- consensus_rows %>%
      filter(podium_rank <= 3) %>%
      transmute(
        season, round, race_date, race_name,
        selected_model_count,
        bet_market = "podium",
        consensus_rank = podium_rank,
        driver_code, driver_name, constructor_name,
        predicted_probability = predicted_podium_probability,
        actual_finish = finish_position,
        bet_won = actual_podium,
        odds_american_label = podium_display_american_odds_label,
        market_no_vig_probability = podium_display_no_vig_probability,
        model_edge = predicted_probability - market_no_vig_probability,
        odds_source = podium_display_odds_source
      ) %>%
      filter(podium_odds_allowed(odds_american_label, favorite_limit)) %>%
      filter(model_edge_allowed(model_edge, min_podium_edge_pct))

    bind_rows(winner_bets, podium_bets) %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(
          is.na(actual_finish) ~ "No result",
          is.na(odds_decimal) ~ "No odds",
          bet_won ~ "Won",
          TRUE ~ "Lost"
        )
      )
  }

  selected_probability_consensus_bets <- reactive({
    req(input$prob_roi_start_season, input$prob_roi_end_season, input$prob_models)
    bounds <- roi_window_bounds(input$prob_roi_start_season, input$prob_roi_end_season)
    selected_rows <- rf_probability_predictions %>%
      filter(
        season >= bounds[["start"]],
        season <= bounds[["end"]],
        model %in% input$prob_models
      )

    build_probability_consensus_bets(selected_rows, input$prob_use_estimated_podium_odds, input$prob_podium_favorite_limit, input$prob_min_edge_pct)
  })

  output$prob_betting_season_summary <- renderTable({
    req(input$prob_roi_start_season, input$prob_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(selected_probability_consensus_bets(), input$prob_roi_start_season, input$prob_roi_end_season)

    validate(need(nrow(summary_rows) > 0, "No completed probability bets with odds found."))

    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$prob_consensus_bets_table <- renderTable({
    req(input$prob_season, input$prob_round)

    bet_rows <- selected_probability_consensus_bets() %>%
      filter(season == as.integer(input$prob_season), round == as.integer(input$prob_round))

    validate(need(nrow(bet_rows) > 0, "No selected probability consensus bets found for this race."))

    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(
        Market = bet_market,
        Rank = consensus_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Probability = format_pct(predicted_probability, 0.1),
        Odds = odds_american_label,
        Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"),
        `Market %` = format_pct(market_no_vig_probability, 1),
        Edge = format_pct(model_edge, 0.1),
        Result = bet_status,
        `Actual finish` = format_int(actual_finish),
        Stake = format_num(stake, 0),
        Profit = format_num(profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$prob_winner_table <- renderTable({
    rows <- selected_probability_predictions()

    rows %>%
      filter(predicted_win_rank == 1) %>%
      transmute(
        Model = model_label,
        Pick = driver_name,
        Constructor = constructor_name,
        `P win` = format_pct(predicted_win_probability, 0.1),
        `P podium` = format_pct(predicted_podium_probability, 0.1),
        `Win odds` = win_avg_american_odds_label,
        `Mkt win %` = format_pct(win_market_no_vig_probability, 1),
        `Win edge` = format_pct(predicted_win_probability - win_market_no_vig_probability, 0.1),
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(winner_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$prob_prediction_table <- renderTable({
    rows <- selected_probability_predictions()

    rows %>%
      arrange(model_label, predicted_win_rank) %>%
      transmute(
        Model = model_label,
        `Win rank` = predicted_win_rank,
        `Podium rank` = predicted_podium_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        `P win` = format_pct(predicted_win_probability, 0.1),
        `P podium` = format_pct(predicted_podium_probability, 0.1),
        `Win edge` = format_pct(predicted_win_probability - win_market_no_vig_probability, 0.1),
        `Podium edge` = format_pct(predicted_podium_probability - podium_display_no_vig_probability, 0.1),
        `Win odds` = win_avg_american_odds_label,
        `Podium odds` = podium_display_american_odds_label,
        `Actual finish` = format_int(finish_position),
        `Actual rank` = actual_rank_in_race
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allrf_race_selector <- renderUI({
    req(input$allrf_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$allrf_season))
    selectInput("allrf_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  build_family_consensus_ranks <- function(season_value, round_value = NULL) {
    family_rows <- list()

    if (isTRUE(input$allrf_use_finish)) {
      rows <- rf_main_predictions %>% filter(season == as.integer(season_value))
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      rows <- rows %>% filter(model %in% selected_or_default_models(input$tree_models, rf_model_selected))
      family_rows$finish <- rows %>%
        group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
        summarise(
          winner_score = mean(predicted_finish_position, na.rm = TRUE),
          podium_score = winner_score,
          winner_probability_score = NA_real_,
          podium_probability_score = NA_real_,
          .groups = "drop"
        ) %>%
        group_by(season, round) %>%
        arrange(winner_score, driver_name, .by_group = TRUE) %>%
        mutate(winner_family_rank = row_number()) %>%
        arrange(podium_score, driver_name, .by_group = TRUE) %>%
        mutate(podium_family_rank = row_number(), family = "finish") %>%
        ungroup()
    }

    if (isTRUE(input$allrf_use_probability)) {
      rows <- rf_probability_predictions %>% filter(season == as.integer(season_value))
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      rows <- rows %>% filter(model %in% selected_or_default_models(input$prob_models, rf_probability_model_lookup$model))
      family_rows$probability <- rows %>%
        group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
        summarise(
          winner_score = mean(predicted_win_probability, na.rm = TRUE),
          podium_score = mean(predicted_podium_probability, na.rm = TRUE),
          winner_probability_score = winner_score,
          podium_probability_score = podium_score,
          .groups = "drop"
        ) %>%
        group_by(season, round) %>%
        arrange(desc(winner_score), driver_name, .by_group = TRUE) %>%
        mutate(winner_family_rank = row_number()) %>%
        arrange(desc(podium_score), driver_name, .by_group = TRUE) %>%
        mutate(podium_family_rank = row_number(), family = "probability") %>%
        ungroup()
    }

    bind_rows(family_rows)
  }

  add_allrf_odds <- function(rows, use_estimated_podium_odds) {
    odds_lookup <- bind_rows(
      rf_main_predictions,
      rf_probability_predictions,
      xgb_finish_predictions,
      xgb_probability_predictions,
      xgb_points_predictions
    ) %>%
      select(any_of(c(
        "season", "round", "driver_code",
        "win_avg_american_odds_label", "win_market_no_vig_probability",
        "podium_avg_american_odds", "podium_avg_american_odds_label", "podium_market_no_vig_probability",
        "podium_effective_avg_american_odds_label", "podium_effective_no_vig_probability", "podium_odds_source"
      ))) %>%
      filter(!is.na(season), !is.na(round), !is.na(driver_code)) %>%
      arrange(
        desc(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != ""),
        desc(!is.na(podium_effective_avg_american_odds_label) & podium_effective_avg_american_odds_label != ""),
        desc(!is.na(podium_avg_american_odds_label) & podium_avg_american_odds_label != "")
      ) %>%
      distinct(
        season, round, driver_code,
        win_avg_american_odds_label, win_market_no_vig_probability,
        podium_avg_american_odds, podium_avg_american_odds_label, podium_market_no_vig_probability,
        podium_effective_avg_american_odds_label, podium_effective_no_vig_probability, podium_odds_source
      ) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)

    rows %>%
      left_join(odds_lookup, by = c("season", "round", "driver_code")) %>%
      apply_podium_display_odds(use_estimated_podium_odds)
  }

  build_allrf_predictions_from_family_rows <- function(rows, use_estimated_podium_odds) {
    if (nrow(rows) == 0) return(tibble())

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_rank_score = mean(winner_family_rank, na.rm = TRUE),
        podium_rank_score = mean(podium_family_rank, na.rm = TRUE),
        model_win_probability = if (all(is.na(winner_probability_score))) NA_real_ else mean(winner_probability_score, na.rm = TRUE),
        model_podium_probability = if (all(is.na(podium_probability_score))) NA_real_ else mean(podium_probability_score, na.rm = TRUE),
        family_count = n_distinct(family),
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(winner_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(
        consensus_rank = row_number(),
        predicted_winner = consensus_rank == 1
      ) %>%
      arrange(podium_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(
        consensus_podium_rank = row_number(),
        predicted_podium = consensus_podium_rank <= 3,
        winner_pick_correct = predicted_winner & actual_winner,
        podium_pick_correct = predicted_podium & actual_podium
      ) %>%
      ungroup() %>%
      add_allrf_odds(use_estimated_podium_odds)
  }

  empty_allrf_bets <- function() {
    tibble(
      season = integer(),
      round = integer(),
      race_date = as.Date(character()),
      race_name = character(),
      bet_market = character(),
      consensus_rank = integer(),
      driver_code = character(),
      driver_name = character(),
      constructor_name = character(),
      actual_finish = numeric(),
      bet_won = logical(),
      odds_american_label = character(),
      market_no_vig_probability = numeric(),
      model_edge = numeric(),
      odds_source = character(),
      odds_decimal = numeric(),
      stake = numeric(),
      profit = numeric(),
      roi = numeric(),
      bet_status = character()
    )
  }

  allrf_predictions <- reactive({
    req(input$allrf_season)
    rows <- build_family_consensus_ranks(input$allrf_season, input$allrf_round)
    validate(need(nrow(rows) > 0, "Select at least one RF family."))

    build_allrf_predictions_from_family_rows(rows, input$allrf_use_estimated_podium_odds)
  })

  build_allrf_bets_from_family_rows <- function(family_rows, force_consensus, use_estimated_podium_odds, min_edge_pct = -100) {
    if (nrow(family_rows) == 0) return(empty_allrf_bets())

    rows <- build_allrf_predictions_from_family_rows(family_rows, use_estimated_podium_odds)
    if (nrow(rows) == 0) return(empty_allrf_bets())

    if (isTRUE(force_consensus)) {
      family_counts <- family_rows %>%
        distinct(season, round, family) %>%
        count(season, round, name = "required_family_count") %>%
        filter(required_family_count >= 2)

      winner_keys <- family_rows %>%
        filter(winner_family_rank == 1) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      podium_keys <- family_rows %>%
        filter(podium_family_rank <= 3) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      winner_rows <- rows %>% semi_join(winner_keys, by = c("season", "round", "driver_code"))
      podium_rows <- rows %>% semi_join(podium_keys, by = c("season", "round", "driver_code"))
    } else {
      winner_rows <- rows %>% filter(consensus_rank == 1)
      podium_rows <- rows %>% filter(consensus_podium_rank <= 3)
    }

    winner_bets <- winner_rows %>%
      transmute(season, round, race_date, race_name, bet_market = "win", consensus_rank, driver_code, driver_name, constructor_name, actual_finish = finish_position, bet_won = actual_winner, odds_american_label = win_avg_american_odds_label, market_no_vig_probability = win_market_no_vig_probability, model_edge = model_win_probability - market_no_vig_probability, odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing"))

    podium_bets <- podium_rows %>%
      transmute(season, round, race_date, race_name, bet_market = "podium", consensus_rank = consensus_podium_rank, driver_code, driver_name, constructor_name, actual_finish = finish_position, bet_won = actual_podium, odds_american_label = podium_display_american_odds_label, market_no_vig_probability = podium_display_no_vig_probability, model_edge = model_podium_probability - market_no_vig_probability, odds_source = podium_display_odds_source) %>%
      filter(podium_odds_allowed(odds_american_label, input$allrf_podium_favorite_limit))

    bind_rows(winner_bets, podium_bets) %>%
      { if (nrow(.) == 0) empty_allrf_bets() else . } %>%
      filter(model_edge_allowed(model_edge, min_edge_pct)) %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(is.na(actual_finish) ~ "No result", is.na(odds_decimal) ~ "No odds", bet_won ~ "Won", TRUE ~ "Lost")
      )
  }

  allrf_consensus_bets <- reactive({
    req(input$allrf_season, input$allrf_round)
    rows <- build_family_consensus_ranks(input$allrf_season, input$allrf_round)
    validate(need(nrow(rows) > 0, "Select at least one RF family."))
    build_allrf_bets_from_family_rows(rows, input$allrf_force_consensus, input$allrf_use_estimated_podium_odds, input$allrf_min_edge_pct)
  })

  output$allrf_model_header <- renderUI({
    rows <- allrf_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    mode <- if (isTRUE(input$allrf_force_consensus)) "Forced consensus only bets when the finish and probability families agree." else "Consensus averages family ranks; lower averaged rank is better."
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p(mode)))
  })

  output$allrf_betting_season_summary <- renderTable({
    req(input$allrf_roi_start_season, input$allrf_roi_end_season)
    bounds <- roi_window_bounds(input$allrf_roi_start_season, input$allrf_roi_end_season)
    rows <- bind_rows(lapply(seq(bounds[["start"]], bounds[["end"]]), function(season_value) {
      build_family_consensus_ranks(season_value, NULL)
    })) %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]])
    validate(need(nrow(rows) > 0, "Select at least one RF family."))
    season_bets <- build_allrf_bets_from_family_rows(rows, input$allrf_force_consensus, input$allrf_use_estimated_podium_odds, input$allrf_min_edge_pct)
    summary_rows <- summarise_consensus_bets_window(season_bets, bounds[["start"]], bounds[["end"]])
    validate(need(nrow(summary_rows) > 0, "No completed consensus bets with odds found for this season."))

    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allrf_consensus_bets_table <- renderTable({
    bet_rows <- allrf_consensus_bets()
    validate(need(nrow(bet_rows) > 0, "No consensus bets found for this race."))

    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(Market = bet_market, Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name, Odds = odds_american_label, Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"), `Market %` = format_pct(market_no_vig_probability, 1), Edge = format_pct(model_edge, 0.1), Result = bet_status, `Actual finish` = format_int(actual_finish), Stake = format_num(stake, 0), Profit = format_num(profit, 2), ROI = format_pct(roi, 0.1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allrf_winner_table <- renderTable({
    allrf_predictions() %>% filter(consensus_rank == 1) %>% transmute(Pick = driver_name, Constructor = constructor_name, `Avg win rank` = format_num(winner_rank_score, 2), Families = family_count, `Win odds` = win_avg_american_odds_label, `Actual finish` = format_int(finish_position), Correct = ifelse(winner_pick_correct, "Yes", "No"))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allrf_prediction_table <- renderTable({
    allrf_predictions() %>% arrange(consensus_rank) %>% transmute(`Win rank` = consensus_rank, `Podium rank` = consensus_podium_rank, Driver = driver_name, Constructor = constructor_name, `Avg win rank` = format_num(winner_rank_score, 2), `Avg podium rank` = format_num(podium_rank_score, 2), Families = family_count, `Win odds` = win_avg_american_odds_label, `Podium odds` = podium_display_american_odds_label, `Actual finish` = format_int(finish_position), `Actual rank` = actual_rank_in_race)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  rank_finish_family <- function(rows, family_label) {
    if (nrow(rows) == 0) {
      return(tibble(
        season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
        driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
        finish_position = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
        winner_score = numeric(), podium_score = numeric(),
        winner_probability_score = numeric(), podium_probability_score = numeric(),
        winner_family_rank = integer(), podium_family_rank = integer(), family = character()
      ))
    }

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_score = mean(predicted_finish_position, na.rm = TRUE),
        podium_score = winner_score,
        winner_probability_score = NA_real_,
        podium_probability_score = NA_real_,
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(winner_score, driver_name, .by_group = TRUE) %>%
      mutate(winner_family_rank = row_number()) %>%
      arrange(podium_score, driver_name, .by_group = TRUE) %>%
      mutate(podium_family_rank = row_number(), family = family_label) %>%
      ungroup()
  }

  rank_probability_family <- function(rows, family_label) {
    if (nrow(rows) == 0) {
      return(tibble(
        season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
        driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
        finish_position = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
        winner_score = numeric(), podium_score = numeric(),
        winner_probability_score = numeric(), podium_probability_score = numeric(),
        winner_family_rank = integer(), podium_family_rank = integer(), family = character()
      ))
    }

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_score = mean(predicted_win_probability, na.rm = TRUE),
        podium_score = mean(predicted_podium_probability, na.rm = TRUE),
        winner_probability_score = winner_score,
        podium_probability_score = podium_score,
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(desc(winner_score), driver_name, .by_group = TRUE) %>%
      mutate(winner_family_rank = row_number()) %>%
      arrange(desc(podium_score), driver_name, .by_group = TRUE) %>%
      mutate(podium_family_rank = row_number(), family = family_label) %>%
      ungroup()
  }

  rank_points_family <- function(rows, family_label) {
    if (nrow(rows) == 0) {
      return(tibble(
        season = integer(), round = integer(), race_date = as.Date(character()), race_name = character(),
        driver_id = character(), driver_code = character(), driver_name = character(), constructor_name = character(),
        finish_position = numeric(), actual_rank_in_race = integer(), actual_winner = logical(), actual_podium = logical(),
        winner_score = numeric(), podium_score = numeric(),
        winner_probability_score = numeric(), podium_probability_score = numeric(),
        winner_family_rank = integer(), podium_family_rank = integer(), family = character()
      ))
    }

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_score = mean(predicted_points, na.rm = TRUE),
        podium_score = winner_score,
        winner_probability_score = NA_real_,
        podium_probability_score = NA_real_,
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(desc(winner_score), driver_name, .by_group = TRUE) %>%
      mutate(winner_family_rank = row_number()) %>%
      arrange(desc(podium_score), driver_name, .by_group = TRUE) %>%
      mutate(podium_family_rank = row_number(), family = family_label) %>%
      ungroup()
  }

  ensure_track_flags <- function(rows) {
    route_flags <- c(
      "is_street",
      "is_permanent_road_course",
      "is_high_speed",
      "is_stop_start",
      "is_flowing_high_downforce",
      "is_low_overtake"
    )

    if (all(c("season", "round") %in% names(rows))) {
      flag_lookup <- race_choices %>%
        select(season, round, all_of(route_flags)) %>%
        distinct() %>%
        rename_with(~ paste0(.x, "_lookup"), all_of(route_flags))

      rows <- rows %>%
        left_join(flag_lookup, by = c("season", "round"))

      for (flag in route_flags) {
        lookup_flag <- paste0(flag, "_lookup")
        if (!flag %in% names(rows)) {
          rows[[flag]] <- rows[[lookup_flag]]
        } else {
          rows[[flag]] <- coalesce(rows[[flag]], rows[[lookup_flag]])
        }
      }

      rows <- rows %>% select(-any_of(paste0(route_flags, "_lookup")))
    }

    for (flag in route_flags) {
      if (!flag %in% names(rows)) {
        rows[[flag]] <- 0L
      }
    }

    rows
  }

  routed_track_type_label <- function(season_value, round_value) {
    race_flags <- race_choices %>%
      filter(season == as.integer(season_value), round == as.integer(round_value)) %>%
      ensure_track_flags() %>%
      slice(1)

    if (nrow(race_flags) == 0) return("none")

    types <- c(
      if (coalesce(as.integer(race_flags$is_street[[1]]), 0L) == 1L) "street",
      if (coalesce(as.integer(race_flags$is_permanent_road_course[[1]]), 0L) == 1L) "permanent",
      if (coalesce(as.integer(race_flags$is_high_speed[[1]]), 0L) == 1L) "high-speed",
      if (
        coalesce(as.integer(race_flags$is_high_speed[[1]]), 0L) == 0L &&
          (
            coalesce(as.integer(race_flags$is_stop_start[[1]]), 0L) == 1L ||
              coalesce(as.integer(race_flags$is_flowing_high_downforce[[1]]), 0L) == 1L ||
              coalesce(as.integer(race_flags$is_low_overtake[[1]]), 0L) == 1L
          )
      ) "technical"
    )

    if (length(types) == 0) "none" else paste(types, collapse = ", ")
  }

  filter_routed_specialist_rows <- function(rows, selected_models) {
    selected_models <- selected_or_default_models(selected_models, routed_specialist_default_models)
    if (nrow(rows) == 0 || length(selected_models) == 0) return(rows %>% filter(FALSE))

    rows %>%
      filter(model %in% selected_models) %>%
      left_join(
        routed_specialist_model_lookup %>% select(model, routed_type),
        by = "model"
      ) %>%
      ensure_track_flags() %>%
      mutate(
        route_match = case_when(
          routed_type == "street" ~ coalesce(as.integer(is_street), 0L) == 1L,
          routed_type == "permanent" ~ coalesce(as.integer(is_permanent_road_course), 0L) == 1L,
          routed_type == "high_speed" ~ coalesce(as.integer(is_high_speed), 0L) == 1L,
          routed_type == "technical" ~ coalesce(as.integer(is_high_speed), 0L) == 0L &
            (
              coalesce(as.integer(is_stop_start), 0L) == 1L |
                coalesce(as.integer(is_flowing_high_downforce), 0L) == 1L |
                coalesce(as.integer(is_low_overtake), 0L) == 1L
            ),
          TRUE ~ FALSE
        )
      ) %>%
      filter(route_match) %>%
      select(-routed_type, -route_match)
  }

  consensus_routed_models_for_race <- function(season_value, round_value = NULL) {
    if (!is.null(round_value)) {
      return(default_routed_specialist_models_for_race(season_value, round_value))
    }

    selected_or_default_models(input$routed_models, routed_specialist_default_models)
  }

  build_routed_specialist_family_ranks <- function(season_value, round_value = NULL, selected_models = NULL) {
    selected_models <- selected_or_default_models(selected_models, routed_specialist_default_models)
    season_value <- as.integer(season_value)

    finish_rows <- xgb_finish_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)
    probability_rows <- xgb_probability_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)
    points_rows <- xgb_points_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)

    if (!is.null(round_value)) {
      round_value <- as.integer(round_value)
      finish_rows <- finish_rows %>% filter(round == round_value)
      probability_rows <- probability_rows %>% filter(round == round_value)
      points_rows <- points_rows %>% filter(round == round_value)
    }

    bind_rows(
      rank_finish_family(finish_rows, "Routed specialist finish"),
      rank_probability_family(probability_rows, "Routed specialist probability"),
      rank_points_family(points_rows, "Routed specialist points")
    )
  }

  rank_each_selected_model <- function(rows, ranker, family_prefix) {
    if (nrow(rows) == 0 || !"model" %in% names(rows)) {
      return(ranker(rows, family_prefix))
    }

    bind_rows(lapply(sort(unique(rows$model)), function(model_id) {
      model_rows <- rows %>% filter(model == model_id)
      model_label <- if ("model_label" %in% names(model_rows)) {
        model_rows %>%
          filter(!is.na(model_label), model_label != "") %>%
          distinct(model_label) %>%
          pull(model_label) %>%
          dplyr::first()
      } else {
        NA_character_
      }
      ranker(model_rows, paste(family_prefix, coalesce(model_label, model_id), sep = ": "))
    }))
  }

  build_routed_specialist_model_ranks <- function(season_value, round_value = NULL, selected_models = NULL) {
    selected_models <- selected_or_default_models(selected_models, routed_specialist_default_models)
    season_value <- as.integer(season_value)

    finish_rows <- xgb_finish_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)
    probability_rows <- xgb_probability_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)
    points_rows <- xgb_points_predictions %>%
      filter(season == season_value) %>%
      filter_routed_specialist_rows(selected_models)

    if (!is.null(round_value)) {
      round_value <- as.integer(round_value)
      finish_rows <- finish_rows %>% filter(round == round_value)
      probability_rows <- probability_rows %>% filter(round == round_value)
      points_rows <- points_rows %>% filter(round == round_value)
    }

    bind_rows(
      rank_each_selected_model(finish_rows, rank_finish_family, "Routed specialist finish"),
      rank_each_selected_model(probability_rows, rank_probability_family, "Routed specialist probability"),
      rank_each_selected_model(points_rows, rank_points_family, "Routed specialist points")
    )
  }

  build_selected_family_consensus_ranks <- function(
    season_value,
    round_value = NULL,
    use_finish,
    use_probability,
    use_points,
    finish_rows,
    probability_rows,
    points_rows,
    finish_models,
    probability_models,
    points_models,
    family_prefix,
    consensus_mode = "family"
  ) {
    family_rows <- list()
    season_value <- as.integer(season_value)
    consensus_mode <- consensus_mode %||% "family"

    if (isTRUE(use_finish)) {
      rows <- finish_rows %>% filter(season == season_value, model %in% finish_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$finish <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_finish_family, paste(family_prefix, "finish"))
      } else {
        rank_finish_family(rows, paste(family_prefix, "finish"))
      }
    }

    if (isTRUE(use_probability)) {
      rows <- probability_rows %>% filter(season == season_value, model %in% probability_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$probability <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_probability_family, paste(family_prefix, "probability"))
      } else {
        rank_probability_family(rows, paste(family_prefix, "probability"))
      }
    }

    if (isTRUE(use_points)) {
      rows <- points_rows %>% filter(season == season_value, model %in% points_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$points <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_points_family, paste(family_prefix, "points"))
      } else {
        rank_points_family(rows, paste(family_prefix, "points"))
      }
    }

    bind_rows(family_rows)
  }

  build_family_predictions_from_ranks <- function(rows, use_estimated_podium_odds) {
    if (nrow(rows) == 0) return(tibble())

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_rank_score = mean(winner_family_rank, na.rm = TRUE),
        podium_rank_score = mean(podium_family_rank, na.rm = TRUE),
        model_win_probability = if (all(is.na(winner_probability_score))) NA_real_ else mean(winner_probability_score, na.rm = TRUE),
        model_podium_probability = if (all(is.na(podium_probability_score))) NA_real_ else mean(podium_probability_score, na.rm = TRUE),
        family_count = n_distinct(family),
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(winner_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(consensus_rank = row_number(), predicted_winner = consensus_rank == 1) %>%
      arrange(podium_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(
        consensus_podium_rank = row_number(),
        predicted_podium = consensus_podium_rank <= 3,
        field_size = n(),
        rank_weight = pmax(field_size - consensus_rank + 1, 0),
        rank_win_probability = rank_weight / sum(rank_weight, na.rm = TRUE),
        rank_podium_probability = pmax(0, (4 - consensus_podium_rank) / 3),
        model_win_probability = coalesce(model_win_probability, rank_win_probability),
        model_podium_probability = coalesce(model_podium_probability, rank_podium_probability),
        winner_pick_correct = predicted_winner & actual_winner,
        podium_pick_correct = predicted_podium & actual_podium
      ) %>%
      ungroup() %>%
      add_allrf_odds(use_estimated_podium_odds)
  }

  build_family_bets_from_ranks <- function(family_rows, force_consensus, use_estimated_podium_odds, favorite_limit, min_win_edge_pct = -100, min_podium_edge_pct = -100) {
    if (nrow(family_rows) == 0) return(empty_allrf_bets())

    rows <- build_family_predictions_from_ranks(family_rows, use_estimated_podium_odds)
    if (nrow(rows) == 0) return(empty_allrf_bets())

    if (isTRUE(force_consensus)) {
      family_counts <- family_rows %>%
        distinct(season, round, family) %>%
        count(season, round, name = "required_family_count") %>%
        filter(required_family_count >= 2)

      winner_keys <- family_rows %>%
        filter(winner_family_rank == 1) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      podium_keys <- family_rows %>%
        filter(podium_family_rank <= 3) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      winner_rows <- rows %>% semi_join(winner_keys, by = c("season", "round", "driver_code"))
      podium_rows <- rows %>% semi_join(podium_keys, by = c("season", "round", "driver_code"))
    } else {
      winner_rows <- rows %>% filter(consensus_rank == 1)
      podium_rows <- rows %>% filter(consensus_podium_rank <= 3)
    }

    winner_bets <- winner_rows %>%
      transmute(
        season, round, race_date, race_name,
        bet_market = "win",
        consensus_rank,
        driver_code, driver_name, constructor_name,
        actual_finish = finish_position,
        bet_won = actual_winner,
        odds_american_label = win_avg_american_odds_label,
        market_no_vig_probability = win_market_no_vig_probability,
        model_edge = model_win_probability - market_no_vig_probability,
        odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")
      ) %>%
      filter(model_edge_allowed(model_edge, min_win_edge_pct))

    podium_bets <- podium_rows %>%
      transmute(
        season, round, race_date, race_name,
        bet_market = "podium",
        consensus_rank = consensus_podium_rank,
        driver_code, driver_name, constructor_name,
        actual_finish = finish_position,
        bet_won = actual_podium,
        odds_american_label = podium_display_american_odds_label,
        market_no_vig_probability = podium_display_no_vig_probability,
        model_edge = model_podium_probability - market_no_vig_probability,
        odds_source = podium_display_odds_source
      ) %>%
      filter(podium_odds_allowed(odds_american_label, favorite_limit)) %>%
      filter(model_edge_allowed(model_edge, min_podium_edge_pct))

    bind_rows(winner_bets, podium_bets) %>%
      { if (nrow(.) == 0) empty_allrf_bets() else . } %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(
          is.na(actual_finish) ~ "No result",
          is.na(odds_decimal) ~ "No odds",
          bet_won ~ "Won",
          TRUE ~ "Lost"
        )
      )
  }

  register_model_family_consensus_outputs <- function(
    prefix,
    family_label,
    finish_rows,
    probability_rows,
    points_rows,
    finish_model_input,
    probability_model_input,
    points_model_input,
    default_finish_models,
    default_probability_models,
    default_points_models
  ) {
    input_id <- function(suffix) paste0(prefix, "_", suffix)
    output_id <- function(suffix) paste0(prefix, "_", suffix)

    build_ranks <- function(season_value, round_value = NULL) {
      finish_models <- selected_or_default_models(input[[finish_model_input]], default_finish_models)
      probability_models <- selected_or_default_models(input[[probability_model_input]], default_probability_models)
      points_models <- selected_or_default_models(input[[points_model_input]], default_points_models)

      build_selected_family_consensus_ranks(
        season_value,
        round_value,
        input[[input_id("use_finish")]],
        input[[input_id("use_probability")]],
        input[[input_id("use_points")]],
        finish_rows,
        probability_rows,
        points_rows,
        finish_models,
        probability_models,
        points_models,
        family_label,
        input[[input_id("consensus_mode")]] %||% "family"
      )
    }

    predictions <- reactive({
      req(input[[input_id("season")]], input[[input_id("round")]])
      rows <- build_ranks(input[[input_id("season")]], input[[input_id("round")]])
      validate(need(nrow(rows) > 0, paste0("Select at least one ", family_label, " family.")))
      build_family_predictions_from_ranks(rows, input[[input_id("use_estimated_podium_odds")]])
    })

    consensus_bets <- reactive({
      req(input[[input_id("season")]], input[[input_id("round")]])
      rows <- build_ranks(input[[input_id("season")]], input[[input_id("round")]])
      validate(need(nrow(rows) > 0, paste0("Select at least one ", family_label, " family.")))
      win_edge <- if (isTRUE(input[[input_id("use_edge_filter")]])) input[[input_id("min_win_edge_pct")]] else -100
      podium_edge <- if (isTRUE(input[[input_id("use_edge_filter")]])) input[[input_id("min_podium_edge_pct")]] else -100
      build_family_bets_from_ranks(rows, input[[input_id("force_consensus")]], input[[input_id("use_estimated_podium_odds")]], input[[input_id("podium_favorite_limit")]], win_edge, podium_edge)
    })

    output[[output_id("race_selector")]] <- renderUI({
      req(input[[input_id("season")]])
      choices <- rf_race_choices %>% filter(season == as.integer(input[[input_id("season")]]))
      selectInput(input_id("round"), "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
    })

    output[[output_id("model_header")]] <- renderUI({
      rows <- predictions()
      race <- rows %>% distinct(season, round, race_name) %>% slice(1)
      mode <- if (isTRUE(input[[input_id("force_consensus")]])) {
        if (identical(input[[input_id("consensus_mode")]] %||% "family", "model")) {
          "Forced consensus only bets when all selected model rankings agree."
        } else {
          "Forced consensus only bets when all selected families agree."
        }
      } else if (identical(input[[input_id("consensus_mode")]] %||% "family", "model")) {
        "Consensus averages each selected model ranking directly."
      } else {
        "Consensus averages selected family ranks across finish, probability, and points families."
      }
      div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p(mode)))
    })

    output[[output_id("betting_season_summary")]] <- renderTable({
      req(input[[input_id("roi_start_season")]], input[[input_id("roi_end_season")]])
      bounds <- roi_window_bounds(input[[input_id("roi_start_season")]], input[[input_id("roi_end_season")]])
      rows <- bind_rows(lapply(seq(bounds[["start"]], bounds[["end"]]), function(season_value) {
        build_ranks(season_value, NULL)
      })) %>%
        filter(season >= bounds[["start"]], season <= bounds[["end"]])
      validate(need(nrow(rows) > 0, paste0("Select at least one ", family_label, " family.")))
      win_edge <- if (isTRUE(input[[input_id("use_edge_filter")]])) input[[input_id("min_win_edge_pct")]] else -100
      podium_edge <- if (isTRUE(input[[input_id("use_edge_filter")]])) input[[input_id("min_podium_edge_pct")]] else -100
      summary_rows <- summarise_consensus_bets_window(
        build_family_bets_from_ranks(rows, input[[input_id("force_consensus")]], input[[input_id("use_estimated_podium_odds")]], input[[input_id("podium_favorite_limit")]], win_edge, podium_edge),
        bounds[["start"]],
        bounds[["end"]]
      )
      validate(need(nrow(summary_rows) > 0, paste0("No completed ", family_label, " consensus bets with odds found for this season.")))
      render_betting_summary_table(summary_rows)
    }, striped = TRUE, hover = TRUE, bordered = FALSE)

    output[[output_id("consensus_bets_table")]] <- renderTable({
      bet_rows <- consensus_bets()
      validate(need(nrow(bet_rows) > 0, paste0("No ", family_label, " consensus bets found for this race.")))
      bet_rows %>%
        mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
        transmute(
          Market = bet_market,
          Rank = consensus_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Odds = odds_american_label,
          Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"),
          `Market %` = format_pct(market_no_vig_probability, 1),
          Edge = format_pct(model_edge, 0.1),
          Result = bet_status,
          `Actual finish` = format_int(actual_finish),
          Stake = format_num(stake, 0),
          Profit = format_num(profit, 2),
          ROI = format_pct(roi, 0.1)
        )
    }, striped = TRUE, hover = TRUE, bordered = FALSE)

    output[[output_id("winner_table")]] <- renderTable({
      predictions() %>%
        filter(consensus_rank == 1) %>%
        transmute(
          Pick = driver_name,
          Constructor = constructor_name,
          `Avg win rank` = format_num(winner_rank_score, 2),
          Families = family_count,
          `Win odds` = win_avg_american_odds_label,
          `Actual finish` = format_int(finish_position),
          Correct = ifelse(winner_pick_correct, "Yes", "No")
        )
    }, striped = TRUE, hover = TRUE, bordered = FALSE)

    output[[output_id("prediction_table")]] <- renderTable({
      predictions() %>%
        add_prerace_display() %>%
        arrange(consensus_rank) %>%
        transmute(
          `Win rank` = consensus_rank,
          `Podium rank` = consensus_podium_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Start = display_start_position_label,
          Quali = display_quali_position_label,
          `Q delta` = display_quali_delta_label,
          `Avg win rank` = format_num(winner_rank_score, 2),
          `Avg podium rank` = format_num(podium_rank_score, 2),
          Families = family_count,
          `Win odds` = win_avg_american_odds_label,
          `Podium odds` = podium_display_american_odds_label,
          `Win edge` = format_pct(model_win_probability - win_market_no_vig_probability, 0.1),
          `Podium edge` = format_pct(model_podium_probability - podium_display_no_vig_probability, 0.1),
          `Actual finish` = format_int(finish_position),
          `Actual rank` = actual_rank_in_race
        )
    }, striped = TRUE, hover = TRUE, bordered = FALSE)
  }

  register_model_family_consensus_outputs(
    "allxgb",
    "XGBoost",
    xgb_finish_predictions,
    xgb_probability_predictions,
    xgb_points_predictions,
    "xgb_models",
    "xgb_prob_models",
    "xgb_points_models",
    xgb_finish_default_models,
    xgb_probability_default_models,
    xgb_points_default_models
  )

  output$qualifying_race_selector <- renderUI({
    validate(need(nrow(qualifying_predictions) > 0, "Run Stage 7 qualifying modeling to create qualifying predictions."))
    req(input$qualifying_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$qualifying_season))
    selectInput("qualifying_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  selected_qualifying_predictions <- reactive({
    validate(need(nrow(qualifying_predictions) > 0, "Run Stage 7 qualifying modeling to create qualifying predictions."))
    req(input$qualifying_season, input$qualifying_round)
    selected_models <- input$qualifying_models
    validate(need(length(selected_models) > 0, "Select at least one qualifying model."))
    selected_rows <- qualifying_predictions %>%
      filter(
        season == as.integer(input$qualifying_season),
        round == as.integer(input$qualifying_round),
        model %in% selected_models
      )
    validate(need(nrow(selected_rows) > 0, "No qualifying predictions found for this selection."))
    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          finish_position, actual_quali_position, actual_quali_delta_sec, actual_grid
        ) %>%
        summarise(
          predicted_quali_position = mean(predicted_quali_position, na.rm = TRUE),
          predicted_quali_delta_sec = mean(predicted_quali_delta_sec, na.rm = TRUE),
          model_count = n_distinct(model),
          .groups = "drop"
        ) %>%
        group_by(season, round) %>%
        arrange(predicted_quali_position, driver_name, .by_group = TRUE) %>%
        mutate(
          model = "qualifying_consensus_selected",
          model_label = paste0("Consensus of selected qualifying models (", max(model_count, na.rm = TRUE), ")"),
          predicted_quali_rank = row_number(),
          predicted_grid = predicted_quali_rank,
          actual_quali_rank = actual_quali_position,
          predicted_pole = predicted_quali_rank == 1,
          actual_pole = !is.na(finish_position) & actual_quali_position == 1,
          predicted_top3_quali = predicted_quali_rank <= 3,
          actual_top3_quali = !is.na(finish_position) & actual_quali_position <= 3,
          pole_pick_correct = if_else(!is.na(finish_position) & !is.na(actual_quali_position), predicted_pole & actual_pole, NA),
          top3_quali_pick_correct = if_else(!is.na(finish_position) & !is.na(actual_quali_position), predicted_top3_quali & actual_top3_quali, NA)
        ) %>%
        ungroup() %>%
        select(-model_count)

      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      group_by(model, season, round) %>%
      mutate(
        pole_rank_score = 1 / pmax(predicted_quali_rank, 1),
        pole_score_total = sum(pole_rank_score, na.rm = TRUE),
        rank_normalized_pole_probability = if_else(
          pole_score_total > 0,
          pole_rank_score / pole_score_total,
          NA_real_
        )
      ) %>%
      ungroup() %>%
      mutate(model_order = case_when(
        model == "qualifying_consensus_selected" ~ 0L,
        TRUE ~ match(model, qualifying_model_lookup$model)
      )) %>%
      arrange(model_order, predicted_quali_rank) %>%
      select(-model_order, -pole_rank_score, -pole_score_total)
  })

  output$qualifying_model_header <- renderUI({
    rows <- selected_qualifying_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p("The linear qualifying model projects pole, top-three qualifying order, and the pre-race grid fallback used by downstream race models.")))
  })

  output$qualifying_metrics_table <- renderTable({
    selected_models <- input$qualifying_models
    validate(need(length(selected_models) > 0, "Select at least one qualifying model."))

    metric_rows <- qualifying_predictions %>%
      filter(
        model %in% selected_models,
        season %in% c(2025L, 2026L)
      )

    if (n_distinct(metric_rows$model) >= 2) {
      metric_rows <- metric_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          finish_position, actual_quali_position, actual_quali_delta_sec, actual_grid
        ) %>%
        summarise(
          model = "qualifying_consensus_selected",
          model_label = "Consensus of selected qualifying models",
          predicted_quali_position = mean(predicted_quali_position, na.rm = TRUE),
          predicted_quali_delta_sec = mean(predicted_quali_delta_sec, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(season, round) %>%
        arrange(predicted_quali_position, driver_name, .by_group = TRUE) %>%
        mutate(
          predicted_quali_rank = row_number(),
          predicted_grid = predicted_quali_rank,
          predicted_pole = predicted_quali_rank == 1,
          actual_pole = !is.na(finish_position) & actual_quali_position == 1,
          predicted_top3_quali = predicted_quali_rank <= 3,
          actual_top3_quali = !is.na(finish_position) & actual_quali_position <= 3
        ) %>%
        ungroup()
    }

    completed <- metric_rows %>%
      filter(
        !is.na(finish_position),
        !is.na(actual_quali_position),
        !is.na(predicted_quali_position)
      )

    summarise_quali_success <- function(rows, period_label) {
      if (nrow(rows) == 0) {
        return(tibble(
          period = period_label,
          model_races = 0L,
          rows = 0L,
          position_rmse = NA_real_,
          position_mae = NA_real_,
          delta_rmse = NA_real_,
          delta_mae = NA_real_,
          pole_hit_rate = NA_real_,
          top3_hit_rate = NA_real_
        ))
      }

      model_races <- rows %>% distinct(season, round, model) %>% nrow()
      top3_actual_count <- sum(rows$actual_top3_quali, na.rm = TRUE)

      rows %>%
        summarise(
          period = period_label,
          model_races = model_races,
          rows = n(),
          position_rmse = sqrt(mean((actual_quali_position - predicted_quali_position)^2, na.rm = TRUE)),
          position_mae = mean(abs(actual_quali_position - predicted_quali_position), na.rm = TRUE),
          delta_rmse = sqrt(mean((actual_quali_delta_sec - predicted_quali_delta_sec)^2, na.rm = TRUE)),
          delta_mae = mean(abs(actual_quali_delta_sec - predicted_quali_delta_sec), na.rm = TRUE),
          pole_hit_rate = sum(predicted_quali_rank == 1 & actual_pole, na.rm = TRUE) / model_races,
          top3_hit_rate = if_else(top3_actual_count > 0, sum(predicted_top3_quali & actual_top3_quali, na.rm = TRUE) / top3_actual_count, NA_real_),
          .groups = "drop"
        )
    }

    bind_rows(
      summarise_quali_success(completed %>% filter(season == 2025L), "2025"),
      summarise_quali_success(completed %>% filter(season == 2026L), "2026"),
      summarise_quali_success(completed, "Overall")
    ) %>%
      mutate(
        has_completed = model_races > 0,
        `Position RMSE` = ifelse(has_completed, format_num(position_rmse, 2), ""),
        `Position MAE` = ifelse(has_completed, format_num(position_mae, 2), ""),
        `Delta RMSE` = ifelse(has_completed, format_num(delta_rmse, 3), ""),
        `Delta MAE` = ifelse(has_completed, format_num(delta_mae, 3), ""),
        `Pole hit %` = ifelse(has_completed, format_pct(pole_hit_rate, 0.1), ""),
        `Top 3 hit %` = ifelse(has_completed, format_pct(top3_hit_rate, 0.1), "")
      ) %>%
      transmute(Period = period, Races = model_races, Rows = rows, `Position RMSE`, `Position MAE`, `Delta RMSE`, `Delta MAE`, `Pole hit %`, `Top 3 hit %`)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$qualifying_pole_table <- renderTable({
    selected_qualifying_predictions() %>%
      filter(predicted_quali_rank == 1) %>%
      mutate(has_actual_quali = !is.na(finish_position) & !is.na(actual_quali_position)) %>%
      transmute(
        Model = model_label,
        Pick = driver_name,
        Constructor = constructor_name,
        `Pred quali` = format_num(predicted_quali_position, 2),
        `Pred delta` = format_num(predicted_quali_delta_sec, 3),
        `Pole norm %` = format_pct(rank_normalized_pole_probability, 0.1),
        `Implied pole ML` = probability_to_american_label(rank_normalized_pole_probability),
        `Actual quali` = ifelse(has_actual_quali, format_int(actual_quali_position), ""),
        Correct = ifelse(has_actual_quali, ifelse(coalesce(pole_pick_correct, FALSE), "Yes", "No"), "")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$qualifying_prediction_table <- renderTable({
    selected_qualifying_predictions() %>%
      left_join(
        qualifying_market_odds_lookup %>%
          select(season, round, driver_code, pole_current_american_odds),
        by = c("season", "round", "driver_code")
      ) %>%
      arrange(model_label, predicted_quali_rank) %>%
      mutate(
        has_completed_race = !is.na(finish_position),
        has_actual_quali = has_completed_race & !is.na(actual_quali_position),
        has_actual_delta = has_completed_race & !is.na(actual_quali_delta_sec),
        has_actual_grid = has_completed_race & !is.na(actual_grid)
      ) %>%
      transmute(
        Model = model_label,
        Rank = predicted_quali_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        `Pred quali` = format_num(predicted_quali_position, 2),
        `Pred delta` = format_num(predicted_quali_delta_sec, 3),
        `Pole norm %` = format_pct(rank_normalized_pole_probability, 0.1),
        `Implied pole ML` = probability_to_american_label(rank_normalized_pole_probability),
        `Current pole ML` = fmt_american_label(pole_current_american_odds),
        `Pred grid` = format_predicted_position(predicted_grid),
        `Actual quali` = ifelse(has_actual_quali, format_int(actual_quali_position), ""),
        `Actual delta` = ifelse(has_actual_delta, format_num(actual_quali_delta_sec, 3), ""),
        `Actual grid` = ifelse(has_actual_grid, format_int(actual_grid), "")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$winner_without_race_selector <- renderUI({
    validate(need(nrow(winner_without_predictions) > 0, "Run Stage 17 winner-without modeling to create predictions."))
    req(input$winner_without_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$winner_without_season))
    selectInput("winner_without_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  selected_winner_without_predictions <- reactive({
    validate(need(nrow(winner_without_predictions) > 0, "Run Stage 17 winner-without modeling to create predictions."))
    req(input$winner_without_season, input$winner_without_round)
    selected_models <- input$winner_without_models
    validate(need(length(selected_models) > 0, "Select at least one winner-without model."))
    selected_rows <- winner_without_predictions %>%
      filter(
        season == as.integer(input$winner_without_season),
        round == as.integer(input$winner_without_round),
        model %in% selected_models
      )
    validate(need(nrow(selected_rows) > 0, "No winner-without predictions found for this selection."))

    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          track_profile_id, finish_position, without_market_candidate,
          winner_without_target
        ) %>%
        summarise(
          predicted_winner_without_probability = mean(predicted_winner_without_probability, na.rm = TRUE),
          drv_points_long50_mean = mean(drv_points_long50_mean, na.rm = TRUE),
          drv_podium_long50_mean = mean(drv_podium_long50_mean, na.rm = TRUE),
          model_count = n_distinct(model),
          .groups = "drop"
        ) %>%
        group_by(data_split, season, round, race_name) %>%
        arrange(desc(predicted_winner_without_probability), desc(drv_points_long50_mean), desc(drv_podium_long50_mean), driver_name, .by_group = TRUE) %>%
        mutate(
          model = "winner_without_consensus_selected",
          model_label = paste0("Consensus of selected winner-without models (", max(model_count, na.rm = TRUE), ")"),
          selected_model = TRUE,
          predicted_without_rank = row_number(),
          predicted_winner_without = predicted_without_rank == 1,
          actual_without_rank = min_rank(finish_position),
          actual_winner_without = actual_without_rank == 1,
          winner_without_pick_correct = predicted_winner_without & actual_winner_without
        ) %>%
        ungroup()

      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      left_join(winner_without_market_odds_lookup, by = c("season", "round", "driver_code")) %>%
      group_by(model, season, round) %>%
      mutate(
        race_probability_total = sum(predicted_winner_without_probability, na.rm = TRUE),
        race_normalized_winner_without_probability = if_else(
          race_probability_total > 0,
          predicted_winner_without_probability / race_probability_total,
          NA_real_
        )
      ) %>%
      ungroup() %>%
      mutate(model_order = if_else(model == "winner_without_consensus_selected", 0L, match(model, winner_without_model_lookup$model))) %>%
      arrange(model_order, predicted_without_rank) %>%
      select(-model_order, -race_probability_total)
  })

  output$winner_without_model_header <- renderUI({
    rows <- selected_winner_without_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p("Winner-without excludes Red Bull, McLaren, Mercedes, Ferrari, and Aston Martin, then ranks the remaining eligible field.")
      )
    )
  })

  output$winner_without_metrics_table <- renderTable({
    selected_models <- input$winner_without_models
    validate(need(length(selected_models) > 0, "Select at least one winner-without model."))

    metric_rows <- winner_without_predictions %>%
      filter(
        model %in% selected_models,
        season %in% c(2025L, 2026L)
      )
    validate(need(nrow(metric_rows) > 0, "No winner-without predictions found for this model selection."))

    if (n_distinct(metric_rows$model) >= 2) {
      metric_rows <- metric_rows %>%
        group_by(
          data_split, season, round, race_date, race_name,
          driver_id, driver_code, driver_name, constructor_name,
          track_profile_id, finish_position, without_market_candidate,
          winner_without_target
        ) %>%
        summarise(
          model = "winner_without_consensus_selected",
          model_label = "Consensus of selected winner-without models",
          predicted_winner_without_probability = mean(predicted_winner_without_probability, na.rm = TRUE),
          drv_points_long50_mean = mean(drv_points_long50_mean, na.rm = TRUE),
          drv_podium_long50_mean = mean(drv_podium_long50_mean, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(data_split, season, round, race_name) %>%
        arrange(desc(predicted_winner_without_probability), desc(drv_points_long50_mean), desc(drv_podium_long50_mean), driver_name, .by_group = TRUE) %>%
        mutate(
          predicted_without_rank = row_number(),
          predicted_winner_without = predicted_without_rank == 1,
          actual_without_rank = min_rank(finish_position),
          actual_winner_without = actual_without_rank == 1,
          winner_without_pick_correct = predicted_winner_without & actual_winner_without
        ) %>%
        ungroup()
    }

    completed <- metric_rows %>%
      filter(!is.na(finish_position), !is.na(winner_without_target)) %>%
      mutate(
        actual = as.integer(winner_without_target),
        probability = pmin(pmax(predicted_winner_without_probability, 1e-6), 1 - 1e-6)
      )
    validate(need(nrow(completed) > 0, "No completed winner-without predictions are available."))

    by_season <- completed %>%
      group_by(season) %>%
      summarise(
        races = n_distinct(round),
        correct_picks = sum(predicted_without_rank == 1 & actual_winner_without, na.rm = TRUE),
        hit_rate = correct_picks / races,
        log_loss = -mean(actual * log(probability) + (1 - actual) * log(1 - probability), na.rm = TRUE),
        brier_score = mean((probability - actual)^2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(period = as.character(season)) %>%
      select(period, races, correct_picks, hit_rate, log_loss, brier_score)

    overall <- completed %>%
      summarise(
        period = "Overall",
        races = n_distinct(season, round),
        correct_picks = sum(predicted_without_rank == 1 & actual_winner_without, na.rm = TRUE),
        hit_rate = correct_picks / races,
        log_loss = -mean(actual * log(probability) + (1 - actual) * log(1 - probability), na.rm = TRUE),
        brier_score = mean((probability - actual)^2, na.rm = TRUE)
      )

    bind_rows(by_season, overall) %>%
      transmute(
        Period = period,
        Races = races,
        Correct = correct_picks,
        `Hit rate` = format_pct(hit_rate, 0.1),
        `Log loss` = format_num(log_loss, 3),
        `Brier score` = format_num(brier_score, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$winner_without_pick_table <- renderTable({
    selected_winner_without_predictions() %>%
      filter(predicted_without_rank == 1) %>%
      transmute(
        Model = model_label,
        Pick = driver_name,
        Constructor = constructor_name,
        `Model win %` = format_pct(predicted_winner_without_probability, 0.1),
        `Race norm win %` = format_pct(race_normalized_winner_without_probability, 0.1),
        `Implied ML` = probability_to_american_label(race_normalized_winner_without_probability),
        `Market ML` = probability_to_american_label(ww_current_no_vig_probability),
        `Market raw ML` = ifelse(is.na(ww_current_american_odds), "", paste0(ifelse(ww_current_american_odds > 0, "+", ""), format_int(ww_current_american_odds))),
        `Actual without rank` = format_int(actual_without_rank),
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(coalesce(winner_without_pick_correct, FALSE), "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$winner_without_prediction_table <- renderTable({
    selected_winner_without_predictions() %>%
      arrange(model_label, predicted_without_rank) %>%
      transmute(
        Model = model_label,
        Rank = predicted_without_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        `Model win %` = format_pct(predicted_winner_without_probability, 0.1),
        `Race norm win %` = format_pct(race_normalized_winner_without_probability, 0.1),
        `Implied ML` = probability_to_american_label(race_normalized_winner_without_probability),
        `Market ML` = probability_to_american_label(ww_current_no_vig_probability),
        `Market raw ML` = ifelse(is.na(ww_current_american_odds), "", paste0(ifelse(ww_current_american_odds > 0, "+", ""), format_int(ww_current_american_odds))),
        `Actual without rank` = format_int(actual_without_rank),
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(coalesce(winner_without_pick_correct, FALSE), "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_race_selector <- renderUI({
    validate(need(nrow(xgb_finish_predictions) > 0, "Run Stage 11 XGBoost finish modeling to create XGBoost predictions."))
    req(input$xgb_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$xgb_season))
    selectInput("xgb_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  observeEvent(list(input$xgb_season, input$xgb_round), {
    req(input$xgb_season, input$xgb_round)
    updateCheckboxGroupInput(
      session,
      "xgb_models",
      selected = default_xgb_finish_models_for_race(input$xgb_season, input$xgb_round)
    )
  }, ignoreInit = FALSE)

  selected_xgb_predictions <- reactive({
    validate(need(nrow(xgb_finish_predictions) > 0, "Run Stage 11 XGBoost finish modeling to create XGBoost predictions."))
    req(input$xgb_season, input$xgb_round, input$xgb_models)

    selected_rows <- xgb_finish_predictions %>%
      filter(season == as.integer(input$xgb_season), round == as.integer(input$xgb_round), model %in% input$xgb_models) %>%
      add_probability_display_odds(input$xgb_use_estimated_podium_odds)

    validate(need(nrow(selected_rows) > 0, "No XGBoost predictions found for this selection."))

    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(data_split, season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
        summarise(predicted_finish_position = mean(predicted_finish_position, na.rm = TRUE), model_count = n_distinct(model), .groups = "drop") %>%
        arrange(predicted_finish_position, driver_name) %>%
        mutate(
          model = "xgb_finish_consensus_selected",
          model_label = paste0("Consensus of selected XGBoosts (", max(model_count, na.rm = TRUE), ")"),
          selected_model = TRUE,
          predicted_rank_in_race = row_number(),
          predicted_winner = predicted_rank_in_race == 1,
          winner_pick_correct = predicted_winner & actual_winner,
          predicted_podium = predicted_rank_in_race <= 3,
          podium_pick_correct = predicted_podium & actual_podium
        ) %>%
        left_join(
          selected_rows %>%
            distinct(
              season, round, driver_code,
              win_avg_american_odds_label,
              win_market_no_vig_probability,
              podium_display_american_odds_label,
              podium_display_no_vig_probability,
              podium_display_odds_source
            ),
          by = c("season", "round", "driver_code")
        )

      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      mutate(model_order = if_else(model == "xgb_finish_consensus_selected", 0L, match(model, xgb_finish_model_lookup$model))) %>%
      arrange(model_order, predicted_rank_in_race) %>%
      select(-model_order)
  })

  output$xgb_model_header <- renderUI({
    rows <- selected_xgb_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p("Finish models rank drivers by predicted finish position. Market value uses the routed XGBoost probability consensus rather than a rank-derived probability proxy.")))
  })

  add_routed_xgb_probability_valuation <- function(rows) {
    if (nrow(rows) == 0 || nrow(xgb_probability_predictions) == 0) {
      return(rows %>% mutate(
        model_win_probability = NA_real_,
        model_podium_probability = NA_real_,
        valuation_model_count = 0L
      ))
    }

    race_keys <- rows %>% distinct(season, round)
    valuation_rows <- bind_rows(lapply(seq_len(nrow(race_keys)), function(i) {
      season_value <- race_keys$season[[i]]
      round_value <- race_keys$round[[i]]
      routed_models <- default_xgb_probability_models_for_race(season_value, round_value)

      xgb_probability_predictions %>%
        filter(
          season == season_value,
          round == round_value,
          model %in% routed_models
        )
    })) %>%
      group_by(season, round, driver_code) %>%
      summarise(
        model_win_probability = safe_mean(predicted_win_probability),
        model_podium_probability = safe_mean(predicted_podium_probability),
        valuation_model_count = n_distinct(model),
        .groups = "drop"
      )

    rows %>% left_join(valuation_rows, by = c("season", "round", "driver_code"))
  }

  build_xgb_finish_consensus_bets <- function(selected_rows, use_estimated, favorite_limit, min_win_edge_pct = -100, min_podium_edge_pct = -100) {
    validate(need(nrow(selected_rows) > 0, "No XGBoost predictions found for this model selection."))

    selected_rows <- selected_rows %>% add_probability_display_odds(use_estimated)

    consensus_rows <- selected_rows %>%
      group_by(data_split, season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(predicted_finish = mean(predicted_finish_position, na.rm = TRUE), selected_model_count = n_distinct(model), .groups = "drop") %>%
      arrange(season, round, predicted_finish, driver_name) %>%
      group_by(season, round) %>%
      mutate(consensus_rank = row_number()) %>%
      ungroup() %>%
      left_join(
        selected_rows %>%
          distinct(
            season, round, driver_code,
            win_avg_american_odds_label,
            win_market_no_vig_probability,
            podium_display_american_odds_label,
            podium_display_no_vig_probability,
            podium_display_odds_source
          ),
        by = c("season", "round", "driver_code")
      ) %>%
      add_routed_xgb_probability_valuation()

    winner_bets <- consensus_rows %>%
      filter(consensus_rank == 1) %>%
      transmute(season, round, race_date, race_name, selected_model_count, bet_market = "win", consensus_rank, driver_code, driver_name, constructor_name, predicted_finish, model_win_probability, model_podium_probability, actual_finish = finish_position, bet_won = actual_winner, odds_american_label = win_avg_american_odds_label, market_no_vig_probability = win_market_no_vig_probability, model_edge = model_win_probability - market_no_vig_probability, odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")) %>%
      filter(model_edge_allowed(model_edge, min_win_edge_pct))

    podium_bets <- consensus_rows %>%
      filter(consensus_rank <= 3) %>%
      transmute(season, round, race_date, race_name, selected_model_count, bet_market = "podium", consensus_rank, driver_code, driver_name, constructor_name, predicted_finish, model_win_probability, model_podium_probability, actual_finish = finish_position, bet_won = actual_podium, odds_american_label = podium_display_american_odds_label, market_no_vig_probability = podium_display_no_vig_probability, model_edge = model_podium_probability - market_no_vig_probability, odds_source = podium_display_odds_source) %>%
      filter(podium_odds_allowed(odds_american_label, favorite_limit)) %>%
      filter(model_edge_allowed(model_edge, min_podium_edge_pct))

    bind_rows(winner_bets, podium_bets) %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(is.na(actual_finish) ~ "No result", is.na(odds_decimal) ~ "No odds", bet_won ~ "Won", TRUE ~ "Lost")
      )
  }

  selected_xgb_consensus_bets <- reactive({
    req(input$xgb_models)
    selected_rows <- xgb_finish_predictions %>%
      filter(model %in% input$xgb_models)
    win_edge <- if (isTRUE(input$xgb_use_edge_filter)) input$xgb_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$xgb_use_edge_filter)) input$xgb_min_podium_edge_pct else -100
    build_xgb_finish_consensus_bets(selected_rows, input$xgb_use_estimated_podium_odds, input$xgb_podium_favorite_limit, win_edge, podium_edge)
  })

  race_routed_xgb_consensus_bets <- reactive({
    race_keys <- xgb_finish_predictions %>%
      distinct(season, round)
    selected_rows <- bind_rows(lapply(seq_len(nrow(race_keys)), function(i) {
      xgb_finish_predictions %>% filter(season == race_keys$season[[i]], round == race_keys$round[[i]], model %in% default_xgb_finish_models_for_race(race_keys$season[[i]], race_keys$round[[i]]))
    }))
    win_edge <- if (isTRUE(input$xgb_use_edge_filter)) input$xgb_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$xgb_use_edge_filter)) input$xgb_min_podium_edge_pct else -100
    build_xgb_finish_consensus_bets(selected_rows, input$xgb_use_estimated_podium_odds, input$xgb_podium_favorite_limit, win_edge, podium_edge)
  })

  render_betting_summary_table <- function(summary_rows) {
    summary_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium", combined = "Combined")) %>%
      transmute(
        Period = if ("period" %in% names(.)) period else as.character(season),
        Type = if ("race_condition" %in% names(.)) race_condition else "All",
        Market = bet_market,
        Races = races_with_available_bets,
        Bets = bets_available,
        Wins = bets_won,
        `Hit rate` = format_pct(hit_rate, 1),
        `Avg edge` = if ("avg_model_edge" %in% names(.)) {
          if_else(is.na(avg_model_edge) | is.nan(avg_model_edge), "", format_pct(avg_model_edge, 0.1))
        } else {
          ""
        },
        Stake = format_num(stake, 0),
        Profit = format_num(profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }

  output$xgb_betting_season_summary <- renderTable({
    req(input$xgb_roi_start_season, input$xgb_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(selected_xgb_consensus_bets(), input$xgb_roi_start_season, input$xgb_roi_end_season)
    validate(need(nrow(summary_rows) > 0, "No completed XGBoost bets with odds found."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_routed_betting_season_summary <- renderTable({
    req(input$xgb_roi_start_season, input$xgb_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(race_routed_xgb_consensus_bets(), input$xgb_roi_start_season, input$xgb_roi_end_season)
    validate(need(nrow(summary_rows) > 0, "No completed race-routed XGBoost bets with odds found."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_consensus_bets_table <- renderTable({
    bet_rows <- selected_xgb_consensus_bets() %>% filter(season == as.integer(input$xgb_season), round == as.integer(input$xgb_round))
    validate(need(nrow(bet_rows) > 0, "No selected XGBoost consensus bets found for this race."))
    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(Market = bet_market, Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name, `Pred finish` = format_num(predicted_finish, 2), `Value probability` = format_pct(if_else(bet_market == "Winner", model_win_probability, model_podium_probability), 0.1), Odds = odds_american_label, Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"), `Market %` = format_pct(market_no_vig_probability, 1), `Value edge` = format_pct(model_edge, 0.1), Result = bet_status, `Actual finish` = format_int(actual_finish), Stake = format_num(stake, 0), Profit = format_num(profit, 2), ROI = format_pct(roi, 0.1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_winner_table <- renderTable({
    selected_xgb_predictions() %>%
      filter(predicted_rank_in_race == 1) %>%
      transmute(Model = model_label, Pick = driver_name, Constructor = constructor_name, `Pred finish` = format_num(predicted_finish_position, 2), `Win odds` = win_avg_american_odds_label, `Mkt win %` = format_pct(win_market_no_vig_probability, 1), `Actual finish` = format_int(finish_position), Correct = ifelse(winner_pick_correct, "Yes", "No"))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prediction_table <- renderTable({
    selected_xgb_predictions() %>%
      add_prerace_display() %>%
      arrange(model_label, predicted_rank_in_race) %>%
      transmute(Model = model_label, Rank = predicted_rank_in_race, Driver = driver_name, Constructor = constructor_name, Start = display_start_position_label, Quali = display_quali_position_label, `Q delta` = display_quali_delta_label, `Pred finish` = format_num(predicted_finish_position, 2), `Win odds` = win_avg_american_odds_label, `Podium odds` = podium_display_american_odds_label, `Actual finish` = format_int(finish_position), `Actual rank` = actual_rank_in_race)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prob_race_selector <- renderUI({
    validate(need(nrow(xgb_probability_predictions) > 0, "Run Stage 12 XGBoost probability modeling to create predictions."))
    req(input$xgb_prob_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$xgb_prob_season))
    selectInput("xgb_prob_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  observeEvent(list(input$xgb_prob_season, input$xgb_prob_round), {
    req(input$xgb_prob_season, input$xgb_prob_round)
    updateCheckboxGroupInput(
      session,
      "xgb_prob_models",
      selected = default_xgb_probability_models_for_race(input$xgb_prob_season, input$xgb_prob_round)
    )
  }, ignoreInit = FALSE)

  selected_xgb_probability_predictions <- reactive({
    validate(need(nrow(xgb_probability_predictions) > 0, "Run Stage 12 XGBoost probability modeling to create predictions."))
    req(input$xgb_prob_season, input$xgb_prob_round, input$xgb_prob_models)
    selected_rows <- xgb_probability_predictions %>%
      filter(season == as.integer(input$xgb_prob_season), round == as.integer(input$xgb_prob_round), model %in% input$xgb_prob_models) %>%
      add_probability_display_odds(input$xgb_prob_use_estimated_podium_odds)
    validate(need(nrow(selected_rows) > 0, "No XGBoost probability predictions found for this selection."))

    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>%
        group_by(data_split, season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
        summarise(predicted_win_probability = mean(predicted_win_probability, na.rm = TRUE), predicted_podium_probability = mean(predicted_podium_probability, na.rm = TRUE), model_count = n_distinct(model), .groups = "drop") %>%
        arrange(desc(predicted_win_probability), driver_name) %>%
        mutate(model = "xgb_prob_consensus_selected", model_label = paste0("Consensus of selected XGBoost probability models (", max(model_count, na.rm = TRUE), ")"), selected_model = TRUE, predicted_win_rank = row_number(), predicted_winner = predicted_win_rank == 1, winner_pick_correct = predicted_winner & actual_winner) %>%
        arrange(desc(predicted_podium_probability), driver_name) %>%
        mutate(predicted_podium_rank = row_number(), predicted_podium = predicted_podium_rank <= 3, podium_pick_correct = predicted_podium & actual_podium, predicted_rank_in_race = predicted_win_rank) %>%
        left_join(
          selected_rows %>% distinct(season, round, driver_code, win_avg_american_odds_label, win_market_no_vig_probability, podium_display_american_odds_label, podium_display_no_vig_probability, podium_display_odds_source),
          by = c("season", "round", "driver_code")
        )
      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }

    selected_rows %>%
      mutate(model_order = if_else(model == "xgb_prob_consensus_selected", 0L, match(model, xgb_probability_model_lookup$model))) %>%
      arrange(model_order, predicted_win_rank) %>%
      select(-model_order)
  })

  output$xgb_prob_model_header <- renderUI({
    rows <- selected_xgb_probability_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p("XGBoost probability models rank winner picks by predicted win probability and podium picks by predicted podium probability.")))
  })

  selected_xgb_probability_consensus_bets <- reactive({
    req(input$xgb_prob_models)
    selected_rows <- xgb_probability_predictions %>%
      filter(model %in% input$xgb_prob_models)
    win_edge <- if (isTRUE(input$xgb_prob_use_edge_filter)) input$xgb_prob_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$xgb_prob_use_edge_filter)) input$xgb_prob_min_podium_edge_pct else -100
    build_probability_consensus_bets(selected_rows, input$xgb_prob_use_estimated_podium_odds, input$xgb_prob_podium_favorite_limit, win_edge, podium_edge)
  })

  race_routed_xgb_probability_consensus_bets <- reactive({
    race_keys <- xgb_probability_predictions %>%
      distinct(season, round)
    selected_rows <- bind_rows(lapply(seq_len(nrow(race_keys)), function(i) {
      xgb_probability_predictions %>% filter(season == race_keys$season[[i]], round == race_keys$round[[i]], model %in% default_xgb_probability_models_for_race(race_keys$season[[i]], race_keys$round[[i]]))
    }))
    win_edge <- if (isTRUE(input$xgb_prob_use_edge_filter)) input$xgb_prob_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$xgb_prob_use_edge_filter)) input$xgb_prob_min_podium_edge_pct else -100
    build_probability_consensus_bets(selected_rows, input$xgb_prob_use_estimated_podium_odds, input$xgb_prob_podium_favorite_limit, win_edge, podium_edge)
  })

  output$xgb_prob_betting_season_summary <- renderTable({
    req(input$xgb_prob_roi_start_season, input$xgb_prob_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(selected_xgb_probability_consensus_bets(), input$xgb_prob_roi_start_season, input$xgb_prob_roi_end_season)
    validate(need(nrow(summary_rows) > 0, "No completed XGBoost probability bets with odds found."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prob_routed_betting_season_summary <- renderTable({
    req(input$xgb_prob_roi_start_season, input$xgb_prob_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(race_routed_xgb_probability_consensus_bets(), input$xgb_prob_roi_start_season, input$xgb_prob_roi_end_season)
    validate(need(nrow(summary_rows) > 0, "No completed race-routed XGBoost probability bets with odds found."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prob_consensus_bets_table <- renderTable({
    bet_rows <- selected_xgb_probability_consensus_bets() %>% filter(season == as.integer(input$xgb_prob_season), round == as.integer(input$xgb_prob_round))
    validate(need(nrow(bet_rows) > 0, "No selected XGBoost probability consensus bets found for this race."))
    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(Market = bet_market, Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name, Probability = format_pct(predicted_probability, 0.1), Odds = odds_american_label, Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"), `Market %` = format_pct(market_no_vig_probability, 1), Edge = format_pct(model_edge, 0.1), Result = bet_status, `Actual finish` = format_int(actual_finish), Stake = format_num(stake, 0), Profit = format_num(profit, 2), ROI = format_pct(roi, 0.1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prob_winner_table <- renderTable({
    selected_xgb_probability_predictions() %>%
      filter(predicted_win_rank == 1) %>%
      transmute(Model = model_label, Pick = driver_name, Constructor = constructor_name, `P win` = format_pct(predicted_win_probability, 0.1), `P podium` = format_pct(predicted_podium_probability, 0.1), `Win odds` = win_avg_american_odds_label, `Mkt win %` = format_pct(win_market_no_vig_probability, 1), `Win edge` = format_pct(predicted_win_probability - win_market_no_vig_probability, 0.1), `Podium edge` = format_pct(predicted_podium_probability - podium_display_no_vig_probability, 0.1), `Actual finish` = format_int(finish_position), Correct = ifelse(winner_pick_correct, "Yes", "No"))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_prob_prediction_table <- renderTable({
    selected_xgb_probability_predictions() %>%
      add_prerace_display() %>%
      arrange(model_label, predicted_win_rank) %>%
      transmute(Model = model_label, `Win rank` = predicted_win_rank, `Podium rank` = predicted_podium_rank, Driver = driver_name, Constructor = constructor_name, Start = display_start_position_label, Quali = display_quali_position_label, `Q delta` = display_quali_delta_label, `P win` = format_pct(predicted_win_probability, 0.1), `P podium` = format_pct(predicted_podium_probability, 0.1), `Win edge` = format_pct(predicted_win_probability - win_market_no_vig_probability, 0.1), `Podium edge` = format_pct(predicted_podium_probability - podium_display_no_vig_probability, 0.1), `Win odds` = win_avg_american_odds_label, `Podium odds` = podium_display_american_odds_label, `Actual finish` = format_int(finish_position))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_points_race_selector <- renderUI({
    validate(need(nrow(xgb_points_predictions) > 0, "Run Stage 13 XGBoost points modeling to create predictions."))
    req(input$xgb_points_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$xgb_points_season))
    selectInput("xgb_points_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  observeEvent(list(input$xgb_points_season, input$xgb_points_round), {
    req(input$xgb_points_season, input$xgb_points_round)
    updateCheckboxGroupInput(
      session,
      "xgb_points_models",
      selected = default_xgb_points_models_for_race(input$xgb_points_season, input$xgb_points_round)
    )
  }, ignoreInit = FALSE)

  selected_xgb_points_predictions <- reactive({
    validate(need(nrow(xgb_points_predictions) > 0, "Run Stage 13 XGBoost points modeling to create predictions."))
    req(input$xgb_points_season, input$xgb_points_round, input$xgb_points_models)
    selected_rows <- xgb_points_predictions %>% filter(season == as.integer(input$xgb_points_season), round == as.integer(input$xgb_points_round), model %in% input$xgb_points_models) %>% add_probability_display_odds(input$xgb_points_use_estimated_podium_odds)
    validate(need(nrow(selected_rows) > 0, "No XGBoost points predictions found for this selection."))
    if (n_distinct(selected_rows$model) >= 2) {
      consensus_rows <- selected_rows %>% group_by(data_split, season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>% summarise(predicted_points = mean(predicted_points, na.rm = TRUE), model_count = n_distinct(model), .groups = "drop") %>% arrange(desc(predicted_points), driver_name) %>% mutate(model = "xgb_points_consensus_selected", model_label = paste0("Consensus of selected XGBoost points models (", max(model_count, na.rm = TRUE), ")"), selected_model = TRUE, predicted_rank_in_race = row_number(), predicted_winner = predicted_rank_in_race == 1, winner_pick_correct = predicted_winner & actual_winner, predicted_podium = predicted_rank_in_race <= 3, podium_pick_correct = predicted_podium & actual_podium) %>% left_join(selected_rows %>% distinct(season, round, driver_code, win_avg_american_odds_label, win_market_no_vig_probability, podium_display_american_odds_label, podium_display_no_vig_probability, podium_display_odds_source), by = c("season", "round", "driver_code"))
      selected_rows <- bind_rows(selected_rows, consensus_rows)
    }
    selected_rows %>% mutate(model_order = if_else(model == "xgb_points_consensus_selected", 0L, match(model, xgb_points_model_lookup$model))) %>% arrange(model_order, predicted_rank_in_race) %>% select(-model_order)
  })

  output$xgb_points_model_header <- renderUI({
    rows <- selected_xgb_points_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p("Points models rank drivers by predicted points scored. Market value uses the routed XGBoost probability consensus rather than a rank-derived probability proxy.")))
  })

  build_xgb_points_consensus_bets <- function(selected_rows, use_estimated, favorite_limit, min_win_edge_pct = -100, min_podium_edge_pct = -100) {
    validate(need(nrow(selected_rows) > 0, "No XGBoost points predictions found for this model selection."))
    selected_rows <- selected_rows %>% add_probability_display_odds(use_estimated)
    consensus_rows <- selected_rows %>%
      group_by(data_split, season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(predicted_points = mean(predicted_points, na.rm = TRUE), selected_model_count = n_distinct(model), .groups = "drop") %>%
      arrange(season, round, desc(predicted_points), driver_name) %>%
      group_by(season, round) %>%
      mutate(
        consensus_rank = row_number()
      ) %>%
      ungroup() %>%
      left_join(selected_rows %>% distinct(season, round, driver_code, win_avg_american_odds_label, win_market_no_vig_probability, podium_display_american_odds_label, podium_display_no_vig_probability, podium_display_odds_source), by = c("season", "round", "driver_code")) %>%
      add_routed_xgb_probability_valuation()
    winner_bets <- consensus_rows %>%
      filter(consensus_rank == 1) %>%
      transmute(season, round, race_date, race_name, selected_model_count, bet_market = "win", consensus_rank, driver_code, driver_name, constructor_name, predicted_finish = predicted_points, model_win_probability, model_podium_probability, actual_finish = finish_position, bet_won = actual_winner, odds_american_label = win_avg_american_odds_label, market_no_vig_probability = win_market_no_vig_probability, model_edge = model_win_probability - market_no_vig_probability, odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")) %>%
      filter(model_edge_allowed(model_edge, min_win_edge_pct))
    podium_bets <- consensus_rows %>%
      filter(consensus_rank <= 3) %>%
      transmute(season, round, race_date, race_name, selected_model_count, bet_market = "podium", consensus_rank, driver_code, driver_name, constructor_name, predicted_finish = predicted_points, model_win_probability, model_podium_probability, actual_finish = finish_position, bet_won = actual_podium, odds_american_label = podium_display_american_odds_label, market_no_vig_probability = podium_display_no_vig_probability, model_edge = model_podium_probability - market_no_vig_probability, odds_source = podium_display_odds_source) %>%
      filter(podium_odds_allowed(odds_american_label, favorite_limit)) %>%
      filter(model_edge_allowed(model_edge, min_podium_edge_pct))
    bind_rows(winner_bets, podium_bets) %>% mutate(odds_decimal = american_label_to_decimal(odds_american_label), stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0), profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_), roi = if_else(stake > 0, profit / stake, NA_real_), bet_status = case_when(is.na(actual_finish) ~ "No result", is.na(odds_decimal) ~ "No odds", bet_won ~ "Won", TRUE ~ "Lost"))
  }

  selected_xgb_points_consensus_bets <- reactive({
    req(input$xgb_points_models)
    selected_rows <- xgb_points_predictions %>% filter(model %in% input$xgb_points_models)
    win_edge <- if (isTRUE(input$xgb_points_use_edge_filter)) input$xgb_points_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$xgb_points_use_edge_filter)) input$xgb_points_min_podium_edge_pct else -100
    build_xgb_points_consensus_bets(selected_rows, input$xgb_points_use_estimated_podium_odds, input$xgb_points_podium_favorite_limit, win_edge, podium_edge)
  })

  output$xgb_points_betting_season_summary <- renderTable({
    req(input$xgb_points_roi_start_season, input$xgb_points_roi_end_season)
    summary_rows <- summarise_consensus_bets_window(selected_xgb_points_consensus_bets(), input$xgb_points_roi_start_season, input$xgb_points_roi_end_season)
    validate(need(nrow(summary_rows) > 0, "No completed XGBoost points bets with odds found."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_points_consensus_bets_table <- renderTable({
    bet_rows <- selected_xgb_points_consensus_bets() %>% filter(season == as.integer(input$xgb_points_season), round == as.integer(input$xgb_points_round))
    validate(need(nrow(bet_rows) > 0, "No selected XGBoost points consensus bets found for this race."))
    bet_rows %>% mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>% transmute(Market = bet_market, Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name, `Pred points` = format_num(predicted_finish, 2), `Value probability` = format_pct(if_else(bet_market == "Winner", model_win_probability, model_podium_probability), 0.1), Odds = odds_american_label, Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"), `Market %` = format_pct(market_no_vig_probability, 1), `Value edge` = format_pct(model_edge, 0.1), Result = bet_status, `Actual finish` = format_int(actual_finish), Stake = format_num(stake, 0), Profit = format_num(profit, 2), ROI = format_pct(roi, 0.1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_points_winner_table <- renderTable({
    selected_xgb_points_predictions() %>% filter(predicted_rank_in_race == 1) %>% transmute(Model = model_label, Pick = driver_name, Constructor = constructor_name, `Pred points` = format_num(predicted_points, 2), `Win odds` = win_avg_american_odds_label, `Mkt win %` = format_pct(win_market_no_vig_probability, 1), `Actual finish` = format_int(finish_position), Correct = ifelse(winner_pick_correct, "Yes", "No"))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$xgb_points_prediction_table <- renderTable({
    selected_xgb_points_predictions() %>% add_prerace_display() %>% arrange(model_label, predicted_rank_in_race) %>% transmute(Model = model_label, Rank = predicted_rank_in_race, Driver = driver_name, Constructor = constructor_name, Start = display_start_position_label, Quali = display_quali_position_label, `Q delta` = display_quali_delta_label, `Pred points` = format_num(predicted_points, 2), `Win odds` = win_avg_american_odds_label, `Podium odds` = podium_display_american_odds_label, `Actual finish` = format_int(finish_position), `Actual rank` = actual_rank_in_race)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_race_selector <- renderUI({
    validate(need(nrow(chatter_team_features) > 0, "Run Stage 18 chatter overlay to create chatter features."))
    req(input$chatter_season)
    choices <- rf_race_choices %>%
      filter(season == as.integer(input$chatter_season)) %>%
      semi_join(chatter_team_features %>% distinct(season, round), by = c("season", "round"))
    selectInput("chatter_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  selected_chatter_teams <- reactive({
    validate(need(nrow(chatter_team_features) > 0, "Run Stage 18 chatter overlay to create chatter features."))
    req(input$chatter_season, input$chatter_round)
    chatter_team_features %>%
      filter(season == as.integer(input$chatter_season), round == as.integer(input$chatter_round))
  })

  selected_chatter_finish <- reactive({
    req(input$chatter_season, input$chatter_round)
    chatter_finish_overlay %>% filter(season == as.integer(input$chatter_season), round == as.integer(input$chatter_round))
  })

  selected_chatter_probability <- reactive({
    req(input$chatter_season, input$chatter_round)
    chatter_probability_overlay %>%
      filter(season == as.integer(input$chatter_season), round == as.integer(input$chatter_round)) %>%
      add_expected_win_moneyline_impact()
  })

  selected_chatter_points <- reactive({
    req(input$chatter_season, input$chatter_round)
    chatter_points_overlay %>% filter(season == as.integer(input$chatter_season), round == as.integer(input$chatter_round))
  })

  selected_chatter_winner_without <- reactive({
    req(input$chatter_season, input$chatter_round)
    chatter_winner_without_overlay %>% filter(season == as.integer(input$chatter_season), round == as.integer(input$chatter_round))
  })
  consensus_selection_value <- function(selection, name, fallback) {
    if (!is.null(selection) && !is.null(selection[[name]])) selection[[name]] else fallback
  }

  build_chatter_adjusted_allmodel_family_ranks <- function(season_value, round_value = NULL, chatter_weight = 1, bound_chatter = FALSE, selection = NULL) {
    family_rows <- list()
    season_value <- as.integer(season_value)
    consensus_mode <- consensus_selection_value(selection, "consensus_mode", input$allmodel_consensus_mode %||% "family")
    use_xgb_finish <- isTRUE(consensus_selection_value(selection, "use_xgb_finish", input$allmodel_use_xgb_finish))
    use_xgb_probability <- isTRUE(consensus_selection_value(selection, "use_xgb_probability", input$allmodel_use_xgb_probability))
    use_xgb_points <- isTRUE(consensus_selection_value(selection, "use_xgb_points", input$allmodel_use_xgb_points))
    use_routed_specialists <- isTRUE(consensus_selection_value(selection, "use_routed_specialists", input$allmodel_use_routed_specialists))
    selected_finish_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_finish_models", input$xgb_models), xgb_finish_default_models)
    selected_probability_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_probability_models", input$xgb_prob_models), xgb_probability_default_models)
    selected_points_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_points_models", input$xgb_points_models), xgb_points_default_models)

    finish_lookup <- chatter_finish_overlay %>%
      select(any_of(c("season", "round", "driver_code", "chatter_finish_nudge"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)
    probability_lookup <- chatter_probability_overlay %>%
      select(any_of(c("season", "round", "driver_code", "chatter_win_logit_nudge", "chatter_podium_logit_nudge"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)
    points_lookup <- chatter_points_overlay %>%
      select(any_of(c("season", "round", "driver_code", "chatter_points_nudge"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)
    if (isTRUE(bound_chatter)) {
      finish_lookup <- finish_lookup %>%
        group_by(season, round) %>%
        mutate(chatter_finish_nudge = bounded_centered_chatter_nudge(chatter_finish_nudge, 1, chatter_weight)) %>%
        ungroup()
      probability_lookup <- probability_lookup %>%
        group_by(season, round) %>%
        mutate(
          chatter_win_logit_nudge = bounded_centered_chatter_nudge(chatter_win_logit_nudge, 0.35, chatter_weight),
          chatter_podium_logit_nudge = bounded_centered_chatter_nudge(chatter_podium_logit_nudge, 0.35, chatter_weight)
        ) %>%
        ungroup()
      points_lookup <- points_lookup %>%
        group_by(season, round) %>%
        mutate(chatter_points_nudge = bounded_centered_chatter_nudge(chatter_points_nudge, 2, chatter_weight)) %>%
        ungroup()
    }

    if (use_xgb_finish) {
      rows <- xgb_finish_predictions %>%
        filter(season == season_value, model %in% selected_finish_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      rows <- rows %>%
        left_join(finish_lookup, by = c("season", "round", "driver_code")) %>%
        mutate(predicted_finish_position = predicted_finish_position - coalesce(chatter_finish_nudge, 0))
      family_rows$xgb_finish <- if (identical(consensus_mode, "model")) rank_each_selected_model(rows, rank_finish_family, "Finish model") else rank_finish_family(rows, "Finish model")
    }

    if (use_xgb_probability) {
      rows <- xgb_probability_predictions %>%
        filter(season == season_value, model %in% selected_probability_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      rows <- rows %>%
        left_join(probability_lookup, by = c("season", "round", "driver_code")) %>%
        mutate(
          predicted_win_probability = clamp_probability_numeric(plogis(qlogis(clamp_probability_numeric(predicted_win_probability, 1e-6)) + coalesce(chatter_win_logit_nudge, 0)), 1e-6),
          predicted_podium_probability = clamp_probability_numeric(plogis(qlogis(clamp_probability_numeric(predicted_podium_probability, 1e-6)) + coalesce(chatter_podium_logit_nudge, 0)), 1e-6)
        )
      family_rows$xgb_probability <- if (identical(consensus_mode, "model")) rank_each_selected_model(rows, rank_probability_family, "Probability model") else rank_probability_family(rows, "Probability model")
    }

    if (use_xgb_points) {
      rows <- xgb_points_predictions %>%
        filter(season == season_value, model %in% selected_points_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      rows <- rows %>%
        left_join(points_lookup, by = c("season", "round", "driver_code")) %>%
        mutate(predicted_points = predicted_points + coalesce(chatter_points_nudge, 0))
      family_rows$xgb_points <- if (identical(consensus_mode, "model")) rank_each_selected_model(rows, rank_points_family, "Points model") else rank_points_family(rows, "Points model")
    }

    if (use_routed_specialists) {
      routed_models <- consensus_routed_models_for_race(season_value, round_value)
      finish_rows <- xgb_finish_predictions %>% filter(season == season_value) %>% filter_routed_specialist_rows(routed_models)
      probability_rows <- xgb_probability_predictions %>% filter(season == season_value) %>% filter_routed_specialist_rows(routed_models)
      points_rows <- xgb_points_predictions %>% filter(season == season_value) %>% filter_routed_specialist_rows(routed_models)
      if (!is.null(round_value)) {
        round_value <- as.integer(round_value)
        finish_rows <- finish_rows %>% filter(round == round_value)
        probability_rows <- probability_rows %>% filter(round == round_value)
        points_rows <- points_rows %>% filter(round == round_value)
      }
      finish_rows <- finish_rows %>% left_join(finish_lookup, by = c("season", "round", "driver_code")) %>% mutate(predicted_finish_position = predicted_finish_position - coalesce(chatter_finish_nudge, 0))
      probability_rows <- probability_rows %>% left_join(probability_lookup, by = c("season", "round", "driver_code")) %>% mutate(predicted_win_probability = clamp_probability_numeric(plogis(qlogis(clamp_probability_numeric(predicted_win_probability, 1e-6)) + coalesce(chatter_win_logit_nudge, 0)), 1e-6), predicted_podium_probability = clamp_probability_numeric(plogis(qlogis(clamp_probability_numeric(predicted_podium_probability, 1e-6)) + coalesce(chatter_podium_logit_nudge, 0)), 1e-6))
      points_rows <- points_rows %>% left_join(points_lookup, by = c("season", "round", "driver_code")) %>% mutate(predicted_points = predicted_points + coalesce(chatter_points_nudge, 0))
      family_rows$routed_specialists <- if (identical(consensus_mode, "model")) {
        bind_rows(rank_each_selected_model(finish_rows, rank_finish_family, "Routed specialist finish"), rank_each_selected_model(probability_rows, rank_probability_family, "Routed specialist probability"), rank_each_selected_model(points_rows, rank_points_family, "Routed specialist points"))
      } else {
        bind_rows(rank_finish_family(finish_rows, "Routed specialist finish"), rank_probability_family(probability_rows, "Routed specialist probability"), rank_points_family(points_rows, "Routed specialist points"))
      }
    }

    bind_rows(family_rows)
  }

  selected_chatter_allmodel_consensus <- reactive({
    req(input$chatter_season, input$chatter_round)
    base_family_rows <- build_allmodel_family_consensus_ranks(input$chatter_season, input$chatter_round)
    adjusted_family_rows <- build_chatter_adjusted_allmodel_family_ranks(input$chatter_season, input$chatter_round)
    validate(need(nrow(base_family_rows) > 0, "Select at least one model family on the Model Consensus tab."))
    validate(need(nrow(adjusted_family_rows) > 0, "No chatter-adjusted model consensus rows found for this race."))

    base_rows <- build_allmodel_predictions_from_family_rows(base_family_rows, input$chatter_use_estimated_podium_odds) %>%
      select(season, round, race_name, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium, base_consensus_rank = consensus_rank, base_consensus_podium_rank = consensus_podium_rank, base_winner_rank_score = winner_rank_score, base_podium_rank_score = podium_rank_score, base_model_win_probability = model_win_probability, base_model_podium_probability = model_podium_probability, base_family_count = family_count, base_family_list = family_list, win_avg_american_odds_label, win_market_no_vig_probability, podium_display_american_odds_label, podium_display_no_vig_probability, podium_display_odds_source)

    adjusted_rows <- build_allmodel_predictions_from_family_rows(adjusted_family_rows, input$chatter_use_estimated_podium_odds) %>%
      select(season, round, driver_code, adjusted_consensus_rank = consensus_rank, adjusted_consensus_podium_rank = consensus_podium_rank, adjusted_winner_rank_score = winner_rank_score, adjusted_podium_rank_score = podium_rank_score, adjusted_model_win_probability = model_win_probability, adjusted_model_podium_probability = model_podium_probability, adjusted_family_count = family_count, adjusted_family_list = family_list)

    moneyline_rows <- selected_chatter_probability() %>%
      select(any_of(c("season", "round", "driver_code", "race_centered_chatter", "opening_win_avg_american_odds_label", "win_current_american_odds_label", "grid_win_avg_american_odds_label", "expected_win_american_odds_label", "expected_win_moneyline_delta_label", "actual_win_moneyline_delta_label", "grid_win_moneyline_delta_label", "grid_vs_chatter_moneyline_delta_label"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)

    finish_overlay_rows <- selected_chatter_finish() %>%
      select(any_of(c("season", "round", "driver_code", "base_predicted_finish", "adjusted_predicted_finish", "chatter_finish_nudge"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)

    points_overlay_rows <- selected_chatter_points() %>%
      select(any_of(c("season", "round", "driver_code", "base_predicted_points", "adjusted_predicted_points", "chatter_points_nudge"))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)

    base_rows %>%
      left_join(adjusted_rows, by = c("season", "round", "driver_code")) %>%
      left_join(moneyline_rows, by = c("season", "round", "driver_code")) %>%
      left_join(finish_overlay_rows, by = c("season", "round", "driver_code")) %>%
      left_join(points_overlay_rows, by = c("season", "round", "driver_code"))
  })

  chatter_allmodel_predictions <- reactive({
    req(input$chatter_season, input$chatter_round)
    rows <- build_chatter_adjusted_allmodel_family_ranks(input$chatter_season, input$chatter_round)
    validate(need(nrow(rows) > 0, "No chatter-adjusted model consensus rows found for this race."))
    build_allmodel_predictions_from_family_rows(rows, input$chatter_use_estimated_podium_odds)
  })

  chatter_allmodel_consensus_bets <- reactive({
    req(input$chatter_season, input$chatter_round)
    rows <- build_chatter_adjusted_allmodel_family_ranks(input$chatter_season, input$chatter_round)
    validate(need(nrow(rows) > 0, "No chatter-adjusted model consensus rows found for this race."))
    win_edge <- if (isTRUE(input$chatter_use_edge_filter)) input$chatter_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$chatter_use_edge_filter)) input$chatter_min_podium_edge_pct else -100
    build_allmodel_bets_from_family_rows(
      rows,
      input$allmodel_force_consensus,
      input$chatter_use_estimated_podium_odds,
      input$chatter_podium_favorite_limit,
      win_edge,
      podium_edge
    )
  })

  output$chatter_allmodel_betting_season_summary <- renderTable({
    req(input$allmodel_roi_start_season, input$allmodel_roi_end_season)
    bounds <- roi_window_bounds(input$allmodel_roi_start_season, input$allmodel_roi_end_season)
    rows <- bind_rows(lapply(seq(bounds[["start"]], bounds[["end"]]), function(season_value) {
      build_chatter_adjusted_allmodel_family_ranks(season_value, NULL)
    })) %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]])
    validate(need(nrow(rows) > 0, "No chatter-adjusted model consensus rows found for this season."))
    win_edge <- if (isTRUE(input$chatter_use_edge_filter)) input$chatter_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$chatter_use_edge_filter)) input$chatter_min_podium_edge_pct else -100
    summary_rows <- summarise_consensus_bets_window(
      build_allmodel_bets_from_family_rows(
        rows,
        input$allmodel_force_consensus,
        input$chatter_use_estimated_podium_odds,
        input$chatter_podium_favorite_limit,
        win_edge,
        podium_edge
      ),
      bounds[["start"]],
      bounds[["end"]]
    )
    validate(need(nrow(summary_rows) > 0, "No completed chatter-adjusted consensus bets with odds found for this season."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_allmodel_consensus_bets_table <- renderTable({
    bet_rows <- chatter_allmodel_consensus_bets()
    validate(need(nrow(bet_rows) > 0, "No chatter-adjusted consensus bets found for this race."))
    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(
        Market = bet_market,
        Rank = consensus_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Odds = odds_american_label,
        Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"),
        `Market %` = format_pct(market_no_vig_probability, 1),
        Edge = format_pct(model_edge, 0.1),
        Result = bet_status,
        `Actual finish` = format_int(actual_finish),
        Stake = format_num(stake, 0),
        Profit = format_num(profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_allmodel_winner_table <- renderTable({
    chatter_allmodel_predictions() %>%
      filter(consensus_rank == 1) %>%
      transmute(
        Pick = driver_name,
        Constructor = constructor_name,
        `Avg win rank` = format_num(winner_rank_score, 2),
        Families = family_count,
        `Win odds` = win_avg_american_odds_label,
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(winner_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_header <- renderUI({
    teams <- selected_chatter_teams()
    validate(need(nrow(teams) > 0, "No chatter rows found for this race."))
    race_label <- teams %>% distinct(season, round, Grand_Prix) %>% slice(1)
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race_label$season, " Round ", race_label$round)),
        h1(paste0(race_label$Grand_Prix, " Grand Prix")),
        p("Race-centered pre-race chatter applied to the same finish, probability, points, and routed-specialist blend selected on Model Consensus.")
      )
    )
  })

  output$chatter_team_table <- renderTable({
    rows <- selected_chatter_teams()
    validate(need(nrow(rows) > 0, "No chatter rows found for this race."))
    rows %>%
      arrange(desc(race_centered_chatter), Team) %>%
      transmute(
        Team,
        `Composite score` = format_num(Composite_Chatter_Score, 3),
        `Race-centered` = format_num(race_centered_chatter, 3),
        Momentum = format_num(Momentum_Raw, 2),
        Upgrade = format_num(Upgrade_Raw, 2),
        Context = format_num(Context_Raw, 2),
        Confidence = format_num(Source_Confidence, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  build_chatter_history_table <- function(row_filter) {
    validate(need(
      any(c(
        nrow(chatter_finish_overlay),
        nrow(chatter_probability_overlay),
        nrow(chatter_points_overlay),
        nrow(chatter_winner_without_overlay)
      ) > 0),
      "Run Stage 18 chatter overlay to create historical impact checks."
    ))

    scalar_or_na <- function(x) {
      if (length(x) == 0 || all(is.na(x))) NA_real_ else as.numeric(x[[1]])
    }
    clamp_probability <- function(x) pmin(pmax(as.numeric(x), 1e-6), 1 - 1e-6)
    brier_score <- function(actual, predicted) {
      actual <- as.integer(as.logical(actual))
      predicted <- clamp_probability(predicted)
      mean((predicted - actual)^2, na.rm = TRUE)
    }
    log_loss <- function(actual, predicted) {
      actual <- as.integer(as.logical(actual))
      predicted <- clamp_probability(predicted)
      -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted), na.rm = TRUE)
    }
    top_pick_hit <- function(rows, base_rank_col, adjusted_rank_col, actual_col) {
      if (nrow(rows) == 0) return(c(base = NA_real_, adjusted = NA_real_))
      base_hits <- rows %>%
        filter(!is.na(.data[[base_rank_col]]), !is.na(.data[[actual_col]])) %>%
        group_by(season, round) %>%
        arrange(.data[[base_rank_col]], driver_name, .by_group = TRUE) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(value = mean(as.logical(.data[[actual_col]]), na.rm = TRUE)) %>%
        pull(value)
      adjusted_hits <- rows %>%
        filter(!is.na(.data[[adjusted_rank_col]]), !is.na(.data[[actual_col]])) %>%
        group_by(season, round) %>%
        arrange(.data[[adjusted_rank_col]], driver_name, .by_group = TRUE) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(value = mean(as.logical(.data[[actual_col]]), na.rm = TRUE)) %>%
        pull(value)
      c(base = scalar_or_na(base_hits), adjusted = scalar_or_na(adjusted_hits))
    }
    metric_row <- function(family, metric, base, adjusted, races, rows, direction) {
      tibble(
        Family = family,
        Metric = metric,
        Base = base,
        Adjusted = adjusted,
        Change = adjusted - base,
        Races = races,
        Rows = rows,
        Direction = direction
      )
    }
    roi_metric_rows <- function(rows, family, market, base_rank_col, adjusted_rank_col, actual_col, odds_col, top_n = 1L) {
      if (nrow(rows) == 0) return(tibble())

      odds_lookup <- tryCatch(
        {
          if (nrow(xgb_probability_predictions) == 0) {
            stop("No probability odds lookup available.", call. = FALSE)
          }
          xgb_probability_predictions %>%
            add_probability_display_odds(TRUE) %>%
            transmute(
              season, round, driver_code,
              win_odds = win_avg_american_odds_label,
              podium_odds = podium_display_american_odds_label
            ) %>%
            distinct(season, round, driver_code, .keep_all = TRUE)
        },
        error = function(e) {
          tibble(season = integer(), round = integer(), driver_code = character(), win_odds = character(), podium_odds = character())
        }
      )

      summarise_side <- function(rank_col) {
        bets <- rows %>%
          filter(!is.na(.data[[rank_col]]), .data[[rank_col]] <= top_n, !is.na(.data[[actual_col]])) %>%
          left_join(odds_lookup, by = c("season", "round", "driver_code")) %>%
          mutate(
            odds_label = .data[[odds_col]],
            odds_decimal = american_label_to_decimal(odds_label),
            stake = if_else(!is.na(odds_decimal), 1, 0),
            profit = if_else(stake > 0, bet_profit(odds_decimal, as.logical(.data[[actual_col]]), stake), NA_real_)
          ) %>%
          filter(stake > 0)

        if (nrow(bets) == 0 || sum(bets$stake, na.rm = TRUE) <= 0) {
          return(tibble(hit_rate = NA_real_, stake = 0, profit = NA_real_, roi = NA_real_))
        }

        bets %>%
          summarise(
            hit_rate = mean(as.logical(.data[[actual_col]]), na.rm = TRUE),
            stake = sum(stake, na.rm = TRUE),
            profit = sum(profit, na.rm = TRUE),
            roi = profit / stake,
            .groups = "drop"
          )
      }

      base <- summarise_side(base_rank_col)
      adjusted <- summarise_side(adjusted_rank_col)
      race_count <- n_distinct(rows$season, rows$round)

      tibble(
        Family = family,
        Metric = paste0(market, " ROI"),
        Base = base$roi,
        Adjusted = adjusted$roi,
        Change = adjusted$roi - base$roi,
        Races = race_count,
        Rows = nrow(rows),
        Direction = "Higher",
        Extra = paste0("Base stake ", format_num(base$stake, 0), " / Adj stake ", format_num(adjusted$stake, 0))
      )
    }

    filter_history_rows <- function(rows) {
      if (nrow(rows) == 0) return(rows)
      row_filter(rows)
    }

    finish_rows <- chatter_finish_overlay %>%
      filter(!is.na(finish_position)) %>%
      filter_history_rows()
    probability_rows <- chatter_probability_overlay %>%
      filter(!is.na(finish_position)) %>%
      filter_history_rows()
    points_rows <- chatter_points_overlay %>%
      filter(!is.na(points)) %>%
      filter_history_rows()
    winner_without_rows <- chatter_winner_without_overlay %>%
      filter(!is.na(finish_position)) %>%
      filter_history_rows()

    summary_rows <- list()

    if (nrow(finish_rows) > 0) {
      top_hits <- top_pick_hit(finish_rows, "base_finish_rank", "adjusted_finish_rank", "actual_winner")
      summary_rows <- append(summary_rows, list(
        metric_row(
          "Finish", "MAE",
          mean(abs(finish_rows$base_predicted_finish - finish_rows$finish_position), na.rm = TRUE),
          mean(abs(finish_rows$adjusted_predicted_finish - finish_rows$finish_position), na.rm = TRUE),
          n_distinct(finish_rows$season, finish_rows$round), nrow(finish_rows), "Lower"
        ),
        metric_row(
          "Finish", "Winner top-pick hit rate",
          top_hits[["base"]], top_hits[["adjusted"]],
          n_distinct(finish_rows$season, finish_rows$round), nrow(finish_rows), "Higher"
        ),
        roi_metric_rows(finish_rows, "Finish", "Winner", "base_finish_rank", "adjusted_finish_rank", "actual_winner", "win_odds", 1L)
      ))
    }

    if (nrow(probability_rows) > 0) {
      win_hits <- top_pick_hit(probability_rows, "base_win_rank", "adjusted_win_rank", "actual_winner")
      podium_hits <- top_pick_hit(probability_rows, "base_podium_rank", "adjusted_podium_rank", "actual_podium")
      summary_rows <- append(summary_rows, list(
        metric_row("Probability", "Win Brier", brier_score(probability_rows$actual_winner, probability_rows$base_win_probability), brier_score(probability_rows$actual_winner, probability_rows$adjusted_win_probability), n_distinct(probability_rows$season, probability_rows$round), nrow(probability_rows), "Lower"),
        metric_row("Probability", "Win log loss", log_loss(probability_rows$actual_winner, probability_rows$base_win_probability), log_loss(probability_rows$actual_winner, probability_rows$adjusted_win_probability), n_distinct(probability_rows$season, probability_rows$round), nrow(probability_rows), "Lower"),
        metric_row("Probability", "Winner top-pick hit rate", win_hits[["base"]], win_hits[["adjusted"]], n_distinct(probability_rows$season, probability_rows$round), nrow(probability_rows), "Higher"),
        metric_row("Probability", "Podium Brier", brier_score(probability_rows$actual_podium, probability_rows$base_podium_probability), brier_score(probability_rows$actual_podium, probability_rows$adjusted_podium_probability), n_distinct(probability_rows$season, probability_rows$round), nrow(probability_rows), "Lower"),
        metric_row("Probability", "Podium top-pick hit rate", podium_hits[["base"]], podium_hits[["adjusted"]], n_distinct(probability_rows$season, probability_rows$round), nrow(probability_rows), "Higher"),
        roi_metric_rows(probability_rows, "Probability", "Winner", "base_win_rank", "adjusted_win_rank", "actual_winner", "win_odds", 1L),
        roi_metric_rows(probability_rows, "Probability", "Podium", "base_podium_rank", "adjusted_podium_rank", "actual_podium", "podium_odds", 3L)
      ))
    }

    if (nrow(points_rows) > 0) {
      points_hits <- top_pick_hit(points_rows, "base_points_rank", "adjusted_points_rank", "actual_winner")
      summary_rows <- append(summary_rows, list(
        metric_row("Points", "MAE", mean(abs(points_rows$base_predicted_points - points_rows$points), na.rm = TRUE), mean(abs(points_rows$adjusted_predicted_points - points_rows$points), na.rm = TRUE), n_distinct(points_rows$season, points_rows$round), nrow(points_rows), "Lower"),
        metric_row("Points", "Winner top-pick hit rate", points_hits[["base"]], points_hits[["adjusted"]], n_distinct(points_rows$season, points_rows$round), nrow(points_rows), "Higher"),
        roi_metric_rows(points_rows, "Points", "Winner", "base_points_rank", "adjusted_points_rank", "actual_winner", "win_odds", 1L)
      ))
    }

    if (nrow(winner_without_rows) > 0) {
      without_hits <- top_pick_hit(winner_without_rows, "base_winner_without_rank", "adjusted_winner_without_rank", "actual_winner_without")
      summary_rows <- append(summary_rows, list(
        metric_row("Winner without", "Brier", brier_score(winner_without_rows$actual_winner_without, winner_without_rows$base_winner_without_probability), brier_score(winner_without_rows$actual_winner_without, winner_without_rows$adjusted_winner_without_probability), n_distinct(winner_without_rows$season, winner_without_rows$round), nrow(winner_without_rows), "Lower"),
        metric_row("Winner without", "Top-pick hit rate", without_hits[["base"]], without_hits[["adjusted"]], n_distinct(winner_without_rows$season, winner_without_rows$round), nrow(winner_without_rows), "Higher")
      ))
    }

    validate(need(length(summary_rows) > 0, "No completed chatter overlay rows found for historical impact checks."))

    bind_rows(summary_rows) %>%
      mutate(
        Extra = if ("Extra" %in% names(.)) coalesce(Extra, "") else "",
        Improved = case_when(
          Direction == "Lower" & Change < 0 ~ "Yes",
          Direction == "Higher" & Change > 0 ~ "Yes",
          abs(Change) < 1e-12 ~ "Flat",
          TRUE ~ "No"
        )
      ) %>%
      transmute(
        Family,
        Metric,
        Base = if_else(str_detect(Metric, "hit rate|ROI"), format_pct(Base, 0.1), format_num(Base, 4)),
        Adjusted = if_else(str_detect(Metric, "hit rate|ROI"), format_pct(Adjusted, 0.1), format_num(Adjusted, 4)),
        Change = if_else(str_detect(Metric, "hit rate|ROI"), format_pct(Change, 0.1), format_num(Change, 4)),
        Improved,
        Races,
        Rows
      )
  }

  output$chatter_history_training_table <- renderTable({
    build_chatter_history_table(function(rows) rows %>% filter(!is.na(season), season <= 2024L))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_history_results_table <- renderTable({
    build_chatter_history_table(function(rows) rows %>% filter(season %in% c(2025L, 2026L)))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_coefficients_table <- renderTable({
    validate(need(nrow(chatter_coefficients) > 0, "Run Stage 18 chatter overlay to create fitted coefficients."))
    chatter_coefficients %>%
      transmute(
        Family = overlay_family,
        Target = target,
        Type = coefficient_type,
        Beta = format_num(beta, 4),
        `Train through` = train_end_season,
        `Max nudge` = format_num(max_nudge, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_projection_table <- renderTable({
    req(input$chatter_projection)

    if (input$chatter_projection == "finish") {
      rows <- selected_chatter_finish()
      validate(need(nrow(rows) > 0, "No finish chatter overlay rows found for this race."))
      return(rows %>%
        add_prerace_display() %>%
        arrange(adjusted_finish_rank) %>%
        transmute(
          Rank = adjusted_finish_rank,
          `Base rank` = base_finish_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Start = display_start_position_label,
          Quali = display_quali_position_label,
          `Q delta` = display_quali_delta_label,
          `Chatter signal` = format_num(race_centered_chatter, 3),
          `Base finish` = format_num(base_predicted_finish, 2),
          `Finish nudge` = format_num(chatter_finish_nudge, 2),
          `Adjusted finish` = format_num(adjusted_predicted_finish, 2),
          `Actual finish` = format_int(finish_position)
        ))
    }

    if (input$chatter_projection == "probability") {
      rows <- selected_chatter_probability()
      validate(need(nrow(rows) > 0, "No probability chatter overlay rows found for this race."))
      return(rows %>%
        add_prerace_display() %>%
        arrange(adjusted_win_rank) %>%
        transmute(
          `Win rank` = adjusted_win_rank,
          `Base win rank` = base_win_rank,
          `Podium rank` = adjusted_podium_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Start = display_start_position_label,
          Quali = display_quali_position_label,
          `Q delta` = display_quali_delta_label,
          `Chatter signal` = format_num(race_centered_chatter, 3),
          `Base win` = format_pct(base_win_probability, 0.1),
          `Adj win` = format_pct(adjusted_win_probability, 0.1),
          `Win nudge` = format_pct(adjusted_win_probability - base_win_probability, 0.1),
          `Opening ML` = opening_win_avg_american_odds_label,
          `Post-chatter ML` = win_current_american_odds_label,
          `Post-grid ML` = grid_win_avg_american_odds_label,
          `Model chatter ML` = expected_win_american_odds_label,
          `Expected chatter Delta` = expected_win_moneyline_delta_label,
          `Actual chatter Delta` = actual_win_moneyline_delta_label,
          `Actual grid Delta` = grid_win_moneyline_delta_label,
          `Grid move` = grid_vs_chatter_moneyline_delta_label,
          `Base podium` = format_pct(base_podium_probability, 0.1),
          `Adj podium` = format_pct(adjusted_podium_probability, 0.1),
          `Podium nudge` = format_pct(adjusted_podium_probability - base_podium_probability, 0.1),
          `Actual finish` = format_int(finish_position)
        ))
    }

    if (input$chatter_projection == "points") {
      rows <- selected_chatter_points()
      validate(need(nrow(rows) > 0, "No points chatter overlay rows found for this race."))
      return(rows %>%
        add_prerace_display() %>%
        arrange(adjusted_points_rank) %>%
        transmute(
          Rank = adjusted_points_rank,
          `Base rank` = base_points_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Start = display_start_position_label,
          Quali = display_quali_position_label,
          `Q delta` = display_quali_delta_label,
          `Chatter signal` = format_num(race_centered_chatter, 3),
          `Base points` = format_num(base_predicted_points, 2),
          `Points nudge` = format_num(chatter_points_nudge, 2),
          `Adjusted points` = format_num(adjusted_predicted_points, 2),
          `Actual points` = format_num(points, 1),
          `Actual finish` = format_int(finish_position)
        ))
    }

    if (input$chatter_projection == "winner_without") {
      rows <- selected_chatter_winner_without()
      validate(need(nrow(rows) > 0, "No winner-without chatter overlay rows found for this race."))
      return(rows %>%
        add_prerace_display() %>%
        arrange(adjusted_winner_without_rank) %>%
        transmute(
          Rank = adjusted_winner_without_rank,
          `Base rank` = base_winner_without_rank,
          Driver = driver_name,
          Constructor = constructor_name,
          Start = display_start_position_label,
          Quali = display_quali_position_label,
          `Q delta` = display_quali_delta_label,
          `Chatter signal` = format_num(race_centered_chatter, 3),
          `Base probability` = format_pct(base_winner_without_probability, 0.1),
          `Adjusted probability` = format_pct(adjusted_winner_without_probability, 0.1),
          `Actual finish` = format_int(finish_position),
          Correct = ifelse(actual_winner_without, "Yes", "No")
        ))
    }

    rows <- selected_chatter_allmodel_consensus()
    validate(need(nrow(rows) > 0, "No model-consensus chatter overlay rows found for this race."))

    rows %>%
      add_prerace_display() %>%
      arrange(adjusted_consensus_rank, driver_name) %>%
      transmute(
        `Win rank` = adjusted_consensus_rank,
        `Base win rank` = base_consensus_rank,
        `Podium rank` = adjusted_consensus_podium_rank,
        `Base podium rank` = base_consensus_podium_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Start = display_start_position_label,
        Quali = display_quali_position_label,
        `Q delta` = display_quali_delta_label,
        `Chatter signal` = format_num(race_centered_chatter, 3),
        `Base finish` = format_num(base_predicted_finish, 2),
        `Adj finish` = format_num(adjusted_predicted_finish, 2),
        `Finish nudge` = format_num(chatter_finish_nudge, 2),
        `Base points` = format_num(base_predicted_points, 2),
        `Adj points` = format_num(adjusted_predicted_points, 2),
        `Points nudge` = format_num(chatter_points_nudge, 2),
        `Base win %` = format_pct(base_model_win_probability, 0.1),
        `Adj win %` = format_pct(adjusted_model_win_probability, 0.1),
        `Win nudge` = format_pct(adjusted_model_win_probability - base_model_win_probability, 0.1),
        `Opening ML` = opening_win_avg_american_odds_label,
        `Post-chatter ML` = win_current_american_odds_label,
        `Post-grid ML` = grid_win_avg_american_odds_label,
        `Model chatter ML` = expected_win_american_odds_label,
        `Expected chatter Delta` = expected_win_moneyline_delta_label,
        `Actual chatter Delta` = actual_win_moneyline_delta_label,
        `Actual grid Delta` = grid_win_moneyline_delta_label,
        `Grid move` = grid_vs_chatter_moneyline_delta_label,
        `Base podium %` = format_pct(base_model_podium_probability, 0.1),
        `Adj podium %` = format_pct(adjusted_model_podium_probability, 0.1),
        `Podium nudge` = format_pct(adjusted_model_podium_probability - base_model_podium_probability, 0.1),
        Families = adjusted_family_count,
        `Consensus method` = rep(if ((input$allmodel_consensus_mode %||% "family") == "model") "Selected model weighted" else "Family weighted", n()),
        `Actual finish` = format_int(finish_position)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_quali_race_selector <- renderUI({
    validate(need(nrow(chatter_qualifying_overlay) > 0, "Run Stage 18 chatter overlay to create qualifying chatter predictions."))
    req(input$chatter_quali_season)
    choices <- rf_race_choices %>%
      filter(season == as.integer(input$chatter_quali_season)) %>%
      semi_join(chatter_qualifying_overlay %>% distinct(season, round), by = c("season", "round"))
    selectInput("chatter_quali_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  build_chatter_qualifying_overlay <- function(selected_models) {
    if (length(selected_models) == 0 || nrow(qualifying_predictions) == 0 || nrow(chatter_qualifying_overlay) == 0) {
      return(empty_chatter_qualifying_overlay)
    }

    selected_rows <- qualifying_predictions %>%
      filter(model %in% selected_models)

    if (nrow(selected_rows) == 0) {
      return(empty_chatter_qualifying_overlay)
    }

    chatter_lookup <- chatter_qualifying_overlay %>%
      select(any_of(c(
        "season", "round", "driver_code", "join_team", "Team", "Composite_Chatter_Score",
        "team_race_centered_chatter", "Source_Confidence", "driver_momentum_score",
        "practice_signal_score", "sprint_signal_score", "qualifying_signal_score",
        "context_score", "confidence", "driver_weekend_raw_score", "driver_weekend_notes",
        "driver_weekend_race_average", "driver_weekend_race_centered",
        "driver_weekend_team_average", "driver_weekend_teammate_centered",
        "has_driver_weekend_signal", "raw_overlay_signal", "overlay_signal_average",
        "race_centered_chatter", "chatter_beta"
      ))) %>%
      distinct(season, round, driver_code, .keep_all = TRUE)

    selected_rows %>%
      group_by(
        data_split, season, round, race_date, race_name, circuit_id, circuit_name,
        driver_id, driver_code, driver_name, constructor_id, constructor_name,
        finish_position, current_grid, current_quali_position, current_best_quali_delta_sec,
        actual_quali_position, actual_quali_delta_sec, actual_grid
      ) %>%
      summarise(
        base_predicted_quali_position = mean(predicted_quali_position, na.rm = TRUE),
        base_predicted_quali_delta_sec = mean(predicted_quali_delta_sec, na.rm = TRUE),
        model_count = n_distinct(model),
        .groups = "drop"
      ) %>%
      left_join(chatter_lookup, by = c("season", "round", "driver_code")) %>%
      mutate(
        chatter_beta = coalesce(chatter_beta, 0),
        race_centered_chatter = coalesce(race_centered_chatter, 0),
        chatter_quali_nudge = pmax(pmin(chatter_beta * race_centered_chatter, 2.25), -2.25),
        adjusted_predicted_quali_position = base_predicted_quali_position - chatter_quali_nudge
      ) %>%
      group_by(season, round, race_name) %>%
      arrange(base_predicted_quali_position, driver_name, .by_group = TRUE) %>%
      mutate(base_quali_rank = row_number()) %>%
      arrange(adjusted_predicted_quali_position, driver_name, .by_group = TRUE) %>%
      mutate(adjusted_quali_rank = row_number()) %>%
      ungroup() %>%
      mutate(
        base_predicted_pole = base_quali_rank == 1,
        adjusted_predicted_pole = adjusted_quali_rank == 1,
        actual_pole_bool = !is.na(actual_quali_position) & actual_quali_position == 1,
        base_pole_pick_correct = if_else(!is.na(actual_quali_position), base_predicted_pole & actual_pole_bool, NA),
        adjusted_pole_pick_correct = if_else(!is.na(actual_quali_position), adjusted_predicted_pole & actual_pole_bool, NA),
        base_predicted_top3_quali = base_quali_rank <= 3,
        adjusted_predicted_top3_quali = adjusted_quali_rank <= 3,
        actual_top3_quali_bool = !is.na(actual_quali_position) & actual_quali_position <= 3,
        base_top3_quali_pick_correct = if_else(!is.na(actual_quali_position), base_predicted_top3_quali & actual_top3_quali_bool, NA),
        adjusted_top3_quali_pick_correct = if_else(!is.na(actual_quali_position), adjusted_predicted_top3_quali & actual_top3_quali_bool, NA)
      )
  }

  selected_chatter_qualifying_all <- reactive({
    validate(need(nrow(chatter_qualifying_overlay) > 0, "Run Stage 18 chatter overlay to create qualifying chatter predictions."))
    selected_models <- input$chatter_quali_models
    validate(need(length(selected_models) > 0, "Select at least one qualifying model."))
    rows <- build_chatter_qualifying_overlay(selected_models)
    validate(need(nrow(rows) > 0, "No qualifying chatter overlay rows found for this model selection."))
    rows
  })

  selected_chatter_qualifying <- reactive({
    req(input$chatter_quali_season, input$chatter_quali_round)
    selected_chatter_qualifying_all() %>%
      filter(season == as.integer(input$chatter_quali_season), round == as.integer(input$chatter_quali_round)) %>%
      add_expected_pole_moneyline_impact()
  })

  output$chatter_quali_header <- renderUI({
    rows <- selected_chatter_qualifying()
    validate(need(nrow(rows) > 0, "No qualifying chatter overlay rows found for this race."))
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p("Qualifying consensus adjusted by race-centered pre-race chatter.")
      )
    )
  })

  output$chatter_quali_history_table <- renderTable({
    validate(need(nrow(chatter_qualifying_overlay) > 0, "Run Stage 18 chatter overlay to create qualifying chatter predictions."))
    rows <- selected_chatter_qualifying_all() %>% filter(!is.na(actual_quali_position))
    training_rows <- chatter_qualifying_overlay %>%
      filter(!is.na(actual_quali_position), !is.na(season), season <= 2024L)
    validate(need(nrow(rows) > 0 || nrow(training_rows) > 0, "No completed qualifying chatter overlay rows found."))

    scalar_or_na <- function(x) {
      if (length(x) == 0 || all(is.na(x))) NA_real_ else as.numeric(x[[1]])
    }
    top_hit <- function(data, rank_col, actual_col) {
      data %>%
        filter(!is.na(.data[[rank_col]]), !is.na(.data[[actual_col]])) %>%
        group_by(season, round) %>%
        arrange(.data[[rank_col]], driver_name, .by_group = TRUE) %>%
        slice(1) %>%
        ungroup() %>%
        summarise(value = mean(as.logical(.data[[actual_col]]), na.rm = TRUE)) %>%
        pull(value) %>%
        scalar_or_na()
    }

    top3_hit <- function(data, flag_col, actual_col) {
      data %>%
        filter(.data[[flag_col]], !is.na(.data[[actual_col]])) %>%
        summarise(value = mean(as.logical(.data[[actual_col]]), na.rm = TRUE)) %>%
        pull(value) %>%
        scalar_or_na()
    }

    summarise_scope <- function(data, sample_label) {
      if (nrow(data) == 0) {
        return(tibble())
      }

      tibble(
        Sample = sample_label,
        Metric = c("Qualifying MAE", "Pole top-pick hit rate", "Top-3 qualifying pick hit rate"),
        Base = c(
          mean(abs(data$base_predicted_quali_position - data$actual_quali_position), na.rm = TRUE),
          top_hit(data, "base_quali_rank", "actual_pole_bool"),
          top3_hit(data, "base_predicted_top3_quali", "actual_top3_quali_bool")
        ),
        Adjusted = c(
          mean(abs(data$adjusted_predicted_quali_position - data$actual_quali_position), na.rm = TRUE),
          top_hit(data, "adjusted_quali_rank", "actual_pole_bool"),
          top3_hit(data, "adjusted_predicted_top3_quali", "actual_top3_quali_bool")
        ),
        Direction = c("Lower", "Higher", "Higher"),
        Races = n_distinct(data$season, data$round),
        Rows = nrow(data)
      )
    }

    summary <- bind_rows(
      summarise_scope(training_rows, "Training through 2024"),
      summarise_scope(rows %>% filter(season == 2025L), "2025"),
      summarise_scope(rows %>% filter(season == 2026L), "2026")
    ) %>%
      mutate(
        Change = Adjusted - Base,
        Improved = case_when(
          Direction == "Lower" & Change < 0 ~ "Yes",
          Direction == "Higher" & Change > 0 ~ "Yes",
          abs(Change) < 1e-12 ~ "Flat",
          TRUE ~ "No"
        )
      )

    summary %>%
      transmute(
        Sample,
        Metric,
        Base = if_else(str_detect(Metric, "hit rate"), format_pct(Base, 0.1), format_num(Base, 4)),
        Adjusted = if_else(str_detect(Metric, "hit rate"), format_pct(Adjusted, 0.1), format_num(Adjusted, 4)),
        Change = if_else(str_detect(Metric, "hit rate"), format_pct(Change, 0.1), format_num(Change, 4)),
        Improved,
        Races,
        Rows
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_quali_pole_table <- renderTable({
    selected_chatter_qualifying() %>%
      filter(adjusted_quali_rank == 1) %>%
      transmute(
        Pick = driver_name,
        Constructor = constructor_name,
        `Chatter signal` = format_num(race_centered_chatter, 3),
        `Base rank` = base_quali_rank,
        `Adjusted rank` = adjusted_quali_rank,
        `Base quali` = format_num(base_predicted_quali_position, 2),
        `Quali nudge` = format_num(chatter_quali_nudge, 2),
        `Adjusted quali` = format_num(adjusted_predicted_quali_position, 2),
        `Current pole ML` = pole_current_american_odds_label,
        `Implied pole ML` = qualifying_implied_pole_american_odds_label,
        `Chatter pole ML` = expected_pole_american_odds_label,
        `Exp ML Delta` = expected_pole_moneyline_delta_label,
        `Actual ML Delta` = actual_pole_moneyline_delta_label,
        `Model adj ML` = pole_model_implied_adjusted_american_odds_label,
        `Actual quali` = format_int(actual_quali_position),
        Correct = ifelse(adjusted_pole_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$chatter_quali_prediction_table <- renderTable({
    selected_chatter_qualifying() %>%
      add_prerace_display() %>%
      arrange(adjusted_quali_rank, driver_name) %>%
      transmute(
        `Base grid` = format_predicted_position(base_quali_rank),
        `Adj grid` = format_predicted_position(adjusted_quali_rank),
        Driver = driver_name,
        Constructor = constructor_name,
        Start = display_start_position_label,
        Quali = display_quali_position_label,
        `Chatter signal` = format_num(race_centered_chatter, 3),
        `Base quali` = format_num(base_predicted_quali_position, 2),
        `Quali nudge` = format_num(chatter_quali_nudge, 2),
        `Adjusted quali` = format_num(adjusted_predicted_quali_position, 2),
        `Current pole ML` = pole_current_american_odds_label,
        `Implied pole ML` = qualifying_implied_pole_american_odds_label,
        `Chatter pole ML` = expected_pole_american_odds_label,
        `Exp ML Delta` = expected_pole_moneyline_delta_label,
        `Actual ML Delta` = actual_pole_moneyline_delta_label,
        `Model adj ML` = pole_model_implied_adjusted_american_odds_label,
        `Actual quali` = format_int(actual_quali_position),
        `Actual grid` = format_int(actual_grid)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)
  output$routed_race_selector <- renderUI({
    validate(need(nrow(routed_specialist_model_lookup) > 0, "Run specialist XGBoost models to create routed specialist predictions."))
    req(input$routed_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$routed_season))
    selectInput("routed_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  observeEvent(input$routed_use_defaults, {
    if (isTRUE(input$routed_use_defaults)) {
      updateCheckboxGroupInput(
        session,
        "routed_models",
        selected = default_routed_specialist_models_for_race(input$routed_season, input$routed_round)
      )
      updateCheckboxInput(session, "routed_use_defaults", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$routed_season, input$routed_round), {
    req(input$routed_season, input$routed_round)
    updateCheckboxGroupInput(
      session,
      "routed_models",
      selected = default_routed_specialist_models_for_race(input$routed_season, input$routed_round)
    )
  }, ignoreInit = FALSE)

  observeEvent(input$routed_select_all, {
    if (isTRUE(input$routed_select_all)) {
      updateCheckboxGroupInput(session, "routed_models", selected = routed_specialist_model_lookup$model)
      updateCheckboxInput(session, "routed_select_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$routed_clear_all, {
    if (isTRUE(input$routed_clear_all)) {
      updateCheckboxGroupInput(session, "routed_models", selected = character(0))
      updateCheckboxInput(session, "routed_clear_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  validate_routed_model_selection <- function() {
    validate(need(length(input$routed_models) > 0, "Select at least one routed specialist model."))
  }

  routed_specialist_predictions <- reactive({
    req(input$routed_season, input$routed_round)
    validate_routed_model_selection()
    rows <- build_routed_specialist_family_ranks(input$routed_season, input$routed_round, input$routed_models)
    validate(need(nrow(rows) > 0, "No routed specialist predictions found for this race and model selection."))
    build_family_predictions_from_ranks(rows, input$routed_use_estimated_podium_odds)
  })

  routed_specialist_consensus_bets <- reactive({
    req(input$routed_season, input$routed_round)
    validate_routed_model_selection()
    rows <- build_routed_specialist_family_ranks(input$routed_season, input$routed_round, input$routed_models)
    validate(need(nrow(rows) > 0, "No routed specialist predictions found for this race and model selection."))
    win_edge <- if (isTRUE(input$routed_use_edge_filter)) input$routed_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$routed_use_edge_filter)) input$routed_min_podium_edge_pct else -100
    build_family_bets_from_ranks(rows, input$routed_force_consensus, input$routed_use_estimated_podium_odds, input$routed_podium_favorite_limit, win_edge, podium_edge)
  })

  output$routed_model_header <- renderUI({
    rows <- routed_specialist_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    routed_types <- routed_track_type_label(race$season[[1]], race$round[[1]])
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p("Specialist models are only included on races whose track flags match the selected specialist type."), p("This race routes to: ", strong(routed_types), ".")))
  })

  output$routed_betting_season_summary <- renderTable({
    req(input$routed_roi_start_season, input$routed_roi_end_season)
    validate_routed_model_selection()
    bounds <- roi_window_bounds(input$routed_roi_start_season, input$routed_roi_end_season)
    rows <- bind_rows(lapply(seq(bounds[["start"]], bounds[["end"]]), function(season_value) {
      build_routed_specialist_family_ranks(season_value, NULL, input$routed_models)
    })) %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]])
    validate(need(nrow(rows) > 0, "No routed specialist model rows found for this season window."))
    win_edge <- if (isTRUE(input$routed_use_edge_filter)) input$routed_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$routed_use_edge_filter)) input$routed_min_podium_edge_pct else -100
    summary_rows <- summarise_consensus_bets_window(
      build_family_bets_from_ranks(rows, input$routed_force_consensus, input$routed_use_estimated_podium_odds, input$routed_podium_favorite_limit, win_edge, podium_edge),
      bounds[["start"]],
      bounds[["end"]]
    )
    validate(need(nrow(summary_rows) > 0, "No completed routed specialist consensus bets with odds found for this season."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$routed_consensus_bets_table <- renderTable({
    bet_rows <- routed_specialist_consensus_bets()
    validate(need(nrow(bet_rows) > 0, "No routed specialist consensus bets found for this race."))
    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(Market = bet_market, Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name, Odds = odds_american_label, Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"), `Market %` = format_pct(market_no_vig_probability, 1), Edge = format_pct(model_edge, 0.1), Result = bet_status, `Actual finish` = format_int(actual_finish), Stake = format_num(stake, 0), Profit = format_num(profit, 2), ROI = format_pct(roi, 0.1))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$routed_winner_table <- renderTable({
    routed_specialist_predictions() %>%
      filter(consensus_rank == 1) %>%
      transmute(Pick = driver_name, Constructor = constructor_name, `Avg win rank` = format_num(winner_rank_score, 2), Families = family_count, `Win odds` = win_avg_american_odds_label, `Actual finish` = format_int(finish_position), Correct = ifelse(winner_pick_correct, "Yes", "No"))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$routed_prediction_table <- renderTable({
    routed_specialist_predictions() %>%
      add_prerace_display() %>%
      arrange(consensus_rank) %>%
      transmute(`Win rank` = consensus_rank, `Podium rank` = consensus_podium_rank, Driver = driver_name, Constructor = constructor_name, Start = display_start_position_label, Quali = display_quali_position_label, `Q delta` = display_quali_delta_label, `Avg win rank` = format_num(winner_rank_score, 2), `Avg podium rank` = format_num(podium_rank_score, 2), Families = family_count, `Win odds` = win_avg_american_odds_label, `Podium odds` = podium_display_american_odds_label, `Win edge` = format_pct(model_win_probability - win_market_no_vig_probability, 0.1), `Podium edge` = format_pct(model_podium_probability - podium_display_no_vig_probability, 0.1), `Actual finish` = format_int(finish_position), `Actual rank` = actual_rank_in_race)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  build_allmodel_family_consensus_ranks <- function(season_value, round_value = NULL, selection = NULL) {
    family_rows <- list()
    season_value <- as.integer(season_value)
    consensus_mode <- consensus_selection_value(selection, "consensus_mode", input$allmodel_consensus_mode %||% "family")
    use_xgb_finish <- isTRUE(consensus_selection_value(selection, "use_xgb_finish", input$allmodel_use_xgb_finish))
    use_xgb_probability <- isTRUE(consensus_selection_value(selection, "use_xgb_probability", input$allmodel_use_xgb_probability))
    use_xgb_points <- isTRUE(consensus_selection_value(selection, "use_xgb_points", input$allmodel_use_xgb_points))
    use_routed_specialists <- isTRUE(consensus_selection_value(selection, "use_routed_specialists", input$allmodel_use_routed_specialists))
    selected_finish_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_finish_models", input$xgb_models), xgb_finish_default_models)
    selected_probability_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_probability_models", input$xgb_prob_models), xgb_probability_default_models)
    selected_points_models <- selected_or_default_models(consensus_selection_value(selection, "xgb_points_models", input$xgb_points_models), xgb_points_default_models)

    if (use_xgb_finish) {
      rows <- xgb_finish_predictions %>%
        filter(season == season_value, model %in% selected_finish_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$xgb_finish <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_finish_family, "Finish model")
      } else {
        rank_finish_family(rows, "Finish model")
      }
    }

    if (use_xgb_probability) {
      rows <- xgb_probability_predictions %>%
        filter(season == season_value, model %in% selected_probability_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$xgb_probability <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_probability_family, "Probability model")
      } else {
        rank_probability_family(rows, "Probability model")
      }
    }

    if (use_xgb_points) {
      rows <- xgb_points_predictions %>%
        filter(season == season_value, model %in% selected_points_models)
      if (!is.null(round_value)) rows <- rows %>% filter(round == as.integer(round_value))
      family_rows$xgb_points <- if (identical(consensus_mode, "model")) {
        rank_each_selected_model(rows, rank_points_family, "Points model")
      } else {
        rank_points_family(rows, "Points model")
      }
    }

    if (use_routed_specialists) {
      routed_models <- consensus_routed_models_for_race(season_value, round_value)
      family_rows$routed_specialists <- if (identical(consensus_mode, "model")) {
        build_routed_specialist_model_ranks(
          season_value,
          round_value,
          routed_models
        )
      } else {
        build_routed_specialist_family_ranks(
          season_value,
          round_value,
          routed_models
        )
      }
    }

    bind_rows(family_rows)
  }

  allmodel_empty_message <- function(season_value, round_value = NULL) {
    standard_family_selected <- any(c(
      isTRUE(input$allmodel_use_xgb_finish),
      isTRUE(input$allmodel_use_xgb_probability),
      isTRUE(input$allmodel_use_xgb_points)
    ))

    if (!standard_family_selected && isTRUE(input$allmodel_use_routed_specialists)) {
      routed_models <- consensus_routed_models_for_race(season_value, round_value)
      if (length(routed_models) == 0) {
        return("No routed specialist models match this race's track profile.")
      }
      return("No routed specialist predictions were found for this race.")
    }

    "Select at least one model family."
  }

  build_allmodel_predictions_from_family_rows <- function(rows, use_estimated_podium_odds) {
    if (nrow(rows) == 0) return(tibble())

    rows %>%
      group_by(season, round, race_date, race_name, driver_id, driver_code, driver_name, constructor_name, finish_position, actual_rank_in_race, actual_winner, actual_podium) %>%
      summarise(
        winner_rank_score = mean(winner_family_rank, na.rm = TRUE),
        podium_rank_score = mean(podium_family_rank, na.rm = TRUE),
        model_win_probability = if (all(is.na(winner_probability_score))) NA_real_ else mean(winner_probability_score, na.rm = TRUE),
        model_podium_probability = if (all(is.na(podium_probability_score))) NA_real_ else mean(podium_probability_score, na.rm = TRUE),
        family_count = n_distinct(family),
        family_list = paste(sort(unique(family)), collapse = " / "),
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(winner_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(consensus_rank = row_number(), predicted_winner = consensus_rank == 1) %>%
      arrange(podium_rank_score, driver_name, .by_group = TRUE) %>%
      mutate(
        consensus_podium_rank = row_number(),
        predicted_podium = consensus_podium_rank <= 3,
        field_size = n(),
        rank_weight = pmax(field_size - consensus_rank + 1, 0),
        rank_win_probability = rank_weight / sum(rank_weight, na.rm = TRUE),
        rank_podium_probability = pmax(0, (4 - consensus_podium_rank) / 3),
        model_win_probability = coalesce(model_win_probability, rank_win_probability),
        model_podium_probability = coalesce(model_podium_probability, rank_podium_probability),
        winner_pick_correct = predicted_winner & actual_winner,
        podium_pick_correct = predicted_podium & actual_podium
      ) %>%
      ungroup() %>%
      add_allrf_odds(use_estimated_podium_odds)
  }

  output$allmodel_race_selector <- renderUI({
    req(input$allmodel_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$allmodel_season))
    selectInput("allmodel_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  allmodel_predictions <- reactive({
    req(input$allmodel_season, input$allmodel_round)
    rows <- build_allmodel_family_consensus_ranks(input$allmodel_season, input$allmodel_round)
    validate(need(nrow(rows) > 0, allmodel_empty_message(input$allmodel_season, input$allmodel_round)))
    build_allmodel_predictions_from_family_rows(rows, input$allmodel_use_estimated_podium_odds)
  })

  build_allmodel_bets_from_family_rows <- function(
    family_rows,
    force_consensus,
    use_estimated_podium_odds,
    podium_favorite_limit = 350,
    min_win_edge_pct = -100,
    min_podium_edge_pct = -100
  ) {
    if (nrow(family_rows) == 0) return(empty_allrf_bets())

    rows <- build_allmodel_predictions_from_family_rows(family_rows, use_estimated_podium_odds)
    if (nrow(rows) == 0) return(empty_allrf_bets())

    if (isTRUE(force_consensus)) {
      family_counts <- family_rows %>%
        distinct(season, round, family) %>%
        count(season, round, name = "required_family_count") %>%
        filter(required_family_count >= 2)

      winner_keys <- family_rows %>%
        filter(winner_family_rank == 1) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      podium_keys <- family_rows %>%
        filter(podium_family_rank <= 3) %>%
        distinct(season, round, driver_code, family) %>%
        count(season, round, driver_code, name = "agreeing_family_count") %>%
        left_join(family_counts, by = c("season", "round")) %>%
        filter(agreeing_family_count == required_family_count) %>%
        select(season, round, driver_code)

      winner_rows <- rows %>% semi_join(winner_keys, by = c("season", "round", "driver_code"))
      podium_rows <- rows %>% semi_join(podium_keys, by = c("season", "round", "driver_code"))
    } else {
      winner_rows <- rows %>% filter(consensus_rank == 1)
      podium_rows <- rows %>% filter(consensus_podium_rank <= 3)
    }

    winner_bets <- winner_rows %>%
      transmute(
        season, round, race_date, race_name,
        bet_market = "win",
        consensus_rank,
        driver_code, driver_name, constructor_name,
        actual_finish = finish_position,
        bet_won = actual_winner,
        odds_american_label = win_avg_american_odds_label,
        market_no_vig_probability = win_market_no_vig_probability,
        model_edge = model_win_probability - market_no_vig_probability,
        odds_source = if_else(!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != "", "market", "missing")
      ) %>%
      filter(model_edge_allowed(model_edge, min_win_edge_pct))

    podium_bets <- podium_rows %>%
      transmute(
        season, round, race_date, race_name,
        bet_market = "podium",
        consensus_rank = consensus_podium_rank,
        driver_code, driver_name, constructor_name,
        actual_finish = finish_position,
        bet_won = actual_podium,
        odds_american_label = podium_display_american_odds_label,
        market_no_vig_probability = podium_display_no_vig_probability,
        model_edge = model_podium_probability - market_no_vig_probability,
        odds_source = podium_display_odds_source
      ) %>%
      filter(podium_odds_allowed(odds_american_label, podium_favorite_limit)) %>%
      filter(model_edge_allowed(model_edge, min_podium_edge_pct))

    bind_rows(winner_bets, podium_bets) %>%
      { if (nrow(.) == 0) empty_allrf_bets() else . } %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_american_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_),
        roi = if_else(stake > 0, profit / stake, NA_real_),
        bet_status = case_when(
          is.na(actual_finish) ~ "No result",
          is.na(odds_decimal) ~ "No odds",
          bet_won ~ "Won",
          TRUE ~ "Lost"
        )
      )
  }

  allmodel_consensus_bets <- reactive({
    req(input$allmodel_season, input$allmodel_round)
    rows <- build_allmodel_family_consensus_ranks(input$allmodel_season, input$allmodel_round)
    validate(need(nrow(rows) > 0, allmodel_empty_message(input$allmodel_season, input$allmodel_round)))
    win_edge <- if (isTRUE(input$allmodel_use_edge_filter)) input$allmodel_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$allmodel_use_edge_filter)) input$allmodel_min_podium_edge_pct else -100
    build_allmodel_bets_from_family_rows(
      rows,
      input$allmodel_force_consensus,
      input$allmodel_use_estimated_podium_odds,
      input$allmodel_podium_favorite_limit,
      win_edge,
      podium_edge
    )
  })

  output$allmodel_model_header <- renderUI({
    rows <- allmodel_predictions()
    race <- rows %>% distinct(season, round, race_name) %>% dplyr::slice(1)
    mode <- if (isTRUE(input$allmodel_force_consensus)) {
      if (identical(input$allmodel_consensus_mode %||% "family", "model")) {
        "Forced consensus only bets when all selected model rankings agree."
      } else {
        "Forced consensus only bets when all selected families agree."
      }
    } else if (identical(input$allmodel_consensus_mode %||% "family", "model")) {
      "Consensus averages each selected model ranking directly."
    } else {
      "Consensus averages selected family ranks across selected model families."
    }
    div(class = "event-header", div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name), p(mode)))
  })

  output$allmodel_betting_season_summary <- renderTable({
    req(input$allmodel_roi_start_season, input$allmodel_roi_end_season)
    bounds <- roi_window_bounds(input$allmodel_roi_start_season, input$allmodel_roi_end_season)
    rows <- bind_rows(lapply(seq(bounds[["start"]], bounds[["end"]]), function(season_value) {
      build_allmodel_family_consensus_ranks(season_value, NULL)
    })) %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]])
    validate(need(nrow(rows) > 0, "Select at least one model family."))
    win_edge <- if (isTRUE(input$allmodel_use_edge_filter)) input$allmodel_min_win_edge_pct else -100
    podium_edge <- if (isTRUE(input$allmodel_use_edge_filter)) input$allmodel_min_podium_edge_pct else -100
    summary_rows <- summarise_consensus_bets_window(
      build_allmodel_bets_from_family_rows(
        rows,
        input$allmodel_force_consensus,
        input$allmodel_use_estimated_podium_odds,
        input$allmodel_podium_favorite_limit,
        win_edge,
        podium_edge
      ),
      bounds[["start"]],
      bounds[["end"]]
    )
    validate(need(nrow(summary_rows) > 0, "No completed all-model consensus bets with odds found for this season."))
    render_betting_summary_table(summary_rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allmodel_consensus_bets_table <- renderTable({
    bet_rows <- allmodel_consensus_bets()
    validate(need(nrow(bet_rows) > 0, "No all-model consensus bets found for this race."))
    bet_rows %>%
      mutate(bet_market = recode(bet_market, win = "Winner", podium = "Podium")) %>%
      transmute(
        Market = bet_market,
        Rank = consensus_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Odds = odds_american_label,
        Source = recode(odds_source, estimated_from_win = "Estimated", market = "Market", missing = "Missing", .default = "Market"),
        `Market %` = format_pct(market_no_vig_probability, 1),
        Edge = format_pct(model_edge, 0.1),
        Result = bet_status,
        `Actual finish` = format_int(actual_finish),
        Stake = format_num(stake, 0),
        Profit = format_num(profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allmodel_winner_table <- renderTable({
    allmodel_predictions() %>%
      filter(consensus_rank == 1) %>%
      transmute(
        Pick = driver_name,
        Constructor = constructor_name,
        `Avg win rank` = format_num(winner_rank_score, 2),
        Families = family_count,
        `Win odds` = win_avg_american_odds_label,
        `Actual finish` = format_int(finish_position),
        Correct = ifelse(winner_pick_correct, "Yes", "No")
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$allmodel_prediction_table <- renderTable({
    allmodel_predictions() %>%
      add_prerace_display() %>%
      arrange(consensus_rank) %>%
      transmute(
        `Win rank` = consensus_rank,
        `Podium rank` = consensus_podium_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Start = display_start_position_label,
        Quali = display_quali_position_label,
        `Q delta` = display_quali_delta_label,
        `Avg win rank` = format_num(winner_rank_score, 2),
        `Avg podium rank` = format_num(podium_rank_score, 2),
        `Win probability` = format_pct(model_win_probability, 0.1),
        `Podium probability` = format_pct(model_podium_probability, 0.1),
        Families = family_count,
        `Win odds` = win_avg_american_odds_label,
        `Podium odds` = podium_display_american_odds_label,
        `Win edge` = format_pct(model_win_probability - win_market_no_vig_probability, 0.1),
        `Podium edge` = format_pct(model_podium_probability - podium_display_no_vig_probability, 0.1),
        `Actual finish` = format_int(finish_position),
        `Actual rank` = actual_rank_in_race
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  fantasy_finish_points <- function(position) {
    position <- pmin(pmax(as.integer(round(position)), 1L), 22L)
    points <- c(40, 37, 35, 32, 30, 27, 25, 23, 22, 20, 17, 15, 13, 12, 10, 7, 5, 4, 3, 2, 1, 0)
    points[position]
  }

  expected_race_laps_for_rows <- function(rows, fallback_laps = NULL) {
    if (is.null(fallback_laps)) {
      fallback_laps <- if ("race_laps" %in% names(stage1) && any(!is.na(stage1$race_laps))) {
        median(as.numeric(stage1$race_laps), na.rm = TRUE)
      } else {
        60
      }
    }

    if (!"circuit_id" %in% names(rows)) {
      rows$circuit_id <- NA_character_
    }
    if (!"race_name" %in% names(rows)) {
      rows$race_name <- NA_character_
    }
    if (!"race_laps" %in% names(rows)) {
      rows$race_laps <- NA_real_
    }

    historical_race_laps <- stage1 %>%
      filter(!is.na(race_name), !is.na(circuit_id), !is.na(race_laps), race_laps > 0, !is.na(round)) %>%
      group_by(season, round, race_name, circuit_id) %>%
      summarise(historical_race_laps = max(as.numeric(race_laps), na.rm = TRUE), .groups = "drop")

    same_event_laps <- historical_race_laps %>%
      transmute(season, round, race_name, circuit_id, actual_same_event_laps = historical_race_laps)

    latest_same_race_laps <- historical_race_laps %>%
      group_by(race_name, circuit_id) %>%
      arrange(desc(season), .by_group = TRUE) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(race_name, circuit_id, prior_same_race_laps = historical_race_laps)

    circuit_laps <- historical_race_laps %>%
      group_by(circuit_id) %>%
      summarise(circuit_expected_race_laps = max(historical_race_laps, na.rm = TRUE), .groups = "drop")

    rows %>%
      left_join(same_event_laps, by = c("season", "round", "race_name", "circuit_id")) %>%
      left_join(latest_same_race_laps, by = c("race_name", "circuit_id")) %>%
      left_join(circuit_laps, by = "circuit_id") %>%
      mutate(race_laps = coalesce(actual_same_event_laps, prior_same_race_laps, as.numeric(race_laps), circuit_expected_race_laps, fallback_laps)) %>%
      select(-actual_same_event_laps, -prior_same_race_laps, -circuit_expected_race_laps)
  }

  fantasy_start_bucket <- function(start_position) {
    case_when(
      is.na(start_position) ~ "unknown",
      start_position <= 1 ~ "pole",
      start_position <= 3 ~ "grid_2_3",
      start_position <= 5 ~ "grid_4_5",
      start_position <= 10 ~ "grid_6_10",
      TRUE ~ "grid_11_plus"
    )
  }

  fantasy_finish_bucket <- function(finish_position) {
    case_when(
      is.na(finish_position) ~ "unknown",
      finish_position <= 1.5 ~ "winner",
      finish_position <= 3.5 ~ "podium",
      finish_position <= 5.5 ~ "p4_p5",
      finish_position <= 10.5 ~ "p6_p10",
      TRUE ~ "p11_plus"
    )
  }

  add_fantasy_track_signature <- function(rows) {
    for (flag in family_flags) {
      if (!flag %in% names(rows)) rows[[flag]] <- 0L
    }

    rows %>%
      mutate(across(all_of(family_flags), ~ replace_na(as.integer(.x), 0L))) %>%
      unite("track_signature", all_of(family_flags), sep = "_", remove = FALSE)
  }

  fantasy_recent_driver_form <- function(season_value, round_value, driver_codes, rolling_races) {
    if (length(driver_codes) == 0) return(tibble(driver_code = character(), rolling_points = numeric(), rolling_finish = numeric(), rolling_start = numeric(), projected_classified_points = numeric(), rolling_laps_led = numeric(), rolling_laps_led_share = numeric()))

    cutoff_key <- as.integer(season_value) * 100L + as.integer(round_value)
    history_rows <- stage1
    if ("classified_finish" %in% names(history_rows)) {
      history_rows <- history_rows %>% mutate(dk_classified_flag = as.integer(coalesce(classified_finish, 0L) == 1L))
    } else if ("classified" %in% names(history_rows)) {
      history_rows <- history_rows %>% mutate(dk_classified_flag = as.integer(coalesce(classified, 0L) == 1L))
    } else if ("status" %in% names(history_rows)) {
      history_rows <- history_rows %>% mutate(dk_classified_flag = as.integer(is.na(status) | str_detect(str_to_lower(status), "finished|\\+")))
    } else {
      history_rows <- history_rows %>% mutate(dk_classified_flag = 1L)
    }

    if (!"laps_led" %in% names(history_rows)) {
      history_rows <- history_rows %>% mutate(laps_led = 0)
    }
    if (!"race_laps" %in% names(history_rows)) {
      history_rows <- history_rows %>% mutate(race_laps = NA_real_)
    }

    history_rows %>%
      mutate(
        race_key = season * 100L + round,
        laps_led_share = if_else(!is.na(race_laps) & race_laps > 0, pmax(0, as.numeric(laps_led)) / as.numeric(race_laps), NA_real_)
      ) %>%
      filter(
        driver_code %in% driver_codes,
        race_key < cutoff_key,
        !is.na(finish_position)
      ) %>%
      arrange(driver_code, desc(race_key)) %>%
      group_by(driver_code) %>%
      slice_head(n = rolling_races) %>%
      summarise(
        rolling_points = safe_mean(points),
        rolling_finish = safe_mean(finish_position),
        rolling_start = safe_mean(grid),
        projected_classified_points = mean(coalesce(dk_classified_flag, 0L) == 1L, na.rm = TRUE),
        rolling_laps_led = safe_mean(laps_led),
        rolling_laps_led_share = safe_mean(laps_led_share),
        .groups = "drop"
      )
  }

  fantasy_same_circuit_laps_led_history <- function(season_value, round_value, race_rows) {
    if (nrow(race_rows) == 0 || !"driver_code" %in% names(race_rows)) {
      return(tibble(driver_code = character(), same_circuit_laps_led_share = numeric()))
    }

    history_rows <- stage1
    if (!"laps_led" %in% names(history_rows)) history_rows <- history_rows %>% mutate(laps_led = 0)
    if (!"race_laps" %in% names(history_rows)) history_rows <- history_rows %>% mutate(race_laps = NA_real_)
    if (!"circuit_id" %in% names(history_rows) || !"circuit_id" %in% names(race_rows)) {
      return(tibble(driver_code = unique(race_rows$driver_code), same_circuit_laps_led_share = NA_real_))
    }

    current_circuits <- race_rows %>% distinct(driver_code, circuit_id)
    cutoff_key <- as.integer(season_value) * 100L + as.integer(round_value)

    history_rows %>%
      mutate(
        race_key = season * 100L + round,
        laps_led_share = if_else(!is.na(race_laps) & race_laps > 0, pmax(0, as.numeric(laps_led)) / as.numeric(race_laps), NA_real_)
      ) %>%
      filter(race_key < cutoff_key, !is.na(circuit_id), !is.na(laps_led_share)) %>%
      inner_join(current_circuits, by = c("driver_code", "circuit_id")) %>%
      arrange(driver_code, desc(race_key)) %>%
      group_by(driver_code) %>%
      slice_head(n = 3) %>%
      summarise(same_circuit_laps_led_share = safe_mean(laps_led_share), .groups = "drop") %>%
      right_join(race_rows %>% distinct(driver_code), by = "driver_code")
  }

  estimate_fantasy_laps_led_share <- function(projected_start, projected_finish, recent_share, same_circuit_share) {
    start_pos <- pmax(1, as.numeric(projected_start))
    finish_pos <- pmax(1, as.numeric(projected_finish))
    start_score <- exp(-0.55 * (start_pos - 1))
    finish_score <- exp(-0.38 * (finish_pos - 1))
    historical_score <- pmin(0.8, pmax(0, coalesce(same_circuit_share, recent_share, 0)))
    predicted_winner_bonus <- if_else(finish_pos <= 1.5, 0.45, 0)
    pole_bonus <- if_else(start_pos <= 1.5, 0.35, 0)

    pmax(0, 0.48 * start_score + 0.34 * finish_score + 1.25 * historical_score + predicted_winner_bonus + pole_bonus)
  }

  prediction_prerace_rows <- function(season_value, round_value) {
    season_value <- as.integer(season_value)
    round_value <- as.integer(round_value)

    base_cols <- c("season", "round", "race_date", "race_name", "circuit_id", family_flags, "driver_id", "driver_code", "driver_name", "constructor_id", "constructor_name", "race_laps")
    bind_rows(
      stage1 %>%
        filter(season == season_value, round == round_value) %>%
        select(any_of(base_cols)),
      xgb_points_predictions %>%
        filter(season == season_value, round == round_value) %>%
        select(any_of(base_cols)),
      xgb_finish_predictions %>%
        filter(season == season_value, round == round_value) %>%
        select(any_of(base_cols)),
      qualifying_predictions %>%
        filter(season == season_value, round == round_value) %>%
        select(any_of(base_cols))
    ) %>%
      left_join(
        schedule %>%
          transmute(
            season,
            round,
            schedule_race_name = race_name,
            schedule_circuit_id = circuit_id,
            schedule_race_date = race_date
          ),
        by = c("season", "round")
      ) %>%
      mutate(
        race_name = if ("race_name" %in% names(.)) coalesce(race_name, schedule_race_name) else schedule_race_name,
        circuit_id = if ("circuit_id" %in% names(.)) coalesce(circuit_id, schedule_circuit_id) else schedule_circuit_id,
        race_date = if ("race_date" %in% names(.)) coalesce(race_date, schedule_race_date) else schedule_race_date,
        constructor_id = if ("constructor_id" %in% names(.)) coalesce(constructor_id, constructor_name) else constructor_name
      ) %>%
      select(-schedule_race_name, -schedule_circuit_id, -schedule_race_date) %>%
      expected_race_laps_for_rows() %>%
      distinct(season, round, driver_code, .keep_all = TRUE) %>%
      filter(!is.na(driver_code), driver_code != "")
  }

  fantasy_projection_rows <- function(
    season_value,
    round_value,
    rolling_races,
    use_chatter = FALSE,
    use_rolling = FALSE,
    consensus_rows = NULL
  ) {
    season_value <- as.integer(season_value)
    round_value <- as.integer(round_value)
    fantasy_chatter_weight <- pmin(1, pmax(0, as.numeric(input$fantasy_chatter_strength %||% 50) / 100))

    race_rows <- prediction_prerace_rows(season_value, round_value)

    consensus_lookup <- if (!is.null(consensus_rows) && nrow(consensus_rows) > 0) {
      consensus_rows %>%
        transmute(
          driver_code,
          fantasy_consensus_rank = as.numeric(consensus_rank),
          fantasy_consensus_families = as.character(family_list)
        ) %>%
        distinct(driver_code, .keep_all = TRUE)
    } else {
      tibble(
        driver_code = character(),
        fantasy_consensus_rank = numeric(),
        fantasy_consensus_families = character()
      )
    }

    xgb_point_rows <- if (!isTRUE(use_rolling)) {
      xgb_points_predictions %>%
        filter(
          season == season_value,
          round == round_value,
          model %in% selected_or_default_models(input$xgb_points_models, xgb_points_default_models)
        ) %>%
        transmute(driver_code, projected_points = predicted_points)
    } else {
      tibble(driver_code = character(), projected_points = numeric())
    }

    xgb_point_summary <- if (nrow(xgb_point_rows) == 0 || !"driver_code" %in% names(xgb_point_rows)) {
      tibble(driver_code = character(), model_points = numeric(), model_point_sources = character())
    } else {
      xgb_point_rows %>%
        group_by(driver_code) %>%
        summarise(
          model_points = if (all(is.na(projected_points))) NA_real_ else mean(projected_points, na.rm = TRUE),
          model_point_sources = "XGB",
          .groups = "drop"
      )
    }

    chatter_point_rows <- if (isTRUE(use_chatter) && !isTRUE(use_rolling)) {
      chatter_points_overlay %>%
        filter(season == season_value, round == round_value) %>%
        mutate(
          chatter_points_nudge = if ("chatter_points_nudge" %in% names(.)) chatter_points_nudge else adjusted_predicted_points - base_predicted_points,
          chatter_points_nudge = bounded_centered_chatter_nudge(chatter_points_nudge, 2, fantasy_chatter_weight),
          adjusted_predicted_points = base_predicted_points + chatter_points_nudge
        ) %>%
        transmute(driver_code, chatter_points_nudge, chatter_adjusted_points = adjusted_predicted_points)
    } else {
      tibble(driver_code = character(), chatter_points_nudge = numeric(), chatter_adjusted_points = numeric())
    }

    point_summary <- if (nrow(xgb_point_summary) > 0 && nrow(chatter_point_rows) > 0) {
      xgb_point_summary %>%
        left_join(chatter_point_rows, by = "driver_code") %>%
        mutate(
          model_points = model_points + coalesce(chatter_points_nudge, 0),
          model_point_sources = if_else(!is.na(chatter_points_nudge), "XGB + Chatter", model_point_sources)
        ) %>%
        select(driver_code, model_points, model_point_sources)
    } else if (nrow(xgb_point_summary) > 0) {
      xgb_point_summary
    } else if (nrow(chatter_point_rows) > 0) {
      chatter_point_rows %>%
        transmute(driver_code, model_points = chatter_adjusted_points, model_point_sources = "Chatter")
    } else {
      tibble(driver_code = character(), model_points = numeric(), model_point_sources = character())
    }

    finish_rows <- if (!isTRUE(use_rolling)) {
      xgb_finish_predictions %>%
        filter(
          season == season_value,
          round == round_value,
          model %in% selected_or_default_models(input$xgb_models, xgb_finish_default_models)
        ) %>%
        transmute(driver_code, projected_finish = predicted_finish_position)
    } else {
      tibble(driver_code = character(), projected_finish = numeric())
    }

    finish_summary <- if (nrow(finish_rows) == 0 || !"driver_code" %in% names(finish_rows)) {
      tibble(driver_code = character(), model_finish = numeric())
    } else {
      finish_rows %>%
        group_by(driver_code) %>%
        summarise(
          model_finish = if (all(is.na(projected_finish))) NA_real_ else mean(projected_finish, na.rm = TRUE),
          .groups = "drop"
        )
    }

    chatter_finish_rows <- if (isTRUE(use_chatter) && !isTRUE(use_rolling)) {
      chatter_finish_overlay %>%
        filter(season == season_value, round == round_value) %>%
        mutate(chatter_finish_nudge = bounded_centered_chatter_nudge(chatter_finish_nudge, 1, fantasy_chatter_weight)) %>%
        transmute(driver_code, chatter_finish_nudge)
    } else {
      tibble(driver_code = character(), chatter_finish_nudge = numeric())
    }

    finish_summary <- if (nrow(finish_summary) > 0 && nrow(chatter_finish_rows) > 0) {
      finish_summary %>%
        left_join(chatter_finish_rows, by = "driver_code") %>%
        mutate(model_finish = model_finish - coalesce(chatter_finish_nudge, 0)) %>%
        select(driver_code, model_finish)
    } else {
      finish_summary
    }

    if (nrow(race_rows) == 0 && nrow(point_summary) > 0) {
      race_rows <- point_summary %>%
        mutate(
          season = season_value,
          round = round_value,
          race_date = as.Date(NA),
          race_name = "Selected race",
          driver_id = driver_code,
          driver_name = driver_code,
          constructor_id = "",
          constructor_name = ""
        )
    }

    recent_form <- fantasy_recent_driver_form(
      season_value,
      round_value,
      race_rows$driver_code,
      as.integer(rolling_races)
    )
    laps_led_history <- fantasy_same_circuit_laps_led_history(season_value, round_value, race_rows)

    if (nrow(race_rows) == 0) return(tibble())

    default_race_laps <- if ("race_laps" %in% names(stage1) && any(!is.na(stage1$race_laps))) {
      median(as.numeric(stage1$race_laps), na.rm = TRUE)
    } else {
      60
    }

    race_rows %>%
      left_join(point_summary, by = "driver_code") %>%
      left_join(finish_summary, by = "driver_code") %>%
      left_join(consensus_lookup, by = "driver_code") %>%
      left_join(recent_form, by = "driver_code") %>%
      left_join(laps_led_history, by = "driver_code") %>%
      left_join(
        prerace_display_lookup %>% select(season, round, driver_code, display_start_position, display_quali_position),
        by = c("season", "round", "driver_code")
      ) %>%
      group_by(season, round) %>%
      mutate(
        fallback_rank = rank(coalesce(rolling_points, 0), ties.method = "first"),
        fallback_finish = if_else(!is.na(rolling_finish), rolling_finish, as.numeric(n() - fallback_rank + 1L)),
        base_projected_finish = if (isTRUE(use_rolling)) fallback_finish else coalesce(model_finish, fallback_finish),
        consensus_rank_index = pmin(n(), pmax(1L, as.integer(round(fantasy_consensus_rank)))),
        projected_finish = pmin(
          as.numeric(n()),
          pmax(
            1,
            if_else(
              !is.na(fantasy_consensus_rank),
              sort(base_projected_finish)[consensus_rank_index],
              base_projected_finish
            )
          )
        ),
        projected_start = coalesce(display_start_position, display_quali_position, rolling_start, projected_finish),
        projected_f1_points = if (isTRUE(use_rolling)) coalesce(rolling_points, 0) else coalesce(model_points, rolling_points, 0),
        projection_source = case_when(
          !is.na(fantasy_consensus_rank) ~ paste0(
            if (isTRUE(use_chatter)) "Chatter-adjusted " else "",
            "Model consensus (", coalesce(fantasy_consensus_families, "selected families"), ")"
          ),
          isTRUE(use_rolling) & !is.na(rolling_points) ~ paste0("Rolling ", as.integer(rolling_races), "-race average"),
          !is.na(model_points) ~ if (isTRUE(use_chatter)) paste0(model_point_sources, " + Chatter") else model_point_sources,
          !is.na(rolling_points) ~ paste0("Rolling ", as.integer(rolling_races), "-race average"),
          TRUE ~ "Fallback"
        ),
        start_bucket = fantasy_start_bucket(projected_start),
        finish_bucket = fantasy_finish_bucket(projected_finish)
      ) %>%
      ungroup() %>%
      add_fantasy_track_signature() %>%
      group_by(season, round) %>%
      mutate(
        race_laps_for_projection = coalesce(as.numeric(race_laps), default_race_laps),
        raw_laps_led_score = estimate_fantasy_laps_led_share(
          projected_start,
          projected_finish,
          rolling_laps_led_share,
          same_circuit_laps_led_share
        ),
        projected_laps_led = pmin(race_laps_for_projection, pmax(0, raw_laps_led_score * race_laps_for_projection)),
        target_laps_led = max(coalesce(race_laps_for_projection, default_race_laps), na.rm = TRUE),
        raw_laps_led_total = sum(projected_laps_led, na.rm = TRUE),
        projected_laps_led = if_else(
          is.finite(raw_laps_led_total) & raw_laps_led_total > 0,
          projected_laps_led / raw_laps_led_total * target_laps_led,
          NA_real_
        ),
        fastest_lap_weight = pmax(0.01, exp(-0.18 * min_rank(projected_finish)) * (1 + projected_laps_led / pmax(target_laps_led, 1))),
        projected_fastest_lap_probability = fastest_lap_weight / sum(fastest_lap_weight, na.rm = TRUE),
        dk_finish_points = fantasy_finish_points(projected_finish),
        dk_place_diff = projected_start - projected_finish,
        dk_classified_points = coalesce(projected_classified_points, 1),
        dk_laps_led_points = projected_laps_led * 0.25,
        dk_fastest_lap_points = projected_fastest_lap_probability * 3,
        dk_base_projection = dk_finish_points + dk_place_diff + dk_classified_points + dk_laps_led_points + dk_fastest_lap_points
      ) %>%
      ungroup() %>%
      group_by(season, round, constructor_name) %>%
      mutate(
        constructor_finish_rank = rank(projected_finish, ties.method = "first"),
        dk_teammate_bonus = if_else(n() >= 2L & constructor_finish_rank == 1L, 5, 0),
        fantasy_projection = coalesce(dk_base_projection, 0) + dk_teammate_bonus
      ) %>%
      ungroup() %>%
      select(-target_laps_led, -raw_laps_led_total, -raw_laps_led_score, -fastest_lap_weight, -base_projected_finish, -consensus_rank_index) %>%
      arrange(desc(fantasy_projection), driver_name) %>%
      mutate(
        fantasy_rank = row_number(),
        fallback_salary = pmax(3000, round((12500 - (fantasy_rank - 1) * 425) / 100) * 100)
      ) %>%
      left_join(
        draftkings_driver_salary_lookup,
        by = c("season", "round", "driver_name")
      ) %>%
      mutate(
        mock_salary = coalesce(draftkings_salary, fallback_salary),
        salary_source = if_else(!is.na(draftkings_salary), "DraftKings", "Estimated"),
        value_per_1k = fantasy_projection / mock_salary * 1000
      ) %>%
      select(-fallback_salary)
  }
  fantasy_constructor_rows <- function(driver_rows) {
    if (nrow(driver_rows) == 0) return(tibble())

    driver_rows %>%
      group_by(season, round, race_name, constructor_name) %>%
      summarise(
        driver_count = n(),
        constructor_finish_points = sum(dk_finish_points, na.rm = TRUE),
        constructor_laps_led_points = sum(dk_laps_led_points, na.rm = TRUE),
        constructor_fastest_lap_points = sum(dk_fastest_lap_points, na.rm = TRUE),
        constructor_classified_points = if_else(
          n() >= 2L,
          2 * prod(pmin(1, pmax(0, coalesce(projected_classified_points, 0))), na.rm = TRUE),
          0
        ),
        constructor_double_top10_points = if_else(n() >= 2L && all(projected_finish <= 10, na.rm = TRUE), 5, 0),
        constructor_double_podium_points = if_else(n() >= 2L && all(projected_finish <= 3, na.rm = TRUE), 3, 0),
        projected_finish_total = sum(projected_finish, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        fantasy_projection = constructor_finish_points +
          constructor_laps_led_points +
          constructor_fastest_lap_points +
          constructor_classified_points +
          constructor_double_top10_points +
          constructor_double_podium_points
      ) %>%
      arrange(desc(fantasy_projection), projected_finish_total, constructor_name) %>%
      mutate(
        constructor_rank = row_number(),
        fallback_salary = pmax(4000, round((13000 - (constructor_rank - 1) * 900) / 100) * 100)
      ) %>%
      left_join(
        draftkings_constructor_salary_lookup,
        by = c("season", "round", "constructor_name")
      ) %>%
      mutate(
        mock_salary = coalesce(draftkings_salary, fallback_salary),
        salary_source = if_else(!is.na(draftkings_salary), "DraftKings", "Estimated"),
        value_per_1k = fantasy_projection / mock_salary * 1000
      ) %>%
      select(-fallback_salary)
  }

  optimize_fantasy_lineup <- function(
    driver_rows,
    constructor_rows,
    salary_cap,
    flex_count,
    include_constructor,
    captain_preference = c("any", "high", "low"),
    excluded_captains = character(),
    excluded_drivers = character(),
    required_captains = character(),
    required_drivers = character(),
    required_any_drivers = character(),
    captain_salary_break = NULL,
    constructor_preference = c("any", "high", "low"),
    excluded_constructors = character(),
    required_constructors = character(),
    previous_lineups = tibble(),
    min_major_changes = 0L,
    min_driver_replacements = 0L,
    max_shared_roster_components = Inf
  ) {
    if (nrow(driver_rows) == 0) return(tibble())

    captain_preference <- match.arg(captain_preference)
    constructor_preference <- match.arg(constructor_preference)
    salary_cap <- as.numeric(salary_cap %||% 50000)
    flex_count <- as.integer(flex_count %||% 4)
    driver_pool <- driver_rows %>%
      filter(!driver_name %in% excluded_drivers) %>%
      arrange(desc(fantasy_projection), desc(value_per_1k), driver_name)

    salary_break <- if (!is.null(captain_salary_break) && is.finite(captain_salary_break)) {
      as.numeric(captain_salary_break)
    } else {
      as.numeric(quantile(driver_pool$mock_salary, probs = 0.75, na.rm = TRUE, type = 1))
    }
    eligible_captain_indexes <- which(!driver_pool$driver_name %in% excluded_captains)
    if (length(required_captains) > 0L) {
      eligible_captain_indexes <- eligible_captain_indexes[driver_pool$driver_name[eligible_captain_indexes] %in% required_captains]
    }
    captain_indexes <- eligible_captain_indexes
    if (captain_preference == "high") {
      captain_indexes <- captain_indexes[driver_pool$mock_salary[captain_indexes] >= salary_break]
    } else if (captain_preference == "low") {
      captain_indexes <- captain_indexes[driver_pool$mock_salary[captain_indexes] < salary_break]
    }
    if (length(captain_indexes) == 0 && length(eligible_captain_indexes) > 0) {
      captain_indexes <- eligible_captain_indexes
    }
    if (length(captain_indexes) == 0) return(tibble())

    constructor_pool <- if (isTRUE(include_constructor) && nrow(constructor_rows) > 0) {
      constructor_rows %>%
        filter(!constructor_name %in% excluded_constructors) %>%
        filter(length(required_constructors) == 0L | constructor_name %in% required_constructors) %>%
        arrange(desc(fantasy_projection), desc(value_per_1k), constructor_name)
    } else {
      tibble(constructor_name = "No constructor", fantasy_projection = 0, mock_salary = 0, value_per_1k = 0)
    }
    if (nrow(constructor_pool) == 0) return(tibble())
    constructor_salary_break <- as.numeric(quantile(constructor_pool$mock_salary, probs = 0.5, na.rm = TRUE, type = 1))
    if (constructor_preference == "high") {
      preferred <- constructor_pool %>% filter(mock_salary >= constructor_salary_break)
      if (nrow(preferred) > 0) constructor_pool <- preferred
    } else if (constructor_preference == "low") {
      preferred <- constructor_pool %>% filter(mock_salary < constructor_salary_break)
      if (nrow(preferred) > 0) constructor_pool <- preferred
    }

    lineup_is_diverse <- function(candidate) {
      if (nrow(previous_lineups) == 0 || min_major_changes <= 0L) return(TRUE)
      prior_groups <- if ("Lineup" %in% names(previous_lineups)) split(previous_lineups, previous_lineups$Lineup) else list(previous_lineups)
      candidate_drivers <- candidate %>% filter(Slot %in% c("CPT", "DRV")) %>% pull(Name) %>% unique()
      candidate_captain <- candidate %>% filter(Slot == "CPT") %>% pull(Name) %>% .[1]
      candidate_constructor <- candidate %>% filter(Slot == "CON") %>% pull(Name) %>% .[1]
      all(vapply(prior_groups, function(prior) {
        prior_drivers <- prior %>% filter(Slot %in% c("CPT", "DRV")) %>% pull(Name) %>% unique()
        prior_captain <- prior %>% filter(Slot == "CPT") %>% pull(Name) %>% .[1]
        prior_constructor <- prior %>% filter(Slot == "CON") %>% pull(Name) %>% .[1]
        driver_replacements <- max(length(candidate_drivers), length(prior_drivers)) - length(intersect(candidate_drivers, prior_drivers))
        major_changes <- driver_replacements + as.integer(!identical(candidate_captain, prior_captain)) + as.integer(!identical(candidate_constructor, prior_constructor))
        shared_roster_components <- length(intersect(c(candidate_drivers, candidate_constructor), c(prior_drivers, prior_constructor)))
        driver_replacements >= min_driver_replacements &&
          major_changes >= min_major_changes &&
          shared_roster_components <= max_shared_roster_components
      }, logical(1)))
    }

    best <- NULL
    best_points <- -Inf
    for (captain_index in captain_indexes) {
      captain <- driver_pool[captain_index, ]
      flex_pool <- driver_pool[-captain_index, ]
      if (nrow(flex_pool) < flex_count) next

      combo_matrix <- utils::combn(seq_len(nrow(flex_pool)), flex_count)
      combo_salary <- colSums(matrix(flex_pool$mock_salary[combo_matrix], nrow = flex_count))
      combo_projection <- colSums(matrix(flex_pool$fantasy_projection[combo_matrix], nrow = flex_count), na.rm = TRUE)
      required_driver_present <- if (length(required_drivers) == 0L) {
        rep(TRUE, ncol(combo_matrix))
      } else {
        vapply(seq_len(ncol(combo_matrix)), function(column) {
          selected_names <- c(captain$driver_name[[1]], flex_pool$driver_name[combo_matrix[, column]])
          all(required_drivers %in% selected_names)
        }, logical(1))
      }
      required_any_driver_present <- if (length(required_any_drivers) == 0L) {
        rep(TRUE, ncol(combo_matrix))
      } else {
        vapply(seq_len(ncol(combo_matrix)), function(column) {
          selected_names <- c(captain$driver_name[[1]], flex_pool$driver_name[combo_matrix[, column]])
          any(required_any_drivers %in% selected_names)
        }, logical(1))
      }

      for (constructor_index in seq_len(nrow(constructor_pool))) {
        constructor <- constructor_pool[constructor_index, ]
        total_salary <- captain$mock_salary * 1.5 + constructor$mock_salary + combo_salary
        total_projection <- captain$fantasy_projection * 1.5 + constructor$fantasy_projection + combo_projection
        same_constructor_count <- colSums(matrix(flex_pool$constructor_name[combo_matrix] == constructor$constructor_name, nrow = flex_count), na.rm = TRUE) +
          as.integer(captain$constructor_name == constructor$constructor_name)
        valid <- which(
          total_salary <= salary_cap &
            same_constructor_count <= 1L &
            required_driver_present &
            required_any_driver_present &
            !is.na(total_projection) &
            is.finite(total_projection)
        )
        if (length(valid) == 0) next

        ordered_valid <- valid[order(total_projection[valid], decreasing = TRUE)]
        for (candidate_index in ordered_valid) {
          if (total_projection[[candidate_index]] <= best_points) break
          flex_rows <- flex_pool[combo_matrix[, candidate_index], ]
          candidate <- bind_rows(
            captain %>% transmute(Slot = "CPT", Name = driver_name, Constructor = constructor_name, Salary = mock_salary * 1.5, Projection = fantasy_projection * 1.5, Value = Projection / Salary * 1000),
            flex_rows %>% transmute(Slot = "DRV", Name = driver_name, Constructor = constructor_name, Salary = mock_salary, Projection = fantasy_projection, Value = Projection / Salary * 1000),
            constructor %>% transmute(Slot = "CON", Name = constructor_name, Constructor = constructor_name, Salary = mock_salary, Projection = fantasy_projection, Value = Projection / Salary * 1000)
          ) %>%
            mutate(total_salary = total_salary[[candidate_index]], total_projection = total_projection[[candidate_index]])
          if (lineup_is_diverse(candidate)) {
            best_points <- total_projection[[candidate_index]]
            best <- candidate
            break
          }
        }
      }
    }

    if (is.null(best)) tibble() else best
  }
  output$fastest_lap_race_selector <- renderUI({
    validate(need(nrow(fastest_lap_predictions) > 0, "Run Stage 19 to create fastest-lap predictions."))
    req(input$fastest_lap_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$fastest_lap_season))
    selectInput("fastest_lap_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  observeEvent(list(input$fastest_lap_season, input$fastest_lap_round), {
    req(input$fastest_lap_season, input$fastest_lap_round)
    updateCheckboxGroupInput(
      session, "fastest_lap_models",
      selected = default_fastest_lap_models_for_race(input$fastest_lap_season, input$fastest_lap_round)
    )
  }, ignoreInit = FALSE)

  observeEvent(input$fastest_lap_use_defaults, {
    if (isTRUE(input$fastest_lap_use_defaults)) {
      req(input$fastest_lap_season, input$fastest_lap_round)
      updateCheckboxGroupInput(
        session, "fastest_lap_models",
        selected = default_fastest_lap_models_for_race(input$fastest_lap_season, input$fastest_lap_round)
      )
      updateCheckboxInput(session, "fastest_lap_use_defaults", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fastest_lap_select_all, {
    if (isTRUE(input$fastest_lap_select_all)) {
      updateCheckboxGroupInput(session, "fastest_lap_models", selected = fastest_lap_model_lookup$model)
      updateCheckboxInput(session, "fastest_lap_select_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fastest_lap_clear_all, {
    if (isTRUE(input$fastest_lap_clear_all)) {
      updateCheckboxGroupInput(session, "fastest_lap_models", selected = character(0))
      updateCheckboxInput(session, "fastest_lap_clear_all", value = FALSE)
    }
  }, ignoreInit = TRUE)

  selected_fastest_lap_rows <- reactive({
    req(input$fastest_lap_season, input$fastest_lap_round)
    active_routes <- race_route_types(input$fastest_lap_season, input$fastest_lap_round)
    active_models <- intersect(input$fastest_lap_models %||% character(0), fastest_lap_model_lookup$model)
    validate(need(length(active_models) > 0, "Select at least one routed fastest-lap model."))
    rows <- fastest_lap_predictions %>%
      filter(
        season == as.integer(input$fastest_lap_season),
        round == as.integer(input$fastest_lap_round),
        model %in% active_models,
        routed_type %in% active_routes
      )
    validate(need(nrow(rows) > 0, "No active fastest-lap specialist rows match this race."))
    rows
  })

  fastest_lap_consensus <- reactive({
    selected_fastest_lap_rows() %>%
      group_by(
        season, round, race_date, race_name, driver_id, driver_code, driver_name,
        constructor_name, fastest_lap_sec, fastest_lap_delta_sec, fastest_rank, fastest_lap_winner
      ) %>%
      summarise(
        average_model_rank = mean(predicted_fastest_lap_rank, na.rm = TRUE),
        probability = if (all(is.na(predicted_fastest_lap_probability))) NA_real_ else mean(predicted_fastest_lap_probability, na.rm = TRUE),
        predicted_rank_target = if (all(is.na(predicted_fastest_rank))) NA_real_ else mean(predicted_fastest_rank, na.rm = TRUE),
        predicted_delta_sec = if (all(is.na(predicted_fastest_lap_delta_sec))) NA_real_ else mean(predicted_fastest_lap_delta_sec, na.rm = TRUE),
        model_count = n_distinct(model),
        .groups = "drop"
      ) %>%
      group_by(season, round) %>%
      arrange(average_model_rank, desc(probability), predicted_delta_sec, driver_name, .by_group = TRUE) %>%
      mutate(consensus_rank = row_number(), predicted_fastest_lap_winner = consensus_rank == 1) %>%
      ungroup() %>%
      left_join(fastest_lap_odds, by = c("season", "round", "driver_code")) %>%
      mutate(fastest_lap_model_edge = probability - fastest_lap_no_vig_probability)
  })

  fastest_lap_historical_picks <- reactive({
    req(input$fastest_lap_history_start_season, input$fastest_lap_history_end_season)
    bounds <- roi_window_bounds(input$fastest_lap_history_start_season, input$fastest_lap_history_end_season)

    history_rows <- fastest_lap_predictions %>%
      filter(
        season >= bounds[["start"]], season <= bounds[["end"]],
        !is.na(finish_position)
      ) %>%
      filter_fastest_lap_route_rows()

    validate(need(nrow(history_rows) > 0, "No completed routed fastest-lap predictions found for this history window."))

    family_picks <- history_rows %>%
      group_by(
        season, round, race_name, is_wet_race, model_family,
        driver_id, driver_code, driver_name, constructor_name,
        fastest_rank, fastest_lap_winner
      ) %>%
      summarise(average_model_rank = mean(predicted_fastest_lap_rank, na.rm = TRUE), .groups = "drop") %>%
      group_by(season, round, model_family) %>%
      arrange(average_model_rank, driver_name, .by_group = TRUE) %>%
      mutate(pick_rank = row_number()) %>%
      filter(pick_rank == 1L) %>%
      ungroup() %>%
      mutate(
        Scope = recode(model_family, rank = "Rank family", probability = "Probability family", delta = "Delta family"),
        pick_correct = fastest_lap_winner == 1L
      )

    consensus_picks <- history_rows %>%
      group_by(
        season, round, race_name, is_wet_race,
        driver_id, driver_code, driver_name, constructor_name,
        fastest_rank, fastest_lap_winner
      ) %>%
      summarise(average_model_rank = mean(predicted_fastest_lap_rank, na.rm = TRUE), .groups = "drop") %>%
      group_by(season, round) %>%
      arrange(average_model_rank, driver_name, .by_group = TRUE) %>%
      mutate(pick_rank = row_number()) %>%
      filter(pick_rank == 1L) %>%
      ungroup() %>%
      mutate(Scope = "Three-family consensus", pick_correct = fastest_lap_winner == 1L)

    bind_rows(consensus_picks, family_picks) %>%
      mutate(race_condition = if_else(coalesce(is_wet_race, FALSE), "Wet", "Dry"))
  })

  output$fastest_lap_history_table <- renderTable({
    picks <- fastest_lap_historical_picks()

    by_condition <- picks %>%
      group_by(Scope, race_condition) %>%
      summarise(
        Races = n(), Correct = sum(pick_correct, na.rm = TRUE),
        hit_rate = mean(pick_correct, na.rm = TRUE),
        top_three_rate = mean(fastest_rank <= 3, na.rm = TRUE),
        average_actual_rank = mean(fastest_rank, na.rm = TRUE),
        mean_reciprocal_rank = mean(1 / fastest_rank, na.rm = TRUE),
        .groups = "drop"
      )

    all_conditions <- picks %>%
      group_by(Scope) %>%
      summarise(
        race_condition = "All", Races = n(), Correct = sum(pick_correct, na.rm = TRUE),
        hit_rate = mean(pick_correct, na.rm = TRUE),
        top_three_rate = mean(fastest_rank <= 3, na.rm = TRUE),
        average_actual_rank = mean(fastest_rank, na.rm = TRUE),
        mean_reciprocal_rank = mean(1 / fastest_rank, na.rm = TRUE),
        .groups = "drop"
      )

    bind_rows(by_condition, all_conditions) %>%
      arrange(match(Scope, c("Three-family consensus", "Rank family", "Probability family", "Delta family")), match(race_condition, c("All", "Dry", "Wet"))) %>%
      transmute(
        Model = Scope, Type = race_condition, Races, Correct,
        `Winner hit rate` = format_pct(hit_rate, 1),
        `Top-3 hit rate` = format_pct(top_three_rate, 1),
        `Avg actual FL rank` = format_num(average_actual_rank, 2),
        `Mean reciprocal rank` = format_num(mean_reciprocal_rank, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fastest_lap_header <- renderUI({
    rows <- fastest_lap_consensus()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    routes <- routed_track_type_label(race$season[[1]], race$round[[1]])
    div(
      class = "event-header",
      div(class = "event-title-block", div(class = "eyebrow", paste0(race$season, " Round ", race$round)), h1(race$race_name),
          p("Three fastest-lap model families are routed through the existing track specialist flags."),
          p("This race routes to: ", strong(routes), "."))
    )
  })

  output$fastest_lap_winner_table <- renderTable({
    fastest_lap_consensus() %>%
      filter(consensus_rank == 1) %>%
      transmute(
        Pick = driver_name, Constructor = constructor_name,
        `Avg model rank` = format_num(average_model_rank, 2),
        `Fastest-lap probability` = format_pct(probability, 1),
        `Market odds` = fastest_lap_american_odds_label,
        `Market probability` = format_pct(fastest_lap_no_vig_probability, 1),
        `Model edge` = format_pct(fastest_lap_model_edge, 0.1),
        Models = model_count,
        `Actual fastest lap` = ifelse(is.na(fastest_lap_sec), "", paste0(format_num(fastest_lap_sec, 3), " sec")),
        Correct = ifelse(is.na(fastest_lap_winner), "", ifelse(fastest_lap_winner == 1, "Yes", "No"))
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fastest_lap_prediction_table <- renderTable({
    fastest_lap_consensus() %>%
      arrange(consensus_rank) %>%
      transmute(
        Rank = consensus_rank, Driver = driver_name, Constructor = constructor_name,
        `Avg model rank` = format_num(average_model_rank, 2),
        `FL probability` = format_pct(probability, 1),
        `FL odds` = fastest_lap_american_odds_label,
        `Market %` = format_pct(fastest_lap_no_vig_probability, 1),
        Edge = format_pct(fastest_lap_model_edge, 0.1),
        `Pred FL rank` = format_num(predicted_rank_target, 2),
        `Pred delta sec` = format_num(predicted_delta_sec, 3),
        Models = model_count,
        `Actual FL` = ifelse(is.na(fastest_lap_sec), "", paste0(format_num(fastest_lap_sec, 3), " sec")),
        `Actual delta` = ifelse(is.na(fastest_lap_delta_sec), "", paste0("+", format_num(fastest_lap_delta_sec, 3))),
        `Actual rank` = format_int(fastest_rank)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fastest_lap_model_table <- renderTable({
    selected_fastest_lap_rows() %>%
      distinct(routed_type, model_family, model_label) %>%
      mutate(
        routed_type = recode(routed_type, street = "Street", permanent = "Permanent", high_speed = "High speed", technical = "Technical"),
        model_family = recode(model_family, rank = "Rank", probability = "Probability", delta = "Delta seconds")
      ) %>%
      transmute(`Track route` = routed_type, Family = model_family, Model = model_label)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$wet_weather_race_selector <- renderUI({
    validate(need(nrow(meaningful_wet_races) > 0, "Wet-race history is unavailable."))
    req(input$wet_weather_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$wet_weather_season))
    selectInput(
      "wet_weather_round", "Race",
      choices = setNames(choices$round, choices$label),
      selected = default_race_round(choices)
    )
  })

  wet_track_class_for_race <- function(season_value, round_value) {
    routes <- race_route_types(season_value, round_value)
    case_when(
      "high_speed" %in% routes ~ "High speed",
      "technical" %in% routes ~ "Technical",
      "street" %in% routes ~ "Street",
      "permanent" %in% routes ~ "Permanent",
      TRUE ~ "Other"
    )
  }

  wet_residual_history <- bind_rows(lapply(seq_len(nrow(meaningful_wet_races)), function(i) {
    season_value <- meaningful_wet_races$season[[i]]
    round_value <- meaningful_wet_races$round[[i]]
    dry_models <- default_xgb_finish_models_for_race(season_value, round_value)

    wet_dry_prediction_history %>%
      filter(season == season_value, round == round_value, model %in% dry_models) %>%
      group_by(season, round, race_name, driver_code, driver_name, finish_position) %>%
      summarise(historical_dry_finish = mean(predicted_finish_position, na.rm = TRUE), .groups = "drop") %>%
      group_by(season, round) %>%
      arrange(historical_dry_finish, driver_name, .by_group = TRUE) %>%
      mutate(
        historical_dry_rank = row_number(),
        historical_actual_rank = rank(finish_position, ties.method = "first"),
        wet_rank_residual = historical_actual_rank - historical_dry_rank
      ) %>%
      ungroup() %>%
      mutate(wet_track_class = wet_track_class_for_race(season_value, round_value))
  }))

  wet_weather_history_for_race <- function(season_value, round_value, driver_codes = NULL, race_limit = 10L) {
    cutoff_key <- as.integer(season_value) * 100L + as.integer(round_value)
    rows <- stage1 %>%
      inner_join(
        meaningful_wet_races %>% select(season, round, wet_exclusion_reason),
        by = c("season", "round")
      ) %>%
      mutate(
        race_key = season * 100L + round,
        wet_grid = if_else(!is.na(grid) & grid > 0, as.numeric(grid), NA_real_),
        wet_places_gained = wet_grid - finish_position
      ) %>%
      filter(race_key < cutoff_key, !is.na(finish_position)) %>%
      left_join(
        wet_residual_history %>%
          select(season, round, driver_code, historical_dry_rank, historical_actual_rank, wet_rank_residual, wet_track_class),
        by = c("season", "round", "driver_code")
      )

    if (!is.null(driver_codes)) rows <- rows %>% filter(driver_code %in% driver_codes)

    rows %>%
      arrange(driver_code, desc(race_key)) %>%
      group_by(driver_code) %>%
      slice_head(n = as.integer(race_limit)) %>%
      mutate(
        wet_recency_index = row_number(),
        wet_intensity_weight = if_else(wet_exclusion_reason == "rainfall_recorded", 0.25, 1),
        wet_recency_weight = 0.5^((wet_recency_index - 1) / 4) * wet_intensity_weight
      ) %>%
      ungroup()
  }

  build_wet_weather_projection <- function(season_value, round_value, intensity_multiplier = 1) {
    season_value <- as.integer(season_value)
    round_value <- as.integer(round_value)
    intensity_multiplier <- pmin(1, pmax(0, as.numeric(intensity_multiplier)))
    selected_track_class <- wet_track_class_for_race(season_value, round_value)
    dry_models <- default_xgb_finish_models_for_race(season_value, round_value)

    dry_rows <- xgb_finish_predictions %>%
      filter(season == season_value, round == round_value, model %in% dry_models) %>%
      group_by(
        season, round, race_date, race_name, driver_code, driver_name,
        constructor_name, finish_position
      ) %>%
      summarise(
        dry_projected_finish = mean(predicted_finish_position, na.rm = TRUE),
        dry_model_count = n_distinct(model),
        win_odds_label = dplyr::first(win_avg_american_odds_label[!is.na(win_avg_american_odds_label) & win_avg_american_odds_label != ""], default = ""),
        podium_odds_label = dplyr::first(podium_effective_avg_american_odds_label[!is.na(podium_effective_avg_american_odds_label) & podium_effective_avg_american_odds_label != ""], default = ""),
        .groups = "drop"
      ) %>%
      add_prerace_display()

    if (nrow(dry_rows) == 0) return(tibble())

    wet_form <- wet_weather_history_for_race(
      season_value, round_value, unique(dry_rows$driver_code), 10L
    ) %>%
      group_by(driver_code) %>%
      summarise(
        wet_starts = n(),
        wet_average_finish = mean(finish_position, na.rm = TRUE),
        wet_average_grid = if (all(is.na(wet_grid))) NA_real_ else mean(wet_grid, na.rm = TRUE),
        wet_average_places_gained = if (all(is.na(wet_places_gained))) NA_real_ else mean(wet_places_gained, na.rm = TRUE),
        wet_wins = sum(finish_position == 1, na.rm = TRUE),
        wet_podiums = sum(finish_position <= 3, na.rm = TRUE),
        wet_finish_sd = if (n() > 1) sd(finish_position, na.rm = TRUE) else NA_real_,
        global_wet_residual = if (all(is.na(wet_rank_residual))) NA_real_ else weighted.mean(pmin(5, pmax(-5, wet_rank_residual)), wet_recency_weight, na.rm = TRUE),
        track_wet_starts = sum(wet_track_class == selected_track_class & !is.na(wet_rank_residual)),
        track_wet_residual = if (track_wet_starts >= 4) {
          weighted.mean(
            pmin(5, pmax(-5, wet_rank_residual[wet_track_class == selected_track_class])),
            wet_recency_weight[wet_track_class == selected_track_class],
            na.rm = TRUE
          )
        } else {
          NA_real_
        },
        .groups = "drop"
      ) %>%
      mutate(
        global_shrunk_residual = global_wet_residual * wet_starts / (wet_starts + 5),
        track_shrunk_residual = track_wet_residual * track_wet_starts / (track_wet_starts + 4),
        combined_wet_residual = if_else(
          track_wet_starts >= 4 & !is.na(track_shrunk_residual),
          0.75 * global_shrunk_residual + 0.25 * track_shrunk_residual,
          global_shrunk_residual
        )
      )

    dry_rows %>%
      left_join(wet_form, by = "driver_code") %>%
      group_by(season, round) %>%
      mutate(
        dry_rank = min_rank(dry_projected_finish),
        wet_starts = coalesce(wet_starts, 0L),
        track_wet_starts = coalesce(track_wet_starts, 0L),
        wet_residual_adjustment = if_else(
          wet_starts > 0 & !is.na(combined_wet_residual),
          pmin(1, pmax(-1, intensity_multiplier * combined_wet_residual)),
          0
        ),
        wet_projected_finish = dry_projected_finish + wet_residual_adjustment,
        wet_rank = rank(wet_projected_finish, ties.method = "first"),
        wet_position_change = dry_projected_finish - wet_projected_finish,
        wet_track_class = selected_track_class,
        wet_evidence = case_when(
          wet_starts >= 10 ~ "Full (10)",
          wet_starts >= 5 ~ paste0("Moderate (", wet_starts, ")"),
          wet_starts >= 3 ~ paste0("Limited (", wet_starts, ")"),
          wet_starts > 0 ~ paste0("Very limited (", wet_starts, ")"),
          TRUE ~ "None"
        )
      ) %>%
      ungroup() %>%
      arrange(wet_rank, driver_name)
  }

  wet_weather_projection_rows <- reactive({
    req(input$wet_weather_season, input$wet_weather_round, input$wet_weather_intensity)
    rows <- build_wet_weather_projection(
      input$wet_weather_season,
      input$wet_weather_round,
      input$wet_weather_intensity
    )
    validate(need(nrow(rows) > 0, "No dry-model finish projections found for this race."))
    rows
  })

  output$wet_weather_blend_note <- renderUI({
    req(input$wet_weather_intensity)
    scenario_label <- if (as.numeric(input$wet_weather_intensity) >= 1) "Sustained wet" else "Light rain"
    tagList(
      p(paste0("Scenario: ", scenario_label)),
      p("10-race recency window; four-race half-life"),
      p("75% overall residual / 25% matching track class")
    )
  })

  wet_weather_historical_projections <- reactive({
    req(input$wet_weather_roi_start_season, input$wet_weather_roi_end_season)
    bounds <- roi_window_bounds(input$wet_weather_roi_start_season, input$wet_weather_roi_end_season)
    event_keys <- meaningful_wet_races %>%
      filter(season >= bounds[["start"]], season <= bounds[["end"]]) %>%
      semi_join(
        xgb_finish_predictions %>% filter(!is.na(finish_position)) %>% distinct(season, round),
        by = c("season", "round")
      ) %>%
      arrange(season, round)

    bind_rows(lapply(seq_len(nrow(event_keys)), function(i) {
      historical_intensity <- if (event_keys$wet_exclusion_reason[[i]] == "rainfall_recorded") 0.25 else 1
      build_wet_weather_projection(
        event_keys$season[[i]],
        event_keys$round[[i]],
        historical_intensity
      )
    }))
  })

  wet_weather_historical_bets <- reactive({
    rows <- wet_weather_historical_projections()
    validate(need(nrow(rows) > 0, "No completed wet-race projections found in this season window."))

    ranked <- bind_rows(
      rows %>% mutate(Strategy = "Dry consensus", strategy_finish = dry_projected_finish),
      rows %>% mutate(Strategy = "Wet residual model", strategy_finish = wet_projected_finish)
    ) %>%
      group_by(Strategy, season, round) %>%
      arrange(strategy_finish, driver_name, .by_group = TRUE) %>%
      mutate(strategy_rank = row_number()) %>%
      ungroup()

    winner_bets <- ranked %>%
      filter(strategy_rank == 1L) %>%
      transmute(
        Strategy, season, round, race_name, Market = "Winner", driver_name,
        actual_finish = finish_position, bet_won = finish_position == 1,
        odds_label = win_odds_label
      )

    podium_bets <- ranked %>%
      filter(strategy_rank <= 3L) %>%
      transmute(
        Strategy, season, round, race_name, Market = "Podium", driver_name,
        actual_finish = finish_position, bet_won = finish_position <= 3,
        odds_label = podium_odds_label
      )

    bind_rows(winner_bets, podium_bets) %>%
      mutate(
        odds_decimal = american_label_to_decimal(odds_label),
        stake = if_else(!is.na(odds_decimal) & !is.na(actual_finish), 1, 0),
        profit = if_else(stake > 0, bet_profit(odds_decimal, bet_won, stake), NA_real_)
      )
  })

  output$wet_weather_roi_table <- renderTable({
    bets <- wet_weather_historical_bets() %>% filter(stake > 0)
    validate(need(nrow(bets) > 0, "No completed wet-race bets with odds found in this season window."))

    summarise_roi <- function(rows, period_label) {
      market_rows <- rows %>%
        group_by(Strategy, Market) %>%
        summarise(
          Races = n_distinct(season, round), Bets = n(), Wins = sum(bet_won, na.rm = TRUE),
          hit_rate = Wins / Bets, Stake = sum(stake), Profit = sum(profit, na.rm = TRUE),
          roi = Profit / Stake, .groups = "drop"
        )
      combined_rows <- rows %>%
        group_by(Strategy) %>%
        summarise(
          Market = "Combined", Races = n_distinct(season, round), Bets = n(), Wins = sum(bet_won, na.rm = TRUE),
          hit_rate = Wins / Bets, Stake = sum(stake), Profit = sum(profit, na.rm = TRUE),
          roi = Profit / Stake, .groups = "drop"
        )
      bind_rows(market_rows, combined_rows) %>% mutate(Period = period_label)
    }

    bounds <- roi_window_bounds(input$wet_weather_roi_start_season, input$wet_weather_roi_end_season)
    period_label <- if (bounds[["start"]] == bounds[["end"]]) {
      as.character(bounds[["start"]])
    } else {
      paste0(bounds[["start"]], "-", bounds[["end"]])
    }
    summary_rows <- summarise_roi(bets, period_label)

    summary_rows %>%
      arrange(Period, Strategy, match(Market, c("Winner", "Podium", "Combined"))) %>%
      transmute(
        Period, Strategy, Market, Races, Bets, Wins,
        `Hit rate` = format_pct(hit_rate, 1),
        Stake = format_num(Stake, 0),
        Profit = format_num(Profit, 2),
        ROI = format_pct(roi, 0.1)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$wet_weather_header <- renderUI({
    rows <- wet_weather_projection_rows()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round, " · Wet scenario")),
        h1(race$race_name),
        p("This is a scenario overlay, not a separately trained wet-weather model."),
        p("It adjusts the dry XGBoost consensus using recency-weighted wet rank residuals, shrunk for sample size, partially pooled with the matching track class, and capped at one position. The selected race is always excluded from its own history.")
      )
    )
  })

  output$wet_weather_projection_table <- renderTable({
    wet_weather_projection_rows() %>%
      transmute(
        Rank = wet_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        Start = display_start_position_label,
        Quali = display_quali_position_label,
        `Dry projection` = format_num(dry_projected_finish, 2),
        `Global wet residual` = format_num(global_wet_residual, 2),
        `Track wet residual` = format_num(track_wet_residual, 2),
        `Wet projection` = format_num(wet_projected_finish, 2),
        `Wet boost` = ifelse(
          is.na(wet_position_change) | abs(wet_position_change) < 0.005,
          "0.00",
          paste0(ifelse(wet_position_change > 0, "+", ""), format_num(wet_position_change, 2))
        ),
        Evidence = wet_evidence,
        `Actual finish` = format_int(finish_position)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$wet_weather_specialist_table <- renderTable({
    wet_weather_projection_rows() %>%
      arrange(combined_wet_residual, wet_average_finish, driver_name) %>%
      transmute(
        Driver = driver_name,
        Starts = wet_starts,
        `Track class` = wet_track_class,
        `Track starts` = track_wet_starts,
        `Global vs dry` = format_num(global_wet_residual, 2),
        `Track vs dry` = format_num(track_wet_residual, 2),
        `Avg grid` = format_num(wet_average_grid, 2),
        `Avg finish` = format_num(wet_average_finish, 2),
        `Avg places gained` = ifelse(
          is.na(wet_average_places_gained), "",
          paste0(ifelse(wet_average_places_gained > 0, "+", ""), format_num(wet_average_places_gained, 2))
        ),
        Wins = coalesce(wet_wins, 0L),
        Podiums = coalesce(wet_podiums, 0L)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$wet_weather_races_table <- renderTable({
    req(input$wet_weather_season, input$wet_weather_round)
    cutoff_key <- as.integer(input$wet_weather_season) * 100L + as.integer(input$wet_weather_round)
    winners <- stage1 %>%
      filter(finish_position == 1) %>%
      select(season, round, Winner = driver_name)

    meaningful_wet_races %>%
      mutate(race_key = season * 100L + round) %>%
      filter(race_key < cutoff_key) %>%
      arrange(desc(race_key)) %>%
      slice_head(n = 10L) %>%
      left_join(winners, by = c("season", "round")) %>%
      transmute(
        Season = season,
        Round = round,
        Race = race_name,
        Date = format(race_date, "%Y-%m-%d"),
        Winner
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_race_selector <- renderUI({
    req(input$fantasy_season)
    choices <- rf_race_choices %>%
      filter(season == as.integer(input$fantasy_season))
    if (nrow(choices) == 0) {
      choices <- race_choices %>%
        filter(season == as.integer(input$fantasy_season))
    }
    selectInput("fantasy_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  fantasy_locked_consensus_selection <- list(
    consensus_mode = "family",
    use_xgb_finish = TRUE,
    use_xgb_probability = TRUE,
    use_xgb_points = TRUE,
    use_routed_specialists = TRUE,
    xgb_finish_models = xgb_finish_default_models,
    xgb_probability_models = xgb_probability_default_models,
    xgb_points_models = xgb_points_default_models
  )

  fantasy_mode_consensus_predictions <- function(use_chatter) {
    req(input$fantasy_season, input$fantasy_round)
    family_rows <- if (isTRUE(use_chatter)) {
      build_chatter_adjusted_allmodel_family_ranks(
        input$fantasy_season,
        input$fantasy_round,
        as.numeric(input$fantasy_chatter_strength %||% 50) / 100,
        TRUE,
        fantasy_locked_consensus_selection
      )
    } else {
      build_allmodel_family_consensus_ranks(
        input$fantasy_season,
        input$fantasy_round,
        fantasy_locked_consensus_selection
      )
    }

    if (nrow(family_rows) == 0) return(tibble())
    build_allmodel_predictions_from_family_rows(family_rows, TRUE)
  }

  fantasy_mode_driver_projections <- function(use_chatter) {
    consensus_rows <- fantasy_mode_consensus_predictions(use_chatter)
    if (nrow(consensus_rows) == 0) return(tibble())
    fantasy_projection_rows(
      input$fantasy_season,
      input$fantasy_round,
      6L,
      isTRUE(use_chatter),
      FALSE,
      consensus_rows
    )
  }

  fantasy_consensus_predictions <- reactive({
    rows <- fantasy_mode_consensus_predictions(isTRUE(input$fantasy_use_chatter))
    validate(need(nrow(rows) > 0, allmodel_empty_message(input$fantasy_season, input$fantasy_round)))
    rows
  })

  fantasy_driver_projections <- reactive({
    req(input$fantasy_season, input$fantasy_round)
    rows <- fantasy_mode_driver_projections(isTRUE(input$fantasy_use_chatter))
    validate(need(nrow(rows) > 0, "No fantasy projection rows found for this race."))
    rows
  })

  fantasy_constructor_projections <- reactive({
    fantasy_constructor_rows(fantasy_driver_projections())
  })

  fantasy_best_lineup <- reactive({
    optimize_fantasy_lineup(
      fantasy_driver_projections(),
      fantasy_constructor_projections(),
      input$fantasy_salary_cap,
      input$fantasy_flex_count,
      TRUE,
      "any"
    )
  })

  fantasy_alt_lineup <- reactive({
    primary <- fantasy_best_lineup()
    driver_rows <- fantasy_driver_projections()
    captain_row <- primary %>% filter(Slot == "CPT") %>% slice(1)
    primary_driver_names <- primary %>% filter(Slot %in% c("CPT", "DRV")) %>% pull(Name) %>% unique()
    enforce_disjoint <- isTRUE(input$fantasy_disjoint_drivers)
    enforce_captain_exclusive <- isTRUE(input$fantasy_captain_exclusive) || enforce_disjoint
    excluded_alt_drivers <- if (enforce_disjoint) {
      primary_driver_names
    } else if (enforce_captain_exclusive) {
      captain_row$Name %||% character()
    } else {
      character()
    }
    excluded_alt_captains <- if (enforce_captain_exclusive) {
      primary_driver_names
    } else {
      captain_row$Name %||% character()
    }
    captain_preference <- "high"

    if (nrow(captain_row) > 0 && nrow(driver_rows) > 0) {
      captain_base_salary <- as.numeric(captain_row$Salary[[1]]) / 1.5
      salary_break <- as.numeric(quantile(driver_rows$mock_salary, probs = 0.75, na.rm = TRUE, type = 1))
      captain_preference <- if (is.finite(captain_base_salary) && captain_base_salary >= salary_break) "low" else "high"
    } else {
      salary_break <- NULL
    }

    optimize_fantasy_lineup(
      driver_rows,
      fantasy_constructor_projections(),
      input$fantasy_salary_cap,
      input$fantasy_flex_count,
      TRUE,
      captain_preference = captain_preference,
      excluded_captains = excluded_alt_captains,
      excluded_drivers = excluded_alt_drivers,
      captain_salary_break = salary_break
    )
  })

  generate_fantasy_portfolio <- function(drivers, constructors, source_label) {
    primary <- optimize_fantasy_lineup(
      drivers,
      constructors,
      input$fantasy_salary_cap,
      input$fantasy_flex_count,
      TRUE,
      "any"
    )
    if (nrow(primary) == 0 || nrow(drivers) == 0) return(tibble())
    primary_captain <- primary %>% filter(Slot == "CPT") %>% pull(Name) %>% .[1]
    primary_constructor <- primary %>% filter(Slot == "CON") %>% pull(Name) %>% .[1]
    portfolio <- tibble()
    portfolio_size <- 8L
    max_driver_entries <- max(1L, floor(portfolio_size * pmin(100, pmax(25, as.numeric(input$fantasy_driver_exposure %||% 75))) / 100))
    max_constructor_entries <- max(1L, floor(portfolio_size * pmin(100, pmax(10, as.numeric(input$fantasy_constructor_exposure %||% 50))) / 100))
    min_major_changes <- as.integer(input$fantasy_min_major_changes %||% 2L)
    build <- function(label, captain_preference = "any", excluded_captains = character(), excluded_drivers = character(), required_captains = character(), required_drivers = character(), salary_break = NULL, constructor_preference = "any", required_constructors = character(), scenario = label) {
      constructor_counts <- if (nrow(portfolio) == 0) integer() else table(portfolio$Name[portfolio$Slot == "CON"])
      exposure_exclusions <- names(constructor_counts[constructor_counts >= max_constructor_entries])
      driver_counts <- if (nrow(portfolio) == 0) integer() else table(portfolio$Name[portfolio$Slot %in% c("CPT", "DRV")])
      driver_exposure_exclusions <- names(driver_counts[driver_counts >= max_driver_entries])
      x <- optimize_fantasy_lineup(
        drivers, constructors, input$fantasy_salary_cap, input$fantasy_flex_count, TRUE,
        captain_preference = captain_preference,
        excluded_captains = setdiff(excluded_captains, required_captains),
        excluded_drivers = setdiff(union(excluded_drivers, driver_exposure_exclusions), c(required_captains, required_drivers)),
        required_captains = required_captains,
        required_drivers = required_drivers,
        captain_salary_break = salary_break,
        constructor_preference = constructor_preference,
        excluded_constructors = exposure_exclusions,
        required_constructors = required_constructors,
        previous_lineups = portfolio,
        min_major_changes = min_major_changes,
        min_driver_replacements = 1L,
        max_shared_roster_components = 4L
      )
      if (nrow(x) == 0) return(tibble())
      x <- x %>% mutate(Lineup = label, Scenario = scenario, Source = source_label, .before = 1)
      portfolio <<- bind_rows(portfolio, x)
      x
    }
    primary_drivers <- primary %>% filter(Slot %in% c("CPT", "DRV")) %>% pull(Name) %>% unique()
    salary_break <- as.numeric(quantile(drivers$mock_salary, probs = 0.75, na.rm = TRUE, type = 1))
    result <- bind_rows(
      build("A — Highest-projected lineup", "any"),
      build("B — Premium captain pivot", "high", excluded_captains = primary_captain),
      build("C — Rival / leverage constructor", "low", excluded_captains = c(primary_captain, primary_drivers[2]), required_constructors = setdiff(constructors$constructor_name, primary_constructor)),
      build("D — Chaos / place differential", "low", excluded_captains = c(primary_captain), excluded_drivers = primary_drivers[2]),
      build("E — Favorite constructor dominance", "high", excluded_captains = c(primary_captain), salary_break = salary_break, required_constructors = primary_constructor)
    )
    premium_captain_target <- drivers %>%
      filter(mock_salary >= salary_break) %>%
      mutate(current_appearances = vapply(driver_name, function(driver) sum(portfolio$Name[portfolio$Slot %in% c("CPT", "DRV")] == driver), integer(1))) %>%
      arrange(current_appearances, desc(mock_salary), desc(fantasy_projection), driver_name) %>%
      slice(1) %>%
      pull(driver_name)
    premium_expansion_exclusion <- portfolio %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      count(Name, sort = TRUE) %>%
      slice(1) %>%
      pull(Name)
    result <- bind_rows(
      result,
      build("F — Undercovered premium outcome", "high", excluded_drivers = premium_expansion_exclusion, required_drivers = premium_captain_target, constructor_preference = "low"),
      build("G — Diversified projection build", "any", excluded_captains = c(primary_captain, primary_drivers[2], primary_drivers[3]))
    )
    top_constructor_pool <- constructors %>%
      arrange(desc(mock_salary), desc(fantasy_projection), constructor_name) %>%
      slice_head(n = 4L) %>%
      pull(constructor_name)
    constructor_coverage_target <- constructors %>%
      filter(constructor_name %in% top_constructor_pool) %>%
      arrange(fantasy_projection, mock_salary, constructor_name) %>%
      slice(1) %>%
      pull(constructor_name)
    constructor_expansion_exclusion <- portfolio %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      count(Name, sort = TRUE) %>%
      slice(1) %>%
      pull(Name)
    result <- bind_rows(
      result,
      build(
        "H — Value captain / constructor coverage",
        "low",
        excluded_captains = primary_captain,
        excluded_drivers = constructor_expansion_exclusion,
        salary_break = salary_break,
        required_constructors = constructor_coverage_target
      )
    )
    result
  }

  fantasy_portfolio_lineups <- reactive({
    generate_fantasy_portfolio(
      fantasy_driver_projections(),
      fantasy_constructor_projections(),
      if (isTRUE(input$fantasy_use_chatter)) "Chatter" else "Baseline"
    )
  })

  fantasy_combined_candidates_experimental <- reactive({
    req(input$fantasy_season, input$fantasy_round)
    chatter_drivers <- fantasy_mode_driver_projections(TRUE)
    baseline_drivers <- fantasy_mode_driver_projections(FALSE)
    validate(need(nrow(chatter_drivers) > 0 && nrow(baseline_drivers) > 0, "Both chatter and baseline projections are required for the combined portfolio."))
    chatter_constructors <- fantasy_constructor_rows(chatter_drivers)
    baseline_constructors <- fantasy_constructor_rows(baseline_drivers)
    base_candidates <- bind_rows(
      generate_fantasy_portfolio(chatter_drivers, chatter_constructors, "Chatter"),
      generate_fantasy_portfolio(baseline_drivers, baseline_constructors, "Baseline")
    )
    robust_drivers <- full_join(
      chatter_drivers %>% select(driver_name, constructor_name, mock_salary, chatter_projection = fantasy_projection),
      baseline_drivers %>% select(driver_name, baseline_projection = fantasy_projection),
      by = "driver_name"
    ) %>%
      mutate(
        median_projection = (chatter_projection + baseline_projection) / 2,
        projection_spread = abs(chatter_projection - baseline_projection),
        robust_projection = median_projection - 0.25 * projection_spread,
        robust_value = robust_projection / mock_salary * 1000
      ) %>%
      arrange(desc(robust_projection), desc(robust_value), driver_name)
    candidate_exposure <- base_candidates %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      count(Name, name = "candidate_lineups")
    robust_drivers <- robust_drivers %>%
      left_join(candidate_exposure, by = c("driver_name" = "Name")) %>%
      mutate(candidate_lineups = coalesce(candidate_lineups, 0L))
    top_driver_row <- robust_drivers %>%
      arrange(desc(pmax(chatter_projection, baseline_projection)), desc(robust_projection)) %>%
      slice(1)
    top_driver <- top_driver_row$driver_name[[1]]
    top_constructor <- top_driver_row$constructor_name[[1]]
    top_teammate <- robust_drivers %>%
      filter(constructor_name == top_constructor, driver_name != top_driver) %>%
      slice(1) %>%
      pull(driver_name)
    undercovered_contenders <- robust_drivers %>%
      slice_head(n = 8L) %>%
      filter(driver_name != top_driver) %>%
      arrange(candidate_lineups, desc(robust_value), desc(robust_projection)) %>%
      slice_head(n = 3L) %>%
      pull(driver_name)
    value_captain <- robust_drivers %>%
      slice_head(n = 6L) %>%
      arrange(mock_salary, desc(robust_projection)) %>%
      slice(1) %>%
      pull(driver_name)
    robust_constructors <- full_join(
      chatter_constructors %>% select(constructor_name, chatter_projection = fantasy_projection),
      baseline_constructors %>% select(constructor_name, baseline_projection = fantasy_projection),
      by = "constructor_name"
    ) %>%
      mutate(
        median_projection = (chatter_projection + baseline_projection) / 2,
        projection_spread = abs(chatter_projection - baseline_projection),
        robust_projection = median_projection - 0.25 * projection_spread
      ) %>%
      arrange(desc(robust_projection), constructor_name)
    coverage_constructors <- robust_constructors$constructor_name[seq_len(min(4L, nrow(robust_constructors)))]
    constructor_candidate_exposure <- base_candidates %>%
      filter(Slot == "CON") %>%
      count(Name, name = "candidate_lineups")
    constructor_targets <- tibble(constructor_name = coverage_constructors) %>%
      left_join(constructor_candidate_exposure, by = c("constructor_name" = "Name")) %>%
      mutate(candidate_lineups = coalesce(candidate_lineups, 0L)) %>%
      filter(candidate_lineups < 2L) %>%
      pull(constructor_name)
    coverage_driver_names <- unique(c(
      robust_drivers$driver_name[seq_len(min(4L, nrow(robust_drivers)))],
      undercovered_contenders[[1]],
      value_captain
    ))
    fragile_drivers <- robust_drivers %>%
      filter(
        projection_spread >= quantile(projection_spread, 0.75, na.rm = TRUE, names = FALSE),
        robust_projection < median(robust_projection, na.rm = TRUE)
      ) %>%
      pull(driver_name)
    targeted_lineup <- function(driver_rows, constructor_rows, source_label, label, required_captain = character(), required_driver = character(), excluded_driver = character(), required_constructor = character()) {
      x <- optimize_fantasy_lineup(
        driver_rows,
        constructor_rows,
        input$fantasy_salary_cap,
        input$fantasy_flex_count,
        TRUE,
        captain_preference = "any",
        excluded_drivers = excluded_driver,
        required_captains = required_captain,
        required_drivers = required_driver,
        required_constructors = required_constructor
      )
      if (nrow(x) == 0) return(tibble())
      x %>% mutate(Lineup = label, Scenario = label, Source = source_label, .before = 1)
    }
    candidate_rows <- bind_rows(
      base_candidates,
      targeted_lineup(chatter_drivers, chatter_constructors, "Chatter", "I — Top-driver captain ceiling", required_captain = top_driver),
      targeted_lineup(chatter_drivers, chatter_constructors, "Chatter", "J — Top-driver teammate fade", required_driver = top_driver, excluded_driver = top_teammate),
      bind_rows(lapply(undercovered_contenders[1], function(driver) {
        targeted_lineup(chatter_drivers, chatter_constructors, "Chatter", paste0("K — Undercovered contender: ", driver), required_driver = driver)
      })),
      targeted_lineup(baseline_drivers, baseline_constructors, "Baseline", paste0("K — Undercovered contender: ", undercovered_contenders[[1]]), required_driver = undercovered_contenders[[1]]),
      targeted_lineup(chatter_drivers, chatter_constructors, "Chatter", "L — Robust value captain", required_captain = value_captain),
      bind_rows(lapply(constructor_targets, function(constructor) {
        targeted_lineup(chatter_drivers, chatter_constructors, "Chatter", paste0("M — Constructor outcome: ", constructor), required_constructor = constructor)
      }))
    ) %>%
      mutate(
        CandidateID = paste(Source, Lineup, sep = "::"),
        CoverageDrivers = paste(coverage_driver_names, collapse = "|"),
        CoverageConstructors = paste(coverage_constructors, collapse = "|"),
        FragileDrivers = paste(fragile_drivers, collapse = "|"),
        TopDriverTarget = top_driver,
        .before = 1
      )
    projection_lookup <- function(driver_rows, constructor_rows, value_name) {
      bind_rows(
        driver_rows %>% transmute(AssetType = "Driver", Name = driver_name, Evaluation = fantasy_projection),
        constructor_rows %>% transmute(AssetType = "Constructor", Name = constructor_name, Evaluation = fantasy_projection)
      ) %>% rename(!!value_name := Evaluation)
    }
    evaluated <- candidate_rows %>%
      mutate(AssetType = if_else(Slot == "CON", "Constructor", "Driver")) %>%
      left_join(projection_lookup(chatter_drivers, chatter_constructors, "Chatter asset projection"), by = c("AssetType", "Name")) %>%
      left_join(projection_lookup(baseline_drivers, baseline_constructors, "Baseline asset projection"), by = c("AssetType", "Name")) %>%
      mutate(
        `Chatter evaluated points` = `Chatter asset projection` * if_else(Slot == "CPT", 1.5, 1),
        `Baseline evaluated points` = `Baseline asset projection` * if_else(Slot == "CPT", 1.5, 1)
      ) %>%
      group_by(CandidateID) %>%
      mutate(
        `Chatter portfolio projection` = sum(`Chatter evaluated points`, na.rm = TRUE),
        `Baseline portfolio projection` = sum(`Baseline evaluated points`, na.rm = TRUE),
        `Median model projection` = (`Chatter portfolio projection` + `Baseline portfolio projection`) / 2,
        `Model projection spread` = abs(`Chatter portfolio projection` - `Baseline portfolio projection`),
        `Robust projection` = `Median model projection` - 0.25 * `Model projection spread`,
        `Model ceiling projection` = pmax(`Chatter portfolio projection`, `Baseline portfolio projection`)
      ) %>%
      ungroup() %>%
      select(-AssetType, -`Chatter asset projection`, -`Baseline asset projection`, -`Chatter evaluated points`, -`Baseline evaluated points`)
    evaluated
  })

  fantasy_combined_portfolio_experimental <- reactive({
    candidates <- fantasy_combined_candidates_experimental()
    validate(need(nrow(candidates) > 0, "No combined-portfolio candidates are available."))
    coverage_drivers <- str_split(first(candidates$CoverageDrivers), fixed("|"))[[1]]
    coverage_constructors <- str_split(first(candidates$CoverageConstructors), fixed("|"))[[1]]
    fragile_drivers <- str_split(first(candidates$FragileDrivers), fixed("|"))[[1]]
    fragile_drivers <- fragile_drivers[nzchar(fragile_drivers)]
    top_driver_target <- first(candidates$TopDriverTarget)
    candidate_groups <- split(candidates, candidates$CandidateID)
    meta <- bind_rows(lapply(names(candidate_groups), function(candidate_id) {
      x <- candidate_groups[[candidate_id]]
      tibble(
        CandidateID = candidate_id,
        Source = first(x$Source),
        Candidate = first(x$Lineup),
        Scenario = first(x$Scenario),
        Projection = first(x$total_projection),
        RobustProjection = first(x$`Robust projection`),
        MedianProjection = first(x$`Median model projection`),
        CeilingProjection = first(x$`Model ceiling projection`),
        ProjectionSpread = first(x$`Model projection spread`),
        Captain = x$Name[x$Slot == "CPT"][1],
        Constructor = x$Name[x$Slot == "CON"][1],
        Drivers = list(unique(x$Name[x$Slot %in% c("CPT", "DRV")])),
        Roster = list(c(unique(x$Name[x$Slot %in% c("CPT", "DRV")]), x$Name[x$Slot == "CON"][1]))
      )
    }))
    if (nrow(meta) > 16L) {
      targeted_all <- meta %>% filter(str_detect(Candidate, "^[I-M] —"))
      targeted_k_chatter <- targeted_all %>%
        filter(Source == "Chatter", str_detect(Candidate, "^K — Undercovered")) %>%
        arrange(desc(RobustProjection), desc(CeilingProjection)) %>%
        slice_head(n = 1L)
      targeted_meta <- targeted_all %>%
        filter(!(Source == "Chatter" & str_detect(Candidate, "^K — Undercovered"))) %>%
        bind_rows(targeted_k_chatter)
      base_meta <- meta %>%
        filter(
          !CandidateID %in% targeted_meta$CandidateID,
          !str_detect(Candidate, "^A — Highest-projected")
        ) %>%
        arrange(desc(RobustProjection), desc(CeilingProjection), Source, Candidate)
      baseline_floor <- base_meta %>% filter(Source == "Baseline") %>% slice_head(n = 3L)
      remaining_base <- base_meta %>%
        filter(!CandidateID %in% baseline_floor$CandidateID) %>%
        slice_head(n = max(0L, 16L - nrow(targeted_meta) - nrow(baseline_floor)))
      meta <- bind_rows(targeted_meta, baseline_floor, remaining_base) %>% distinct(CandidateID, .keep_all = TRUE)
    }
    validate(need(nrow(meta) >= 8L, "Fewer than eight candidates were generated."))

    target_chatter <- as.integer(pmin(7, pmax(1, input$fantasy_combined_chatter_count %||% 5L)))
    max_driver_entries <- max(1L, floor(8L * pmin(100, pmax(25, as.numeric(input$fantasy_driver_exposure %||% 75))) / 100))
    max_constructor_entries <- max(1L, floor(8L * pmin(100, pmax(10, as.numeric(input$fantasy_constructor_exposure %||% 50))) / 100))
    combinations <- utils::combn(seq_len(nrow(meta)), 8L)
    best_indexes <- integer()
    best_coverage_penalty <- Inf
    best_source_delta <- Inf
    best_score <- -Inf

    for (column in seq_len(ncol(combinations))) {
      indexes <- combinations[, column]
      selected <- meta[indexes, ]
      source_delta <- abs(sum(selected$Source == "Chatter") - target_chatter)
      driver_counts <- table(unlist(selected$Drivers, use.names = FALSE))
      constructor_counts <- table(selected$Constructor)
      if (max(driver_counts) > max_driver_entries || max(constructor_counts) > max_constructor_entries) next
      pair_indexes <- utils::combn(seq_along(indexes), 2L)
      shared_counts <- apply(pair_indexes, 2, function(pair) length(intersect(selected$Roster[[pair[1]]], selected$Roster[[pair[2]]])))
      if (any(shared_counts > 4L)) next
      exact_keys <- vapply(indexes, function(index) {
        x <- candidate_groups[[meta$CandidateID[[index]]]]
        paste(x$Name[x$Slot == "CPT"][1], paste(sort(x$Name[x$Slot %in% c("CPT", "DRV")]), collapse = "|"), x$Name[x$Slot == "CON"][1], sep = "::")
      }, character(1))
      if (n_distinct(exact_keys) < 8L) next
      if (!top_driver_target %in% selected$Captain) next
      selected_driver_names <- unique(unlist(selected$Drivers, use.names = FALSE))
      fragile_excess <- if (length(fragile_drivers) == 0L) 0L else sum(vapply(fragile_drivers, function(driver) {
        max(0L, sum(vapply(selected$Drivers, function(x) driver %in% x, logical(1))) - 1L)
      }, integer(1)))
      coverage_penalty <- sum(!coverage_drivers %in% selected_driver_names) +
        2L * sum(!coverage_constructors %in% selected$Constructor) +
        3L * max(0L, 2L - sum(vapply(selected$Drivers, function(x) top_driver_target %in% x, logical(1)))) +
        fragile_excess
      if (source_delta > best_source_delta) next
      if (source_delta == best_source_delta && coverage_penalty > best_coverage_penalty) next
      selection_score <- sum(selected$RobustProjection) +
        0.1 * sum(selected$CeilingProjection - selected$MedianProjection) +
        2 * n_distinct(selected$Scenario) +
        1.5 * n_distinct(selected$Captain) +
        n_distinct(selected$Constructor) -
        0.5 * sum(shared_counts)
      if (
        source_delta < best_source_delta ||
          (source_delta == best_source_delta && coverage_penalty < best_coverage_penalty) ||
          (source_delta == best_source_delta && coverage_penalty == best_coverage_penalty && selection_score > best_score)
      ) {
        best_indexes <- indexes
        best_coverage_penalty <- coverage_penalty
        best_source_delta <- source_delta
        best_score <- selection_score
      }
    }
    validate(need(length(best_indexes) == 8L, "No eight-lineup combination satisfies the current exposure and overlap settings."))
    selected_meta <- meta[best_indexes, ]
    selected_meta$AverageSharedSelections <- vapply(seq_len(nrow(selected_meta)), function(i) {
      others <- setdiff(seq_len(nrow(selected_meta)), i)
      mean(vapply(others, function(j) length(intersect(selected_meta$Roster[[i]], selected_meta$Roster[[j]])), numeric(1)))
    }, numeric(1))
    single_entry_id <- selected_meta$CandidateID[[which.max(selected_meta$RobustProjection)]]
    ceiling_pool <- which(selected_meta$CandidateID != single_entry_id)
    ceiling_id <- selected_meta$CandidateID[[ceiling_pool[which.max(selected_meta$CeilingProjection[ceiling_pool])]]]
    contrarian_pool <- which(
      !selected_meta$CandidateID %in% c(single_entry_id, ceiling_id) &
        selected_meta$RobustProjection >= quantile(selected_meta$RobustProjection, 0.25, names = FALSE)
    )
    contrarian_id <- if (length(contrarian_pool) == 0L) NA_character_ else selected_meta$CandidateID[[contrarian_pool[which.min(selected_meta$AverageSharedSelections[contrarian_pool])]]]
    selected_meta <- selected_meta %>%
      arrange(desc(RobustProjection), desc(CeilingProjection), Source, Candidate) %>%
      mutate(
        `Combined lineup` = paste0("Entry ", row_number()),
        `Portfolio role` = case_when(
          CandidateID == single_entry_id ~ "Model-agreement single-entry candidate",
          CandidateID == ceiling_id ~ "Model ceiling anchor",
          CandidateID == contrarian_id ~ "Contrarian viable diversifier",
          Source == "Baseline" ~ "Baseline diversifier",
          TRUE ~ "Chatter race-script diversifier"
        ),
        SelectionScore = best_score
      )
    selection_map <- selected_meta %>% select(CandidateID, `Combined lineup`, Candidate, `Portfolio role`, AverageSharedSelections, SelectionScore)
    candidates %>%
      inner_join(selection_map, by = "CandidateID") %>%
      arrange(as.integer(str_remove(`Combined lineup`, "Entry ")), factor(Slot, levels = c("CPT", "DRV", "CON")))
  })

  fantasy_combined_candidates <- reactive({
    req(input$fantasy_season, input$fantasy_round)
    chatter_drivers <- fantasy_mode_driver_projections(TRUE)
    baseline_drivers <- fantasy_mode_driver_projections(FALSE)
    validate(need(nrow(chatter_drivers) > 0 && nrow(baseline_drivers) > 0, "Both projection modes are required for the optional mixed portfolio."))
    chatter_constructors <- fantasy_constructor_rows(chatter_drivers)
    baseline_constructors <- fantasy_constructor_rows(baseline_drivers)
    candidates <- bind_rows(
      generate_fantasy_portfolio(chatter_drivers, chatter_constructors, "Chatter"),
      generate_fantasy_portfolio(baseline_drivers, baseline_constructors, "Baseline")
    ) %>% mutate(CandidateID = paste(Source, Lineup, sep = "::"), .before = 1)
    projection_lookup <- function(driver_rows, constructor_rows, value_name) {
      bind_rows(
        driver_rows %>% transmute(AssetType = "Driver", Name = driver_name, Evaluation = fantasy_projection),
        constructor_rows %>% transmute(AssetType = "Constructor", Name = constructor_name, Evaluation = fantasy_projection)
      ) %>% rename(!!value_name := Evaluation)
    }
    candidates %>%
      mutate(AssetType = if_else(Slot == "CON", "Constructor", "Driver")) %>%
      left_join(projection_lookup(chatter_drivers, chatter_constructors, "ChatterEvaluation"), by = c("AssetType", "Name")) %>%
      left_join(projection_lookup(baseline_drivers, baseline_constructors, "BaselineEvaluation"), by = c("AssetType", "Name")) %>%
      mutate(
        ChatterEvaluation = ChatterEvaluation * if_else(Slot == "CPT", 1.5, 1),
        BaselineEvaluation = BaselineEvaluation * if_else(Slot == "CPT", 1.5, 1)
      ) %>%
      group_by(CandidateID) %>%
      mutate(
        `Chatter portfolio projection` = sum(ChatterEvaluation, na.rm = TRUE),
        `Baseline portfolio projection` = sum(BaselineEvaluation, na.rm = TRUE),
        `Median model projection` = (`Chatter portfolio projection` + `Baseline portfolio projection`) / 2,
        `Model projection spread` = abs(`Chatter portfolio projection` - `Baseline portfolio projection`),
        `Robust projection` = `Median model projection` - 0.25 * `Model projection spread`,
        `Model ceiling projection` = pmax(`Chatter portfolio projection`, `Baseline portfolio projection`)
      ) %>%
      ungroup() %>%
      select(-AssetType, -ChatterEvaluation, -BaselineEvaluation)
  })

  fantasy_combined_portfolio <- reactive({
    candidates <- fantasy_combined_candidates()
    groups <- split(candidates, candidates$CandidateID)
    driver_salaries <- candidates %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      transmute(Name, BaseSalary = if_else(Slot == "CPT", Salary / 1.5, Salary)) %>%
      group_by(Name) %>% summarise(BaseSalary = median(BaseSalary), .groups = "drop")
    premium_cutoff <- quantile(driver_salaries$BaseSalary, 0.75, na.rm = TRUE, names = FALSE)
    value_cutoff <- quantile(driver_salaries$BaseSalary, 0.25, na.rm = TRUE, names = FALSE)
    premium_drivers <- driver_salaries$Name[driver_salaries$BaseSalary >= premium_cutoff]
    bottom_value_drivers <- driver_salaries$Name[driver_salaries$BaseSalary <= value_cutoff]
    trusted_value_captains <- candidates %>%
      filter(str_detect(Lineup, "^H\\s"), Slot == "CPT") %>%
      pull(Name) %>% unique()
    value_drivers <- setdiff(bottom_value_drivers, trusted_value_captains)
    meta <- bind_rows(lapply(names(groups), function(id) {
      x <- groups[[id]]
      tibble(
        CandidateID = id,
        Source = first(x$Source),
        Candidate = first(x$Lineup),
        Scenario = first(x$Scenario),
        ScenarioKey = str_extract(first(x$Lineup), "^[A-H]"),
        SourceProjection = first(x$total_projection),
        RobustProjection = first(x$`Robust projection`),
        MedianProjection = first(x$`Median model projection`),
        CeilingProjection = first(x$`Model ceiling projection`),
        Captain = x$Name[x$Slot == "CPT"][1],
        Constructor = x$Name[x$Slot == "CON"][1],
        Drivers = list(unique(x$Name[x$Slot %in% c("CPT", "DRV")])),
        Flex = list(unique(x$Name[x$Slot == "DRV"])),
        Roster = list(c(unique(x$Name[x$Slot %in% c("CPT", "DRV")]), x$Name[x$Slot == "CON"][1]))
      )
    }))
    portfolio_size <- 8L
    validate(need(nrow(meta) >= portfolio_size, "Too few Baseline/Chatter candidates are available."))
    combinations <- utils::combn(seq_len(nrow(meta)), portfolio_size)
    best <- integer()
    best_score <- -Inf
    for (column in seq_len(ncol(combinations))) {
      indexes <- combinations[, column]
      selected <- meta[indexes, ]
      if (n_distinct(selected$ScenarioKey) != portfolio_size) next
      if (!setequal(selected$ScenarioKey, LETTERS[1:8])) next
      source_counts <- table(selected$Source)
      if (any(!c("Baseline", "Chatter") %in% names(source_counts))) next
      if (any(source_counts[c("Baseline", "Chatter")] < 3L)) next
      if (max(table(selected$Captain)) > 2L) next
      if (max(table(selected$Constructor)) > 4L) next
      if (n_distinct(selected$Captain) < 5L) next
      if (n_distinct(selected$Constructor) < 3L) next
      driver_counts <- table(unlist(selected$Drivers, use.names = FALSE))
      if (length(intersect(names(driver_counts[driver_counts > 6L]), premium_drivers)) > 0L) next
      if (length(intersect(names(driver_counts[driver_counts > 3L]), value_drivers)) > 0L) next
      pairs <- utils::combn(seq_along(indexes), 2L)
      shared <- apply(pairs, 2, function(pair) length(intersect(selected$Roster[[pair[1]]], selected$Roster[[pair[2]]])))
      near_duplicate <- apply(pairs, 2, function(pair) {
        selected$Captain[pair[1]] == selected$Captain[pair[2]] &&
          selected$Constructor[pair[1]] == selected$Constructor[pair[2]] &&
          length(intersect(selected$Flex[[pair[1]]], selected$Flex[[pair[2]]])) >= 3L
      })
      if (any(near_duplicate)) next
      preferred_source <- if_else(selected$ScenarioKey %in% c("C", "F", "H"), "Chatter", "Baseline")
      source_role_fit <- sum(selected$Source == preferred_source)
      premium_expansion_fit <- sum(selected$ScenarioKey == "F" & selected$Source == "Chatter")
      driver_concentration_penalty <- sum(pmax(as.numeric(driver_counts) - 4, 0)^2)
      constructor_counts <- table(selected$Constructor)
      constructor_concentration_penalty <- sum(pmax(as.numeric(constructor_counts) - 3, 0)^2)
      score <- sum(selected$RobustProjection) +
        0.15 * sum(selected$SourceProjection) +
        0.05 * sum(selected$CeilingProjection) +
        2 * source_role_fit +
        15 * premium_expansion_fit +
        1.5 * n_distinct(selected$Captain) +
        n_distinct(selected$Constructor) -
        20 * driver_concentration_penalty -
        15 * constructor_concentration_penalty -
        0.35 * sum(shared)
      if (score > best_score) {
        best <- indexes
        best_score <- score
      }
    }
    validate(need(length(best) == portfolio_size, "No recommended portfolio satisfies the captain, constructor, source-balance, value-exposure, and near-duplicate safeguards."))
    selected <- meta[best, ]
    selected$AverageSharedSelections <- vapply(seq_len(nrow(selected)), function(i) {
      others <- setdiff(seq_len(nrow(selected)), i)
      mean(vapply(others, function(j) length(intersect(selected$Roster[[i]], selected$Roster[[j]])), numeric(1)))
    }, numeric(1))
    subset_score <- function(indexes) {
      x <- selected[indexes, ]
      pairs <- if (length(indexes) >= 2L) utils::combn(seq_along(indexes), 2L) else matrix(integer(), nrow = 2L)
      shared <- if (ncol(pairs) > 0L) {
        apply(pairs, 2, function(pair) length(intersect(x$Roster[[pair[1]]], x$Roster[[pair[2]]])))
      } else {
        0
      }
      driver_counts <- table(unlist(x$Drivers, use.names = FALSE))
      constructor_counts <- table(x$Constructor)
      sum(x$RobustProjection) +
        0.10 * sum(x$CeilingProjection - x$RobustProjection) +
        4 * n_distinct(x$Captain) +
        8 * n_distinct(x$Constructor) -
        2.5 * sum(shared) -
        8 * sum(pmax(as.numeric(driver_counts) - 3, 0)^2) -
        10 * sum(pmax(as.numeric(constructor_counts) - 2, 0)^2)
    }
    choose_nested_subset <- function(fixed, target_size, rule_sets) {
      additions_needed <- target_size - length(fixed)
      available <- setdiff(seq_len(nrow(selected)), fixed)
      additions <- utils::combn(available, additions_needed)
      if (is.null(dim(additions))) additions <- matrix(additions, nrow = additions_needed)
      for (rules in rule_sets) {
        best_indexes <- integer()
        best_subset_score <- -Inf
        for (column in seq_len(ncol(additions))) {
          indexes <- c(fixed, additions[, column])
          x <- selected[indexes, ]
          driver_counts <- table(unlist(x$Drivers, use.names = FALSE))
          constructor_counts <- table(x$Constructor)
          if (n_distinct(x$Constructor) < rules$min_constructors) next
          if (n_distinct(x$Captain) < rules$min_captains) next
          if (max(driver_counts) > rules$max_driver) next
          if (max(constructor_counts) > rules$max_constructor) next
          candidate_score <- subset_score(indexes)
          if (candidate_score > best_subset_score) {
            best_indexes <- indexes
            best_subset_score <- candidate_score
          }
        }
        if (length(best_indexes) == target_size) return(best_indexes)
      }
      stop("No nested fantasy subset satisfies even the relaxed diversity safeguards.", call. = FALSE)
    }
    single_entry_score <- selected$RobustProjection +
      0.10 * (selected$CeilingProjection - selected$RobustProjection) -
      2 * selected$AverageSharedSelections
    best_one <- which.max(single_entry_score)
    best_three <- choose_nested_subset(
      best_one,
      3L,
      list(
        list(min_constructors = 3L, min_captains = 3L, max_driver = 2L, max_constructor = 1L),
        list(min_constructors = 3L, min_captains = 3L, max_driver = 3L, max_constructor = 1L),
        list(min_constructors = 2L, min_captains = 2L, max_driver = 3L, max_constructor = 2L)
      )
    )
    best_five <- choose_nested_subset(
      best_three,
      5L,
      list(
        list(min_constructors = 3L, min_captains = 4L, max_driver = 4L, max_constructor = 3L),
        list(min_constructors = 3L, min_captains = 3L, max_driver = 4L, max_constructor = 3L),
        list(min_constructors = 2L, min_captains = 3L, max_driver = 4L, max_constructor = 4L)
      )
    )
    order_additions <- function(fixed, additions) {
      ordered <- integer()
      while (length(additions) > 0L) {
        base <- c(fixed, ordered)
        incremental_scores <- vapply(additions, function(index) {
          shared_with_base <- if (length(base) == 0L) 0 else {
            sum(vapply(base, function(other) length(intersect(selected$Roster[[index]], selected$Roster[[other]])), numeric(1)))
          }
          new_captain <- as.numeric(!selected$Captain[[index]] %in% selected$Captain[base])
          new_constructor <- as.numeric(!selected$Constructor[[index]] %in% selected$Constructor[base])
          selected$RobustProjection[[index]] - 3 * shared_with_base + 5 * new_captain + 10 * new_constructor
        }, numeric(1))
        next_index <- additions[[which.max(incremental_scores)]]
        ordered <- c(ordered, next_index)
        additions <- setdiff(additions, next_index)
      }
      ordered
    }
    best_three_additions <- order_additions(best_one, setdiff(best_three, best_one))
    best_five_additions <- order_additions(best_three, setdiff(best_five, best_three))
    full_eight_additions <- order_additions(best_five, setdiff(seq_len(nrow(selected)), best_five))
    ordered_indexes <- c(best_one, best_three_additions, best_five_additions, full_eight_additions)
    ordered_ids <- selected$CandidateID[ordered_indexes]
    use_map <- tibble(
      CandidateID = ordered_ids,
      UseOrder = seq_along(ordered_ids),
      `Portfolio tier` = c(
        "Best single-entry",
        rep("Included in best three", 2L),
        rep("Added for best five", 2L),
        rep("Added for full eight", 3L)
      )
    ) %>%
      mutate(
        `Contest use` = case_when(
          UseOrder == 1L ~ "Single-entry; Entry 1 of every set",
          UseOrder <= 3L ~ "Best three-lineup set",
          UseOrder <= 5L ~ "Best five-lineup set",
          TRUE ~ "Full eight-lineup portfolio"
        ),
        `Portfolio role` = case_when(
          UseOrder == 1L ~ "Single-entry quality anchor",
          UseOrder <= 3L ~ "Best-three scenario diversifier",
          UseOrder <= 5L ~ "Best-five exposure diversifier",
          TRUE ~ "Full-portfolio scenario expansion"
        )
      )
    selected <- selected %>%
      left_join(use_map, by = "CandidateID") %>%
      arrange(UseOrder) %>%
      mutate(
        `Combined lineup` = paste0("Entry ", row_number()),
        SelectionScore = best_score
      )
    candidates %>%
      inner_join(selected %>% select(CandidateID, `Combined lineup`, Candidate, `Portfolio tier`, `Contest use`, `Portfolio role`, AverageSharedSelections, SelectionScore), by = "CandidateID") %>%
      arrange(as.integer(str_remove(`Combined lineup`, "Entry ")), factor(Slot, levels = c("CPT", "DRV", "CON")))
  })

  fantasy_portfolio_card <- function(label) {
    fantasy_portfolio_lineups() %>% filter(Lineup == label)
  }

  render_fantasy_card <- function(label) {
    rows <- fantasy_portfolio_card(label)
    validate(need(nrow(rows) > 0, "No lineup fits under the current cap and slot settings."))
    rows %>% transmute(
      Slot, Name, Constructor,
      Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
      Projection = format_num(Projection, 2),
      `Value / $1k` = format_num(Value, 2)
    )
  }

  render_fantasy_card_summary <- function(label) {
    rows <- fantasy_portfolio_card(label)
    validate(need(nrow(rows) > 0, "No lineup summary available."))
    rows %>% summarise(
      `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
      `Salary left` = paste0("$", format(round(as.numeric(input$fantasy_salary_cap) - first(total_salary), 0), big.mark = ",")),
      `Projected DK points` = format_num(first(total_projection), 2)
    )
  }

  output$fantasy_combined_summary_table <- renderTable({
    rows <- fantasy_combined_portfolio()
    validate(need(nrow(rows) > 0, "No recommended combined portfolio is available."))
    rows %>%
      group_by(`Combined lineup`, `Portfolio tier`, `Contest use`, Source, Candidate, `Portfolio role`) %>%
      summarise(
        Captain = Name[Slot == "CPT"][1],
        Constructor = Name[Slot == "CON"][1],
        `Robust projection` = format_num(first(`Robust projection`), 2),
        `Model median` = format_num(first(`Median model projection`), 2),
        `Model ceiling` = format_num(first(`Model ceiling projection`), 2),
        `Model spread` = format_num(first(`Model projection spread`), 2),
        `Avg shared selections` = format_num(first(AverageSharedSelections), 2),
        `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
        .groups = "drop"
      ) %>%
      arrange(as.integer(str_remove(`Combined lineup`, "Entry ")))
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_combined_table <- renderTable({
    rows <- fantasy_combined_portfolio()
    validate(need(nrow(rows) > 0, "No recommended combined portfolio is available."))
    rows %>% transmute(
      `Combined lineup`, `Portfolio tier`, `Contest use`, Source, Candidate, `Portfolio role`, Slot, Name, Constructor,
      Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
      Projection = format_num(Projection, 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_combined_download <- downloadHandler(
    filename = function() paste0("f1_fantasy_best1_best3_best5_full8_", input$fantasy_season, "_R", input$fantasy_round, ".csv"),
    content = function(file) {
      readr::write_csv(fantasy_combined_portfolio(), file, na = "")
    }
  )

  output$fantasy_single_lineup_table <- renderTable(render_fantasy_card("A — Highest-projected lineup"), striped = TRUE, hover = TRUE, bordered = FALSE)
  output$fantasy_single_lineup_summary <- renderTable(render_fantasy_card_summary("A — Highest-projected lineup"), striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_mode_note <- renderUI({
    mode <- if (isTRUE(input$fantasy_use_chatter)) "Chatter" else "Baseline (no chatter)"
    p(class = "hint", paste0("Candidate preview mode: ", mode, ". The preview download contains only this mode."))
  })

  output$fantasy_portfolio_table <- renderTable({
    rows <- fantasy_portfolio_lineups()
    validate(need(nrow(rows) > 0, "No portfolio lineups fit under the current cap and slot settings."))
    rows %>% transmute(
      Mode = Source, Lineup, Slot, Name, Constructor,
      Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
      Projection = format_num(Projection, 2),
      `Value / $1k` = format_num(Value, 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_summary_table <- renderTable({
    rows <- fantasy_portfolio_lineups()
    validate(need(nrow(rows) > 0, "No portfolio summaries available."))
    rows %>% group_by(Lineup) %>% summarise(
      Mode = first(Source),
      `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
      `Salary left` = paste0("$", format(round(as.numeric(input$fantasy_salary_cap) - first(total_salary), 0), big.mark = ",")),
      `Projected DK points` = format_num(first(total_projection), 2),
      Captain = first(Name[Slot == "CPT"]),
      Constructor = first(Name[Slot == "CON"]),
      .groups = "drop"
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_audit_table <- renderTable({
    rows <- fantasy_portfolio_lineups()
    validate(need(nrow(rows) > 0, "No portfolio audit available."))
    lineup_groups <- split(rows, rows$Lineup)
    exact_keys <- vapply(lineup_groups, function(x) paste(
      x$Name[x$Slot == "CPT"][1],
      paste(sort(x$Name[x$Slot %in% c("CPT", "DRV")]), collapse = "|"),
      x$Name[x$Slot == "CON"][1], sep = "::"
    ), character(1))
    pool_keys <- vapply(lineup_groups, function(x) paste(sort(x$Name[x$Slot %in% c("CPT", "DRV")]), collapse = "|"), character(1))
    roster_keys <- vapply(lineup_groups, function(x) paste(
      paste(sort(x$Name[x$Slot %in% c("CPT", "DRV")]), collapse = "|"),
      x$Name[x$Slot == "CON"][1], sep = "::"
    ), character(1))
    roster_sets <- lapply(lineup_groups, function(x) c(unique(x$Name[x$Slot %in% c("CPT", "DRV")]), x$Name[x$Slot == "CON"][1]))
    overlaps <- if (length(roster_sets) < 2L) numeric() else combn(seq_along(roster_sets), 2, function(indexes) {
      length(intersect(roster_sets[[indexes[1]]], roster_sets[[indexes[2]]])) / 6 * 100
    })
    constructor_counts <- table(rows$Name[rows$Slot == "CON"])
    driver_counts <- table(rows$Name[rows$Slot %in% c("CPT", "DRV")])
    max_driver_allowed <- max(1L, floor(8L * pmin(100, pmax(25, as.numeric(input$fantasy_driver_exposure %||% 75))) / 100))
    max_constructor_allowed <- max(1L, floor(8L * pmin(100, pmax(10, as.numeric(input$fantasy_constructor_exposure %||% 50))) / 100))
    safeguards_pass <- length(lineup_groups) == 8L &&
      length(unique(exact_keys)) == length(lineup_groups) &&
      (length(overlaps) == 0 || max(overlaps) <= (4 / 6 * 100 + 1e-8)) &&
      max(driver_counts) <= max_driver_allowed &&
      max(constructor_counts) <= max_constructor_allowed
    tibble(
      Metric = c("Lineups generated", "Exact-lineup count", "Unique six-piece roster count", "Unique five-driver core count", "Maximum six-piece overlap", "Average six-piece overlap", "Highest driver exposure", "Highest constructor exposure", "Diversification safeguards"),
      Value = c(
        paste0(length(lineup_groups), " of 8"),
        as.character(length(unique(exact_keys))),
        as.character(length(unique(roster_keys))),
        as.character(length(unique(pool_keys))),
        if (length(overlaps) == 0) "N/A" else paste0(format_num(max(overlaps), 1), "%"),
        if (length(overlaps) == 0) "N/A" else paste0(format_num(mean(overlaps), 1), "%"),
        paste0(format_num(max(driver_counts) / length(lineup_groups) * 100, 1), "%"),
        paste0(format_num(max(constructor_counts) / length(lineup_groups) * 100, 1), "%"),
        if (safeguards_pass) "Passed" else "Warning: one or more safeguards could not be satisfied"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_exposure_table <- renderTable({
    rows <- fantasy_portfolio_lineups()
    validate(need(nrow(rows) > 0, "No portfolio exposure data available."))
    lineup_count <- n_distinct(rows$Lineup)
    driver_exposure <- rows %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      group_by(Name) %>%
      summarise(Lineups = n_distinct(Lineup), `Captain lineups` = sum(Slot == "CPT"), .groups = "drop") %>%
      mutate(Type = "Driver", `Constructor lineups` = 0L)
    constructor_exposure <- rows %>%
      filter(Slot == "CON") %>%
      group_by(Name) %>%
      summarise(Lineups = n_distinct(Lineup), .groups = "drop") %>%
      mutate(Type = "Constructor", `Captain lineups` = 0L, `Constructor lineups` = Lineups)
    bind_rows(driver_exposure, constructor_exposure) %>%
      mutate(Exposure = paste0(format_num(Lineups / lineup_count * 100, 1), "%")) %>%
      arrange(Type, desc(Lineups), Name) %>%
      select(Type, Name, Lineups, Exposure, `Captain lineups`, `Constructor lineups`)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_overlap_table <- renderTable({
    rows <- fantasy_portfolio_lineups()
    validate(need(nrow(rows) > 0, "No pairwise overlap data available."))
    lineup_groups <- split(rows, rows$Lineup)
    validate(need(length(lineup_groups) >= 2L, "At least two lineups are required for overlap analysis."))
    pairs <- combn(seq_along(lineup_groups), 2)
    bind_rows(lapply(seq_len(ncol(pairs)), function(i) {
      left_name <- names(lineup_groups)[pairs[1, i]]
      right_name <- names(lineup_groups)[pairs[2, i]]
      left <- lineup_groups[[pairs[1, i]]]
      right <- lineup_groups[[pairs[2, i]]]
      left_set <- c(unique(left$Name[left$Slot %in% c("CPT", "DRV")]), left$Name[left$Slot == "CON"][1])
      right_set <- c(unique(right$Name[right$Slot %in% c("CPT", "DRV")]), right$Name[right$Slot == "CON"][1])
      shared <- sort(intersect(left_set, right_set))
      tibble(
        `Lineup 1` = left_name,
        `Lineup 2` = right_name,
        `Shared selections` = length(shared),
        Overlap = paste0(format_num(length(shared) / 6 * 100, 1), "%"),
        `Shared names` = paste(shared, collapse = ", ")
      )
    })) %>% arrange(desc(`Shared selections`), `Lineup 1`, `Lineup 2`)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_strategy_table <- renderTable({
    tibble(
      Strategy = c("Favorite constructor dominates", "Premium captain pivot", "Rival constructor leverage", "Chaos, attrition, or place differential", "Value captain unlocks premium pieces"),
      `What it protects against` = c("Both cars score", "Winner scores while teammate fails", "Favorite team underperforms", "Penalties, safety cars, or DNFs", "Premium captain plus elite flex combination"),
      `Portfolio expression` = c("E — team-dominance variant", "B — premium-captain pivot", "C — leverage variant", "D — recovery variant", "H — salary-architecture variant")
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_portfolio_download <- downloadHandler(
    filename = function() {
      mode <- if (isTRUE(input$fantasy_use_chatter)) "chatter" else "baseline"
      paste0("f1_fantasy_rosters_", input$fantasy_season, "_R", input$fantasy_round, "_", mode, ".csv")
    },
    content = function(file) {
      rows <- fantasy_portfolio_lineups()
      readr::write_csv(rows, file, na = "")
    }
  )

  output$fantasy_header <- renderUI({
    rows <- fantasy_driver_projections()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    salary_note <- if (any(rows$salary_source == "DraftKings", na.rm = TRUE)) {
      "Official DraftKings salaries are loaded for this slate; captain salary and scoring are 1.5×."
    } else {
      "Estimated salaries are shown until the official DraftKings slate is supplied."
    }
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p(paste("DraftKings projection uses the locked Fantasy consensus recipe so controls on other tabs cannot silently change the rosters; finish magnitudes use the default finish models with rolling form as fallback.", salary_note))
      )
    )
  })

  output$fantasy_lineup_table <- renderTable({
    rows <- fantasy_best_lineup()
    validate(need(nrow(rows) > 0, "No lineup fits under the current cap and slot settings."))
    rows %>%
      transmute(
        Slot,
        Name,
        Constructor,
        Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
        Projection = format_num(Projection, 2),
        `Value / $1k` = format_num(Value, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_lineup_summary_table <- renderTable({
    rows <- fantasy_best_lineup()
    validate(need(nrow(rows) > 0, "No lineup fits under the current cap and slot settings."))
    rows %>%
      summarise(
        `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
        `Salary left` = paste0("$", format(round(as.numeric(input$fantasy_salary_cap) - first(total_salary), 0), big.mark = ",")),
        `Projected DK points` = format_num(first(total_projection), 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_alt_lineup_table <- renderTable({
    rows <- fantasy_alt_lineup()
    validate(need(nrow(rows) > 0, "No alternate lineup fits under the current cap and slot settings."))
    rows %>%
      transmute(
        Slot,
        Name,
        Constructor,
        Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
        Projection = format_num(Projection, 2),
        `Value / $1k` = format_num(Value, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_alt_lineup_summary_table <- renderTable({
    rows <- fantasy_alt_lineup()
    validate(need(nrow(rows) > 0, "No alternate lineup fits under the current cap and slot settings."))
    rows %>%
      summarise(
        `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
        `Salary left` = paste0("$", format(round(as.numeric(input$fantasy_salary_cap) - first(total_salary), 0), big.mark = ",")),
        `Projected DK points` = format_num(first(total_projection), 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy_driver_table <- renderTable({
    fantasy_driver_projections() %>%
      transmute(
        Rank = fantasy_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        `D salary` = paste0("$", format(round(mock_salary, 0), big.mark = ",")),
        `CPT salary` = paste0("$", format(round(mock_salary * 1.5, 0), big.mark = ",")),
        `Proj DK` = format_num(fantasy_projection, 2),
        `Value / $1k` = format_num(value_per_1k, 2),
        `Proj finish` = format_num(projected_finish, 2),
        `Proj start` = format_num(projected_start, 2),
        `Finish DK` = format_num(dk_finish_points, 2),
        `Place diff` = format_num(dk_place_diff, 2),
        `Classified DK` = format_num(dk_classified_points, 2),
        `Proj led laps` = format_num(projected_laps_led, 2),
        `Led lap DK` = format_num(dk_laps_led_points, 2),
        `Fastest lap DK` = format_num(dk_fastest_lap_points, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  fantasy_driver_download_rows <- reactive({
    driver_rows <- fantasy_driver_projections()
    family_rows <- if (isTRUE(input$fantasy_use_chatter)) {
      build_chatter_adjusted_allmodel_family_ranks(
        input$fantasy_season,
        input$fantasy_round,
        as.numeric(input$fantasy_chatter_strength %||% 50) / 100,
        TRUE
      )
    } else {
      build_allmodel_family_consensus_ranks(input$fantasy_season, input$fantasy_round)
    }

    family_rank_lookup <- family_rows %>%
      transmute(
        driver_code,
        family = recode(
          family,
          "Finish model" = "finish_family_rank",
          "Probability model" = "probability_family_rank",
          "Points model" = "points_family_rank",
          "Routed specialist finish" = "routed_finish_rank",
          "Routed specialist probability" = "routed_probability_rank",
          "Routed specialist points" = "routed_points_rank",
          .default = str_replace_all(str_to_lower(family), "[^a-z0-9]+", "_")
        ),
        family_rank = as.numeric(winner_family_rank)
      ) %>%
      distinct(driver_code, family, .keep_all = TRUE) %>%
      pivot_wider(names_from = family, values_from = family_rank)

    required_family_rank_cols <- c(
      "finish_family_rank",
      "probability_family_rank",
      "points_family_rank",
      "routed_finish_rank",
      "routed_probability_rank",
      "routed_points_rank"
    )
    for (rank_col in setdiff(required_family_rank_cols, names(family_rank_lookup))) {
      family_rank_lookup[[rank_col]] <- NA_real_
    }

    actual_rows <- stage1 %>%
      filter(
        season == as.integer(input$fantasy_season),
        round == as.integer(input$fantasy_round),
        !is.na(finish_position)
      ) %>%
      transmute(
        season,
        round,
        driver_code,
        actual_start = as.numeric(grid),
        actual_finish = as.numeric(finish_position),
        actual_classified = classified_finish %in% TRUE,
        actual_laps_led = coalesce(as.numeric(laps_led), 0),
        actual_fastest_lap_rank = as.numeric(fastest_rank),
        actual_race_laps = as.numeric(race_laps),
        constructor_name
      ) %>%
      group_by(season, round, constructor_name) %>%
      mutate(
        actual_constructor_finish_rank = rank(actual_finish, ties.method = "first"),
        actual_teammate_bonus = if_else(n() >= 2L & actual_constructor_finish_rank == 1L, 5, 0)
      ) %>%
      ungroup() %>%
      group_by(season, round) %>%
      mutate(
        actual_recorded_laps_led = sum(actual_laps_led, na.rm = TRUE),
        actual_unassigned_leader_laps = pmax(0, max(actual_race_laps, na.rm = TRUE) - actual_recorded_laps_led),
        actual_data_quality_note = if_else(
          actual_unassigned_leader_laps > 0,
          paste0(actual_unassigned_leader_laps, " race lap(s) have no leader assigned in the race-results source."),
          ""
        )
      ) %>%
      ungroup() %>%
      mutate(
        actual_finish_points = fantasy_finish_points(actual_finish),
        actual_place_diff = actual_start - actual_finish,
        actual_classified_points = if_else(actual_classified, 1, 0),
        actual_laps_led_points = actual_laps_led * 0.25,
        actual_fastest_lap_points = if_else(actual_fastest_lap_rank == 1, 3, 0, missing = 0),
        actual_fantasy_points = actual_finish_points +
          actual_place_diff +
          actual_classified_points +
          actual_laps_led_points +
          actual_fastest_lap_points +
          actual_teammate_bonus
      ) %>%
      select(-constructor_name, -actual_constructor_finish_rank)

    driver_rows %>%
      left_join(family_rank_lookup, by = "driver_code") %>%
      left_join(actual_rows, by = c("season", "round", "driver_code")) %>%
      transmute(
        season,
        round,
        race_name,
        chatter_overlay = isTRUE(input$fantasy_use_chatter),
        consensus_mode = "family",
        include_finish_family = TRUE,
        include_probability_family = TRUE,
        include_points_family = TRUE,
        include_routed_specialists = TRUE,
        consensus_families = fantasy_consensus_families,
        finish_magnitude_source = "Selected XGB finish models; family selections determine consensus order",
        finish_models_used = paste(xgb_finish_default_models, collapse = " | "),
        probability_models_used = paste(xgb_probability_default_models, collapse = " | "),
        points_models_used = paste(xgb_points_default_models, collapse = " | "),
        fantasy_rank,
        driver_code,
        driver_name,
        constructor_name,
        consensus_rank = fantasy_consensus_rank,
        finish_family_rank,
        probability_family_rank,
        points_family_rank,
        routed_finish_rank,
        routed_probability_rank,
        routed_points_rank,
        raw_model_finish = model_finish,
        raw_model_f1_points = model_points,
        projected_start,
        projected_finish,
        projected_f1_points,
        dk_finish_points,
        dk_place_diff,
        dk_classified_points,
        projected_laps_led,
        dk_laps_led_points,
        projected_fastest_lap_probability,
        dk_fastest_lap_points,
        dk_teammate_bonus,
        dk_base_projection,
        fantasy_projection,
        actual_start,
        actual_finish,
        actual_finish_points,
        actual_place_diff,
        actual_classified_points,
        actual_laps_led,
        actual_laps_led_points,
        actual_fastest_lap_rank,
        actual_fastest_lap_points,
        actual_teammate_bonus,
        actual_fantasy_points,
        actual_unassigned_leader_laps,
        actual_data_quality_note,
        finish_points_error = dk_finish_points - actual_finish_points,
        place_diff_error = dk_place_diff - actual_place_diff,
        classified_points_error = dk_classified_points - actual_classified_points,
        laps_led_points_error = dk_laps_led_points - actual_laps_led_points,
        fastest_lap_points_error = dk_fastest_lap_points - actual_fastest_lap_points,
        teammate_bonus_error = dk_teammate_bonus - actual_teammate_bonus,
        total_projection_error = fantasy_projection - actual_fantasy_points,
        driver_salary = mock_salary,
        captain_salary = mock_salary * 1.5,
        salary_source,
        value_per_1k,
        projection_source
      ) %>%
      arrange(fantasy_rank, driver_name)
  })

  output$fantasy_driver_download <- downloadHandler(
    filename = function() {
      rows <- fantasy_driver_projections()
      race_slug <- rows %>%
        distinct(race_name) %>%
        slice(1) %>%
        pull(race_name) %>%
        str_to_lower() %>%
        str_replace_all("[^a-z0-9]+", "_") %>%
        str_replace_all("^_|_$", "")
      overlay_slug <- if (isTRUE(input$fantasy_use_chatter)) "chatter" else "no_chatter"
      paste0("f1_fantasy_driver_projections_", input$fantasy_season, "_round_", input$fantasy_round, "_", race_slug, "_", overlay_slug, ".csv")
    },
    content = function(file) {
      write_csv(fantasy_driver_download_rows(), file, na = "")
    }
  )

  output$fantasy_constructor_table <- renderTable({
    fantasy_constructor_projections() %>%
      transmute(
        Rank = constructor_rank,
        Constructor = constructor_name,
        Drivers = driver_count,
        Salary = paste0("$", format(round(mock_salary, 0), big.mark = ",")),
        `Salary source` = salary_source,
        `Proj DK` = format_num(fantasy_projection, 2),
        `Finish DK` = format_num(constructor_finish_points, 2),
        `Classified DK` = format_num(constructor_classified_points, 2),
        `Double top-10 DK` = format_num(constructor_double_top10_points, 2),
        `Double podium DK` = format_num(constructor_double_podium_points, 2),
        `Led lap DK` = format_num(constructor_laps_led_points, 2),
        `Fastest lap DK` = format_num(constructor_fastest_lap_points, 2),
        `Value / $1k` = format_num(value_per_1k, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_race_selector <- renderUI({
    req(input$fantasy2_season)
    choices <- rf_race_choices %>% filter(season == as.integer(input$fantasy2_season))
    if (nrow(choices) == 0) choices <- race_choices %>% filter(season == as.integer(input$fantasy2_season))
    selectInput("fantasy2_round", "Race", choices = setNames(choices$round, choices$label), selected = default_race_round(choices))
  })

  fantasy2_mode_consensus_predictions <- function(use_chatter) {
    req(input$fantasy2_season, input$fantasy2_round)
    family_rows <- if (isTRUE(use_chatter)) {
      build_chatter_adjusted_allmodel_family_ranks(
        input$fantasy2_season,
        input$fantasy2_round,
        as.numeric(input$fantasy2_chatter_strength %||% 50) / 100,
        TRUE,
        fantasy_locked_consensus_selection
      )
    } else {
      build_allmodel_family_consensus_ranks(
        input$fantasy2_season,
        input$fantasy2_round,
        fantasy_locked_consensus_selection
      )
    }
    if (nrow(family_rows) == 0) return(tibble())
    build_allmodel_predictions_from_family_rows(family_rows, TRUE)
  }

  fantasy2_mode_driver_projections <- function(use_chatter) {
    consensus_rows <- fantasy2_mode_consensus_predictions(use_chatter)
    if (nrow(consensus_rows) == 0) return(tibble())
    fantasy_projection_rows(
      input$fantasy2_season,
      input$fantasy2_round,
      6L,
      isTRUE(use_chatter),
      FALSE,
      consensus_rows
    )
  }

  fantasy2_driver_projections <- reactive({
    rows <- fantasy2_mode_driver_projections(isTRUE(input$fantasy2_use_chatter))
    validate(need(nrow(rows) > 0, "No fantasy projection rows found for this race."))
    rows
  })

  fantasy2_constructor_projections <- reactive({
    fantasy_constructor_rows(fantasy2_driver_projections())
  })

  fantasy2_percentile <- function(values) {
    values <- as.numeric(values)
    if (length(values) <= 1L || n_distinct(values[is.finite(values)]) <= 1L) return(rep(1, length(values)))
    dplyr::percent_rank(values)
  }

  fantasy2_strategy_inputs <- reactive({
    chatter_drivers <- fantasy2_mode_driver_projections(TRUE)
    baseline_drivers <- fantasy2_mode_driver_projections(FALSE)
    validate(need(nrow(chatter_drivers) > 0 && nrow(baseline_drivers) > 0, "Both Baseline and Chatter projections are required."))
    chatter_constructors <- fantasy_constructor_rows(chatter_drivers)
    baseline_constructors <- fantasy_constructor_rows(baseline_drivers)

    robust_constructors <- full_join(
      chatter_constructors %>% select(constructor_name, mock_salary, salary_source, chatter_projection = fantasy_projection),
      baseline_constructors %>% select(constructor_name, baseline_projection = fantasy_projection),
      by = "constructor_name"
    ) %>%
      mutate(
        median_projection = rowMeans(cbind(chatter_projection, baseline_projection), na.rm = TRUE),
        projection_spread = abs(chatter_projection - baseline_projection),
        robust_projection = median_projection - 0.25 * projection_spread,
        robust_value = robust_projection / mock_salary * 1000,
        projection_rank = min_rank(desc(robust_projection)),
        value_rank = min_rank(desc(robust_value)),
        projection_percentile = fantasy2_percentile(robust_projection),
        value_percentile = fantasy2_percentile(robust_value),
        fit_score = 100 * (0.58 * projection_percentile + 0.42 * value_percentile),
        quality_gate = projection_rank <= 5L & robust_projection >= 0.60 * max(robust_projection, na.rm = TRUE)
      ) %>%
      arrange(desc(fit_score), desc(robust_projection), constructor_name)

    max_constructor_entries <- max(1L, floor(8L * pmin(100, pmax(10, as.numeric(input$fantasy2_constructor_exposure %||% 50))) / 100))
    minimum_constructor_pool <- ceiling(8L / max_constructor_entries)
    quality_names <- robust_constructors %>% filter(quality_gate) %>% pull(constructor_name)
    if (length(quality_names) < minimum_constructor_pool) {
      quality_names <- robust_constructors %>% slice_head(n = minimum_constructor_pool) %>% pull(constructor_name)
    }
    anchor_constructor_names <- robust_constructors %>%
      filter(str_detect(str_to_lower(constructor_name), "mercedes|ferrari|mclaren")) %>%
      pull(constructor_name)
    fit_pool_names <- robust_constructors %>%
      filter(constructor_name %in% quality_names) %>%
      slice_head(n = max(4L, minimum_constructor_pool)) %>%
      pull(constructor_name)
    primary_constructor_name <- first(fit_pool_names)
    alternate_slots <- 8L - min(8L, max_constructor_entries)
    projected_alternates <- robust_constructors %>%
      filter(constructor_name != .env$primary_constructor_name) %>%
      arrange(projection_rank, desc(robust_projection)) %>%
      pull(constructor_name)
    required_anchors <- setdiff(anchor_constructor_names, primary_constructor_name)
    alternate_pool_names <- unique(c(required_anchors, projected_alternates))
    alternate_pool_names <- alternate_pool_names[seq_len(min(alternate_slots, length(alternate_pool_names)))]
    portfolio_pool_names <- unique(c(primary_constructor_name, alternate_pool_names))
    robust_constructors <- robust_constructors %>%
      mutate(
        portfolio_pool = constructor_name %in% portfolio_pool_names,
        primary_constructor = constructor_name == primary_constructor_name
      )

    robust_drivers <- full_join(
      chatter_drivers %>% select(driver_name, constructor_name, mock_salary, salary_source, chatter_projection = fantasy_projection),
      baseline_drivers %>% select(driver_name, baseline_projection = fantasy_projection),
      by = "driver_name"
    ) %>%
      mutate(
        median_projection = rowMeans(cbind(chatter_projection, baseline_projection), na.rm = TRUE),
        projection_spread = abs(chatter_projection - baseline_projection),
        robust_projection = median_projection - 0.25 * projection_spread,
        robust_value = robust_projection / mock_salary * 1000,
        projection_rank = min_rank(desc(robust_projection)),
        value_rank = min_rank(desc(robust_value))
      )
    flex_only_names <- robust_drivers %>%
      arrange(desc(mock_salary), desc(robust_projection), driver_name) %>%
      slice_head(n = 2L) %>%
      pull(driver_name)
    captain_pool <- robust_drivers %>%
      filter(!driver_name %in% flex_only_names, projection_rank <= 8L) %>%
      mutate(
        projection_percentile = fantasy2_percentile(robust_projection),
        value_percentile = fantasy2_percentile(robust_value),
        captain_fit_score = 100 * (0.55 * projection_percentile + 0.45 * value_percentile)
      ) %>%
      arrange(desc(captain_fit_score), desc(robust_projection), driver_name) %>%
      slice_head(n = 6L)
    robust_drivers <- robust_drivers %>%
      left_join(captain_pool %>% select(driver_name, captain_fit_score), by = "driver_name") %>%
      mutate(
        captain_status = case_when(
          driver_name %in% flex_only_names ~ "Flex only — top-two salary",
          !is.na(captain_fit_score) ~ "Captain pool",
          TRUE ~ "Outside captain pool"
        )
      ) %>%
      arrange(projection_rank, driver_name)

    list(
      constructors = robust_constructors,
      drivers = robust_drivers,
      primary_constructor = first(portfolio_pool_names),
      captain_names = captain_pool$driver_name,
      flex_only_names = flex_only_names
    )
  })

  fantasy2_generate_portfolio <- function(drivers, constructors, source_label, strategy) {
    portfolio_size <- 8L
    labels <- c(
      "A — Required constructor anchor 1",
      "B — Required constructor anchor 2",
      "C — Required constructor anchor 3",
      "D — Projected constructor coverage",
      "E — Primary-constructor sweet spot",
      "F — Primary-constructor value captain",
      "G — Primary-constructor stud flex",
      "H — Primary-constructor diversification"
    )
    max_driver_entries <- max(1L, floor(portfolio_size * pmin(100, pmax(25, as.numeric(input$fantasy2_driver_exposure %||% 75))) / 100))
    max_constructor_entries <- max(1L, floor(portfolio_size * pmin(100, pmax(10, as.numeric(input$fantasy2_constructor_exposure %||% 50))) / 100))
    primary_target <- min(portfolio_size, max_constructor_entries)
    constructor_names <- strategy$constructors %>%
      filter(portfolio_pool) %>%
      arrange(desc(primary_constructor), desc(fit_score), desc(robust_projection)) %>%
      pull(constructor_name)
    primary_constructor <- strategy$primary_constructor
    alternate_constructors <- setdiff(constructor_names, primary_constructor)
    if (primary_target < portfolio_size && length(alternate_constructors) == 0L) return(tibble())
    required_alternate_anchors <- strategy$constructors %>%
      filter(
        constructor_name != .env$primary_constructor,
        str_detect(str_to_lower(constructor_name), "mercedes|ferrari|mclaren")
      ) %>%
      arrange(projection_rank, desc(robust_projection)) %>%
      pull(constructor_name)
    alternate_by_projection <- strategy$constructors %>%
      filter(constructor_name != .env$primary_constructor) %>%
      arrange(projection_rank, desc(robust_projection)) %>%
      pull(constructor_name)
    alternate_slots <- portfolio_size - primary_target
    alternate_targets <- unique(c(required_alternate_anchors, alternate_by_projection))
    alternate_targets <- alternate_targets[seq_len(min(alternate_slots, length(alternate_targets)))]
    if (length(alternate_targets) < alternate_slots) {
      alternate_targets <- rep(alternate_targets, length.out = alternate_slots)
    }
    constructor_targets <- c(
      if (primary_target < portfolio_size) alternate_targets else character(),
      rep(primary_constructor, primary_target)
    )
    captain_names <- strategy$captain_names
    if (length(captain_names) == 0L || length(strategy$flex_only_names) == 0L) return(tibble())
    captain_targets <- rep(captain_names, length.out = portfolio_size)
    portfolio <- tibble()

    for (lineup_index in seq_len(portfolio_size)) {
      constructor_counts <- if (nrow(portfolio) == 0L) integer() else table(portfolio$Name[portfolio$Slot == "CON"])
      captain_counts <- if (nrow(portfolio) == 0L) integer() else table(portfolio$Name[portfolio$Slot == "CPT"])
      driver_counts <- if (nrow(portfolio) == 0L) integer() else table(portfolio$Name[portfolio$Slot %in% c("CPT", "DRV")])
      preferred_constructor <- constructor_targets[[lineup_index]]
      constructor_options <- preferred_constructor
      constructor_options <- constructor_options[vapply(constructor_options, function(name) {
        count <- if (name %in% names(constructor_counts)) as.integer(constructor_counts[[name]]) else 0L
        count < max_constructor_entries
      }, logical(1))]
      preferred_captain <- captain_targets[[lineup_index]]
      captain_options <- c(preferred_captain, setdiff(captain_names, preferred_captain))
      captain_options <- captain_options[vapply(captain_options, function(name) {
        count <- if (name %in% names(captain_counts)) as.integer(captain_counts[[name]]) else 0L
        count < 2L
      }, logical(1))]
      driver_exclusions <- names(driver_counts[driver_counts >= max_driver_entries])
      captain_options <- setdiff(captain_options, driver_exclusions)
      preferred_elite_flex <- strategy$flex_only_names[[((lineup_index - 1L) %% length(strategy$flex_only_names)) + 1L]]
      elite_flex_options <- c(preferred_elite_flex, setdiff(strategy$flex_only_names, preferred_elite_flex))
      elite_flex_options <- setdiff(elite_flex_options, driver_exclusions)
      if (length(constructor_options) == 0L || length(captain_options) == 0L || length(elite_flex_options) == 0L) {
        return(portfolio)
      }

      candidates <- list()
      candidate_number <- 0L
      for (constructor_name in constructor_options) {
        for (captain_name in captain_options) {
          for (elite_flex_name in elite_flex_options) {
            lineup <- optimize_fantasy_lineup(
              drivers,
              constructors,
              input$fantasy2_salary_cap,
              input$fantasy2_flex_count,
              TRUE,
              captain_preference = "any",
              excluded_drivers = union(driver_exclusions, setdiff(strategy$flex_only_names, elite_flex_name)),
              required_captains = captain_name,
              required_any_drivers = elite_flex_name,
              required_constructors = constructor_name,
              previous_lineups = portfolio,
              min_major_changes = as.integer(input$fantasy2_min_major_changes %||% 2L),
              min_driver_replacements = 1L,
              max_shared_roster_components = 4L
            )
            if (nrow(lineup) == 0L) next
            captain_fit <- strategy$drivers$captain_fit_score[match(captain_name, strategy$drivers$driver_name)]
            constructor_fit <- strategy$constructors$fit_score[match(constructor_name, strategy$constructors$constructor_name)]
            elite_rotation_bonus <- if (identical(elite_flex_name, preferred_elite_flex)) 100 else 0
            candidate_number <- candidate_number + 1L
            candidates[[candidate_number]] <- lineup %>%
              mutate(
                SelectionFit = first(total_projection) + 0.02 * coalesce(captain_fit, 0) + 0.02 * coalesce(constructor_fit, 0) + elite_rotation_bonus,
                CaptainFit = coalesce(captain_fit, 0),
                ConstructorFit = coalesce(constructor_fit, 0)
              )
          }
        }
      }
      if (length(candidates) == 0L) return(portfolio)
      best_index <- which.max(vapply(candidates, function(x) first(x$SelectionFit), numeric(1)))
      selected <- candidates[[best_index]] %>%
        mutate(Lineup = labels[[lineup_index]], Scenario = labels[[lineup_index]], Source = source_label, .before = 1)
      portfolio <- bind_rows(portfolio, selected)
    }
    portfolio
  }

  fantasy2_portfolio_lineups <- reactive({
    strategy <- fantasy2_strategy_inputs()
    rows <- fantasy2_generate_portfolio(
      fantasy2_driver_projections(),
      fantasy2_constructor_projections(),
      if (isTRUE(input$fantasy2_use_chatter)) "Chatter" else "Baseline",
      strategy
    )
    validate(need(n_distinct(rows$Lineup) == 8L, "The experimental rules could not produce eight valid lineups under the current cap and exposure settings."))
    rows
  })

  fantasy2_combined_candidates <- reactive({
    strategy <- fantasy2_strategy_inputs()
    chatter_drivers <- fantasy2_mode_driver_projections(TRUE)
    baseline_drivers <- fantasy2_mode_driver_projections(FALSE)
    chatter_constructors <- fantasy_constructor_rows(chatter_drivers)
    baseline_constructors <- fantasy_constructor_rows(baseline_drivers)
    candidate_rows <- bind_rows(
      fantasy2_generate_portfolio(chatter_drivers, chatter_constructors, "Chatter", strategy),
      fantasy2_generate_portfolio(baseline_drivers, baseline_constructors, "Baseline", strategy)
    ) %>% mutate(CandidateID = paste(Source, Lineup, sep = "::"), .before = 1)
    validate(need(n_distinct(candidate_rows$CandidateID) == 16L, "Both projection modes must produce all eight experimental candidates."))
    projection_lookup <- function(driver_rows, constructor_rows, value_name) {
      bind_rows(
        driver_rows %>% transmute(AssetType = "Driver", Name = driver_name, Evaluation = fantasy_projection),
        constructor_rows %>% transmute(AssetType = "Constructor", Name = constructor_name, Evaluation = fantasy_projection)
      ) %>% rename(!!value_name := Evaluation)
    }
    candidate_rows %>%
      mutate(AssetType = if_else(Slot == "CON", "Constructor", "Driver")) %>%
      left_join(projection_lookup(chatter_drivers, chatter_constructors, "ChatterEvaluation"), by = c("AssetType", "Name")) %>%
      left_join(projection_lookup(baseline_drivers, baseline_constructors, "BaselineEvaluation"), by = c("AssetType", "Name")) %>%
      mutate(
        ChatterEvaluation = ChatterEvaluation * if_else(Slot == "CPT", 1.5, 1),
        BaselineEvaluation = BaselineEvaluation * if_else(Slot == "CPT", 1.5, 1)
      ) %>%
      group_by(CandidateID) %>%
      mutate(
        `Chatter portfolio projection` = sum(ChatterEvaluation, na.rm = TRUE),
        `Baseline portfolio projection` = sum(BaselineEvaluation, na.rm = TRUE),
        `Median model projection` = (`Chatter portfolio projection` + `Baseline portfolio projection`) / 2,
        `Model projection spread` = abs(`Chatter portfolio projection` - `Baseline portfolio projection`),
        `Robust projection` = `Median model projection` - 0.25 * `Model projection spread`,
        `Model ceiling projection` = pmax(`Chatter portfolio projection`, `Baseline portfolio projection`)
      ) %>%
      ungroup() %>%
      select(-AssetType, -ChatterEvaluation, -BaselineEvaluation)
  })

  fantasy2_combined_portfolio <- reactive({
    candidates <- fantasy2_combined_candidates()
    groups <- split(candidates, candidates$CandidateID)
    meta <- bind_rows(lapply(names(groups), function(id) {
      x <- groups[[id]]
      tibble(
        CandidateID = id,
        Source = first(x$Source),
        Candidate = first(x$Lineup),
        ScenarioKey = str_extract(first(x$Lineup), "^[A-H]"),
        RobustProjection = first(x$`Robust projection`),
        MedianProjection = first(x$`Median model projection`),
        CeilingProjection = first(x$`Model ceiling projection`),
        Captain = x$Name[x$Slot == "CPT"][1],
        Constructor = x$Name[x$Slot == "CON"][1],
        Drivers = list(unique(x$Name[x$Slot %in% c("CPT", "DRV")])),
        Roster = list(c(unique(x$Name[x$Slot %in% c("CPT", "DRV")]), x$Name[x$Slot == "CON"][1]))
      )
    }))
    labels <- LETTERS[1:8]
    choice_grid <- expand.grid(rep(list(c("Baseline", "Chatter")), 8L), stringsAsFactors = FALSE)
    max_driver_entries <- max(1L, floor(8L * pmin(100, pmax(25, as.numeric(input$fantasy2_driver_exposure %||% 75))) / 100))
    max_constructor_entries <- max(1L, floor(8L * pmin(100, pmax(10, as.numeric(input$fantasy2_constructor_exposure %||% 50))) / 100))
    best_ids <- character()
    best_score <- -Inf
    for (row_index in seq_len(nrow(choice_grid))) {
      ids <- vapply(seq_along(labels), function(index) {
        row <- meta %>% filter(ScenarioKey == labels[[index]], Source == choice_grid[row_index, index]) %>% slice(1)
        if (nrow(row) == 0L) NA_character_ else row$CandidateID[[1]]
      }, character(1))
      if (anyNA(ids)) next
      selected <- meta[match(ids, meta$CandidateID), ]
      source_counts <- table(selected$Source)
      if (length(source_counts) < 2L || any(source_counts < 3L)) next
      if (max(table(selected$Captain)) > 2L || max(table(selected$Constructor)) > max_constructor_entries) next
      driver_counts <- table(unlist(selected$Drivers, use.names = FALSE))
      if (max(driver_counts) > max_driver_entries) next
      pairs <- utils::combn(seq_len(8L), 2L)
      shared <- apply(pairs, 2, function(pair) length(intersect(selected$Roster[[pair[1]]], selected$Roster[[pair[2]]])))
      if (any(shared > 4L)) next
      exact_keys <- vapply(selected$Roster, function(roster) paste(sort(roster), collapse = "|"), character(1))
      if (n_distinct(exact_keys) < 8L) next
      score <- sum(selected$RobustProjection) + 0.08 * sum(selected$CeilingProjection - selected$MedianProjection) - 0.35 * sum(shared)
      if (score > best_score) {
        best_ids <- ids
        best_score <- score
      }
    }
    validate(need(length(best_ids) == 8L, "No mixed Baseline/Chatter portfolio satisfies the experimental exposure and overlap rules."))
    selected <- meta[match(best_ids, meta$CandidateID), ] %>%
      arrange(desc(RobustProjection), desc(CeilingProjection), Candidate) %>%
      mutate(
        `Combined lineup` = paste0("Entry ", row_number()),
        `Portfolio tier` = case_when(
          row_number() == 1L ~ "Best single-entry",
          row_number() <= 3L ~ "Included in best three",
          row_number() <= 5L ~ "Added for best five",
          TRUE ~ "Added for full eight"
        ),
        `Contest use` = case_when(
          row_number() == 1L ~ "Single-entry; Entry 1 of every set",
          row_number() <= 3L ~ "Best three-lineup set",
          row_number() <= 5L ~ "Best five-lineup set",
          TRUE ~ "Full eight-lineup portfolio"
        )
      )
    candidates %>%
      inner_join(selected %>% select(CandidateID, `Combined lineup`, Candidate, `Portfolio tier`, `Contest use`), by = "CandidateID") %>%
      arrange(as.integer(str_remove(`Combined lineup`, "Entry ")), factor(Slot, levels = c("CPT", "DRV", "CON")))
  })

  fantasy2_format_lineup <- function(rows) {
    rows %>%
      transmute(
        Slot,
        Name,
        Constructor,
        Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
        Projection = format_num(Projection, 2),
        `Value / $1k` = format_num(Value, 2)
      )
  }

  output$fantasy2_header <- renderUI({
    rows <- fantasy2_driver_projections()
    race <- rows %>% distinct(season, round, race_name) %>% slice(1)
    strategy <- fantasy2_strategy_inputs()
    primary <- strategy$primary_constructor
    flex_only <- paste(strategy$flex_only_names, collapse = " and ")
    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round, " · Experimental roster architecture")),
        h1(race$race_name),
        p(paste0(
          "Step 1 currently identifies ", primary, " as the best constructor fit from projected points plus salary value. ",
          "The two highest-salaried drivers (", flex_only, ") are flex-only; captain is chosen from the next high-projection value tier before the remaining roster is optimized."
        ))
      )
    )
  })

  output$fantasy2_single_lineup_table <- renderTable({
    rows <- fantasy2_combined_portfolio() %>% filter(`Combined lineup` == "Entry 1")
    validate(need(nrow(rows) > 0, "No experimental lineup is available."))
    fantasy2_format_lineup(rows)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_single_lineup_summary <- renderTable({
    rows <- fantasy2_combined_portfolio() %>% filter(`Combined lineup` == "Entry 1")
    validate(need(nrow(rows) > 0, "No experimental lineup summary is available."))
    rows %>% summarise(
      Source = first(Source),
      Constructor = Name[Slot == "CON"][1],
      Captain = Name[Slot == "CPT"][1],
      `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
      `Salary left` = paste0("$", format(round(as.numeric(input$fantasy2_salary_cap) - first(total_salary), 0), big.mark = ",")),
      `Robust DK projection` = format_num(first(`Robust projection`), 2)
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_strategy_table <- renderTable({
    strategy <- fantasy2_strategy_inputs()
    primary <- strategy$primary_constructor
    captain_names <- paste(strategy$captain_names, collapse = ", ")
    flex_only <- paste(strategy$flex_only_names, collapse = ", ")
    tibble(
      Step = c(
        "1. Quality-gate constructors",
        "2. Choose constructor sweet spot",
        "3. Remove stud captains",
        "4. Choose value captain",
        "5. Complete the roster"
      ),
      Rule = c(
        "Keep constructors with a competitive projected ceiling; expand only when the exposure setting requires it.",
        "Score 58% projected-point strength and 42% value per $1,000.",
        paste0("Flex-only due to top-two salary: ", flex_only, "."),
        "Among the next eight strongest projections, score 55% projection strength and 45% value; retain six captain candidates.",
        "Require at least one flex-only stud, then maximize the remaining projection under salary, exposure, and overlap rules."
      ),
      `Current result` = c(
        paste(strategy$constructors$constructor_name[strategy$constructors$portfolio_pool], collapse = ", "),
        paste0(primary, " is the primary constructor."),
        "Neither can appear at captain.",
        captain_names,
        "Four flex slots are optimized after constructor and captain are fixed."
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_portfolio_mode_note <- renderUI({
    mode <- if (isTRUE(input$fantasy2_use_chatter)) "Chatter" else "Baseline"
    p(class = "hint", paste0("Candidate preview mode: ", mode, ". The recommended Best 1 / 3 / 5 / 8 portfolio evaluates both modes."))
  })

  output$fantasy2_portfolio_summary_table <- renderTable({
    rows <- fantasy2_portfolio_lineups()
    rows %>%
      group_by(Lineup, Source) %>%
      summarise(
        Constructor = Name[Slot == "CON"][1],
        Captain = Name[Slot == "CPT"][1],
        `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
        `Salary left` = paste0("$", format(round(as.numeric(input$fantasy2_salary_cap) - first(total_salary), 0), big.mark = ",")),
        `Projected DK` = format_num(first(total_projection), 2),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_constructor_table <- renderTable({
    strategy <- fantasy2_strategy_inputs()
    fantasy2_constructor_projections() %>%
      left_join(
        strategy$constructors %>% select(constructor_name, projection_rank, value_rank, fit_score, portfolio_pool, primary_constructor),
        by = "constructor_name"
      ) %>%
      arrange(desc(primary_constructor), desc(portfolio_pool), desc(fit_score)) %>%
      transmute(
        `Fit rank` = min_rank(desc(fit_score)),
        Constructor = constructor_name,
        Role = case_when(primary_constructor ~ "Primary", portfolio_pool ~ "Alternate pool", TRUE ~ "Outside pool"),
        `Robust projection rank` = projection_rank,
        `Robust value rank` = value_rank,
        `Fit score` = format_num(fit_score, 1),
        Salary = paste0("$", format(round(mock_salary, 0), big.mark = ",")),
        `Preview proj DK` = format_num(fantasy_projection, 2),
        `Preview value / $1k` = format_num(value_per_1k, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_driver_table <- renderTable({
    strategy <- fantasy2_strategy_inputs()
    fantasy2_driver_projections() %>%
      left_join(
        strategy$drivers %>% select(driver_name, captain_fit_score, captain_status, robust_projection, robust_value),
        by = "driver_name"
      ) %>%
      transmute(
        Rank = fantasy_rank,
        Driver = driver_name,
        Constructor = constructor_name,
        `Captain status` = captain_status,
        `Captain fit` = if_else(is.na(captain_fit_score), "—", format_num(captain_fit_score, 1)),
        `D salary` = paste0("$", format(round(mock_salary, 0), big.mark = ",")),
        `CPT salary` = paste0("$", format(round(mock_salary * 1.5, 0), big.mark = ",")),
        `Preview proj DK` = format_num(fantasy_projection, 2),
        `Preview value / $1k` = format_num(value_per_1k, 2),
        `Robust proj DK` = format_num(robust_projection, 2),
        `Robust value / $1k` = format_num(robust_value, 2),
        `Proj start` = format_num(projected_start, 2),
        `Proj finish` = format_num(projected_finish, 2),
        `Place diff` = format_num(dk_place_diff, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_driver_download <- downloadHandler(
    filename = function() paste0("f1_fantasy2_driver_projections_", input$fantasy2_season, "_R", input$fantasy2_round, ".csv"),
    content = function(file) {
      strategy <- fantasy2_strategy_inputs()
      fantasy2_driver_projections() %>%
        left_join(strategy$drivers %>% select(driver_name, captain_fit_score, captain_status, robust_projection, robust_value), by = "driver_name") %>%
        write_csv(file, na = "")
    }
  )

  output$fantasy2_combined_summary_table <- renderTable({
    rows <- fantasy2_combined_portfolio()
    rows %>%
      group_by(`Combined lineup`, `Portfolio tier`, `Contest use`, Source, Candidate) %>%
      summarise(
        Constructor = Name[Slot == "CON"][1],
        Captain = Name[Slot == "CPT"][1],
        `Total salary` = paste0("$", format(round(first(total_salary), 0), big.mark = ",")),
        `Robust DK` = format_num(first(`Robust projection`), 2),
        `Median DK` = format_num(first(`Median model projection`), 2),
        `Ceiling DK` = format_num(first(`Model ceiling projection`), 2),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_combined_table <- renderTable({
    fantasy2_combined_portfolio() %>%
      transmute(
        `Combined lineup`, `Portfolio tier`, Source, Candidate, Slot, Name, Constructor,
        Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
        Projection = format_num(Projection, 2),
        `Value / $1k` = format_num(Value, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_combined_download <- downloadHandler(
    filename = function() paste0("f1_fantasy2_best1_best3_best5_full8_", input$fantasy2_season, "_R", input$fantasy2_round, ".csv"),
    content = function(file) write_csv(fantasy2_combined_portfolio(), file, na = "")
  )

  output$fantasy2_portfolio_table <- renderTable({
    fantasy2_portfolio_lineups() %>%
      transmute(
        Lineup, Source, Slot, Name, Constructor,
        Salary = paste0("$", format(round(Salary, 0), big.mark = ",")),
        Projection = format_num(Projection, 2),
        `Value / $1k` = format_num(Value, 2)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_portfolio_download <- downloadHandler(
    filename = function() {
      mode <- if (isTRUE(input$fantasy2_use_chatter)) "chatter" else "baseline"
      paste0("f1_fantasy2_preview_", input$fantasy2_season, "_R", input$fantasy2_round, "_", mode, ".csv")
    },
    content = function(file) write_csv(fantasy2_portfolio_lineups(), file, na = "")
  )

  fantasy2_recommended_lineup_groups <- reactive({
    rows <- fantasy2_combined_portfolio()
    split(rows, rows$`Combined lineup`)
  })

  output$fantasy2_portfolio_audit_table <- renderTable({
    rows <- fantasy2_combined_portfolio()
    groups <- fantasy2_recommended_lineup_groups()
    strategy <- fantasy2_strategy_inputs()
    captain_names <- vapply(groups, function(x) x$Name[x$Slot == "CPT"][1], character(1))
    constructor_names <- vapply(groups, function(x) x$Name[x$Slot == "CON"][1], character(1))
    driver_sets <- lapply(groups, function(x) unique(x$Name[x$Slot %in% c("CPT", "DRV")]))
    roster_sets <- lapply(groups, function(x) c(unique(x$Name[x$Slot %in% c("CPT", "DRV")]), x$Name[x$Slot == "CON"][1]))
    pairs <- utils::combn(seq_along(groups), 2L)
    shared <- apply(pairs, 2, function(pair) length(intersect(roster_sets[[pair[1]]], roster_sets[[pair[2]]])))
    primary_target <- min(8L, max(1L, floor(8L * pmin(100, pmax(10, as.numeric(input$fantasy2_constructor_exposure %||% 50))) / 100)))
    tibble(
      Check = c(
        "Recommended lineups",
        "Top-two salary drivers used at captain",
        "Primary-constructor entries",
        "Lineups with an elite flex driver",
        "Distinct captains",
        "Distinct constructors",
        "Maximum shared roster selections",
        "Overall status"
      ),
      Result = c(
        paste0(length(groups), " of 8"),
        as.character(sum(captain_names %in% strategy$flex_only_names)),
        paste0(sum(constructor_names == strategy$primary_constructor), " of ", primary_target),
        paste0(sum(vapply(driver_sets, function(x) any(strategy$flex_only_names %in% x), logical(1))), " of 8"),
        as.character(n_distinct(captain_names)),
        as.character(n_distinct(constructor_names)),
        as.character(max(shared)),
        if (
          length(groups) == 8L &&
          !any(captain_names %in% strategy$flex_only_names) &&
          sum(constructor_names == strategy$primary_constructor) == primary_target &&
          all(vapply(driver_sets, function(x) any(strategy$flex_only_names %in% x), logical(1))) &&
          max(shared) <= 4L
        ) "Passed" else "Warning"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_portfolio_exposure_table <- renderTable({
    rows <- fantasy2_combined_portfolio()
    lineup_count <- n_distinct(rows$`Combined lineup`)
    driver_exposure <- rows %>%
      filter(Slot %in% c("CPT", "DRV")) %>%
      group_by(Name) %>%
      summarise(Lineups = n_distinct(`Combined lineup`), `Captain lineups` = sum(Slot == "CPT"), .groups = "drop") %>%
      mutate(Type = "Driver")
    constructor_exposure <- rows %>%
      filter(Slot == "CON") %>%
      group_by(Name) %>%
      summarise(Lineups = n_distinct(`Combined lineup`), .groups = "drop") %>%
      mutate(Type = "Constructor", `Captain lineups` = 0L)
    bind_rows(driver_exposure, constructor_exposure) %>%
      mutate(Exposure = paste0(format_num(Lineups / lineup_count * 100, 1), "%")) %>%
      arrange(Type, desc(Lineups), Name) %>%
      select(Type, Name, Lineups, Exposure, `Captain lineups`)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$fantasy2_portfolio_overlap_table <- renderTable({
    groups <- fantasy2_recommended_lineup_groups()
    pairs <- utils::combn(seq_along(groups), 2L)
    bind_rows(lapply(seq_len(ncol(pairs)), function(index) {
      left_name <- names(groups)[pairs[1, index]]
      right_name <- names(groups)[pairs[2, index]]
      left <- groups[[pairs[1, index]]]
      right <- groups[[pairs[2, index]]]
      left_set <- c(unique(left$Name[left$Slot %in% c("CPT", "DRV")]), left$Name[left$Slot == "CON"][1])
      right_set <- c(unique(right$Name[right$Slot %in% c("CPT", "DRV")]), right$Name[right$Slot == "CON"][1])
      shared <- sort(intersect(left_set, right_set))
      tibble(
        `Lineup 1` = left_name,
        `Lineup 2` = right_name,
        `Shared selections` = length(shared),
        Overlap = paste0(format_num(length(shared) / 6 * 100, 1), "%"),
        `Shared names` = paste(shared, collapse = ", ")
      )
    })) %>% arrange(desc(`Shared selections`), `Lineup 1`, `Lineup 2`)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$race_selector <- renderUI({
    choices <- race_choices %>%
      filter(season == input$season)

    selectInput(
      "round",
      "Race",
      choices = setNames(choices$round, choices$label),
      selected = default_race_round(choices)
    )
  })

  event_rows <- reactive({
    req(input$season, input$round)
    selected_event_data(as.integer(input$season), as.integer(input$round))
  })

  event_meta <- reactive({
    req(input$season, input$round)
    selected_event_meta(as.integer(input$season), as.integer(input$round))
  })

  observeEvent(event_rows(), {
    rows <- event_rows()

    if (nrow(rows) == 0) {
      updateSelectInput(
        session,
        "driver",
        choices = c("All drivers" = "all"),
        selected = "all"
      )
      return()
    }

    drivers <- rows %>%
      arrange(finish_position) %>%
      distinct(driver_code, driver_name) %>%
      mutate(label = paste0(driver_code, " - ", driver_name))

    updateSelectInput(
      session,
      "driver",
      choices = c("All drivers" = "all", setNames(drivers$driver_code, drivers$label)),
      selected = "all"
    )
  })

  output$event_header <- renderUI({
    race <- event_meta()
    req(nrow(race) > 0)

    div(
      class = "event-header",
      div(
        class = "event-title-block",
        div(class = "eyebrow", paste0(race$season, " Round ", race$round)),
        h1(race$race_name),
        p(paste(race$circuit_name, race$locality, race$country, sep = " - "))
      ),
      div(
        class = "event-date",
        format(as.Date(race$race_date), "%b %d, %Y")
      )
    )
  })

  output$family_chips <- renderUI({
    race <- event_meta()
    req(nrow(race) > 0)
    flags <- race[1, family_flags, drop = TRUE]
    cluster_id <- if ("track_cluster_id" %in% names(race)) race$track_cluster_id[[1]] else NA_character_
    cluster_label <- if ("track_cluster_label" %in% names(race)) race$track_cluster_label[[1]] else NA_character_
    cluster_chip <- if (!is.na(cluster_id) && cluster_id != "") {
      div(
        class = "family-chip cluster-chip",
        span(class = "chip-label", paste("Cluster", cluster_display_number(cluster_id)))
      )
    } else {
      NULL
    }
    circuit_id <- if ("circuit_id" %in% names(race)) race$circuit_id[[1]] else NA_character_
    circuit_results <- if (!is.na(circuit_id) && circuit_id != "") {
      stage1 %>% filter(.data$circuit_id == .env$circuit_id, !is.na(.data$dnf_flag))
    } else tibble()
    dnf_chip <- if (nrow(circuit_results) > 0) {
      dnf_rate <- mean(as.logical(circuit_results$dnf_flag), na.rm = TRUE)
      div(
        class = "family-chip dnf-chip",
        title = paste0("Overall DNF rate across ", nrow(circuit_results), " driver results from ",
                       min(circuit_results$season, na.rm = TRUE), "–", max(circuit_results$season, na.rm = TRUE)),
        span(class = "chip-label", paste0("DNF ", percent(dnf_rate, accuracy = 0.1)))
      )
    } else {
      NULL
    }

    div(
      class = "chip-grid",
      c(
        lapply(seq_along(family_flags), function(i) {
          flag <- family_flags[[i]]
          active <- !is.na(flags[[flag]]) && flags[[flag]] == 1
          div(
            class = paste("family-chip", if (active) "active" else "inactive"),
            span(class = "chip-number", i),
            span(class = "chip-label", family_labels[[flag]])
          )
        }),
        list(cluster_chip, dnf_chip)
      )
    )
  })

  output$family_notes <- renderUI({
    race <- event_meta()
    req(nrow(race) > 0)
    flags <- race[1, family_flags, drop = TRUE]
    active_flags <- family_flags[vapply(family_flags, function(flag) {
      !is.na(flags[[flag]]) && flags[[flag]] == 1
    }, logical(1))]
    cluster_id <- if ("track_cluster_id" %in% names(race)) race$track_cluster_id[[1]] else NA_character_
    cluster_label <- if ("track_cluster_label" %in% names(race)) race$track_cluster_label[[1]] else NA_character_
    cluster_peers <- if ("cluster_peer_circuits" %in% names(race)) race$cluster_peer_circuits[[1]] else NA_character_

    if (length(active_flags) == 0 && (is.na(cluster_id) || cluster_id == "")) {
      return(p(class = "hint", "No active starter track-family flags for this circuit."))
    }

    tags$ul(
      class = "family-notes",
      c(
        lapply(active_flags, function(flag) {
          tags$li(tags$b(family_labels[[flag]]), paste0(": ", family_descriptions[[flag]]))
        }),
        list(
          if (!is.na(cluster_id) && cluster_id != "") {
            tags$li(
              tags$b(cluster_display_name(cluster_id, cluster_label)),
              paste0(
                ": ",
                if (!is.na(cluster_peers) && cluster_peers != "") cluster_peers else "Similar-track cluster unavailable"
              )
            )
          } else {
            NULL
          }
        )
      )
    )
  })

  output$race_metrics <- renderUI({
    rows <- event_rows()
    race <- event_meta()
    req(nrow(race) > 0)

    if (nrow(rows) == 0) {
      return(
        div(
          class = "metric-grid",
          div(class = "metric", span("Race status"), strong("Scheduled")),
          div(class = "metric", span("Race date"), strong(format(as.Date(race$race_date[[1]]), "%b %d, %Y"))),
          div(class = "metric", span("Circuit"), strong(race$circuit_name[[1]])),
          div(class = "metric", span("Results"), strong("Not available yet"))
        )
      )
    }

    winner <- rows %>% arrange(finish_position) %>% slice(1)
    polesitter <- rows %>% arrange(quali_position) %>% slice(1)

    if (!is.null(input$driver) && input$driver != "all") {
      driver_row <- rows %>%
        filter(driver_code == input$driver) %>%
        slice(1)

      req(nrow(driver_row) > 0)
      speed_value <- driver_speed_value(driver_row)
      speed_delta <- driver_speed_delta_value(driver_row)

      return(
        div(
          class = "metric-grid",
          div(class = "metric", span("Finish"), strong(driver_row$finish_position)),
          div(class = "metric", span("Status"), strong(driver_row$status)),
          div(class = "metric", span("Grid / Quali"), strong(paste0(driver_row$grid, " / ", driver_row$quali_position))),
          div(class = "metric", span("Points"), strong(format_num(driver_row$points, 1))),
          div(class = "metric", span("Quali delta to pole"), strong(format_delta_sec(driver_row$best_quali_delta_sec))),
          div(class = "metric", span("Fastest lap delta"), strong(format_delta_sec(driver_row$fastest_lap_delta_sec))),
          div(class = "metric", span(driver_speed_label(driver_row)), strong(format_speed(speed_value))),
          div(class = "metric", span(driver_speed_delta_label(driver_row)), strong(format_speed(speed_delta)))
        )
      )
    }

    has_speed_trap <- any(!is.na(rows$max_speed_st_kph))
    if (has_speed_trap) {
      max_speed <- safe_max(rows$max_speed_st_kph)
      speed_row <- rows %>%
        filter(!is.na(max_speed_st_kph), max_speed_st_kph == max_speed) %>%
        slice(1)
      race_speed_label <- "Peak speed"
    } else {
      max_speed <- safe_max(rows$top_speed_kph)
      speed_row <- rows %>%
        filter(!is.na(top_speed_kph), top_speed_kph == max_speed) %>%
        slice(1)
      race_speed_label <- "Max fastest-lap avg speed"
    }
    max_speed_label <- if (nrow(speed_row) == 0 || is.na(max_speed)) {
      "Unavailable"
    } else {
      paste0(speed_row$driver_code, " ", format_speed(max_speed))
    }

    fastest_lap <- if (all(is.na(rows$fastest_lap_sec))) {
      tibble()
    } else {
      rows %>%
        filter(!is.na(fastest_lap_sec), fastest_lap_sec == min(fastest_lap_sec, na.rm = TRUE)) %>%
        slice(1)
    }
    fastest_lap_label <- if (nrow(fastest_lap) == 0 || is.na(fastest_lap$fastest_lap_sec)) {
      "Unavailable"
    } else {
      paste0(fastest_lap$driver_code, " ", format_sec(fastest_lap$fastest_lap_sec))
    }

    div(
      class = "metric-grid",
      div(class = "metric", span("Winner"), strong(paste(winner$driver_code, winner$constructor_name))),
      div(class = "metric", span("Pole"), strong(paste(polesitter$driver_code, polesitter$constructor_name))),
      div(class = "metric", span("DNF rate"), strong(percent(mean(rows$dnf_flag, na.rm = TRUE), accuracy = 0.1))),
      div(class = "metric", span("Lead-lap finishers"), strong(sum(rows$finished_on_lead_lap, na.rm = TRUE))),
      div(class = "metric", span("Pole time"), strong(format_sec(polesitter$best_quali_sec))),
      div(class = "metric", span("Fastest lap"), strong(fastest_lap_label)),
      div(class = "metric", span(race_speed_label), strong(max_speed_label))
    )
  })

  output$snapshot_title <- renderUI({
    rows <- event_rows()

    if (nrow(rows) == 0) {
      return(h2("Event Snapshot"))
    }

    if (!is.null(input$driver) && input$driver != "all") {
      driver_row <- rows %>%
        filter(driver_code == input$driver) %>%
        slice(1)

      if (nrow(driver_row) > 0) {
        return(h2(paste0(driver_row$driver_code, " Snapshot")))
      }
    }

    h2("Race Snapshot")
  })

  output$track_plot <- renderPlot({
    rows <- event_meta()
    req(nrow(rows) > 0)

    p <- plot_track_layout(rows)
    validate(need(!is.null(p), "Track graphic unavailable for this race/session. Try another season/round or turn rendering off."))
    p
  })

  output$track_visual <- renderUI({
    rows <- event_meta()
    req(nrow(rows) > 0)

    image_path <- static_track_image(rows)
    if (!is.null(image_path)) {
      return(
        div(
          class = "static-track-frame",
          tags$img(
            src = paste0("track_plots/", basename(image_path)),
            class = "static-track-image",
            alt = paste(rows$circuit_name[[1]], "track map")
          )
        )
      )
    }

    plotOutput("track_plot", height = "720px")
  })

  output$recent_winners <- renderTable({
    rows <- event_meta()
    req(nrow(rows) > 0)
    circuit <- rows$circuit_id[[1]]

    stage1 %>%
      filter(circuit_id == circuit, finish_position == 1) %>%
      arrange(desc(season), desc(round)) %>%
      transmute(
        Season = season,
        Race = race_name,
        Winner = paste(driver_code, driver_name),
        Constructor = constructor_name,
        Grid = format_int(grid),
        `Gap Raw` = gap_raw
      ) %>%
      head(10)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$race_results <- renderTable({
    rows <- event_rows()

    if (nrow(rows) == 0) {
      return(tibble(Status = "Race results are not available yet."))
    }

    if (!is.null(input$driver) && input$driver != "all") {
      rows <- rows %>% filter(driver_code == input$driver)
    }

    rows %>%
      arrange(finish_position) %>%
      transmute(
        Pos = format_int(finish_position),
        Driver = paste(driver_code, driver_name),
        Constructor = constructor_name,
        Grid = format_int(grid),
        Points = format_int(points),
        Status = status,
        `Fastest Lap Delta` = format_num(fastest_lap_delta_sec, 3),
        `Best Quali Delta` = format_num(best_quali_delta_sec, 3)
      ) %>%
      head(25)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$grid_finish_plot <- renderPlot({
    rows <- event_rows()
    validate(need(nrow(rows) > 0, "Grid and finish data are not available for scheduled races yet."))

    if (!is.null(input$driver) && input$driver != "all") {
      rows <- rows %>% filter(driver_code == input$driver)
    }

    ggplot(rows, aes(x = grid, y = finish_position, label = driver_code)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#9AA0A6") +
      geom_point(size = 3, color = "#E10600") +
      geom_text(nudge_y = -0.35, size = 3.5, color = "#EDF2F8") +
      scale_y_reverse(breaks = pretty_breaks()) +
      scale_x_continuous(breaks = pretty_breaks()) +
      labs(x = "Grid position", y = "Finish position") +
      theme_f1_dark(base_size = 12)
  })

  profile_family_rows_all_unfiltered <- reactive({
    req(input$profile_driver, input$profile_start_season, input$profile_end_season)
    start_season <- min(as.integer(input$profile_start_season), as.integer(input$profile_end_season))
    end_season <- max(as.integer(input$profile_start_season), as.integer(input$profile_end_season))
    driver_family_rows(input$profile_driver, start_season, end_season)
  })

  profile_family_rows_all <- reactive({
    rows <- profile_family_rows_all_unfiltered()
    if (isTRUE(input$profile_exclude_dnf) && "dnf_flag" %in% names(rows)) {
      rows <- rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    }
    rows
  })

  profile_base_rows_unfiltered <- reactive({
    req(input$profile_driver, input$profile_start_season, input$profile_end_season)
    start_season <- min(as.integer(input$profile_start_season), as.integer(input$profile_end_season))
    end_season <- max(as.integer(input$profile_start_season), as.integer(input$profile_end_season))

    stage1 %>%
      filter(
        driver_code == input$profile_driver,
        season >= start_season,
        season <= end_season
      ) %>%
      left_join(circuit_track_cluster_lookup, by = "circuit_id")
  })

  profile_base_rows <- reactive({
    rows <- profile_base_rows_unfiltered()
    if (isTRUE(input$profile_exclude_dnf) && "dnf_flag" %in% names(rows)) {
      rows <- rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    }
    rows
  })

  profile_constructor_selection <- reactive({
    rows <- profile_base_rows_unfiltered()
    if (nrow(rows) == 0) return("all")

    constructor_ids <- rows %>%
      filter(!is.na(constructor_id), constructor_id != "") %>%
      distinct(constructor_id) %>%
      pull(constructor_id)

    if (!is.null(input$profile_constructor)) {
      if (identical(input$profile_constructor, "all")) return("all")
      if (input$profile_constructor %in% constructor_ids) return(input$profile_constructor)
    }

    "all"
  })

  output$profile_constructor_selector <- renderUI({
    rows <- profile_base_rows_unfiltered()
    if (nrow(rows) == 0) return(NULL)

    constructor_history <- rows %>%
      filter(!is.na(constructor_id), constructor_id != "") %>%
      group_by(constructor_id, constructor_name) %>%
      summarise(
        first_season = min(season, na.rm = TRUE),
        last_season = max(season, na.rm = TRUE),
        starts = n_distinct(season, round),
        .groups = "drop"
      ) %>%
      mutate(
        season_label = if_else(first_season == last_season, as.character(first_season), paste0(first_season, "-", last_season))
      ) %>%
      arrange(first_season, constructor_name)

    if (nrow(constructor_history) <= 1) return(NULL)

    constructor_choices <- c(
      "Overall / all constructors" = "all",
      setNames(
        constructor_history$constructor_id,
        paste0(constructor_history$constructor_name, " (", constructor_history$season_label, ")")
      )
    )

    selectInput(
      "profile_constructor",
      "Analyze constructor",
      choices = constructor_choices,
      selected = "all"
    )
  })

  profile_base_rows_selected <- reactive({
    rows <- profile_base_rows()
    selected_constructor <- profile_constructor_selection()

    if (!is.null(selected_constructor) && selected_constructor != "all") {
      rows <- rows %>% filter(constructor_id == selected_constructor)
    }

    rows
  })

  profile_rows <- reactive({
    rows <- profile_family_rows_all()
    selected_constructor <- profile_constructor_selection()

    if (!is.null(selected_constructor) && selected_constructor != "all") {
      rows <- rows %>% filter(constructor_id == selected_constructor)
    }

    rows
  })

  profile_base_rows_selected_unfiltered <- reactive({
    rows <- profile_base_rows_unfiltered()
    selected_constructor <- profile_constructor_selection()

    if (!is.null(selected_constructor) && selected_constructor != "all") {
      rows <- rows %>% filter(constructor_id == selected_constructor)
    }

    rows
  })

  profile_rows_unfiltered <- reactive({
    rows <- profile_family_rows_all_unfiltered()
    selected_constructor <- profile_constructor_selection()

    if (!is.null(selected_constructor) && selected_constructor != "all") {
      rows <- rows %>% filter(constructor_id == selected_constructor)
    }

    rows
  })

  profile_overall_family_summary <- reactive({
    summarise_driver_family(profile_family_rows_all())
  })

  profile_family_summary <- reactive({
    summarise_driver_family(profile_rows())
  })

  profile_cluster_summary <- reactive({
    summarise_driver_cluster(profile_base_rows_selected())
  })

  profile_family_scale_summary <- reactive({
    rows <- profile_rows_unfiltered()
    finished_rows <- if ("dnf_flag" %in% names(rows)) {
      rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    } else {
      rows
    }
    bind_rows(summarise_driver_family(rows), summarise_driver_family(finished_rows))
  })

  profile_cluster_scale_summary <- reactive({
    rows <- profile_base_rows_selected_unfiltered()
    finished_rows <- if ("dnf_flag" %in% names(rows)) {
      rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    } else {
      rows
    }
    bind_rows(summarise_driver_cluster(rows), summarise_driver_cluster(finished_rows))
  })

  output$profile_summary_cards <- renderUI({
    rows <- profile_family_rows_all()
    base_rows <- profile_base_rows()
    summary <- profile_overall_family_summary()

    validate(need(nrow(rows) > 0 && nrow(base_rows) > 0, "No driver results found for this selection."))

    driver_label <- base_rows %>%
      distinct(driver_code, driver_name) %>%
      slice(1)
    driver_age <- age_years(base_rows$date_of_birth)

    best_finish <- summary %>%
      filter(starts >= 2, !is.na(avg_finish)) %>%
      arrange(avg_finish) %>%
      slice(1)

    best_points <- summary %>%
      filter(starts >= 2, !is.na(points_per_start)) %>%
      arrange(desc(points_per_start)) %>%
      slice(1)

    worst_finish <- summary %>%
      filter(starts >= 2, !is.na(avg_finish)) %>%
      arrange(desc(avg_finish)) %>%
      slice(1)

    constructor_history <- base_rows %>%
      group_by(constructor_id, constructor_name) %>%
      summarise(
        first_season = min(season, na.rm = TRUE),
        last_season = max(season, na.rm = TRUE),
        starts = n_distinct(season, round),
        wins = sum(win, na.rm = TRUE),
        avg_finish = safe_mean(finish_position),
        points_per_start = sum(points, na.rm = TRUE) / n_distinct(season, round),
        .groups = "drop"
      ) %>%
      mutate(
        season_label = if_else(first_season == last_season, as.character(first_season), paste0(first_season, "-", last_season))
      ) %>%
      arrange(first_season, constructor_name)

    current_constructor <- profile_constructor_selection()
    if (is.null(current_constructor)) current_constructor <- "all"

    div(
      div(
        class = "profile-section-label",
        "Overall"
      ),
      div(
        class = "profile-card-grid profile-overall-grid",
        div(
          class = "metric profile-card",
          span("Driver"),
          strong(paste(driver_label$driver_code, driver_label$driver_name))
        ),
        div(
          class = "metric profile-card",
          span("Age"),
          strong(if (is.na(driver_age)) "Unavailable" else paste0(driver_age, " years"))
        ),
        div(
          class = "metric profile-card",
          span("Result starts"),
          strong(n_distinct(base_rows$season, base_rows$round))
        ),
        div(
          class = "metric profile-card",
          span("Best avg finish"),
          strong(if (nrow(best_finish) == 0) "Unavailable" else paste0(best_finish$family, " (", format_num(best_finish$avg_finish, 2), ")"))
        ),
        div(
          class = "metric profile-card",
          span("Best points rate"),
          strong(if (nrow(best_points) == 0) "Unavailable" else paste0(best_points$family, " (", format_num(best_points$points_per_start, 2), ")"))
        ),
        div(
          class = "metric profile-card",
          span("Toughest avg finish"),
          strong(if (nrow(worst_finish) == 0) "Unavailable" else paste0(worst_finish$family, " (", format_num(worst_finish$avg_finish, 2), ")"))
        )
      ),
      div(
        class = "profile-section-label",
        "Constructor History"
      ),
      div(
        class = "constructor-history-grid",
        lapply(seq_len(nrow(constructor_history)), function(i) {
          row <- constructor_history[i, ]
          div(
            class = "metric constructor-card",
            span(row$season_label),
            strong(row$constructor_name),
            div(
              class = "constructor-card-stats",
              div(span("Starts"), strong(format_int(row$starts))),
              div(span("Avg finish"), strong(format_num(row$avg_finish, 1))),
              div(span("Pts/start"), strong(format_num(row$points_per_start, 1))),
              div(span("Wins"), strong(format_int(row$wins)))
            )
          )
        })
      ),
      div(
        class = "profile-section-label analysis-scope-label",
        paste0(
          "Analysis Scope: ",
          if (identical(current_constructor, "all")) {
            "Overall / all constructors"
          } else {
            constructor_history %>%
              filter(constructor_id == current_constructor) %>%
              transmute(label = paste0(constructor_name, " (", season_label, ")")) %>%
              pull(label) %>%
              first()
          }
        )
      )
    )
  })

  output$driver_family_plot <- renderPlot({
    summary <- profile_family_summary()
    validate(need(nrow(summary) > 0, "No track-family results found for this driver."))

    metric <- input$profile_metric
    metric_labels <- c(
      avg_finish = "Average finish",
      points_per_start = "Points per start",
      top10_rate = "Top-10 rate",
      podium_rate = "Podium rate",
      avg_quali_delta = "Average quali delta, sec",
      avg_fastest_lap_delta = "Average fastest-lap delta, sec"
    )

    plot_data <- summary %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(
        family_label = paste0(match(family_flag, family_flags), ". ", family),
        family_label = factor(
          family_label,
          levels = rev(paste0(seq_along(family_flags), ". ", family_labels[family_flags]))
        )
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      plot_data <- plot_data %>%
        mutate(value_label = percent(.data[[metric]], accuracy = 0.1))
    } else {
      plot_data <- plot_data %>%
        mutate(value_label = format_num(.data[[metric]], 2))
    }

    validate(need(nrow(plot_data) > 0, "Selected metric is not populated for this driver."))
    axis_limits <- profile_metric_limits(profile_family_scale_summary(), metric)

    p <- ggplot(plot_data, aes(x = family_label, y = .data[[metric]])) +
      geom_col(fill = "#E10600", width = 0.72) +
      geom_text(aes(label = value_label), hjust = -0.12, size = 4.2, fontface = "bold", color = "#EDF2F8") +
      coord_flip(clip = "off") +
      labs(x = NULL, y = metric_labels[[metric]]) +
      theme_f1_dark(base_size = 14) +
      theme(
        plot.margin = margin(8, 34, 8, 8),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 13, face = "bold", color = "#AEB8C7"),
        axis.title = element_text(size = 14, face = "bold", color = "#AEB8C7")
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      p <- p + scale_y_continuous(limits = axis_limits, labels = percent)
    } else {
      p <- p + scale_y_continuous(limits = axis_limits)
    }

    p
  })

  output$driver_family_table <- renderTable({
    summary <- profile_family_summary()

    if (nrow(summary) == 0) {
      return(tibble(Status = "No track-family results found for this driver."))
    }

    summary %>%
      arrange(avg_finish) %>%
      transmute(
        Family = family,
        Starts = starts,
        `Avg Finish` = format_num(avg_finish, 1),
        `Points/Start` = format_num(points_per_start, 1),
        `Top-10` = format_pct(top10_rate),
        Podium = format_pct(podium_rate),
        DNF = format_pct(dnf_rate),
        `Avg Quali Delta` = format_num(avg_quali_delta, 3),
        `Avg Fastest-Lap Delta` = format_num(avg_fastest_lap_delta, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$driver_cluster_plot <- renderPlot({
    summary <- profile_cluster_summary()
    validate(need(nrow(summary) > 0, "No track-cluster results found for this driver."))

    metric <- input$profile_metric
    metric_labels <- c(
      avg_finish = "Average finish",
      points_per_start = "Points per start",
      top10_rate = "Top-10 rate",
      podium_rate = "Podium rate",
      avg_quali_delta = "Average quali delta, sec",
      avg_fastest_lap_delta = "Average fastest-lap delta, sec"
    )

    cluster_levels <- summary %>%
      arrange(cluster_number, cluster) %>%
      pull(cluster)

    plot_data <- summary %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(cluster = factor(cluster, levels = rev(cluster_levels)))

    if (metric %in% c("top10_rate", "podium_rate")) {
      plot_data <- plot_data %>%
        mutate(value_label = percent(.data[[metric]], accuracy = 0.1))
    } else {
      plot_data <- plot_data %>%
        mutate(value_label = format_num(.data[[metric]], 2))
    }

    validate(need(nrow(plot_data) > 0, "Selected metric is not populated for this driver."))
    axis_limits <- profile_metric_limits(profile_cluster_scale_summary(), metric)

    p <- ggplot(plot_data, aes(x = cluster, y = .data[[metric]])) +
      geom_col(fill = "#23A6D5", width = 0.72) +
      geom_text(aes(label = value_label), hjust = -0.12, size = 4.2, fontface = "bold", color = "#EDF2F8") +
      coord_flip(clip = "off") +
      labs(x = NULL, y = metric_labels[[metric]]) +
      theme_f1_dark(base_size = 14) +
      theme(
        plot.margin = margin(8, 34, 8, 8),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 13, face = "bold", color = "#AEB8C7"),
        axis.title = element_text(size = 14, face = "bold", color = "#AEB8C7")
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      p <- p + scale_y_continuous(limits = axis_limits, labels = percent)
    } else {
      p <- p + scale_y_continuous(limits = axis_limits)
    }

    p
  })

  output$driver_cluster_table <- renderTable({
    summary <- profile_cluster_summary()

    if (nrow(summary) == 0) {
      return(tibble(Status = "No track-cluster results found for this driver."))
    }

    summary %>%
      arrange(avg_finish) %>%
      transmute(
        Cluster = cluster,
        `Similar Tracks` = similar_tracks,
        Starts = starts,
        `Avg Finish` = format_num(avg_finish, 1),
        `Points/Start` = format_num(points_per_start, 1),
        `Top-10` = format_pct(top10_rate),
        Podium = format_pct(podium_rate),
        DNF = format_pct(dnf_rate),
        `Avg Quali Delta` = format_num(avg_quali_delta, 3),
        `Avg Fastest-Lap Delta` = format_num(avg_fastest_lap_delta, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$driver_circuit_table <- renderTable({
    rows <- profile_base_rows_selected()

    if (nrow(rows) == 0) {
      return(tibble(Status = "No circuit results found for this driver."))
    }

    rows %>%
      group_by(circuit_id, circuit_name, track_cluster_id, track_cluster_label) %>%
      summarise(
        Starts = n(),
        `Avg Finish` = safe_mean(finish_position),
        `Points/Start` = sum(points, na.rm = TRUE) / n(),
        `Top-10` = mean(top10_finish, na.rm = TRUE),
        Podium = mean(podium, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Starts >= 1) %>%
      arrange(`Avg Finish`, desc(`Points/Start`)) %>%
      transmute(
        Circuit = circuit_name,
        Cluster = cluster_display_label(track_cluster_id, track_cluster_label),
        Starts,
        `Avg Finish` = format_num(`Avg Finish`, 1),
        `Points/Start` = format_num(`Points/Start`, 1),
        `Top-10` = format_pct(`Top-10`),
        Podium = format_pct(Podium)
      ) %>%
      head(12)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$driver_recent_results <- renderTable({
    rows <- profile_base_rows_selected()

    if (nrow(rows) == 0) {
      return(tibble(Status = "No recent results found for this driver."))
    }

    rows %>%
      arrange(desc(season), desc(round)) %>%
      transmute(
        Season = season,
        Round = round,
        Race = race_name,
        Circuit = circuit_name,
        Cluster = cluster_display_label(track_cluster_id, track_cluster_label),
        Constructor = constructor_name,
        Finish = format_int(finish_position),
        Grid = format_int(grid),
        Points = format_int(points),
        Status = status
      ) %>%
      head(20)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  constructor_profile_family_rows_unfiltered <- reactive({
    req(input$constructor_profile_constructor, input$constructor_profile_start_season, input$constructor_profile_end_season)
    start_season <- min(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))
    end_season <- max(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))
    constructor_family_rows(input$constructor_profile_constructor, start_season, end_season)
  })

  constructor_profile_family_rows <- reactive({
    rows <- constructor_profile_family_rows_unfiltered()
    if (isTRUE(input$constructor_profile_exclude_dnf) && "dnf_flag" %in% names(rows)) {
      rows <- rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    }
    rows
  })

  constructor_profile_base_rows_unfiltered <- reactive({
    req(input$constructor_profile_constructor, input$constructor_profile_start_season, input$constructor_profile_end_season)
    start_season <- min(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))
    end_season <- max(as.integer(input$constructor_profile_start_season), as.integer(input$constructor_profile_end_season))

    stage1 %>%
      filter(
        constructor_id == input$constructor_profile_constructor,
        season >= start_season,
        season <= end_season
      ) %>%
      left_join(circuit_track_cluster_lookup, by = "circuit_id")
  })

  constructor_profile_base_rows <- reactive({
    rows <- constructor_profile_base_rows_unfiltered()
    if (isTRUE(input$constructor_profile_exclude_dnf) && "dnf_flag" %in% names(rows)) {
      rows <- rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    }
    rows
  })

  constructor_profile_family_summary <- reactive({
    summarise_driver_family(constructor_profile_family_rows())
  })

  constructor_profile_cluster_summary <- reactive({
    summarise_driver_cluster(constructor_profile_base_rows())
  })

  constructor_profile_family_scale_summary <- reactive({
    rows <- constructor_profile_family_rows_unfiltered()
    finished_rows <- if ("dnf_flag" %in% names(rows)) {
      rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    } else {
      rows
    }
    bind_rows(summarise_driver_family(rows), summarise_driver_family(finished_rows))
  })

  constructor_profile_cluster_scale_summary <- reactive({
    rows <- constructor_profile_base_rows_unfiltered()
    finished_rows <- if ("dnf_flag" %in% names(rows)) {
      rows %>% filter(!coalesce(as.logical(dnf_flag), FALSE))
    } else {
      rows
    }
    bind_rows(summarise_driver_cluster(rows), summarise_driver_cluster(finished_rows))
  })

  output$constructor_profile_summary_cards <- renderUI({
    rows <- constructor_profile_base_rows()
    summary <- constructor_profile_family_summary()

    validate(need(nrow(rows) > 0, "No constructor results found for this selection."))

    constructor_label <- rows %>%
      distinct(constructor_id, constructor_name) %>%
      slice(1)

    best_finish <- summary %>%
      filter(starts >= 2, !is.na(avg_finish)) %>%
      arrange(avg_finish) %>%
      slice(1)

    best_points <- summary %>%
      filter(starts >= 2, !is.na(points_per_start)) %>%
      arrange(desc(points_per_start)) %>%
      slice(1)

    worst_finish <- summary %>%
      filter(starts >= 2, !is.na(avg_finish)) %>%
      arrange(desc(avg_finish)) %>%
      slice(1)

    driver_history <- rows %>%
      group_by(driver_code, driver_name) %>%
      summarise(
        first_season = min(season, na.rm = TRUE),
        last_season = max(season, na.rm = TRUE),
        starts = n(),
        wins = sum(win, na.rm = TRUE),
        avg_finish = safe_mean(finish_position),
        points_per_start = sum(points, na.rm = TRUE) / n(),
        .groups = "drop"
      ) %>%
      mutate(
        season_label = if_else(first_season == last_season, as.character(first_season), paste0(first_season, "-", last_season))
      ) %>%
      arrange(desc(last_season), driver_code)

    div(
      div(
        class = "profile-section-label",
        "Overall"
      ),
      div(
        class = "profile-card-grid profile-overall-grid",
        div(
          class = "metric profile-card",
          span("Constructor"),
          strong(constructor_label$constructor_name)
        ),
        div(
          class = "metric profile-card",
          span("Races"),
          strong(n_distinct(rows$season, rows$round))
        ),
        div(
          class = "metric profile-card",
          span("Car starts"),
          strong(nrow(rows))
        ),
        div(
          class = "metric profile-card",
          span("Best avg finish"),
          strong(if (nrow(best_finish) == 0) "Unavailable" else paste0(best_finish$family, " (", format_num(best_finish$avg_finish, 2), ")"))
        ),
        div(
          class = "metric profile-card",
          span("Best points rate"),
          strong(if (nrow(best_points) == 0) "Unavailable" else paste0(best_points$family, " (", format_num(best_points$points_per_start, 2), ")"))
        ),
        div(
          class = "metric profile-card",
          span("Toughest avg finish"),
          strong(if (nrow(worst_finish) == 0) "Unavailable" else paste0(worst_finish$family, " (", format_num(worst_finish$avg_finish, 2), ")"))
        )
      ),
      div(
        class = "profile-section-label",
        "Driver History"
      ),
      div(
        class = "constructor-history-grid",
        lapply(seq_len(nrow(driver_history)), function(i) {
          row <- driver_history[i, ]
          div(
            class = "metric constructor-card",
            span(row$season_label),
            strong(paste(row$driver_code, row$driver_name)),
            div(
              class = "constructor-card-stats",
              div(span("Starts"), strong(format_int(row$starts))),
              div(span("Avg finish"), strong(format_num(row$avg_finish, 1))),
              div(span("Pts/start"), strong(format_num(row$points_per_start, 1))),
              div(span("Wins"), strong(format_int(row$wins)))
            )
          )
        })
      )
    )
  })

  output$constructor_family_plot <- renderPlot({
    summary <- constructor_profile_family_summary()
    validate(need(nrow(summary) > 0, "No track-family results found for this constructor."))

    metric <- input$constructor_profile_metric
    metric_labels <- c(
      avg_finish = "Average finish",
      points_per_start = "Points per start",
      top10_rate = "Top-10 rate",
      podium_rate = "Podium rate",
      avg_quali_delta = "Average quali delta, sec",
      avg_fastest_lap_delta = "Average fastest-lap delta, sec"
    )

    plot_data <- summary %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(
        family_label = paste0(match(family_flag, family_flags), ". ", family),
        family_label = factor(
          family_label,
          levels = rev(paste0(seq_along(family_flags), ". ", family_labels[family_flags]))
        )
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      plot_data <- plot_data %>%
        mutate(value_label = percent(.data[[metric]], accuracy = 0.1))
    } else {
      plot_data <- plot_data %>%
        mutate(value_label = format_num(.data[[metric]], 2))
    }

    validate(need(nrow(plot_data) > 0, "Selected metric is not populated for this constructor."))
    axis_limits <- profile_metric_limits(constructor_profile_family_scale_summary(), metric)

    p <- ggplot(plot_data, aes(x = family_label, y = .data[[metric]])) +
      geom_col(fill = "#E10600", width = 0.72) +
      geom_text(aes(label = value_label), hjust = -0.12, size = 4.2, fontface = "bold", color = "#EDF2F8") +
      coord_flip(clip = "off") +
      labs(x = NULL, y = metric_labels[[metric]]) +
      theme_f1_dark(base_size = 14) +
      theme(
        plot.margin = margin(8, 34, 8, 8),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 13, face = "bold", color = "#AEB8C7"),
        axis.title = element_text(size = 14, face = "bold", color = "#AEB8C7")
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      p <- p + scale_y_continuous(limits = axis_limits, labels = percent)
    } else {
      p <- p + scale_y_continuous(limits = axis_limits)
    }

    p
  })

  output$constructor_family_table <- renderTable({
    summary <- constructor_profile_family_summary()

    if (nrow(summary) == 0) {
      return(tibble(Status = "No track-family results found for this constructor."))
    }

    summary %>%
      arrange(avg_finish) %>%
      transmute(
        Family = family,
        Starts = starts,
        `Avg Finish` = format_num(avg_finish, 1),
        `Points/Start` = format_num(points_per_start, 1),
        `Top-10` = format_pct(top10_rate),
        Podium = format_pct(podium_rate),
        DNF = format_pct(dnf_rate),
        `Avg Quali Delta` = format_num(avg_quali_delta, 3),
        `Avg Fastest-Lap Delta` = format_num(avg_fastest_lap_delta, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$constructor_cluster_plot <- renderPlot({
    summary <- constructor_profile_cluster_summary()
    validate(need(nrow(summary) > 0, "No track-cluster results found for this constructor."))

    metric <- input$constructor_profile_metric
    metric_labels <- c(
      avg_finish = "Average finish",
      points_per_start = "Points per start",
      top10_rate = "Top-10 rate",
      podium_rate = "Podium rate",
      avg_quali_delta = "Average quali delta, sec",
      avg_fastest_lap_delta = "Average fastest-lap delta, sec"
    )

    cluster_levels <- summary %>%
      arrange(cluster_number, cluster) %>%
      pull(cluster)

    plot_data <- summary %>%
      filter(!is.na(.data[[metric]])) %>%
      mutate(cluster = factor(cluster, levels = rev(cluster_levels)))

    if (metric %in% c("top10_rate", "podium_rate")) {
      plot_data <- plot_data %>%
        mutate(value_label = percent(.data[[metric]], accuracy = 0.1))
    } else {
      plot_data <- plot_data %>%
        mutate(value_label = format_num(.data[[metric]], 2))
    }

    validate(need(nrow(plot_data) > 0, "Selected metric is not populated for this constructor."))
    axis_limits <- profile_metric_limits(constructor_profile_cluster_scale_summary(), metric)

    p <- ggplot(plot_data, aes(x = cluster, y = .data[[metric]])) +
      geom_col(fill = "#23A6D5", width = 0.72) +
      geom_text(aes(label = value_label), hjust = -0.12, size = 4.2, fontface = "bold", color = "#EDF2F8") +
      coord_flip(clip = "off") +
      labs(x = NULL, y = metric_labels[[metric]]) +
      theme_f1_dark(base_size = 14) +
      theme(
        plot.margin = margin(8, 34, 8, 8),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 13, face = "bold", color = "#AEB8C7"),
        axis.title = element_text(size = 14, face = "bold", color = "#AEB8C7")
      )

    if (metric %in% c("top10_rate", "podium_rate")) {
      p <- p + scale_y_continuous(limits = axis_limits, labels = percent)
    } else {
      p <- p + scale_y_continuous(limits = axis_limits)
    }

    p
  })

  output$constructor_cluster_table <- renderTable({
    summary <- constructor_profile_cluster_summary()

    if (nrow(summary) == 0) {
      return(tibble(Status = "No track-cluster results found for this constructor."))
    }

    summary %>%
      arrange(avg_finish) %>%
      transmute(
        Cluster = cluster,
        `Similar Tracks` = similar_tracks,
        Starts = starts,
        `Avg Finish` = format_num(avg_finish, 1),
        `Points/Start` = format_num(points_per_start, 1),
        `Top-10` = format_pct(top10_rate),
        Podium = format_pct(podium_rate),
        DNF = format_pct(dnf_rate),
        `Avg Quali Delta` = format_num(avg_quali_delta, 3),
        `Avg Fastest-Lap Delta` = format_num(avg_fastest_lap_delta, 3)
      )
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$constructor_circuit_table <- renderTable({
    rows <- constructor_profile_base_rows()

    if (nrow(rows) == 0) {
      return(tibble(Status = "No circuit results found for this constructor."))
    }

    rows %>%
      group_by(circuit_id, circuit_name, track_cluster_id, track_cluster_label) %>%
      summarise(
        Starts = n(),
        `Avg Finish` = safe_mean(finish_position),
        `Points/Start` = sum(points, na.rm = TRUE) / n(),
        `Top-10` = mean(top10_finish, na.rm = TRUE),
        Podium = mean(podium, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(Starts >= 1) %>%
      arrange(`Avg Finish`, desc(`Points/Start`)) %>%
      transmute(
        Circuit = circuit_name,
        Cluster = cluster_display_label(track_cluster_id, track_cluster_label),
        Starts,
        `Avg Finish` = format_num(`Avg Finish`, 1),
        `Points/Start` = format_num(`Points/Start`, 1),
        `Top-10` = format_pct(`Top-10`),
        Podium = format_pct(Podium)
      ) %>%
      head(12)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)

  output$constructor_recent_results <- renderTable({
    rows <- constructor_profile_base_rows()

    if (nrow(rows) == 0) {
      return(tibble(Status = "No recent results found for this constructor."))
    }

    rows %>%
      arrange(desc(season), desc(round), finish_position) %>%
      transmute(
        Season = season,
        Round = round,
        Race = race_name,
        Circuit = circuit_name,
        Cluster = cluster_display_label(track_cluster_id, track_cluster_label),
        Driver = paste(driver_code, driver_name),
        Finish = format_int(finish_position),
        Grid = format_int(grid),
        Points = format_int(points),
        Status = status
      ) %>%
      head(24)
  }, striped = TRUE, hover = TRUE, bordered = FALSE)
}

shinyApp(ui, server)




