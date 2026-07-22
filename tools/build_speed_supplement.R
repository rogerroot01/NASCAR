library(dplyr)
library(readr)
library(f1dataR)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
app_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(app_dir, "data")
cache_dir <- file.path(data_dir, "speed_session_cache")
output_csv <- file.path(data_dir, "f1_speed_trap_supplement.csv")

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

stage1 <- read_csv(file.path(data_dir, "f1_stage1_driver_race_backbone_2018_2026.csv"), show_col_types = FALSE) %>%
  mutate(
    season = as.integer(season),
    round = as.integer(round)
  )

target_events <- stage1 %>%
  distinct(season, round, race_name) %>%
  filter(season >= 2025) %>%
  arrange(season, round)

as_num <- function(x) suppressWarnings(as.numeric(x))

summarise_event_speed <- function(season, round_id, race_name) {
  cache_file <- file.path(cache_dir, paste0("session_laps_", season, "_", sprintf("%02d", round_id), "_R.rds"))

  if (file.exists(cache_file)) {
    laps <- readRDS(cache_file)
  } else {
    message("Loading session laps: ", season, " round ", round_id, " - ", race_name)
    laps <- tryCatch(
      f1dataR::load_session_laps(
        season = season,
        round = round_id,
        session = "R",
        add_weather = FALSE
      ),
      error = function(e) {
        message("Failed: ", season, " round ", round_id, " - ", conditionMessage(e))
        tibble()
      }
    )
    saveRDS(laps, cache_file)
    Sys.sleep(0.5)
  }

  if (nrow(laps) == 0 || !"driver" %in% names(laps)) {
    return(tibble())
  }

  speed_cols <- intersect(c("speed_st", "speed_fl", "speed_i1", "speed_i2"), names(laps))
  if (length(speed_cols) == 0) return(tibble())

  laps %>%
    transmute(
      season = season,
      round = round_id,
      driver_code = driver,
      speed_st_kph = if ("speed_st" %in% names(laps)) as_num(speed_st) else NA_real_,
      speed_fl_kph = if ("speed_fl" %in% names(laps)) as_num(speed_fl) else NA_real_,
      speed_i1_kph = if ("speed_i1" %in% names(laps)) as_num(speed_i1) else NA_real_,
      speed_i2_kph = if ("speed_i2" %in% names(laps)) as_num(speed_i2) else NA_real_
    ) %>%
    group_by(season, round, driver_code) %>%
    summarise(
      max_speed_st_kph = if (all(is.na(speed_st_kph))) NA_real_ else max(speed_st_kph, na.rm = TRUE),
      max_speed_fl_kph = if (all(is.na(speed_fl_kph))) NA_real_ else max(speed_fl_kph, na.rm = TRUE),
      max_speed_i1_kph = if (all(is.na(speed_i1_kph))) NA_real_ else max(speed_i1_kph, na.rm = TRUE),
      max_speed_i2_kph = if (all(is.na(speed_i2_kph))) NA_real_ else max(speed_i2_kph, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(season, round) %>%
    mutate(
      race_max_speed_st_kph = if (all(is.na(max_speed_st_kph))) NA_real_ else max(max_speed_st_kph, na.rm = TRUE),
      speed_st_delta_kph = max_speed_st_kph - race_max_speed_st_kph
    ) %>%
    ungroup()
}

speed_supplement <- bind_rows(
  lapply(seq_len(nrow(target_events)), function(i) {
    summarise_event_speed(
      target_events$season[[i]],
      target_events$round[[i]],
      target_events$race_name[[i]]
    )
  })
)

write_csv(speed_supplement, output_csv)
message("Wrote ", nrow(speed_supplement), " rows to ", output_csv)
