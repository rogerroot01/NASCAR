library(dplyr)
library(readr)
library(ggplot2)
library(f1dataR)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
app_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
stage1_csv <- file.path(app_dir, "data", "f1_stage1_driver_race_backbone_2018_2026.csv")
track_dir <- file.path(app_dir, "www", "track_plots")
fallback_only <- identical(tolower(Sys.getenv("F1_TRACK_RENDER_MODE")), "fallback")
track_only <- Sys.getenv("F1_TRACK_ONLY", unset = "")
max_season_env <- Sys.getenv("F1_TRACK_MAX_SEASON", unset = "")
max_season <- suppressWarnings(as.integer(max_season_env))

dir.create(track_dir, recursive = TRUE, showWarnings = FALSE)

stage1 <- read_csv(stage1_csv, show_col_types = FALSE) %>%
  mutate(
    season = as.integer(season),
    round = as.integer(round),
    finish_position = as.numeric(finish_position)
  )

track_events <- stage1 %>%
  filter(!is.na(circuit_id), !is.na(season), !is.na(round)) %>%
  filter(track_only == "" | circuit_id == track_only) %>%
  filter(is.na(max_season) | season <= max_season) %>%
  group_by(circuit_id) %>%
  arrange(desc(season), desc(round), finish_position) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    season,
    round,
    circuit_id,
    circuit_name,
    race_name,
    driver_code
  ) %>%
  arrange(circuit_id)

plot_track_layout <- function(season, round_id, driver_code) {
  telemetry <- f1dataR::load_driver_telemetry(
    season = season,
    round = round_id,
    session = "Q",
    driver = driver_code,
    laps = "fastest"
  )

  circuit <- f1dataR::load_circuit_details(season = season, round = round_id)

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
    corners <- corners %>%
      mutate(
        labx = cos(angle * pi / 180) * 650 + x,
        laby = sin(angle * pi / 180) * 650 + y,
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

plot_corner_layout <- function(season, round_id) {
  circuit <- f1dataR::load_circuit_details(season = season, round = round_id)
  corners <- circuit$corners
  if (is.null(corners) || nrow(corners) == 0) {
    stop("No corner geometry returned.")
  }

  corners <- corners %>%
    arrange(distance) %>%
    mutate(
      corner_label = paste0(number, letter),
      labx = cos(angle * pi / 180) * 650 + x,
      laby = sin(angle * pi / 180) * 650 + y
    )

  ggplot(corners, aes(x = x, y = y)) +
    geom_path(color = "#E10600", linewidth = 2.4, lineend = "round") +
    geom_point(color = "#39A0ED", size = 2.2) +
    geom_label(
      aes(x = labx, y = laby, label = corner_label),
      size = 3,
      label.size = 0,
      fill = "#F8F9FA",
      color = "#202124"
    ) +
    coord_equal() +
    theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#101418", color = NA),
      panel.background = element_rect(fill = "#101418", color = NA)
    )
}

for (i in seq_len(nrow(track_events))) {
  event <- track_events[i, ]
  output_file <- file.path(track_dir, paste0(event$circuit_id, ".png"))

  if (file.exists(output_file)) {
    message("Skipping existing: ", basename(output_file))
    next
  }

  message(
    "Rendering ", event$circuit_id, " from ",
    event$season, " round ", event$round, " using ", event$driver_code
  )

  tryCatch({
    p <- if (fallback_only) {
      plot_corner_layout(event$season, event$round)
    } else {
      tryCatch(
        plot_track_layout(event$season, event$round, event$driver_code),
        error = function(e) {
          message("Telemetry render failed for ", event$circuit_id, "; trying corner geometry fallback.")
          plot_corner_layout(event$season, event$round)
        }
      )
    }

    ggsave(
      filename = output_file,
      plot = p,
      width = 9,
      height = 6,
      dpi = 150,
      bg = "#101418"
    )
  }, error = function(e) {
    message("Failed: ", event$circuit_id, " - ", conditionMessage(e))
  })
}

message("Track plots written to: ", track_dir)
