library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)

options(shiny.maxRequestSize = 30 * 1024^2)
`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[[1]])) y else x
aside <- shiny::tags$aside
main <- shiny::tags$main

app_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
project_dir <- normalizePath(file.path(app_dir, ".."), winslash = "/", mustWork = TRUE)
bundled_data <- file.path(app_dir, "data")
project_data <- file.path(project_dir, "data")
data_path <- function(name) {
  compact_name <- sub("\\.csv$", ".rds", name)
  compressed_name <- paste0(name, ".gz")
  candidates <- c(
    file.path(bundled_data, name),
    file.path(bundled_data, compact_name),
    file.path(bundled_data, compressed_name),
    file.path(project_data, name)
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) hit[[1]] else candidates[[1]]
}
read_optional <- function(name) {
  path <- data_path(name)
  if (!file.exists(path)) return(tibble())
  if (grepl("\\.rds$", path, ignore.case = TRUE)) return(as_tibble(readRDS(path)))
  suppressMessages(read_csv(path, show_col_types = FALSE, progress = FALSE))
}
read_required <- function(name) {
  path <- data_path(name)
  if (!file.exists(path)) stop("Required app artifact is missing: ", name, ". Run the numbered pipeline through the matching Step 4.", call. = FALSE)
  if (grepl("\\.rds$", path, ignore.case = TRUE)) return(as_tibble(readRDS(path)))
  suppressMessages(read_csv(path, show_col_types = FALSE, progress = FALSE))
}
num <- function(x) suppressWarnings(as.numeric(x))
fmt_num <- function(x, digits = 1) ifelse(is.finite(num(x)), format(round(num(x), digits), nsmall = digits, big.mark = ","), "—")
fmt_int <- function(x) ifelse(is.finite(num(x)), format(round(num(x)), big.mark = ","), "—")
fmt_pct <- function(x, digits = 1) ifelse(is.finite(num(x)), percent(num(x), accuracy = 10^-digits), "—")
safe_mean <- function(x) if (any(is.finite(num(x)))) mean(num(x), na.rm = TRUE) else NA_real_
valid_choices <- function(x) sort(unique(as.character(x[!is.na(x) & nzchar(trimws(as.character(x)))])))

model_labels <- c(
  recency_baseline = "Recency baseline",
  xgb_owner_track = "XGBoost — owner + track",
  xgb_no_owner = "XGBoost — no owner",
  xgb_drafting_specialist = "Drafting specialist",
  xgb_road_specialist = "Road-course specialist",
  xgb_speedway_specialist = "Speedway specialist",
  routed_consensus = "Routed consensus",
  xgb_short_steep_specialist = "Short / steep oval specialist",
  selected_ensemble = "Selected-model ensemble"
)
model_label <- function(x) unname(ifelse(x %in% names(model_labels), model_labels[x], str_to_title(str_replace_all(x, "_", " "))))
all_models <- c("recency_baseline","xgb_owner_track","xgb_no_owner","xgb_drafting_specialist","xgb_road_specialist","xgb_speedway_specialist","xgb_short_steep_specialist","routed_consensus")
qualifying_models <- setdiff(all_models, "xgb_short_steep_specialist")
default_models <- c("xgb_owner_track", "xgb_no_owner", "routed_consensus")

load_family <- function(stage, family) {
  historical <- read_required(stage)
  current <- read_required(paste0("nascar_stage15_current_", family, "_model_predictions_latest.csv"))
  if (nrow(current)) current <- current %>% mutate(data_split = "upcoming")
  id_columns <- c("event_id","race_id","track_id","track_cluster_id","driver_id","car_number","owner_id","manufacturer_id")
  historical <- historical %>% mutate(across(any_of(id_columns), as.character))
  current <- current %>% mutate(across(any_of(id_columns), as.character))
  bind_rows(historical, current)
}

active_strategy <- read_required("nascar_active_strategy.csv")
if (nrow(active_strategy) != 1L || !"strategy" %in% names(active_strategy)) stop("nascar_active_strategy.csv is invalid.", call. = FALSE)
active_strategy_name <- as.character(active_strategy$strategy[[1]])
if (!active_strategy_name %in% c("annual", "rolling")) {
  required_runner <- if ("required_evaluation_runner" %in% names(active_strategy)) active_strategy$required_evaluation_runner[[1]] else "the matching Step 4"
  stop("App results are not active (state: ", active_strategy_name, "). Run ", required_runner, " before opening the app.", call. = FALSE)
}

qualifying <- load_family("nascar_stage7_qualifying_predictions_2018_2026.csv", "qualifying")
finish <- load_family("nascar_stage11_finish_predictions_2018_2026.csv", "finish")
probability <- load_family("nascar_stage12_probability_predictions_2018_2026.csv", "probability")
points <- load_family("nascar_stage13_points_predictions_2018_2026.csv", "points")
active_test_rows <- finish %>% filter(.data$season %in% c(2025, 2026), .data$data_split == "test")
if (!nrow(active_test_rows)) stop("The active finish file contains no 2025-2026 test rows.", call. = FALSE)
if (active_strategy_name == "rolling" &&
    (!"training_races" %in% names(active_test_rows) || any(!is.finite(num(active_test_rows$training_races))))) {
  stop("Strategy says rolling, but the canonical finish rows are not race-by-race evaluation rows. Rerun rolling Step 4.", call. = FALSE)
}

family_metrics <- list(
  qualifying = read_optional("nascar_stage7_qualifying_metrics_2018_2026.csv"),
  finish = read_optional("nascar_stage11_finish_metrics_2018_2026.csv"),
  probability = read_optional("nascar_stage12_probability_metrics_2018_2026.csv"),
  points = read_optional("nascar_stage13_points_metrics_2018_2026.csv")
)
family_specs <- list(
  qualifying = read_optional("nascar_stage7_qualifying_model_specs_2018_2026.csv"),
  finish = read_optional("nascar_stage11_finish_model_specs_2018_2026.csv"),
  probability = read_optional("nascar_stage12_probability_model_specs_2018_2026.csv"),
  points = read_optional("nascar_stage13_points_model_specs_2018_2026.csv")
)

forecast <- read_required("nascar_stage18_current_forecast_with_safe_overlays_latest.csv")
qualifying_chatter <- qualifying
if (nrow(forecast) && all(c("predicted_qualifying_position","predicted_qualifying_position_adjusted") %in% names(forecast))) {
  qualifying_chatter_shift <- forecast %>%
    transmute(season=num(season),round=num(round),driver_id=as.character(driver_id),
              chatter_qualifying_shift=num(predicted_qualifying_position_adjusted)-num(predicted_qualifying_position)) %>%
    distinct(season,round,driver_id,.keep_all=TRUE)
  qualifying_chatter <- qualifying %>%
    left_join(qualifying_chatter_shift,by=c("season","round","driver_id")) %>%
    mutate(predicted_position_score=.data$predicted_position_score+coalesce(.data$chatter_qualifying_shift,0)) %>%
    group_by(.data$season,.data$round,.data$model) %>%
    mutate(predicted_qualifying_rank=rank(.data$predicted_position_score,ties.method="first")) %>%
    ungroup() %>% select(-chatter_qualifying_shift)
}
tracks <- read_required("nascar_stage4_track_profile_inventory_2018_2026.csv")
current_track_features <- read_required("nascar_stage14_upcoming_race_features_latest.csv")
if (nrow(current_track_features)) current_track_features <- current_track_features %>%
  select(any_of(c("season","round","race_name","track_name","track_primary_family","track_cluster_id","track_cluster_label",
                  "track_length_miles","max_banking_deg","surface","track_type","length_bucket","banking_bucket",
                  "is_high_banked","is_tire_wear_heavy","is_braking_heavy","is_high_speed","is_pack_racing"))) %>%
  distinct(season,round,.keep_all=TRUE)
history <- read_required("nascar_stage1_stage2_stage3_driver_race_backbone_2018_2026.csv")
if (nrow(history) && !"track_primary_family" %in% names(history) && nrow(tracks)) {
  history <- history %>% left_join(
    tracks %>% distinct(track_name, .keep_all=TRUE) %>% select(track_name,track_primary_family,track_cluster_id),
    by="track_name"
  )
}
backtest <- read_required("nascar_stage16_active_backtest_2025_2026.csv")
backtest_metrics <- backtest %>% filter(.data$season == 2025L) %>% summarise(
  Races = n_distinct(.data$round),
  `Winner-pick accuracy` = mean(.data$predicted_finish_rank == 1 & .data$target_finish_position == 1, na.rm = TRUE) /
    mean(.data$predicted_finish_rank == 1, na.rm = TRUE),
  `Finish-rank MAE` = mean(abs(.data$predicted_finish_rank - .data$target_finish_position), na.rm = TRUE)
)
overlay_backtest <- read_optional("nascar_stage18_2025_backtest_safe_overlay.csv")
overlay_metrics <- read_optional("nascar_stage18_2025_backtest_safe_overlay_metrics.csv")
dk <- read_required("nascar_stage19_draftkings_driver_projections_latest.csv")
dk_lineups <- read_required("nascar_stage19_draftkings_top_lineups_latest.csv")
dk_members <- read_required("nascar_stage19_draftkings_top_lineup_members_latest.csv")
dk_initial_lineups <- if ("projection_variant" %in% names(dk_lineups)) {
  dk_lineups %>% filter(.data$projection_variant == "base")
} else dk_lineups
dk_backtest <- read_optional("nascar_stage20_2025_draftkings_points_backtest.csv")
dk_backtest_metrics <- read_optional("nascar_stage20_2025_draftkings_points_backtest_metrics.csv")
external_audit <- read_required("nascar_stage17_external_inputs_audit.csv")
winner_odds <- read_required("nascar_stage17_market_winner_full_boards_2025_2026.csv")
top3_odds <- read_optional("nascar_stage17_market_top3_verified_partial_2025_2026.csv")

winner_odds_lookup <- if (!nrow(winner_odds)) {
  tibble(season=double(),round=double(),driver_id=character(),win_odds_american=double(),
         win_odds_decimal=double(),win_market_probability=double(),win_sportsbook=character())
} else winner_odds %>%
    transmute(season=num(season),round=num(round),driver_id=as.character(driver_id),
              win_odds_american=num(odds_american),win_odds_decimal=num(odds_decimal),
              win_market_probability=num(if("market_no_vig_probability"%in%names(winner_odds))market_no_vig_probability else implied_probability),win_sportsbook=sportsbook) %>%
    distinct(season,round,driver_id,.keep_all=TRUE)
plackett_luce_top3 <- function(probability) {
  p <- num(probability); p[!is.finite(p) | p < 0] <- 0
  if (sum(p) <= 0) return(rep(NA_real_, length(p)))
  p <- p / sum(p); n <- length(p); out <- numeric(n)
  for (i in seq_len(n)) {
    out[[i]] <- p[[i]]; others <- setdiff(seq_len(n), i)
    for (j in others) {
      out[[i]] <- out[[i]] + p[[j]] * p[[i]] / (1 - p[[j]])
      remaining <- setdiff(others, j)
      if (length(remaining)) out[[i]] <- out[[i]] + sum(p[[j]] * p[remaining] / (1 - p[[j]]) * p[[i]] / (1 - p[[j]] - p[remaining]), na.rm=TRUE)
    }
  }
  pmin(pmax(out, 0), 1)
}
decimal_to_american <- function(decimal) ifelse(decimal >= 2, 100 * (decimal - 1), -100 / (decimal - 1))
derived_top3_odds <- if (!nrow(winner_odds)) {
  tibble(season=double(),round=double(),driver_id=character(),top3_market_probability=double(),
         top3_odds_decimal=double(),top3_odds_american=double(),top3_sportsbook=character())
} else winner_odds %>%
    transmute(season=num(season),round=num(round),driver_id=as.character(driver_id),market_probability=num(if("market_no_vig_probability"%in%names(winner_odds))market_no_vig_probability else implied_probability)) %>%
    group_by(season,round) %>%
    mutate(top3_market_probability=plackett_luce_top3(market_probability),top3_odds_decimal=1/top3_market_probability,top3_odds_american=decimal_to_american(top3_odds_decimal),top3_sportsbook="Implied fair from winner board") %>%
    ungroup() %>% select(-market_probability)
verified_top3_odds <- if (!nrow(top3_odds)) {
  tibble(season=double(),round=double(),driver_id=character(),top3_odds_american=double(),
         top3_odds_decimal=double(),top3_market_probability=double(),top3_sportsbook=character())
} else top3_odds %>%
    transmute(season=num(season),round=num(round),driver_id=as.character(driver_id),top3_odds_american=num(odds_american),top3_odds_decimal=num(odds_decimal),top3_market_probability=num(implied_probability),top3_sportsbook=sportsbook)
top3_odds_lookup <- if (!nrow(derived_top3_odds)) {
  tibble(season=double(),round=double(),driver_id=character(),top3_odds_american=double(),
         top3_odds_decimal=double(),top3_market_probability=double(),top3_sportsbook=character())
} else bind_rows(
  verified_top3_odds%>%mutate(source_priority=1L),
  derived_top3_odds%>%mutate(source_priority=2L)
) %>% arrange(source_priority) %>% distinct(season,round,driver_id,.keep_all=TRUE) %>% select(-source_priority)

for (column in intersect(c("season", "round", "finish_position", "start_position", "points", "laps_led", "driver_rating", "average_running_position", "quality_pass_rate", "fastest_lap_pct"), names(history))) history[[column]] <- num(history[[column]])

specialist_model_map <- c(
  drafting_superspeedway="xgb_drafting_specialist",
  road_course="xgb_road_specialist",
  conventional_speedway="xgb_speedway_specialist",
  short_steep_oval="xgb_short_steep_specialist"
)
routed_specialist_model_lookup <- tidyr::crossing(
  route_group = names(specialist_model_map),
  outcome = c("finish", "probability", "points")
) %>%
  mutate(
    model = unname(specialist_model_map[route_group]),
    choice_id = paste(outcome, model, sep="__"),
    route_label = recode(route_group,drafting_superspeedway="Drafting",road_course="Road Course",conventional_speedway="Speedway",short_steep_oval="Short / Steep"),
    outcome_label = recode(outcome,finish="Position",probability="Probability",points="Points"),
    choice_label = paste(route_label, outcome_label, sep=" — ")
  ) %>%
  arrange(match(route_group,names(specialist_model_map)),match(outcome,c("finish","probability","points")))
routed_specialist_model_choices <- setNames(routed_specialist_model_lookup$choice_id,routed_specialist_model_lookup$choice_label)
default_routed_specialist_models <- function(route_group) routed_specialist_model_lookup %>%
  filter(.data$route_group %in% .env$route_group) %>% pull(.data$choice_id)
specialist_route <- function(track_family, track_name) case_when(
  track_family=="drafting_superspeedway" ~ "drafting_superspeedway",
  track_family%in%c("road_course","roval","street_course") ~ "road_course",
  track_family=="short_track"|str_detect(track_name,regex("Dover",ignore_case=TRUE)) ~ "short_steep_oval",
  track_family%in%c("intermediate_speedway","large_speedway") ~ "conventional_speedway",
  TRUE ~ "overall_fallback"
)
specialist_route_label <- function(x) recode(x,drafting_superspeedway="Drafting",road_course="Road Course",conventional_speedway="Conventional Speedway",short_steep_oval="Short / Steep Oval",overall_fallback="Overall fallback")
specialist_keys <- c("season","round","driver_id","model")
active_specialists <- finish %>%
  filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%unname(specialist_model_map)) %>%
  select(any_of(c(specialist_keys,"race_name","race_date","track_name","track_primary_family","driver_name","owner_name","manufacturer","target_finish_position","target_win","target_top3","predicted_finish_position","predicted_finish_rank","finish_route"))) %>%
  inner_join(probability%>%filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%unname(specialist_model_map))%>%select(any_of(c(specialist_keys,"win_probability","top3_probability","predicted_win_rank","predicted_top3_rank","probability_route"))),by=specialist_keys) %>%
  inner_join(points%>%filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%unname(specialist_model_map))%>%select(any_of(c(specialist_keys,"target_points","predicted_points","predicted_points_rank","points_route"))),by=specialist_keys)
specialist_all <- active_specialists %>%
  mutate(
    season=num(season),round=num(round),driver_id=as.character(driver_id),
    route_group=specialist_route(track_primary_family,track_name),
    model_route_group=names(specialist_model_map)[match(model,unname(specialist_model_map))],
    active_model=unname(specialist_model_map[route_group])
  ) %>%
  filter(route_group%in%names(specialist_model_map),model_route_group%in%names(specialist_model_map))
specialist_history <- specialist_all %>% filter(route_group==model_route_group)

family_colors <- c(drafting_superspeedway="#F4C542", short_track="#E5533D", intermediate="#23A6D5", conventional_speedway="#23A6D5", large_speedway="#8A6FE8", road_course="#38B27A", street_course="#38B27A", roval="#2DCCB3", dirt_oval="#B7834A")
metric_card <- function(label, value, note = NULL, accent = "gold") div(class=paste("metric-card", accent), div(class="metric-label", label), div(class="metric-value", value), if (!is.null(note)) div(class="metric-note", note))

race_choices <- function(data, season) {
  season_value <- as.integer(season)
  data %>% filter(.data$season == .env$season_value) %>% distinct(round, race_name, track_name, data_split) %>%
    arrange(round) %>% mutate(label = paste0("R", sprintf("%02d",as.integer(round)), " — ", track_name, " — ", race_name, if_else(data_split == "upcoming", " (upcoming)", "")))
}

track_schedule_2026 <- finish %>%
  filter(season==2026) %>%
  mutate(.upcoming=data_split=="upcoming") %>%
  arrange(round,desc(.upcoming)) %>%
  distinct(round,.keep_all=TRUE) %>%
  transmute(season,round,race_name,track_name,track_primary_family,track_cluster_id,data_split,
            schedule_key=paste(round,track_name,sep="||"),
            schedule_label=paste0("R",sprintf("%02d",as.integer(round))," — ",track_name," — ",race_name)) %>%
  arrange(round)
track_schedule_choices <- setNames(track_schedule_2026$schedule_key,track_schedule_2026$schedule_label)
default_track_schedule_key <- {
  upcoming <- track_schedule_2026 %>% filter(data_split=="upcoming") %>% slice_min(round,n=1,with_ties=FALSE)
  if(nrow(upcoming)) upcoming$schedule_key[[1]] else if(nrow(track_schedule_2026)) tail(track_schedule_2026$schedule_key,1) else NULL
}

first_metadata_value <- function(sources,name,default=NA) {
  for(source in sources) if(nrow(source) && name%in%names(source)) {
    value <- source[[name]][[1]]
    if(length(value) && !is.na(value) && nzchar(as.character(value))) return(value)
  }
  default
}
race_track_metadata <- function(row) {
  current <- if(nrow(current_track_features) && all(c("season","round")%in%names(row))) current_track_features %>%
    filter(season==num(row$season[[1]]),round==num(row$round[[1]])) %>% slice(1) else tibble()
  track_name <- first_metadata_value(list(current,row),"track_name","")
  inventory <- tracks %>% filter(.data$track_name==.env$track_name) %>% slice(1)
  sources <- list(current,row,inventory)
  list(
    track_name=track_name,
    family=first_metadata_value(sources,"track_primary_family","unknown"),
    cluster=first_metadata_value(sources,"track_cluster_id","unknown"),
    cluster_label=first_metadata_value(sources,"track_cluster_label",""),
    length=as.numeric(first_metadata_value(sources,"track_length_miles",NA_real_)),
    banking=as.numeric(first_metadata_value(sources,"max_banking_deg",NA_real_)),
    surface=first_metadata_value(sources,"surface",""),
    high_banked=isTRUE(as.logical(first_metadata_value(sources,"is_high_banked",FALSE))),
    tire_wear=isTRUE(as.logical(first_metadata_value(sources,"is_tire_wear_heavy",FALSE))),
    braking=isTRUE(as.logical(first_metadata_value(sources,"is_braking_heavy",FALSE))),
    high_speed=isTRUE(as.logical(first_metadata_value(sources,"is_high_speed",FALSE))),
    pack_racing=isTRUE(as.logical(first_metadata_value(sources,"is_pack_racing",FALSE)))
  )
}
track_characteristics_label <- function(row) {
  meta <- race_track_metadata(row)
  flags <- c(if(meta$high_banked)"High-banked",if(meta$tire_wear)"Tire-wear heavy",if(meta$braking)"Braking-heavy",if(meta$high_speed)"High-speed",if(meta$pack_racing)"Pack racing")
  parts <- c(str_to_title(str_replace_all(meta$family,"_"," ")),
             str_to_title(str_replace_all(meta$cluster,"_"," ")),
             if(is.finite(meta$length))paste0(format(meta$length,nsmall=if(meta$length<1)3 else 2)," mi"),
             if(is.finite(meta$banking))paste0(format(round(meta$banking),trim=TRUE),"° banking"),flags)
  paste(parts[nzchar(parts)],collapse=" • ")
}

make_ensemble <- function(rows, family) {
  if (!nrow(rows)) return(rows)
  keys <- c("data_split","season","round","event_id","race_id","race_name","track_id","track_name","track_primary_family","track_cluster_id","race_date","driver_id","driver_name","car_number","owner_id","owner_name","manufacturer_id","manufacturer")
  keys <- intersect(keys, names(rows))
  targets <- switch(family,
    qualifying = c("target_qualifying_position","target_qualifying_speed","target_qualifying_speed_deficit_pct"),
    finish = c("target_finish_position","target_finish_score_0_1","target_win","target_top3"),
    probability = c("target_win","target_top3","target_finish_position"),
    points = c("target_points","target_stage_points","target_finish_position","actual_points_rank"))
  targets <- intersect(targets, names(rows))
  grouped <- rows %>% group_by(across(all_of(keys)))
  if (family == "qualifying") {
    out <- grouped %>% summarise(across(all_of(targets), ~first(.x)), predicted_position_score=safe_mean(predicted_position_score), predicted_speed_deficit_pct=safe_mean(predicted_speed_deficit_pct), .groups="drop") %>%
      group_by(season, round) %>% mutate(predicted_qualifying_rank=rank(predicted_position_score, ties.method="first"), qualifying_route="selected_models") %>% ungroup()
  } else if (family == "finish") {
    out <- grouped %>% summarise(across(all_of(targets), ~first(.x)), predicted_finish_score=safe_mean(predicted_finish_score), predicted_finish_position=safe_mean(predicted_finish_position), .groups="drop") %>%
      group_by(season, round) %>% mutate(predicted_finish_rank=rank(predicted_finish_position, ties.method="first"), finish_route="selected_models") %>% ungroup()
  } else if (family == "probability") {
    out <- grouped %>% summarise(across(all_of(targets), ~first(.x)), raw_win_probability=safe_mean(win_probability), raw_top3_probability=safe_mean(top3_probability), .groups="drop") %>%
      group_by(season, round) %>% mutate(win_probability=raw_win_probability/sum(raw_win_probability,na.rm=TRUE), top3_probability=3*raw_top3_probability/sum(raw_top3_probability,na.rm=TRUE), predicted_win_rank=rank(-win_probability,ties.method="first"), predicted_top3_rank=rank(-top3_probability,ties.method="first"), probability_route="selected_models") %>% ungroup()
  } else {
    out <- grouped %>% summarise(across(all_of(targets), ~first(.x)), predicted_points=safe_mean(predicted_points), .groups="drop") %>%
      group_by(season, round) %>% mutate(predicted_points_rank=rank(-predicted_points,ties.method="first"), points_route="selected_models") %>% ungroup()
  }
  out %>% mutate(model="selected_ensemble")
}

scored_metrics <- function(rows, family) {
  if (!nrow(rows)) return(tibble())
  if (family == "qualifying") {
    base <- rows %>% group_by(model) %>% summarise(Rows=n(), RMSE=sqrt(mean((predicted_position_score-target_qualifying_position)^2,na.rm=TRUE)), MAE=mean(abs(predicted_position_score-target_qualifying_position),na.rm=TRUE), `Rank MAE`=mean(abs(predicted_qualifying_rank-target_qualifying_position),na.rm=TRUE), .groups="drop")
    picks <- rows %>% group_by(model,season,round) %>% slice_min(predicted_qualifying_rank,n=1,with_ties=FALSE) %>% group_by(model) %>% summarise(`Pole pick`=mean(target_qualifying_position==1,na.rm=TRUE),.groups="drop")
    front_row <- rows %>% filter(predicted_qualifying_rank<=2) %>% group_by(model) %>% summarise(`Front-row hit`=mean(target_qualifying_position<=2,na.rm=TRUE),.groups="drop")
    base %>% left_join(picks,by="model") %>% left_join(front_row,by="model")
  } else if (family == "finish") {
    base <- rows %>% group_by(model) %>% summarise(Rows=n(), RMSE=sqrt(mean((predicted_finish_position-target_finish_position)^2,na.rm=TRUE)), MAE=mean(abs(predicted_finish_position-target_finish_position),na.rm=TRUE), `Rank MAE`=mean(abs(predicted_finish_rank-target_finish_position),na.rm=TRUE),.groups="drop")
    picks <- rows %>% group_by(model,season,round) %>% slice_min(predicted_finish_rank,n=1,with_ties=FALSE) %>% group_by(model) %>% summarise(`Winner pick`=mean(target_finish_position==1,na.rm=TRUE),.groups="drop")
    top3 <- rows %>% filter(predicted_finish_rank<=3) %>% group_by(model) %>% summarise(`Top-3 hit`=mean(target_finish_position<=3,na.rm=TRUE),.groups="drop")
    base %>% left_join(picks,by="model") %>% left_join(top3,by="model")
  } else if (family == "probability") {
    base <- rows %>% group_by(model) %>% summarise(Rows=n(), `Win Brier`=mean((win_probability-target_win)^2,na.rm=TRUE), `Top-3 Brier`=mean((top3_probability-target_top3)^2,na.rm=TRUE), .groups="drop")
    picks <- rows %>% group_by(model,season,round) %>% slice_min(predicted_win_rank,n=1,with_ties=FALSE) %>% group_by(model) %>% summarise(`Winner pick`=mean(target_win==1,na.rm=TRUE),.groups="drop")
    top3 <- rows %>% filter(predicted_top3_rank<=3) %>% group_by(model) %>% summarise(`Top-3 hit`=mean(target_top3==1,na.rm=TRUE),.groups="drop")
    base %>% left_join(picks,by="model") %>% left_join(top3,by="model")
  } else {
    base <- rows %>% group_by(model) %>% summarise(Rows=n(), RMSE=sqrt(mean((predicted_points-target_points)^2,na.rm=TRUE)), MAE=mean(abs(predicted_points-target_points),na.rm=TRUE), `Rank MAE`=mean(abs(predicted_points_rank-actual_points_rank),na.rm=TRUE),.groups="drop")
    picks <- rows %>% group_by(model,season,round) %>% slice_min(predicted_points_rank,n=1,with_ties=FALSE) %>% group_by(model) %>% summarise(`Winner pick`=mean(target_finish_position==1,na.rm=TRUE),.groups="drop")
    base %>% left_join(picks,by="model")
  }
}

american_label <- function(x) ifelse(is.finite(num(x)),paste0(ifelse(num(x)>0,"+",""),round(num(x))),"—")

family_consensus_rows <- function(data, family, models) {
  models <- intersect(models,all_models)
  active_rows <- data %>% filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%models)
  if(nrow(active_rows)) {
    active_rows <- if(n_distinct(active_rows$model)>1) make_ensemble(active_rows,family) else active_rows %>% mutate(model="selected_ensemble")
    active_contract <- if(family=="finish") active_rows %>% transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_finish_rank,predicted_value=predicted_finish_position,actual_finish=target_finish_position)
      else if(family=="probability") active_rows %>% transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_win_rank,predicted_value=win_probability,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability)
      else active_rows %>% transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_points_rank,predicted_value=predicted_points,actual_finish=target_finish_position)
    if(family!="probability") {
      prob_rows <- probability %>% filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%models)
      prob_rows <- if(n_distinct(prob_rows$model)>1) make_ensemble(prob_rows,"probability") else prob_rows
      active_contract <- active_contract %>% left_join(prob_rows%>%select(season,round,driver_id,model_win_probability=win_probability,model_top3_probability=top3_probability),by=c("season","round","driver_id"))
    }
  } else active_contract <- tibble()
  active_contract%>%mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
}

rows_to_bets <- function(rows) {
  if(!nrow(rows)) return(tibble())
  winner_bets <- rows %>% filter(consensus_rank==1) %>% left_join(winner_odds_lookup,by=c("season","round","driver_id")) %>%
    transmute(season,round,race_name,bet_market="win",consensus_rank,driver_name,owner_name,predicted_value,model_probability=model_win_probability,odds_american=win_odds_american,odds_decimal=win_odds_decimal,market_probability=win_market_probability,odds_source=win_sportsbook,actual_finish,bet_won=actual_winner) %>%
    mutate(stake=if_else(is.finite(actual_finish)&is.finite(odds_decimal),1,0),profit=case_when(stake==0~NA_real_,bet_won~odds_decimal-1,TRUE~-1),model_edge=model_probability-market_probability)
  podium_bets <- rows %>% filter(consensus_rank<=3) %>% left_join(top3_odds_lookup,by=c("season","round","driver_id")) %>%
    transmute(season,round,race_name,bet_market="podium",consensus_rank,driver_name,owner_name,predicted_value,model_probability=model_top3_probability,odds_american=top3_odds_american,odds_decimal=top3_odds_decimal,market_probability=top3_market_probability,odds_source=top3_sportsbook,actual_finish,bet_won=actual_top3) %>%
    mutate(stake=if_else(is.finite(actual_finish)&is.finite(odds_decimal),1,0),profit=case_when(stake==0~NA_real_,bet_won~odds_decimal-1,TRUE~-1),model_edge=model_probability-market_probability)
  bind_rows(winner_bets,podium_bets)%>%mutate(roi=if_else(stake>0,profit/stake,NA_real_),bet_status=case_when(!is.finite(actual_finish)~"No result",stake==0~"No odds",bet_won~"Won",TRUE~"Lost"))
}

summarise_bets_window <- function(bets,start_season,end_season) {
  lo<-min(as.integer(start_season),as.integer(end_season));hi<-max(as.integer(start_season),as.integer(end_season))
  x<-bets%>%filter(season>=lo,season<=hi,is.finite(actual_finish))
  if(!nrow(x)) return(tibble())
  summarise_market<-function(tbl,market) {
    priced_stake<-sum(tbl$stake,na.rm=TRUE)
    priced_profit<-if(priced_stake>0)sum(tbl$profit,na.rm=TRUE)else NA_real_
    tibble(
      bet_market=market,races=n_distinct(paste(tbl$season,tbl$round)),bets=nrow(tbl),
      wins=sum(tbl$bet_won,na.rm=TRUE),hit_rate=wins/bets,priced_bets=sum(tbl$stake>0,na.rm=TRUE),
      avg_edge=safe_mean(ifelse(tbl$stake>0,tbl$model_edge,NA_real_)),stake=priced_stake,
      profit=priced_profit,roi=ifelse(priced_stake>0,priced_profit/priced_stake,NA_real_)
    )
  }
  by_market<-x%>%group_split(bet_market)%>%lapply(function(tbl)summarise_market(tbl,first(tbl$bet_market)))%>%bind_rows()
  combined<-summarise_market(x,"combined")%>%mutate(avg_edge=NA_real_)
  bind_rows(by_market,combined)%>%mutate(period=paste0(lo,"–",hi))%>%arrange(match(bet_market,c("win","podium","combined")))
}

render_roi_table <- function(x) x%>%transmute(Period=period,Market=recode(bet_market,win="Winner",podium="Podium",combined="Combined"),Races=races,Bets=bets,Wins=wins,`Hit rate`=fmt_pct(hit_rate,1),`Priced bets`=priced_bets,`Average edge`=ifelse(bet_market!="combined"&is.finite(avg_edge),fmt_pct(avg_edge,1),"—"),Stake=fmt_num(stake,0),Profit=fmt_num(profit,2),ROI=fmt_pct(roi,1))
render_bets_table <- function(x) x%>%mutate(bet_market=recode(bet_market,win="Winner",podium="Podium"))%>%transmute(Market=bet_market,Rank=fmt_int(consensus_rank),Driver=driver_name,Owner=owner_name,Projection=fmt_num(predicted_value,2),`Value probability`=fmt_pct(model_probability,1),Odds=american_label(odds_american),Source=coalesce(odds_source,"Missing"),`Market %`=fmt_pct(market_probability,1),Edge=fmt_pct(model_edge,1),Result=bet_status,`Actual finish`=fmt_int(actual_finish),Stake=fmt_num(stake,0),Profit=fmt_num(profit,2),ROI=fmt_pct(roi,1))

qualifying_consensus_rows <- function(data,models) {
  models<-intersect(models,all_models)
  active<-data%>%filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"),model%in%models)
  if(nrow(active)) {
    active<-if(n_distinct(active$model)>1)make_ensemble(active,"qualifying")else active%>%mutate(model="selected_ensemble")
    active<-active%>%transmute(season,round,race_name,consensus_rank=predicted_qualifying_rank,actual_qualifying=target_qualifying_position)
  }
  active
}

summarise_qualifying_window <- function(rows,start_season,end_season) {
  lo<-min(as.integer(start_season),as.integer(end_season));hi<-max(as.integer(start_season),as.integer(end_season))
  x<-rows%>%filter(season>=lo,season<=hi,is.finite(actual_qualifying))
  if(!nrow(x))return(tibble())
  pole<-x%>%filter(consensus_rank==1)
  front<-x%>%filter(consensus_rank<=2)
  tibble(Period=paste0(lo,"–",hi),Races=n_distinct(paste(x$season,x$round)),`Pole picks`=nrow(pole),`Pole hits`=sum(pole$actual_qualifying==1,na.rm=TRUE),`Pole hit rate`=fmt_pct(mean(pole$actual_qualifying==1,na.rm=TRUE),1),`Front-row picks`=nrow(front),`Front-row hits`=sum(front$actual_qualifying<=2,na.rm=TRUE),`Front-row hit rate`=fmt_pct(mean(front$actual_qualifying<=2,na.rm=TRUE),1))
}

chatter_contract <- function(data,adjusted=FALSE) {
  if(!nrow(data))return(tibble())
  rank_col<-if(adjusted)"predicted_finish_rank_adjusted"else"predicted_finish_rank"
  finish_col<-if(adjusted)"predicted_finish_position_adjusted"else"predicted_finish_position"
  win_col<-if(adjusted)"win_probability_adjusted"else"win_probability"
  top3_col<-if(adjusted)"top3_probability_adjusted"else"top3_probability"
  data%>%transmute(season,round,race_name,driver_id=as.character(driver_id),driver_name,owner_name,manufacturer,consensus_rank=.data[[rank_col]],predicted_value=.data[[finish_col]],actual_finish=target_finish_position,model_win_probability=.data[[win_col]],model_top3_probability=.data[[top3_col]])%>%mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
}

bubble_performance_plot <- function(data, entity, min_starts=2, limit=14, accent="#F4C542") {
  summary <- data %>%
    filter(is.finite(finish_position), !is.na(.data[[entity]]), nzchar(trimws(as.character(.data[[entity]])))) %>%
    group_by(label=.data[[entity]]) %>%
    summarise(starts=n(), avg_finish=mean(finish_position,na.rm=TRUE), wins=sum(finish_position==1,na.rm=TRUE), win_rate=wins/starts, .groups="drop")
  eligible <- summary %>% filter(starts>=min_starts)
  if(!nrow(eligible)) eligible <- summary
  eligible %>%
    arrange(avg_finish,desc(starts)) %>% slice_head(n=limit) %>%
    mutate(label=reorder(label,-avg_finish)) %>%
    ggplot(aes(avg_finish,label,size=starts,color=win_rate))+
    geom_point(alpha=.9)+scale_x_reverse()+scale_size_area(max_size=13)+
    scale_color_gradient(low="#8994A5",high=accent,labels=percent)+
    labs(x="Average finish (lower is better)",y=NULL,size="Starts",color="Win rate")+
    theme_minimal(base_size=12)+theme_dark_custom()
}

family_performance_plot <- function(data, accent="#38B27A") {
  data %>%
    filter(is.finite(finish_position),!is.na(track_primary_family),nzchar(track_primary_family)) %>%
    mutate(family=str_to_title(str_replace_all(track_primary_family,"_"," "))) %>%
    group_by(family) %>%
    summarise(starts=n(),avg_finish=mean(finish_position,na.rm=TRUE),top3_rate=mean(finish_position<=3,na.rm=TRUE),.groups="drop") %>%
    mutate(family=reorder(family,-avg_finish)) %>%
    ggplot(aes(avg_finish,family,size=starts,color=top3_rate))+
    geom_point(alpha=.9)+scale_x_reverse()+scale_size_area(max_size=14)+
    scale_color_gradient(low="#8994A5",high=accent,labels=percent)+
    labs(x="Average finish (lower is better)",y=NULL,size="Starts",color="Top-3 rate")+
    theme_minimal(base_size=12)+theme_dark_custom()
}

family_tab <- function(prefix, title, subtitle, note = NULL, betting = FALSE, qualifying_summary = FALSE) {
  selector_models <- if (prefix %in% c("qual","qchat")) qualifying_models else all_models
  tabPanel(title,
    div(class="page-shell",
      div(class="page-hero", div(class="eyebrow","MODEL LAB"), h1(title), p(subtitle)),
      div(class="app-grid",
        aside(class="control-rail",
          h3("Race and models"),
          if(betting||qualifying_summary) tagList(
            selectInput(paste0(prefix,"_roi_start"),"ROI start season",choices=c(2026,2025),selected=2025),
            selectInput(paste0(prefix,"_roi_end"),"ROI end season",choices=c(2026,2025),selected=2026)
          ),
          selectInput(paste0(prefix,"_season"),"Season",choices=sort(unique(qualifying$season),decreasing=TRUE),selected=max(qualifying$season,na.rm=TRUE)),
          selectInput(paste0(prefix,"_round"),"Race",choices=character()),
          actionButton(paste0(prefix,"_all"),"Select all",class="mini-button"),
          actionButton(paste0(prefix,"_default"),"Reset",class="mini-button"),
          checkboxGroupInput(paste0(prefix,"_models"),"Models",choices=setNames(selector_models,model_label(selector_models)),selected=default_models),
          if (!is.null(note)) div(class="rail-note",note),
          if(betting) div(class="rail-note",paste0("Winner ROI counts every consensus pick. Average winner edge appears only when every pick has a saved price. Top-three ROI uses supplied verified quotes or fair prices derived from the complete winner board. Active evaluation: ",active_strategy_name,".")),
          if(qualifying_summary) div(class="rail-note","Pole and front-row hit rates use completed qualifying results. NASCAR's front row is the first two starting positions; qualifying ROI remains unavailable without complete pole-market boards.")
        ),
        main(class="content-stack",
          if(betting) div(class="panel",h2("Consensus Season Betting ROI"),tableOutput(paste0(prefix,"_roi"))),
          if(betting) div(class="panel",h2("Selected Race Consensus Bets"),tableOutput(paste0(prefix,"_bets"))),
          if(qualifying_summary) div(class="panel",h2("Consensus Season Qualifying Results"),tableOutput(paste0(prefix,"_qual_summary"))),
          uiOutput(paste0(prefix,"_context")),
          div(class="panel",h2(if(betting)"Predicted Winner"else"Model picks"),tableOutput(paste0(prefix,"_picks"))),
          div(class="panel",h2("Full predicted order — every selected model plus ensemble"),div(class="table-scroll",tableOutput(paste0(prefix,"_predictions")))),
          uiOutput(paste0(prefix,"_headline")),
          div(class="panel",h2("Out-of-sample performance"),p(class="panel-note","Completed 2026 races; fixed models trained through 2025."),tableOutput(paste0(prefix,"_metrics")))
        )
      )
    )
  )
}

profile_shell <- function(title, subtitle, controls, body) tabPanel(title, div(class="page-shell", div(class="page-hero",div(class="eyebrow","HISTORY LAB"),h1(title),p(subtitle)), div(class="app-grid",aside(class="control-rail",controls),main(class="content-stack",body))))

app_shell <- navbarPage(
  title=div(class="brand",span("NASCAR Analytics")), id="main_nav", inverse=TRUE, collapsible=TRUE,
  header=tags$head(
    tags$link(rel="stylesheet",type="text/css",href="styles.css"),
    tags$link(rel="icon",type="image/png",sizes="512x512",href="app-icon-512.png"),
    tags$link(rel="apple-touch-icon",sizes="512x512",href="app-icon-512.png"),
    tags$script(HTML("$(function(){
      $('body').addClass('splash-open');
      var closeSplash=function(){
        $('#app-splash').addClass('is-exiting');
        $('body').removeClass('splash-open');
        setTimeout(function(){ $('#app-splash').remove(); },700);
      };
      $('#splash-enter').on('click',closeSplash).trigger('focus');
      $(document).on('keydown',function(e){ if(e.key==='Escape' && $('#app-splash').length) closeSplash(); });
      $(document).on('click','.navbar-collapse.in a',function(){ $('.navbar-toggle').click(); });
    });"))
  ),

  navbarMenu("Drivers and Tracks",
    profile_shell("Tracks","Track context plus historical driver and owner performance.",
      tagList(h3("Track selection"),selectInput("track_schedule_key","2026 race / track",choices=track_schedule_choices,selected=default_track_schedule_key),selectInput("track_start","From",choices=sort(unique(history$season)),selected=2022),selectInput("track_end","Through",choices=sort(unique(history$season),decreasing=TRUE),selected=2026)),
      tagList(uiOutput("track_cards"),uiOutput("track_history_note"),div(class="two-col",div(class="panel",h2("Driver performance"),plotOutput("track_driver_plot",height=390)),div(class="panel",h2("Owner performance"),plotOutput("track_owner_plot",height=390))),div(class="panel",h2("Historical leaders"),tableOutput("track_leaders")),div(class="panel",h2("Race history"),div(class="table-scroll",tableOutput("track_history"))))),
    profile_shell("Driver Profiles","Form, pace, passing, and results by NASCAR track family.",
      tagList(h3("Driver selection"),selectInput("driver_name","Driver",choices=valid_choices(history$driver_name)),selectInput("driver_start","From",choices=sort(unique(history$season)),selected=2022),selectInput("driver_end","Through",choices=sort(unique(history$season),decreasing=TRUE),selected=2026)),
      tagList(uiOutput("driver_cards"),div(class="two-col",div(class="panel",h2("Finish trend"),plotOutput("driver_plot",height=320)),div(class="panel",h2("Performance by track family"),plotOutput("driver_family_plot",height=320))),div(class="panel",h2("Track-family summary"),tableOutput("driver_family")),div(class="panel",h2("Recent races"),tableOutput("driver_recent")))),
    profile_shell("Owner Profiles","The F1 constructor view translated to NASCAR owners and manufacturers.",
      tagList(h3("Owner selection"),selectInput("owner_name","Owner",choices=valid_choices(history$owner_name)),selectInput("owner_start","From",choices=sort(unique(history$season)),selected=2022),selectInput("owner_end","Through",choices=sort(unique(history$season),decreasing=TRUE),selected=2026)),
      tagList(uiOutput("owner_cards"),div(class="two-col",div(class="panel",h2("Owner finish trend"),plotOutput("owner_plot",height=320)),div(class="panel",h2("Performance by track family"),plotOutput("owner_family_plot",height=320))),div(class="panel",h2("Track-family summary"),tableOutput("owner_family")),div(class="panel",h2("Recent results"),tableOutput("owner_recent"))))
  ),
  navbarMenu("Qualifying",
    family_tab("qual","Qualifying","Compare pole predictions, full grids, specialists, and any selected-model ensemble.",qualifying_summary=TRUE),
    family_tab("qchat","Qualifying With Chatter","Same model controls as qualifying, with the timestamp-gated chatter adjustment applied to the upcoming race.","Historical proxy rows remain neutral to protect the backtest. Current pre-race rows can adjust the projected qualifying order when Step 5 marks them production-eligible.",qualifying_summary=TRUE)
  ),
  navbarMenu("Race Models",
    family_tab("finish","Finish Model","Compare projected finishing order for every XGBoost variant, specialist route, baseline, and ensemble.",betting=TRUE),
    family_tab("prob","Probabilities Model","Compare calibrated win and top-three probabilities model by model.",betting=TRUE),
    family_tab("points","Points Model","Compare NASCAR race-points projections and the selected-model ensemble.",betting=TRUE)
  ),

  navbarMenu("Ensembles",
  tabPanel("Routed Specialists",
    div(class="page-shell",
      div(class="page-hero",div(class="eyebrow","ROUTE LAB"),h1("Routed Specialists"),p("Choose a race and the app automatically activates its Drafting, Road Course, Conventional Speedway, or Short / Steep Oval finish, probability, and points specialists.")),
      div(class="app-grid",
        aside(class="control-rail",h3("Race and specialist models"),selectInput("route_roi_start","ROI start season",choices=c(2026,2025),selected=2025),selectInput("route_roi_end","ROI end season",choices=c(2026,2025),selected=2026),selectInput("route_season","Season",choices=sort(unique(specialist_history$season),decreasing=TRUE),selected=max(specialist_history$season,na.rm=TRUE)),selectInput("route_round","Race",choices=character()),actionButton("route_defaults","Use race defaults",class="mini-button"),actionButton("route_all","Select all",class="mini-button"),actionButton("route_clear","Clear all",class="mini-button"),checkboxGroupInput("route_models","Routed specialist models",choices=routed_specialist_model_choices,selected=character()),div(class="rail-note","All 12 specialist choices remain visible. Changing the race selects its Position, Probability, and Points specialists by default; you can then change any checkbox. Each specialist is scored only on matching tracks.")),
        main(class="content-stack",uiOutput("route_context"),uiOutput("route_cards"),div(class="panel",h2("Routed Specialist Consensus Season Betting ROI"),p(class="panel-note","Winner, podium, and combined returns use one consensus ranking across the selected routed specialist outcomes."),tableOutput("route_roi")),div(class="panel",h2("Selected Race Specialist Picks"),tableOutput("route_winner")),div(class="panel",h2("Selected Race Driver Board"),div(class="table-scroll",tableOutput("route_table"))),div(class="panel",h2("Route-only Model Performance"),tableOutput("route_metrics")))
      )
    )
  ),

  tabPanel("Model Consensus",
    div(class="page-shell",
      div(class="page-hero",div(class="eyebrow","ENSEMBLE LAB"),h1("Model Consensus"),p("Build a transparent cross-family consensus from the qualifying, finish, probability, and points models you choose.")),
      div(class="app-grid",
        aside(class="control-rail",h3("Consensus setup"),selectInput("cons_roi_start","ROI start season",choices=c(2026,2025),selected=2025),selectInput("cons_roi_end","ROI end season",choices=c(2026,2025),selected=2026),selectInput("cons_season","Season",choices=sort(unique(qualifying$season),decreasing=TRUE),selected=max(qualifying$season)),selectInput("cons_round","Race",choices=character()),checkboxInput("cons_use_finish","Include Finish models",value=TRUE),checkboxInput("cons_use_probability","Include Probability models",value=TRUE),checkboxInput("cons_use_points","Include Points models",value=TRUE),checkboxInput("cons_use_routed","Include Routed Specialist models",value=TRUE),div(class="rail-note","Model choices flow from the Finish, Probability, Points, and Routed Specialists tabs. Consensus averages only the families checked here; no models need to be reselected.")),
        main(class="content-stack",div(class="panel",h2("All-Model Consensus Season Betting ROI"),tableOutput("cons_roi")),div(class="panel",h2("Selected Race Consensus Bets"),tableOutput("cons_bets")),uiOutput("cons_context"),div(class="panel",h2("Predicted Winner"),tableOutput("cons_winner")),div(class="panel",h2("Full Predicted Order"),div(class="table-scroll",tableOutput("cons_table"))),uiOutput("cons_cards"),div(class="two-col",div(class="panel",h2("2026 Out-of-Sample Validation"),tableOutput("cons_metrics")),div(class="panel",h2("Model Recipe"),uiOutput("cons_recipe"))))
      )
    )
  ),
  ),

  tabPanel("Chatter Overlay",
    div(class="page-shell",
      div(class="page-hero",div(class="eyebrow","SIGNAL LAB"),h1("Chatter Overlay"),p("Base versus safely adjusted forecasts. Ineligible or missing chatter stays neutral.")),
      div(class="app-grid",
        aside(class="control-rail",h3("Display"),selectInput("chatter_roi_start","ROI start season",choices=c(2026,2025),selected=2025),selectInput("chatter_roi_end","ROI end season",choices=c(2026,2025),selected=2026),selectInput("chatter_view","View",choices=c("Current race"="current","2025 validation"="backtest")),div(class="rail-note","The ROI comparison shows the production consensus before and after the safety-gated chatter adjustment. No adjustment is applied unless the input passes the safety and timing rules.")),
        main(class="content-stack",div(class="panel",h2("Base vs Chatter Season Betting ROI"),tableOutput("chatter_roi")),uiOutput("chatter_cards"),div(class="panel",h2("Adjustment detail"),div(class="table-scroll",tableOutput("chatter_table"))),div(class="panel",h2("Overlay validation"),tableOutput("chatter_metrics")))
      )
    )
  ),

  tabPanel("Fantasy Lineup",
    div(class="page-shell",
      div(class="page-hero",div(class="eyebrow","DRAFTKINGS LAB"),h1("Fantasy Lineup"),p("Current projections, optimized lineups, salary use, place differential, laps led, and fastest laps.")),
      div(class="app-grid",
        aside(class="control-rail",h3("Lineup"),checkboxInput("dk_use_chatter","Include chatter overlay",value=FALSE),selectInput("dk_lineup","Optimized lineup",choices=if(nrow(dk_initial_lineups)) setNames(dk_initial_lineups$lineup_rank,paste0("#",dk_initial_lineups$lineup_rank," — ",fmt_num(dk_initial_lineups$projected_points,1)," pts")) else character()),div(class="rail-note","DraftKings NASCAR Classic uses six equal driver slots and a $50,000 salary cap—there is no captain multiplier. Chatter affects lineups only when a row passes the verified pre-race safety gate.")),
        main(class="content-stack",uiOutput("dk_cards"),div(class="two-col",div(class="panel",h2("Selected lineup"),tableOutput("dk_selected")),div(class="panel",h2("2025 fantasy validation"),tableOutput("dk_metrics"))),div(class="panel",h2("Full driver board"),div(class="table-scroll",tableOutput("dk_board"))),div(class="panel",h2("Top optimized lineups"),tableOutput("dk_top")))
      )
    )
  ),

  navbarMenu("System",
    tabPanel("Model Specifications",div(class="page-shell",div(class="page-hero",div(class="eyebrow","MODEL AUDIT"),h1("Model Specifications"),p("Training scope, specialist filters, and hyperparameters.")),div(class="app-grid",aside(class="control-rail",h3("Model family"),selectInput("spec_family","Family",choices=c("Qualifying"="qualifying","Finish"="finish","Probability"="probability","Points"="points"))),main(class="content-stack",div(class="panel",h2("Specifications"),div(class="table-scroll",tableOutput("spec_table"))))))),
    tabPanel("Data Status",div(class="page-shell",div(class="page-hero",div(class="eyebrow","SYSTEM"),h1("Data Status"),p("App contracts, external inputs, and refresh timestamps.")),div(class="content-stack",div(class="panel",h2("Required app files"),tableOutput("file_table")),div(class="panel",h2("External input audit"),tableOutput("audit_table")))))
  )
)

ui <- tagList(
  tags$div(
    id="app-splash", class="app-splash", role="dialog", `aria-modal`="true", `aria-labelledby`="splash-title",
    tags$div(class="splash-shade"),
    tags$div(
      class="splash-content",
      tags$img(class="splash-logo",src="app-icon-512.png",alt="NASCAR Analytics racing N logo"),
      tags$div(class="splash-eyebrow","RACE INTELLIGENCE • 2026"),
      tags$h1(id="splash-title","NASCAR ANALYTICS"),
      tags$p("Predictions, probabilities, track specialists, betting performance, and fantasy strategy—built for every lap."),
      tags$button(id="splash-enter",type="button",class="splash-enter",span("ENTER THE GARAGE"),span(class="splash-arrow","→")),
      tags$div(class="splash-rule",span(),span(),span(),span())
    )
  ),
  app_shell
)

server <- function(input, output, session) {
  register_family <- function(prefix, data, family) {
    available_models <- if(family=="qualifying") qualifying_models else all_models
    default_models_for_race <- function() {
      selected_season <- input[[paste0(prefix,"_season")]]
      selected_round <- input[[paste0(prefix,"_round")]]
      if(is.null(selected_season)||is.null(selected_round))return(intersect(default_models,available_models))
      rows <- data %>% filter(
        .data$season==as.integer(.env$selected_season),
        .data$round==as.integer(.env$selected_round)
      )
      route_column <- switch(family,qualifying="qualifying_route",finish="finish_route",probability="probability_route",points="points_route")
      routed_rows <- rows %>% filter(.data$model=="routed_consensus")
      routes <- if(route_column%in%names(routed_rows)) as.character(routed_rows[[route_column]]) else character()
      routes <- routes[!is.na(routes)&nzchar(routes)]
      specialist <- if(length(routes)) unname(specialist_model_map[routes[[1]]]) else character()
      intersect(unique(c(default_models,specialist)),available_models)
    }
    observeEvent(input[[paste0(prefix,"_season")]], {
      choices <- race_choices(data,input[[paste0(prefix,"_season")]])
      upcoming <- choices %>% filter(data_split=="upcoming") %>% slice_min(round,n=1,with_ties=FALSE)
      selected_round <- if(nrow(upcoming)) upcoming$round[[1]] else if(nrow(choices)) max(choices$round) else NULL
      updateSelectInput(session,paste0(prefix,"_round"),choices=setNames(choices$round,choices$label),selected=selected_round)
    },ignoreInit=FALSE)
    observeEvent(list(input[[paste0(prefix,"_season")]],input[[paste0(prefix,"_round")]]), {
      req(input[[paste0(prefix,"_season")]],input[[paste0(prefix,"_round")]])
      updateCheckboxGroupInput(session,paste0(prefix,"_models"),selected=default_models_for_race())
    },ignoreInit=FALSE)
    observeEvent(input[[paste0(prefix,"_all")]], updateCheckboxGroupInput(session,paste0(prefix,"_models"),selected=available_models))
    observeEvent(input[[paste0(prefix,"_default")]], updateCheckboxGroupInput(session,paste0(prefix,"_models"),selected=default_models_for_race()))
    selected <- reactive({
      req(input[[paste0(prefix,"_season")]],input[[paste0(prefix,"_round")]],input[[paste0(prefix,"_models")]])
      rows <- data %>% filter(season==as.integer(input[[paste0(prefix,"_season")]]),round==as.integer(input[[paste0(prefix,"_round")]]),model %in% input[[paste0(prefix,"_models")]])
      validate(need(nrow(rows),"No predictions exist for that race and model selection."))
      if(length(unique(rows$model))>1) bind_rows(rows,make_ensemble(rows,family)) else rows
    })
    output[[paste0(prefix,"_context")]] <- renderUI({
      rows<-selected(); r<-rows[1,]; div(class="race-context",span(class="context-chip",paste0(r$season," • Round ",r$round)),strong(r$track_name),span(r$race_name),span(track_characteristics_label(r)),span(paste0("Models: ",paste(model_label(unique(rows$model[rows$model!="selected_ensemble"])),collapse=" + "))))
    })
    output[[paste0(prefix,"_headline")]] <- renderUI({
      rows<-selected(); e<-rows %>% filter(model==if(any(model=="selected_ensemble")) "selected_ensemble" else first(model))
      if(family=="qualifying") top<-e%>%slice_min(predicted_qualifying_rank,n=1) else if(family=="finish") top<-e%>%slice_min(predicted_finish_rank,n=1) else if(family=="probability") top<-e%>%slice_min(predicted_win_rank,n=1) else top<-e%>%slice_min(predicted_points_rank,n=1)
      value<-if(family=="probability") fmt_pct(top$win_probability,1) else if(family=="points") fmt_num(top$predicted_points,1) else if(family=="finish") fmt_num(top$predicted_finish_position,1) else fmt_num(top$predicted_position_score,1)
      div(class="metric-row",metric_card(if(family=="qualifying")"Pole pick" else "Top pick",top$driver_name,"Selected ensemble","gold"),metric_card("Projected value",value,model_label(top$model),"blue"),metric_card("Track family",str_to_title(str_replace_all(top$track_primary_family,"_"," ")),top$track_name,"green"))
    })
    output[[paste0(prefix,"_picks")]] <- renderTable({
      x<-selected(); if(family=="qualifying") x<-x%>%filter(predicted_qualifying_rank==1)%>%transmute(Model=model_label(model),Pick=driver_name,Owner=owner_name,Projection=fmt_num(predicted_position_score,2)) else if(family=="finish") x<-x%>%filter(predicted_finish_rank==1)%>%transmute(Model=model_label(model),Pick=driver_name,Owner=owner_name,Projection=fmt_num(predicted_finish_position,2)) else if(family=="probability") x<-x%>%filter(predicted_win_rank==1)%>%transmute(Model=model_label(model),Pick=driver_name,Owner=owner_name,Projection=fmt_pct(win_probability,1)) else x<-x%>%filter(predicted_points_rank==1)%>%transmute(Model=model_label(model),Pick=driver_name,Owner=owner_name,Projection=fmt_num(predicted_points,1)); x
    },striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
    output[[paste0(prefix,"_predictions")]] <- renderTable({
      x<-selected()
      if(family=="qualifying") x%>%arrange(factor(model,levels=c("selected_ensemble",all_models)),predicted_qualifying_rank)%>%transmute(Model=model_label(model),Rank=fmt_int(predicted_qualifying_rank),Driver=driver_name,Owner=owner_name,Manufacturer=manufacturer,`Pred qualifying`=fmt_num(predicted_position_score,2),`Speed deficit`=fmt_pct(predicted_speed_deficit_pct/100,2),Route=qualifying_route,Actual=fmt_int(target_qualifying_position))
      else if(family=="finish") x%>%arrange(factor(model,levels=c("selected_ensemble",all_models)),predicted_finish_rank)%>%transmute(Model=model_label(model),Rank=fmt_int(predicted_finish_rank),Driver=driver_name,Owner=owner_name,Manufacturer=manufacturer,`Pred finish`=fmt_num(predicted_finish_position,2),Route=finish_route,Actual=fmt_int(target_finish_position))
      else if(family=="probability") x%>%arrange(factor(model,levels=c("selected_ensemble",all_models)),predicted_win_rank)%>%transmute(Model=model_label(model),`Win rank`=fmt_int(predicted_win_rank),Driver=driver_name,Owner=owner_name,Win=fmt_pct(win_probability,1),`Top 3`=fmt_pct(top3_probability,1),Route=probability_route,Actual=ifelse(target_win==1,"Winner",ifelse(target_top3==1,"Top 3","")))
      else x%>%arrange(factor(model,levels=c("selected_ensemble",all_models)),predicted_points_rank)%>%transmute(Model=model_label(model),Rank=fmt_int(predicted_points_rank),Driver=driver_name,Owner=owner_name,Points=fmt_num(predicted_points,1),Route=points_route,Actual=fmt_num(target_points,0))
    },striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
    if(family%in%c("finish","probability","points")) {
      consensus_bets <- reactive({
        req(input[[paste0(prefix,"_models")]])
        rows_to_bets(family_consensus_rows(data,family,input[[paste0(prefix,"_models")]]))
      })
      output[[paste0(prefix,"_roi")]] <- renderTable({
        req(input[[paste0(prefix,"_roi_start")]],input[[paste0(prefix,"_roi_end")]])
        x<-summarise_bets_window(consensus_bets(),input[[paste0(prefix,"_roi_start")]],input[[paste0(prefix,"_roi_end")]])
        validate(need(nrow(x),"No completed consensus picks in this season window."));render_roi_table(x)
      },striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
      output[[paste0(prefix,"_bets")]] <- renderTable({
        x<-consensus_bets()%>%filter(season==as.integer(input[[paste0(prefix,"_season")]]),round==as.integer(input[[paste0(prefix,"_round")]]))
        validate(need(nrow(x),"No consensus bet rows for this race."));render_bets_table(x)
      },striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
    }
    if(family=="qualifying") {
      output[[paste0(prefix,"_qual_summary")]]<-renderTable({
        req(input[[paste0(prefix,"_models")]],input[[paste0(prefix,"_roi_start")]],input[[paste0(prefix,"_roi_end")]])
        x<-summarise_qualifying_window(qualifying_consensus_rows(data,input[[paste0(prefix,"_models")]]),input[[paste0(prefix,"_roi_start")]],input[[paste0(prefix,"_roi_end")]])
        validate(need(nrow(x),"No completed qualifying results in this season window."));x
      },striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
    }
    output[[paste0(prefix,"_metrics")]] <- renderTable({
      req(input[[paste0(prefix,"_models")]])
      test<-data%>%filter(data_split=="test",model%in%input[[paste0(prefix,"_models")]])
      if(length(unique(test$model))>1)test<-bind_rows(test,make_ensemble(test,family))
      result<-scored_metrics(test,family)%>%mutate(Model=model_label(model))%>%select(Model,everything(),-model)
      result<-result%>%mutate(Rows=fmt_int(Rows))
      pct_columns<-intersect(c("Pole pick","Front-row hit","Winner pick","Top-3 hit"),names(result))
      other_numeric<-setdiff(names(result)[vapply(result,is.numeric,logical(1))],pct_columns)
      result%>%mutate(across(all_of(other_numeric),~round(.x,3)),across(all_of(pct_columns),~fmt_pct(.x,1)))
    },striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  }
  register_family("qual",qualifying,"qualifying"); register_family("qchat",qualifying_chatter,"qualifying")
  register_family("finish",finish,"finish"); register_family("prob",probability,"probability"); register_family("points",points,"points")

  if(FALSE) {
  # Superseded multi-model routed controls retained only for audit history.
  observeEvent(input$route_season,{x<-race_choices(qualifying,input$route_season);updateSelectInput(session,"route_round",choices=setNames(x$round,x$label),selected=if(nrow(x))max(x$round)else NULL)},ignoreInit=FALSE)
  route_rows <- reactive({
    req(input$route_models,input$route_round)
    keys<-c("season","round","driver_id","driver_name","owner_name","manufacturer","model")
    q<-qualifying%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round),model%in%input$route_models)%>%select(any_of(keys),track_name,track_primary_family,qualifying_route,predicted_qualifying_rank)
    f<-finish%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round),model%in%input$route_models)%>%select(any_of(keys),finish_route,predicted_finish_rank)
    p<-probability%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round),model%in%input$route_models)%>%select(any_of(keys),probability_route,win_probability,top3_probability)
    pt<-points%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round),model%in%input$route_models)%>%select(any_of(keys),points_route,predicted_points,predicted_points_rank)
    q%>%full_join(f,by=keys)%>%full_join(p,by=keys)%>%full_join(pt,by=keys)
  })
  route_contract<-reactive({req(input$route_models);family_consensus_rows(finish,"finish",input$route_models)})
  route_bet_rows<-reactive(rows_to_bets(route_contract()))
  output$route_context<-renderUI({x<-route_rows();validate(need(nrow(x),"No routed predictions for this race."));div(class="race-context",span(class="context-chip",paste0(first(x$season)," • Round ",first(x$round))),strong(first(x$track_name)),span(str_to_title(str_replace_all(first(x$track_primary_family),"_"," "))))})
  output$route_roi<-renderTable({req(input$route_roi_start,input$route_roi_end);x<-summarise_bets_window(route_bet_rows(),input$route_roi_start,input$route_roi_end);validate(need(nrow(x),"No completed routed picks in this window."));render_roi_table(x)},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$route_bets<-renderTable({x<-route_bet_rows()%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round));validate(need(nrow(x),"No routed consensus bet rows for this race."));render_bets_table(x)},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$route_winner<-renderTable({route_contract()%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round),consensus_rank==1)%>%transmute(Pick=driver_name,Owner=owner_name,`Pred finish`=fmt_num(predicted_value,2),`Win probability`=fmt_pct(model_win_probability,1),`Actual finish`=fmt_int(actual_finish),Correct=ifelse(actual_winner,"Yes",ifelse(is.na(actual_winner),"—","No")))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$route_cards<-renderUI({x<-route_rows();active<-x%>%filter(model=="routed_consensus")%>%slice(1);div(class="metric-row",metric_card("Active route",str_to_title(str_replace_all(active$finish_route%||%first(x$finish_route),"_"," ")),"Race-specific routing","gold"),metric_card("Models shown",length(unique(x$model)),paste(model_label(unique(x$model)),collapse=" • "),"blue"),metric_card("Drivers",length(unique(x$driver_id)),"Expected field","green"))})
  output$route_table<-renderTable({route_rows()%>%arrange(factor(model,levels=c("routed_consensus",setdiff(all_models,"routed_consensus"))),predicted_finish_rank)%>%transmute(Model=model_label(model),Driver=driver_name,Owner=owner_name,Qualifying=fmt_int(predicted_qualifying_rank),Finish=fmt_int(predicted_finish_rank),Win=fmt_pct(win_probability,1),`Top 3`=fmt_pct(top3_probability,1),Points=fmt_num(predicted_points,1),`Q route`=qualifying_route,`Finish route`=finish_route)},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$route_metrics<-renderTable({
    metric_column <- c(qualifying="qualifying_rank_mae",finish="finish_rank_mae",probability="win_brier",points="points_mae")
    bind_rows(lapply(names(family_metrics),function(fam){
      x <- family_metrics[[fam]] %>% filter(data_split=="test",model%in%input$route_models)
      if(!nrow(x)) return(tibble())
      tibble(Family=str_to_title(fam),Model=model_label(x$model),Rows=x$rows,`Primary error`=round(num(x[[metric_column[[fam]]]]),3))
    }))
  },striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  }

  # F1-style routed controls: all 12 route/outcome choices stay visible while
  # the selected race checks its three matching specialists by default.
  observeEvent(input$route_season,{
    x<-specialist_history%>%filter(season==as.integer(input$route_season))%>%distinct(round,race_name,route_group)%>%arrange(round)%>%mutate(label=paste0("R",round," — ",race_name," • ",specialist_route_label(route_group)))
    updateSelectInput(session,"route_round",choices=setNames(x$round,x$label),selected=if(nrow(x))max(x$round)else NULL)
  },ignoreInit=FALSE)
  selected_race_route<-reactive({
    req(input$route_season,input$route_round)
    x<-specialist_history%>%filter(season==as.integer(input$route_season),round==as.integer(input$route_round))%>%slice(1)
    validate(need(nrow(x),"No specialist route is available for this race."));x$route_group[[1]]
  })
  observeEvent(list(input$route_season,input$route_round),{
    req(input$route_season,input$route_round)
    updateCheckboxGroupInput(session,"route_models",choices=routed_specialist_model_choices,selected=default_routed_specialist_models(selected_race_route()))
  },ignoreInit=FALSE)
  observeEvent(input$route_defaults,{updateCheckboxGroupInput(session,"route_models",choices=routed_specialist_model_choices,selected=default_routed_specialist_models(selected_race_route()))},ignoreInit=TRUE)
  observeEvent(input$route_all,{updateCheckboxGroupInput(session,"route_models",choices=routed_specialist_model_choices,selected=routed_specialist_model_lookup$choice_id)},ignoreInit=TRUE)
  observeEvent(input$route_clear,{updateCheckboxGroupInput(session,"route_models",choices=routed_specialist_model_choices,selected=character())},ignoreInit=TRUE)
  route_choice_rows<-reactive({routed_specialist_model_lookup%>%filter(choice_id%in%(input$route_models%||%character()))})
  route_rows_for<-function(outcome,season=NULL,round=NULL,window=FALSE){
    choices<-route_choice_rows()%>%filter(.data$outcome==.env$outcome)
    if(!nrow(choices))return(tibble())
    x<-specialist_all%>%filter(route_group==model_route_group,model%in%choices$model)
    if(!is.null(season))x<-x%>%filter(.data$season==as.integer(.env$season))
    if(!is.null(round))x<-x%>%filter(.data$round==as.integer(.env$round))
    if(isTRUE(window)){lo<-min(as.integer(input$route_roi_start),as.integer(input$route_roi_end));hi<-max(as.integer(input$route_roi_start),as.integer(input$route_roi_end));x<-x%>%filter(season>=lo,season<=hi)}
    x
  }
  route_rows_auto<-reactive({
    req(input$route_season,input$route_round);validate(need(length(input$route_models)>0,"Select at least one routed specialist model."))
    x<-bind_rows(lapply(c("finish","probability","points"),function(outcome)route_rows_for(outcome,input$route_season,input$route_round)))%>%distinct(season,round,driver_id,model,.keep_all=TRUE)
    validate(need(nrow(x),"The selected specialists do not match this race's track type."));x
  })
  active_route_group<-reactive(selected_race_route())
  specialist_contract<-function(x,outcome){
    out<-if(outcome=="Finish")x%>%transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_finish_rank,predicted_value=predicted_finish_position,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability)
    else if(outcome=="Probability")x%>%transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_win_rank,predicted_value=win_probability,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability)
    else x%>%transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank=predicted_points_rank,predicted_value=predicted_points,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability)
    out%>%mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
  }
  routed_consensus_contract<-reactive({
    outcomes<-unique(route_choice_rows()$outcome)
    validate(need(length(outcomes)>0,"Select at least one routed specialist model."))
    rank_columns<-c(finish="predicted_finish_rank",probability="predicted_win_rank",points="predicted_points_rank")
    rank_rows<-bind_rows(lapply(outcomes,function(outcome){
      x<-route_rows_for(outcome,window=TRUE)
      if(!nrow(x))return(tibble())
      x%>%transmute(season,round,driver_id=as.character(driver_id),family=outcome,family_rank=.data[[rank_columns[[outcome]]]])
    }))
    validate(need(nrow(rank_rows),"No completed routed-specialist consensus rows are available in this window."))
    scores<-rank_rows%>%
      group_by(season,round,driver_id)%>%
      summarise(consensus_score=mean(family_rank,na.rm=TRUE),family_count=n_distinct(family),.groups="drop")%>%
      group_by(season,round)%>%mutate(consensus_rank=rank(consensus_score,ties.method="first"))%>%ungroup()
    base<-bind_rows(lapply(outcomes,function(outcome)route_rows_for(outcome,window=TRUE)))%>%
      mutate(driver_id=as.character(driver_id))%>%
      distinct(season,round,driver_id,.keep_all=TRUE)
    base%>%inner_join(scores,by=c("season","round","driver_id"))%>%
      transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,
                consensus_rank,predicted_value=consensus_score,actual_finish=target_finish_position,
                model_win_probability=win_probability,model_top3_probability=top3_probability,family_count)%>%
      mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
  })
  routed_consensus_bets<-reactive(rows_to_bets(routed_consensus_contract()))
  output$route_context<-renderUI({x<-route_rows_auto();r<-x[1,];meta<-race_track_metadata(r);div(class="race-context",span(class="context-chip",paste0(first(x$season)," • Round ",first(x$round))),strong(meta$track_name),span(first(x$race_name)),span(track_characteristics_label(r)),span(paste0("Default route: ",specialist_route_label(active_route_group()))))})
  output$route_cards<-renderUI({x<-route_rows_auto();races<-bind_rows(lapply(c("finish","probability","points"),function(outcome)route_rows_for(outcome,window=TRUE)))%>%distinct(season,round);div(class="metric-row",metric_card("Default route",specialist_route_label(active_route_group()),"Three matching choices checked on race change","gold"),metric_card("Selected choices",length(input$route_models),paste(route_choice_rows()$choice_label,collapse=" • "),"blue"),metric_card("Matching races",nrow(races),paste0(input$route_roi_start,"–",input$route_roi_end),"green"))})
  output$route_roi<-renderTable({result<-summarise_bets_window(routed_consensus_bets(),input$route_roi_start,input$route_roi_end);validate(need(nrow(result),"No completed routed-specialist consensus picks are available in this window."));render_roi_table(result)%>%mutate(Market=paste("Consensus",Market))},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$route_winner<-renderTable({bind_rows(lapply(c(Finish="finish",Probability="probability",Points="points"),function(outcome){x<-route_rows_for(outcome,input$route_season,input$route_round);if(!nrow(x))return(tibble());specialist_contract(x,str_to_title(outcome))%>%filter(consensus_rank==1)%>%transmute(Outcome=str_to_title(outcome),Pick=driver_name,Owner=owner_name,Projection=fmt_num(predicted_value,2),`Win probability`=fmt_pct(model_win_probability,1),`Actual finish`=fmt_int(actual_finish))}))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$route_table<-renderTable({selected<-unique(route_choice_rows()$outcome);route_rows_auto()%>%arrange(predicted_finish_rank)%>%transmute(Driver=driver_name,Owner=owner_name,Manufacturer=manufacturer,`Finish rank`=if("finish"%in%selected)fmt_int(predicted_finish_rank)else"—",`Pred finish`=if("finish"%in%selected)fmt_num(predicted_finish_position,2)else"—",`Win rank`=if("probability"%in%selected)fmt_int(predicted_win_rank)else"—",Win=if("probability"%in%selected)fmt_pct(win_probability,1)else"—",`Top 3`=if("probability"%in%selected)fmt_pct(top3_probability,1)else"—",`Points rank`=if("points"%in%selected)fmt_int(predicted_points_rank)else"—",Points=if("points"%in%selected)fmt_num(predicted_points,1)else"—")},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$route_metrics<-renderTable({result<-bind_rows(lapply(c("finish","probability","points"),function(outcome){x<-route_rows_for(outcome,window=TRUE);if(!nrow(x))return(tibble());if(outcome=="finish")tibble(Outcome="Position",Races=n_distinct(paste(x$season,x$round)),Rows=nrow(x),Metric="Rank MAE",Value=mean(abs(x$predicted_finish_rank-x$target_finish_position),na.rm=TRUE))else if(outcome=="probability")bind_rows(tibble(Outcome="Probability",Races=n_distinct(paste(x$season,x$round)),Rows=nrow(x),Metric="Win Brier",Value=mean((x$win_probability-x$target_win)^2,na.rm=TRUE)),tibble(Outcome="Probability",Races=n_distinct(paste(x$season,x$round)),Rows=nrow(x),Metric="Top-3 Brier",Value=mean((x$top3_probability-x$target_top3)^2,na.rm=TRUE)))else tibble(Outcome="Points",Races=n_distinct(paste(x$season,x$round)),Rows=nrow(x),Metric="MAE",Value=mean(abs(x$predicted_points-x$target_points),na.rm=TRUE))}));validate(need(nrow(result),"Select at least one routed specialist model."));result%>%mutate(Value=round(Value,3))},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)

  if(FALSE) {
  # Superseded consensus controls retained only for audit history.
  # Cross-family model consensus.
  observeEvent(input$cons_season,{x<-race_choices(qualifying,input$cons_season);updateSelectInput(session,"cons_round",choices=setNames(x$round,x$label),selected=if(nrow(x))max(x$round)else NULL)},ignoreInit=FALSE)
  consensus_for <- function(season,round,qmodels,fmodels,pmodels,ptmodels){
    q<-make_ensemble(qualifying%>%filter(.data$season==.env$season,.data$round==.env$round,model%in%qmodels),"qualifying")%>%select(season,round,driver_id,driver_name,owner_name,manufacturer,track_name,track_primary_family,qual_rank=predicted_qualifying_rank)
    f<-make_ensemble(finish%>%filter(.data$season==.env$season,.data$round==.env$round,model%in%fmodels),"finish")%>%select(driver_id,finish_rank=predicted_finish_rank,target_finish_position)
    p<-make_ensemble(probability%>%filter(.data$season==.env$season,.data$round==.env$round,model%in%pmodels),"probability")%>%select(driver_id,win_probability,top3_probability,prob_rank=predicted_win_rank)
    pt<-make_ensemble(points%>%filter(.data$season==.env$season,.data$round==.env$round,model%in%ptmodels),"points")%>%select(driver_id,predicted_points,points_rank=predicted_points_rank)
    wq<-input$cons_weights/100; wr<-(1-wq)/3
    q%>%inner_join(f,by="driver_id")%>%inner_join(p,by="driver_id")%>%inner_join(pt,by="driver_id")%>%mutate(consensus_score=wq*qual_rank+wr*finish_rank+wr*prob_rank+wr*points_rank,consensus_rank=rank(consensus_score,ties.method="first"))%>%arrange(consensus_rank)
  }
  consensus_rows<-reactive({req(input$cons_round,input$cons_qual,input$cons_finish,input$cons_prob,input$cons_points);x<-consensus_for(as.integer(input$cons_season),as.integer(input$cons_round),input$cons_qual,input$cons_finish,input$cons_prob,input$cons_points);validate(need(nrow(x),"No consensus rows for that selection."));x})
  consensus_contract<-reactive({
    req(input$cons_qual,input$cons_finish,input$cons_prob,input$cons_points,input$cons_weights)
    q<-make_ensemble(qualifying%>%filter(season==2026,data_split%in%c("test","upcoming"),model%in%input$cons_qual),"qualifying")%>%select(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,qual_rank=predicted_qualifying_rank)
    f<-make_ensemble(finish%>%filter(season==2026,data_split%in%c("test","upcoming"),model%in%input$cons_finish),"finish")%>%select(season,round,driver_id,finish_rank=predicted_finish_rank,actual_finish=target_finish_position)
    p<-make_ensemble(probability%>%filter(season==2026,data_split%in%c("test","upcoming"),model%in%input$cons_prob),"probability")%>%select(season,round,driver_id,model_win_probability=win_probability,model_top3_probability=top3_probability,prob_rank=predicted_win_rank)
    pt<-make_ensemble(points%>%filter(season==2026,data_split%in%c("test","upcoming"),model%in%input$cons_points),"points")%>%select(season,round,driver_id,points_rank=predicted_points_rank)
    wq<-input$cons_weights/100;wr<-(1-wq)/3
    rows_2026<-q%>%inner_join(f,by=c("season","round","driver_id"))%>%inner_join(p,by=c("season","round","driver_id"))%>%inner_join(pt,by=c("season","round","driver_id"))%>%mutate(consensus_score=wq*qual_rank+wr*finish_rank+wr*prob_rank+wr*points_rank)%>%group_by(season,round)%>%mutate(consensus_rank=rank(consensus_score,ties.method="first"))%>%ungroup()%>%transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,consensus_rank,predicted_value=consensus_score,actual_finish,model_win_probability,model_top3_probability)
    rows_2025<-backtest%>%mutate(consensus_score=wq*predicted_qualifying_rank+wr*predicted_finish_rank+wr*win_probability_rank+wr*predicted_points_rank)%>%group_by(season,round)%>%mutate(consensus_rank=rank(consensus_score,ties.method="first"))%>%ungroup()%>%transmute(season,round,race_name,race_date,driver_id=as.character(driver_id),driver_name,owner_name,manufacturer,consensus_rank,predicted_value=consensus_score,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability)
    bind_rows(rows_2025,rows_2026)%>%mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
  })
  cons_bet_rows<-reactive(rows_to_bets(consensus_contract()))
  output$cons_context<-renderUI({x<-consensus_rows();div(class="race-context",span(class="context-chip",paste0(input$cons_season," • Round ",input$cons_round)),strong(first(x$track_name)),span(str_to_title(str_replace_all(first(x$track_primary_family),"_"," "))))})
  output$cons_roi<-renderTable({req(input$cons_roi_start,input$cons_roi_end);x<-summarise_bets_window(cons_bet_rows(),input$cons_roi_start,input$cons_roi_end);validate(need(nrow(x),"No completed all-model picks in this window."));render_roi_table(x)},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$cons_bets<-renderTable({x<-cons_bet_rows()%>%filter(season==as.integer(input$cons_season),round==as.integer(input$cons_round));validate(need(nrow(x),"No all-model consensus bet rows for this race."));render_bets_table(x)},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$cons_winner<-renderTable({consensus_contract()%>%filter(season==as.integer(input$cons_season),round==as.integer(input$cons_round),consensus_rank==1)%>%transmute(Pick=driver_name,Owner=owner_name,`Consensus score`=fmt_num(predicted_value,2),`Win probability`=fmt_pct(model_win_probability,1),`Actual finish`=fmt_int(actual_finish),Correct=ifelse(actual_winner,"Yes",ifelse(is.na(actual_winner),"—","No")))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$cons_cards<-renderUI({x<-consensus_rows();top<-slice_head(x,n=1);div(class="metric-row",metric_card("Consensus winner",top$driver_name,top$owner_name,"gold"),metric_card("Win probability",fmt_pct(top$win_probability,1),"Selected probability ensemble","blue"),metric_card("Projected points",fmt_num(top$predicted_points,1),paste0("Finish rank ",top$finish_rank),"green"))})
  output$cons_table<-renderTable({consensus_rows()%>%transmute(Rank=fmt_int(consensus_rank),Driver=driver_name,Owner=owner_name,Manufacturer=manufacturer,`Qual rank`=fmt_int(qual_rank),`Finish rank`=fmt_int(finish_rank),`Win rank`=fmt_int(prob_rank),`Points rank`=fmt_int(points_rank),Win=fmt_pct(win_probability,1),`Top 3`=fmt_pct(top3_probability,1),Points=fmt_num(predicted_points,1),Score=fmt_num(consensus_score,2))},striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  output$cons_recipe<-renderUI({wq<-input$cons_weights;wr<-round((100-wq)/3,1);tags$ul(tags$li(paste("Qualifying:",wq,"% —",paste(model_label(input$cons_qual),collapse=", "))),tags$li(paste("Finish:",wr,"% —",paste(model_label(input$cons_finish),collapse=", "))),tags$li(paste("Probability:",wr,"% —",paste(model_label(input$cons_prob),collapse=", "))),tags$li(paste("Points:",wr,"% —",paste(model_label(input$cons_points),collapse=", "))))})
  output$cons_metrics<-renderTable({
    req(input$cons_qual,input$cons_finish,input$cons_prob,input$cons_points,input$cons_weights)
    q<-make_ensemble(qualifying%>%filter(data_split=="test",model%in%input$cons_qual),"qualifying")%>%select(season,round,driver_id,qual_rank=predicted_qualifying_rank)
    f<-make_ensemble(finish%>%filter(data_split=="test",model%in%input$cons_finish),"finish")%>%select(season,round,driver_id,finish_rank=predicted_finish_rank,target_finish_position)
    p<-make_ensemble(probability%>%filter(data_split=="test",model%in%input$cons_prob),"probability")%>%select(season,round,driver_id,prob_rank=predicted_win_rank)
    pt<-make_ensemble(points%>%filter(data_split=="test",model%in%input$cons_points),"points")%>%select(season,round,driver_id,points_rank=predicted_points_rank)
    wq<-input$cons_weights/100; wr<-(1-wq)/3
    x<-q%>%inner_join(f,by=c("season","round","driver_id"))%>%inner_join(p,by=c("season","round","driver_id"))%>%inner_join(pt,by=c("season","round","driver_id"))%>%
      mutate(consensus_score=wq*qual_rank+wr*finish_rank+wr*prob_rank+wr*points_rank)%>%group_by(season,round)%>%mutate(consensus_rank=rank(consensus_score,ties.method="first"))%>%ungroup()
    validate(need(nrow(x),"No completed-race validation rows.")); picks<-x%>%filter(consensus_rank==1)
    tibble(Metric=c("Races","Winner-pick accuracy","Top-3 selection hit rate","Finish-rank MAE"),Value=c(n_distinct(paste(picks$season,picks$round)),fmt_pct(mean(picks$target_finish_position==1,na.rm=TRUE),1),fmt_pct(mean(x$target_finish_position<=3 & x$consensus_rank<=3,na.rm=TRUE)/mean(x$consensus_rank<=3,na.rm=TRUE),1),fmt_num(mean(abs(x$consensus_rank-x$target_finish_position),na.rm=TRUE),2)))
  },striped=TRUE,spacing="s",rownames=FALSE)
  }

  # F1-style consensus: inherit selections from the three race-model tabs and
  # Routed Specialists; this page only includes or excludes whole families.
  observeEvent(input$cons_season,{x<-race_choices(finish,input$cons_season);updateSelectInput(session,"cons_round",choices=setNames(x$round,x$label),selected=if(nrow(x))max(x$round)else NULL)},ignoreInit=FALSE)
  consensus_flags<-reactive(c(Finish=isTRUE(input$cons_use_finish),Probability=isTRUE(input$cons_use_probability),Points=isTRUE(input$cons_use_points),`Routed Specialists`=isTRUE(input$cons_use_routed)))
  validate_consensus_selection<-function(){
    flags<-consensus_flags();validate(need(any(flags),"Include at least one model family."))
    if(flags[["Finish"]])validate(need(length(input$finish_models)>0,"Select Finish models on the Finish tab."))
    if(flags[["Probability"]])validate(need(length(input$prob_models)>0,"Select Probability models on the Probability tab."))
    if(flags[["Points"]])validate(need(length(input$points_models)>0,"Select Points models on the Points tab."))
    if(flags[["Routed Specialists"]])validate(need(length(input$route_models)>0,"Select Routed Specialist models on the Routed Specialists tab."))
    invisible(TRUE)
  }
  routed_family_ranks<-function(season,round=NULL){
    pieces<-lapply(c("finish","probability","points"),function(outcome){
      x<-route_rows_for(outcome,season,round);if(!nrow(x))return(tibble())
      rank_value<-if(outcome=="finish")x$predicted_finish_rank else if(outcome=="probability")x$predicted_win_rank else x$predicted_points_rank
      x%>%transmute(season,round,driver_id,family="Routed Specialists",family_rank=rank_value)
    })
    rows<-bind_rows(pieces)
    if(!nrow(rows))return(tibble())
    rows%>%group_by(season,round,driver_id,family)%>%summarise(family_rank=mean(family_rank,na.rm=TRUE),.groups="drop")
  }
  consensus_for_race<-function(season,round){
    validate_consensus_selection();flags<-consensus_flags()
    fmodels<-input$finish_models%||%default_models;pmodels<-input$prob_models%||%default_models;ptmodels<-input$points_models%||%default_models
    f<-make_ensemble(finish%>%filter(.data$season==as.integer(.env$season),.data$round==as.integer(.env$round),model%in%fmodels),"finish")
    p<-make_ensemble(probability%>%filter(.data$season==as.integer(.env$season),.data$round==as.integer(.env$round),model%in%pmodels),"probability")
    pt<-make_ensemble(points%>%filter(.data$season==as.integer(.env$season),.data$round==as.integer(.env$round),model%in%ptmodels),"points")
    ranks<-bind_rows(
      if(flags[["Finish"]])f%>%transmute(season,round,driver_id,family="Finish",family_rank=predicted_finish_rank)else tibble(),
      if(flags[["Probability"]])p%>%transmute(season,round,driver_id,family="Probability",family_rank=predicted_win_rank)else tibble(),
      if(flags[["Points"]])pt%>%transmute(season,round,driver_id,family="Points",family_rank=predicted_points_rank)else tibble(),
      if(flags[["Routed Specialists"]])routed_family_ranks(season,round)else tibble()
    )
    validate(need(nrow(ranks),"No selected model-family predictions are available for this race."))
    scores<-ranks%>%group_by(season,round,driver_id)%>%summarise(consensus_score=mean(family_rank,na.rm=TRUE),family_count=n_distinct(family),.groups="drop")%>%group_by(season,round)%>%mutate(consensus_rank=rank(consensus_score,ties.method="first"))%>%ungroup()
    f%>%select(season,round,race_name,race_date,track_name,track_primary_family,driver_id,driver_name,owner_name,manufacturer,target_finish_position,predicted_finish_rank)%>%
      left_join(p%>%select(season,round,driver_id,win_probability,top3_probability,predicted_win_rank),by=c("season","round","driver_id"))%>%
      left_join(pt%>%select(season,round,driver_id,predicted_points,predicted_points_rank),by=c("season","round","driver_id"))%>%
      inner_join(scores,by=c("season","round","driver_id"))%>%arrange(consensus_rank)
  }
  consensus_contract<-reactive({
    validate_consensus_selection()
    races<-finish%>%filter(season%in%c(2025,2026),data_split%in%c("test","upcoming"))%>%distinct(season,round)%>%arrange(season,round)
    rows<-bind_rows(lapply(seq_len(nrow(races)),function(i)consensus_for_race(races$season[[i]],races$round[[i]])))
    rows%>%transmute(season,round,race_name,race_date,driver_id,driver_name,owner_name,manufacturer,track_name,track_primary_family,consensus_rank,predicted_value=consensus_score,actual_finish=target_finish_position,model_win_probability=win_probability,model_top3_probability=top3_probability,family_count,predicted_finish_rank,predicted_win_rank,predicted_points,predicted_points_rank)%>%mutate(actual_winner=actual_finish==1,actual_top3=actual_finish<=3)
  })
  consensus_rows<-reactive({req(input$cons_season,input$cons_round);x<-consensus_contract()%>%filter(season==as.integer(input$cons_season),round==as.integer(input$cons_round));validate(need(nrow(x),"No consensus rows for this race and family selection."));x})
  cons_bet_rows<-reactive(rows_to_bets(consensus_contract()))
  output$cons_context<-renderUI({x<-consensus_rows();r<-x[1,];meta<-race_track_metadata(r);div(class="race-context",span(class="context-chip",paste0(input$cons_season," • Round ",input$cons_round)),strong(meta$track_name),span(first(x$race_name)),span(track_characteristics_label(r)),span(paste(names(consensus_flags())[consensus_flags()],collapse=" • ")))})
  output$cons_roi<-renderTable({x<-summarise_bets_window(cons_bet_rows(),input$cons_roi_start,input$cons_roi_end);validate(need(nrow(x),"No completed consensus picks in this window."));render_roi_table(x)},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$cons_bets<-renderTable({x<-cons_bet_rows()%>%filter(season==as.integer(input$cons_season),round==as.integer(input$cons_round));validate(need(nrow(x),"No consensus bet rows for this race."));render_bets_table(x)},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$cons_winner<-renderTable({consensus_rows()%>%filter(consensus_rank==1)%>%transmute(Pick=driver_name,Owner=owner_name,`Consensus score`=fmt_num(predicted_value,2),Families=family_count,`Win probability`=fmt_pct(model_win_probability,1),`Actual finish`=fmt_int(actual_finish),Correct=ifelse(actual_winner,"Yes",ifelse(is.na(actual_winner),"—","No")))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$cons_cards<-renderUI({x<-consensus_rows();top<-slice_head(x,n=1);div(class="metric-row",metric_card("Consensus winner",top$driver_name,top$owner_name,"gold"),metric_card("Families included",top$family_count,paste(names(consensus_flags())[consensus_flags()],collapse=" • "),"blue"),metric_card("Consensus score",fmt_num(top$predicted_value,2),"Lower is better","green"))})
  output$cons_table<-renderTable({consensus_rows()%>%transmute(Rank=fmt_int(consensus_rank),Driver=driver_name,Owner=owner_name,Manufacturer=manufacturer,Families=family_count,`Finish rank`=fmt_int(predicted_finish_rank),`Win rank`=fmt_int(predicted_win_rank),`Points rank`=fmt_int(predicted_points_rank),Win=fmt_pct(model_win_probability,1),`Top 3`=fmt_pct(model_top3_probability,1),Points=fmt_num(predicted_points,1),Score=fmt_num(predicted_value,2))},striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  output$cons_recipe<-renderUI({tags$ul(tags$li(paste("Finish:",if(input$cons_use_finish)paste(model_label(input$finish_models),collapse=", ")else"Excluded")),tags$li(paste("Probability:",if(input$cons_use_probability)paste(model_label(input$prob_models),collapse=", ")else"Excluded")),tags$li(paste("Points:",if(input$cons_use_points)paste(model_label(input$points_models),collapse=", ")else"Excluded")),tags$li(paste("Routed Specialists:",if(input$cons_use_routed)paste(route_choice_rows()$choice_label,collapse=", ")else"Excluded")))})
  output$cons_metrics<-renderTable({x<-consensus_contract()%>%filter(is.finite(actual_finish));validate(need(nrow(x),"No completed-race validation rows."));picks<-x%>%filter(consensus_rank==1);tibble(Metric=c("Races","Winner-pick accuracy","Top-3 selection hit rate","Finish-rank MAE"),Value=c(n_distinct(paste(picks$season,picks$round)),fmt_pct(mean(picks$actual_finish==1,na.rm=TRUE),1),fmt_pct(mean(x$actual_finish<=3&x$consensus_rank<=3,na.rm=TRUE)/mean(x$consensus_rank<=3,na.rm=TRUE),1),fmt_num(mean(abs(x$consensus_rank-x$actual_finish),na.rm=TRUE),2)))},striped=TRUE,spacing="s",rownames=FALSE)

  # Profiles.
  selected_track_schedule<-reactive({req(input$track_schedule_key);x<-track_schedule_2026%>%filter(schedule_key==input$track_schedule_key)%>%slice(1);validate(need(nrow(x),"That 2026 schedule entry is unavailable."));x})
  exact_track_rows<-reactive({s<-selected_track_schedule();history%>%filter(track_name==s$track_name[[1]],season>=as.integer(input$track_start),season<=as.integer(input$track_end))})
  track_rows<-reactive({
    exact<-exact_track_rows()
    if(nrow(exact))return(exact)
    s<-selected_track_schedule();meta<-race_track_metadata(s)
    if("track_cluster_id"%in%names(history)) history%>%filter(track_cluster_id==meta$cluster,season>=as.integer(input$track_start),season<=as.integer(input$track_end))
    else history%>%filter(track_primary_family==meta$family,season>=as.integer(input$track_start),season<=as.integer(input$track_end))
  })
  track_history_is_fallback<-reactive(!nrow(exact_track_rows()))
  output$track_cards<-renderUI({s<-selected_track_schedule();meta<-race_track_metadata(s);characteristics<-track_characteristics_label(s);div(class="metric-row",metric_card(paste0("Round ",s$round[[1]]),meta$track_name,s$race_name[[1]],"gold"),metric_card("Track family",str_to_title(str_replace_all(meta$family,"_"," ")),str_to_title(str_replace_all(meta$cluster,"_"," ")),"blue"),metric_card("Characteristics",if(is.finite(meta$length))paste0(format(meta$length,nsmall=if(meta$length<1)3 else 2)," mi")else"Profile",characteristics,"green"))})
  output$track_history_note<-renderUI({
    s<-selected_track_schedule();x<-track_rows()
    if(!track_history_is_fallback())div(class="rail-note",paste0("Showing exact ",s$track_name[[1]]," history for ",input$track_start,"–",input$track_end,"."))
    else {peers<-sort(unique(x$track_name));div(class="rail-note",strong("Comparable-track fallback: "),paste0("The points-race backbone has no exact ",s$track_name[[1]]," history in this window. Showing ",str_to_title(str_replace_all(race_track_metadata(s)$cluster,"_"," "))," peers: ",paste(peers,collapse=", "),"."))}
  })
  output$track_driver_plot<-renderPlot({x<-track_rows();validate(need(nrow(x),"No track history in this season window."));bubble_performance_plot(x,"driver_name",min_starts=2,limit=15,accent="#F4C542")})
  output$track_owner_plot<-renderPlot({x<-track_rows();validate(need(nrow(x),"No track history in this season window."));bubble_performance_plot(x,"owner_name",min_starts=3,limit=12,accent="#23A6D5")})
  output$track_leaders<-renderTable({track_rows()%>%filter(is.finite(finish_position))%>%group_by(driver_name)%>%summarise(Starts=n(),Wins=sum(finish_position==1,na.rm=TRUE),`Top 3`=sum(finish_position<=3,na.rm=TRUE),`Avg finish`=mean(finish_position,na.rm=TRUE),.groups="drop")%>%arrange(desc(Wins),`Avg finish`)%>%slice_head(n=12)%>%mutate(`Avg finish`=round(`Avg finish`,1))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$track_history<-renderTable({track_rows()%>%arrange(desc(season),desc(round),finish_position)%>%transmute(Season=as.character(as.integer(season)),Round=as.character(as.integer(round)),Track=track_name,Race=race_name,Driver=driver_name,Owner=owner_name,Start=fmt_int(start_position),Finish=fmt_int(finish_position),Points=fmt_int(points))%>%slice_head(n=80)},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)

  profile_rows<-function(entity,column,start,end)history%>%filter(.data[[column]]==entity,season>=as.integer(start),season<=as.integer(end))
  driver_rows<-reactive(profile_rows(input$driver_name,"driver_name",input$driver_start,input$driver_end)); owner_rows<-reactive(profile_rows(input$owner_name,"owner_name",input$owner_start,input$owner_end))
  profile_cards<-function(x,label){div(class="metric-row",metric_card(label,if(nrow(x))first(x[[if(label=="Driver")"driver_name"else"owner_name"]])else"—",paste0(n_distinct(paste(x$season,x$round))," starts"),"gold"),metric_card("Wins",sum(x$finish_position==1,na.rm=TRUE),paste0(sum(x$finish_position<=3,na.rm=TRUE)," top threes"),"blue"),metric_card("Average finish",fmt_num(mean(x$finish_position,na.rm=TRUE),1),paste0(fmt_int(sum(x$laps_led,na.rm=TRUE))," laps led"),"green"))}
  output$driver_cards<-renderUI(profile_cards(driver_rows(),"Driver")); output$owner_cards<-renderUI(profile_cards(owner_rows(),"Owner"))
  profile_plot<-function(x){x%>%filter(is.finite(finish_position))%>%group_by(season)%>%summarise(avg_finish=mean(finish_position,na.rm=TRUE),.groups="drop")%>%ggplot(aes(season,avg_finish))+geom_line(color="#F4C542",linewidth=1.3)+geom_point(color="#FFF2A8",size=2.8)+scale_y_reverse()+labs(x=NULL,y="Average finish")+theme_minimal(base_size=12)+theme_dark_custom()}
  output$driver_plot<-renderPlot(profile_plot(driver_rows())); output$owner_plot<-renderPlot(profile_plot(owner_rows()))
  output$driver_family_plot<-renderPlot({x<-driver_rows();validate(need(nrow(x),"No driver history in this season window."));family_performance_plot(x,"#38B27A")})
  output$owner_family_plot<-renderPlot({x<-owner_rows();validate(need(nrow(x),"No owner history in this season window."));family_performance_plot(x,"#23A6D5")})
  profile_family<-function(x)x%>%group_by(track_primary_family)%>%summarise(Starts=n(),Wins=sum(finish_position==1,na.rm=TRUE),`Top 3`=sum(finish_position<=3,na.rm=TRUE),`Avg finish`=round(mean(finish_position,na.rm=TRUE),1),.groups="drop")%>%arrange(`Avg finish`)
  output$driver_family<-renderTable(profile_family(driver_rows()),striped=TRUE,rownames=FALSE);output$owner_family<-renderTable(profile_family(owner_rows()),striped=TRUE,rownames=FALSE)
  profile_recent<-function(x)x%>%arrange(desc(season),desc(round))%>%transmute(Season=season,Round=round,Race=race_name,Track=track_name,Family=track_primary_family,Driver=driver_name,Start=fmt_int(start_position),Finish=fmt_int(finish_position),Points=fmt_int(points))%>%slice_head(n=25)
  output$driver_recent<-renderTable(profile_recent(driver_rows())%>%select(-Driver),striped=TRUE,hover=TRUE,rownames=FALSE);output$owner_recent<-renderTable(profile_recent(owner_rows()),striped=TRUE,hover=TRUE,rownames=FALSE)

  # Chatter, fantasy, and validation.
  output$chatter_roi<-renderTable({
    req(input$chatter_roi_start,input$chatter_roi_end)
    base<-summarise_bets_window(rows_to_bets(chatter_contract(overlay_backtest,FALSE)),input$chatter_roi_start,input$chatter_roi_end)
    adjusted<-summarise_bets_window(rows_to_bets(chatter_contract(overlay_backtest,TRUE)),input$chatter_roi_start,input$chatter_roi_end)
    validate(need(nrow(base)&&nrow(adjusted),"No completed chatter-validation bets in this season window."))
    bind_rows(render_roi_table(base)%>%mutate(Variant="Base consensus",.before=1),render_roi_table(adjusted)%>%mutate(Variant="With chatter",.before=1))
  },striped=TRUE,hover=TRUE,spacing="s",rownames=FALSE)
  output$chatter_cards<-renderUI({
    x<-if(input$chatter_view=="current")forecast else overlay_backtest;applied<-if("chatter_overlay_applied"%in%names(x))sum(x$chatter_overlay_applied%in%TRUE,na.rm=TRUE)else 0
    if(input$chatter_view=="current"&&nrow(x)){
      base_pick<-x%>%slice_min(predicted_finish_rank,n=1,with_ties=FALSE);adjusted_pick<-x%>%slice_min(predicted_finish_rank_adjusted,n=1,with_ties=FALSE)
      div(class="metric-row",metric_card("Base finish pick",base_pick$driver_name,fmt_num(base_pick$predicted_finish_position,2),"gold"),metric_card("Chatter finish pick",adjusted_pick$driver_name,fmt_num(adjusted_pick$predicted_finish_position_adjusted,2),"blue"),metric_card("Adjustments applied",applied,paste0(nrow(x)-applied," neutral rows"),"green"))
    }else div(class="metric-row",metric_card("Rows",nrow(x),"2025 fixed-season validation","gold"),metric_card("Adjustments applied",applied,"Historical proxies remain neutral","blue"),metric_card("Signal status",if(applied>0)"Active"else"Neutral","Safety-gated overlay","green"))
  })
  output$chatter_table<-renderTable({
    x<-if(input$chatter_view=="current")forecast else overlay_backtest
    required<-c("predicted_finish_position_adjusted","predicted_points_adjusted","win_probability_adjusted","top3_probability_adjusted")
    validate(need(all(required%in%names(x)),"Adjusted chatter columns are unavailable."))
    x%>%mutate(finish_change=predicted_finish_position_adjusted-predicted_finish_position,points_change=predicted_points_adjusted-predicted_points,win_change=win_probability_adjusted-win_probability,top3_change=top3_probability_adjusted-top3_probability)%>%
      arrange(desc(abs(finish_change)),desc(abs(points_change)))%>%
      transmute(Driver=driver_name,Applied=ifelse(chatter_overlay_applied,"Yes","No"),`Chatter score`=fmt_num(composite_chatter_score,3),`Base finish`=fmt_num(predicted_finish_position,2),`Chatter finish`=fmt_num(predicted_finish_position_adjusted,2),`Finish change`=fmt_num(finish_change,2),`Base points`=fmt_num(predicted_points,2),`Chatter points`=fmt_num(predicted_points_adjusted,2),`Points change`=fmt_num(points_change,2),`Base win`=fmt_pct(win_probability,2),`Chatter win`=fmt_pct(win_probability_adjusted,2),`Win change`=fmt_pct(win_change,2),`Base podium`=fmt_pct(top3_probability,2),`Chatter podium`=fmt_pct(top3_probability_adjusted,2),`Podium change`=fmt_pct(top3_change,2))%>%slice_head(n=100)
  },striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$chatter_metrics<-renderTable(overlay_metrics,striped=TRUE,rownames=FALSE)
  dk_variant<-reactive(if(isTRUE(input$dk_use_chatter))"chatter"else"base")
  dk_lineups_view<-reactive({if("projection_variant"%in%names(dk_lineups))dk_lineups%>%filter(projection_variant==dk_variant())else dk_lineups})
  dk_members_view<-reactive({if("projection_variant"%in%names(dk_members))dk_members%>%filter(projection_variant==dk_variant())else dk_members})
  dk_driver_view<-reactive({
    x<-dk
    suffix<-if(dk_variant()=="chatter")"chatter"else"base"
    projection_col<-paste0("dk_projection_",suffix);value_col<-paste0("dk_value_per_1000_",suffix);finish_col<-paste0("dk_projected_finish_rank_",suffix)
    if(all(c(projection_col,value_col,finish_col)%in%names(x)))x<-x%>%mutate(dk_projection=.data[[projection_col]],dk_value_per_1000=.data[[value_col]],dk_projected_finish_rank=.data[[finish_col]])
    x
  })
  observeEvent(dk_variant(),{
    x<-dk_lineups_view();choices<-if(nrow(x))setNames(x$lineup_rank,paste0("#",x$lineup_rank," — ",fmt_num(x$projected_points,1)," pts"))else character()
    updateSelectInput(session,"dk_lineup",choices=choices,selected=if(length(choices))unname(choices[[1]])else character())
  },ignoreInit=TRUE)
  output$dk_cards<-renderUI({r<-dk_lineups_view()%>%filter(lineup_rank==as.integer(input$dk_lineup))%>%slice(1);applied<-if("chatter_overlay_applied"%in%names(dk))sum(dk$chatter_overlay_applied%in%TRUE,na.rm=TRUE)else 0;overlay_note<-if(dk_variant()=="chatter")paste0("Included; ",applied," verified adjustments applied")else"Excluded";div(class="metric-row",metric_card("Projected points",fmt_num(r$projected_points,1),paste0("Lineup #",r$lineup_rank),"gold"),metric_card("Salary",paste0("$",fmt_int(r$total_salary)),paste0("$",fmt_int(r$salary_remaining)," remaining"),"blue"),metric_card("Chatter overlay",if(dk_variant()=="chatter")"Included"else"Excluded",overlay_note,"green"))})
  output$dk_selected<-renderTable({dk_members_view()%>%filter(lineup_rank==as.integer(input$dk_lineup))%>%arrange(driver_slot)%>%transmute(Slot=fmt_int(driver_slot),Driver=driver_name,Salary=paste0("$",fmt_int(salary)),Projection=fmt_num(dk_projection,1),Value=fmt_num(dk_value_per_1000,2),Start=fmt_int(dk_starting_position_used),`Finish rank`=fmt_int(dk_projected_finish_rank))},striped=TRUE,rownames=FALSE)
  output$dk_board<-renderTable({dk_driver_view()%>%arrange(desc(dk_projection))%>%transmute(Rank=row_number(),Driver=driver_name,Salary=paste0("$",fmt_int(salary)),Projection=fmt_num(dk_projection,1),Value=fmt_num(dk_value_per_1000,2),Start=fmt_int(dk_starting_position_used),`Finish rank`=fmt_int(dk_projected_finish_rank),`Laps led`=fmt_num(projected_laps_led,1),`Fastest laps`=fmt_num(projected_fastest_laps,1))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$dk_top<-renderTable({dk_lineups_view()%>%slice_head(n=10)%>%transmute(Rank=fmt_int(lineup_rank),Salary=paste0("$",fmt_int(total_salary)),Remaining=paste0("$",fmt_int(salary_remaining)),Projection=fmt_num(projected_points,1),Drivers=drivers)},striped=TRUE,rownames=FALSE)
  output$dk_metrics<-renderTable(dk_backtest_metrics,striped=TRUE,rownames=FALSE)
  output$bt_cards<-renderUI({x<-backtest%>%filter(round==as.integer(input$bt_round));pick<-x%>%slice_min(predicted_finish_rank,n=1);div(class="metric-row",metric_card("Predicted winner",pick$driver_name,pick$owner_name,"gold"),metric_card("Actual winner",(x%>%filter(target_finish_position==1)%>%pull(driver_name)%>%first())%||%"—",first(x$race_name),"blue"),metric_card("Field",nrow(x),paste0(str_to_title(active_strategy_name)," evaluation"),"green"))})
  output$bt_metrics<-renderTable(backtest_metrics,striped=TRUE,rownames=FALSE)
  output$bt_table<-renderTable({backtest%>%filter(round==as.integer(input$bt_round))%>%arrange(predicted_finish_rank)%>%transmute(Rank=predicted_finish_rank,Driver=driver_name,Owner=owner_name,Qualifying=predicted_qualifying_rank,`Pred finish`=fmt_num(predicted_finish_position,1),Win=fmt_pct(win_probability,1),`Top 3`=fmt_pct(top3_probability,1),Points=fmt_num(predicted_points,1),Actual=fmt_int(target_finish_position))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$spec_table<-renderTable({x<-family_specs[[input$spec_family]];x%>%mutate(model=model_label(model))%>%select(any_of(c("model","route","train_end_season","position_rows","deficit_rows","training_rows","selected_numeric_features","target","max_depth","eta","min_child_weight","subsample","colsample_bytree","nrounds","eval_metric","eval_value","tuning_status")))},striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
  output$file_table<-renderTable({files<-c("nascar_active_strategy.csv","nascar_stage15_upcoming_race_predictions_latest.csv",paste0("nascar_stage15_current_",c("qualifying","finish","probability","points"),"_model_predictions_latest.csv"),"nascar_stage16_active_backtest_2025_2026.csv","nascar_stage17_market_winner_full_boards_2025_2026.csv","nascar_stage18_current_forecast_with_safe_overlays_latest.csv","nascar_stage19_draftkings_driver_projections_latest.csv");tibble(File=files,Exists=vapply(files,function(x)file.exists(data_path(x)),logical(1)),Modified=vapply(files,function(x){p<-data_path(x);if(file.exists(p))format(file.info(p)$mtime,"%Y-%m-%d %H:%M")else"—"},character(1)))},striped=TRUE,hover=TRUE,rownames=FALSE)
  output$audit_table<-renderTable(external_audit,striped=TRUE,hover=TRUE,spacing="xs",rownames=FALSE)
}

theme_dark_custom <- function() theme(panel.grid.minor=element_blank(),panel.grid.major=element_line(color="#263143"),plot.background=element_rect(fill="#111722",color=NA),panel.background=element_rect(fill="#111722",color=NA),text=element_text(color="#DCE5F2"),axis.text=element_text(color="#AEB9C9"),axis.title=element_text(color="#AEB9C9"))

shinyApp(ui, server)
