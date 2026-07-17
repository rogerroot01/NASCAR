args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)

shiny::runApp(
  appDir = normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE),
  host = "127.0.0.1",
  port = 4488,
  launch.browser = FALSE
)
