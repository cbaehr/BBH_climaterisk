##

if (Sys.info()["user"] == "vincentheddesheimer") {
  to_path <- "~/Dropbox (Princeton)/Apps/Overleaf/Climate Exposure and Political Activity_BBH/graphs/"
  plots <- dir("~/Dropbox (Princeton)/BBH/BBH1/results/figures", full.names = T, recursive = T)
} else if (Sys.info()["user"] == "fiona") {
  to_path <- "C:/Users/fiona/Dropbox (Princeton)/Apps/Overleaf/Climate Exposure and Political Activity_BBH/graphs"
  plots <- dir("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1/results/figures", full.names = T, recursive = T)
} else if (Sys.info()["user"] == "christianbaehr") {
  to_path <- "/Users/christianbaehr/Dropbox/BBH/BBH1/Climate Exposure and Political Activity_BBH/graphs"
  plots <- dir("/Users/christianbaehr/Dropbox/BBH/BBH1/results/figures", full.names = T, recursive = T)
} else {
  to_path <- ""
}


plots <- plots %>%
  .[str_detect(., "pdf|jpeg|tex|png")] %>%
  # remove all files with /old/ in the path
  .[!str_detect(., "/old/")]

file.copy(
  from = c(plots),
  to = to_path,
  overwrite = TRUE,
  recursive = FALSE,
  copy.mode = TRUE
)


cat("Plots moved successfully")
