setwd("C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything")
devtools::load_all("replicateEverything")
options(
  replicateEverything.registry_root = file.path(getwd(), "registry"),
  replicateEverything.study_folders_root = getwd()
)
study <- file.path(getwd(), "rep-10.1257-aer.91.5.1369")
ids <- c(paste0("tab_", 1:8), paste0("tab_", 1:8, "_stata"))
for (id in ids) {
  message("=== ", id, " ===")
  tryCatch(
    replicateEverything::build_study_outputs(
      study,
      install_deps = FALSE,
      ids = id,
      registry_root = file.path(getwd(), "registry")
    ),
    error = function(e) message("Skipped ", id, ": ", conditionMessage(e))
  )
}
