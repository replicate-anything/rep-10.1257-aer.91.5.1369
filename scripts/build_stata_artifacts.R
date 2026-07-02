setwd("C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything")
devtools::load_all("replicateEverything")
options(
  replicateEverything.registry_root = file.path(getwd(), "registry"),
  replicateEverything.study_folders_root = getwd()
)
study <- file.path(getwd(), "rep-10.1257-aer.91.5.1369")
registry <- file.path(getwd(), "registry")
for (id in paste0("tab_", 2:8, "_stata")) {
  message("=== ", id, " ===")
  replicateEverything::build_study_artifacts(
    study,
    install_deps = FALSE,
    ids = id,
    registry_root = registry
  )
}
