setwd("C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything")
study <- file.path(getwd(), "rep-10.1257-aer.91.5.1369")
source(file.path(study, "code/format_stata.R"))

artifact_dir <- file.path(study, "artifacts")
staging_dir <- file.path(artifact_dir, "staging")
dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

for (n in 1:8) {
  id <- paste0("tab_", n, "_stata")
  log_path <- file.path(staging_dir, paste0(id, ".log"))
  out_path <- file.path(artifact_dir, paste0(id, ".html"))
  if (!file.exists(log_path)) {
    message("Skip ", id, ": log missing at ", log_path)
    next
  }
  html <- format_stata_log(list(output_path = log_path))
  writeLines(html, out_path, useBytes = TRUE)
  message("Wrote ", out_path)
}
