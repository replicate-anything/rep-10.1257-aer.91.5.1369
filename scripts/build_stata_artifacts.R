setwd("C:/WZB Dropbox/Macartan Humphreys/5_github/replicate_everything")
study <- file.path(getwd(), "rep-10.1257-aer.91.5.1369")
stata <- Sys.which("StataMP-64")
if (!nzchar(stata)) {
  stata <- "C:/Program Files/Stata17/StataMP-64.exe"
}
if (!file.exists(stata)) {
  stop("Stata executable not found.")
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)

for (n in 1:8) {
  do_file <- file.path("code", paste0("tab_", n, ".do"))
  message("Running ", do_file, " ...")
  setwd(study)
  status <- system2(
    stata,
    c("/e", "do", shQuote(do_file, type = "cmd")),
    wait = TRUE,
    stdout = "",
    stderr = ""
  )
  log_path <- file.path(study, "artifacts/staging", paste0("tab_", n, "_stata.log"))
  if (!file.exists(log_path)) {
    warning("Expected log not found after ", basename(do_file), " (status=", status, ")")
  }
}

stray <- list.files(study, pattern = "^replicate_.*\\.log$", full.names = TRUE)
if (length(stray)) {
  unlink(stray)
}

source(file.path(study, "scripts/format_stata_artifacts.R"))
