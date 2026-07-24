# tests/substantive/tab_1.R

# Published values from AJR (2001) Table 1. Add one row per (section, Variable)
# pair you want checked — you don't need to cover all of them.
tab_1_benchmark <- function() {
  data.frame(
    section  = c("whole_world", "base_sample"),
    Variable = c("logpgp95",     "logpgp95"),
    Mean     = c(8.3,  8.059),
    SD       = c(1.1, 1.1),
    N        = c(163L,64L),
    stringsAsFactors = FALSE
  )
}

# Flattens make_tab_1()'s nested output into one long data.frame with a
# `section` column, so it can be matched against the benchmark by name.
flatten_tab_1 <- function(object) {
  sections <- c(
    list(whole_world = object$whole_world, base_sample = object$base_sample),
    object$quartiles   # Q1..Q4
  )
  rows <- lapply(names(sections), function(nm) {
    df <- sections[[nm]]
    df$section <- nm
    df
  })
  do.call(rbind, rows)
}

check_summary_table_benchmark <- function(object, spec, tolerance = 0.01) {
  actual <- flatten_tab_1(object)
  merged <- merge(spec, actual, by = c("section", "Variable"), suffixes = c("_exp", "_act"))
  
  if (nrow(merged) != nrow(spec)) {
    missing <- setdiff(
      paste(spec$section, spec$Variable),
      paste(merged$section, merged$Variable)
    )
    stop("Benchmark rows not found in replicated table: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  failures <- character(0)
  for (i in seq_len(nrow(merged))) {
    row <- merged[i, ]
    label <- paste0(row$section, "/", row$Variable)
    if (abs(row$Mean_exp - row$Mean_act) > tolerance) {
      failures <- c(failures, sprintf("%s Mean: expected %.3f, got %.3f", label, row$Mean_exp, row$Mean_act))
    }
    if (abs(row$SD_exp - row$SD_act) > tolerance) {
      failures <- c(failures, sprintf("%s SD: expected %.3f, got %.3f", label, row$SD_exp, row$SD_act))
    }
    if (row$N_exp != row$N_act) {
      failures <- c(failures, sprintf("%s N: expected %d, got %d", label, row$N_exp, row$N_act))
    }
  }
  
  if (length(failures) > 0L) {
    stop("Published benchmark check failed:\n", paste0(" - ", failures, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

substantive_check_tab_1 <- function(object, tolerance = 0.1) {
  check_summary_table_benchmark(object, tab_1_benchmark(), tolerance = tolerance)
}