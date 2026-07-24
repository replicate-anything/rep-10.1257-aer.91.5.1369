# tests/substantive/tab_3.R

# Published values from AJR (2001) Table 3. Add whichever (panel, column)
# rows you have benchmark numbers for — not all 20 need to be covered.
tab_3_benchmark <- function() {
  list(
    "A1" = list(term = "cons00a",  coef = 0.32, se = 0.08, nobs = 63L),
    "A3" = list(term = "democ00a", coef = 0.24, se = 0.06, nobs = 62L),
    "A7" = list(term = "euro1900", coef = 3.20, se = 0.61, nobs = 66L),
    "A9" = list(term = "logem4",   coef = -0.61, se = 0.13, nobs = 64L),
    "B3" = list(term = "logem4",   coef = -0.82, se = 0.17, nobs = 75L),
    "B9" = list(term = "logem4",   coef = -0.11, se = 0.02, nobs = 73L)
  )
}

check_lm_table_benchmark <- function(models, spec, tolerance = 0.001) {
  if (!is.list(models) || length(models) == 0L) {
    stop("models must be a non-empty list of lm objects.", call. = FALSE)
  }
  missing_cols <- setdiff(names(spec), names(models))
  if (length(missing_cols) > 0L) {
    stop("Benchmark references columns not found in models: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  failures <- character(0)
  for (col in names(spec)) {
    model <- models[[col]]
    row   <- spec[[col]]
    term  <- row$term
    
    if (!inherits(model, "lm")) {
      failures <- c(failures, paste0(col, ": not an lm object"))
      next
    }
    if (!term %in% names(coef(model))) {
      failures <- c(failures, paste0(col, ": term '", term, "' not in model coefficients"))
      next
    }
    
    actual_coef <- unname(coef(model)[term])
    actual_se   <- unname(sqrt(diag(vcov(model)))[term])
    actual_n    <- stats::nobs(model)
    
    if (abs(actual_coef - row$coef) > tolerance) {
      failures <- c(failures, sprintf("%s %s coef: expected %.3f, got %.3f", col, term, row$coef, actual_coef))
    }
    if (abs(actual_se - row$se) > tolerance) {
      failures <- c(failures, sprintf("%s %s se: expected %.3f, got %.3f", col, term, row$se, actual_se))
    }
    if (actual_n != row$nobs) {
      failures <- c(failures, sprintf("%s N: expected %d, got %d", col, row$nobs, actual_n))
    }
  }
  
  if (length(failures) > 0L) {
    stop("Published benchmark check failed:\n", paste0(" - ", failures, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

substantive_check_tab_3 <- function(object, tolerance = 0.1) {
  check_lm_table_benchmark(object, tab_3_benchmark(), tolerance = tolerance)
}
