# tests/substantive/tab_2.R

# Published values from AJR (2001) Table 2, "avexpr" row, columns (1)-(8).
# Keyed by the same column labels make_tab_2() uses, so no positional guesswork.
tab_2_benchmark <- function() {
  list(
    "(1)" = list(term = "avexpr", coef = 0.54, se = 0.06, nobs = 111L),
    "(2)" = list(term = "avexpr", coef = 0.615, se = 0.06, nobs = 64L),
    "(3)" = list(term = "avexpr", coef = 0.428, se = 0.06, nobs = 110L),
    "(4)" = list(term = "avexpr", coef = 0.395, se = 0.05, nobs = 110L),
    "(5)" = list(term = "avexpr", coef = 0.478, se = 0.06, nobs = 64L),
    "(6)" = list(term = "avexpr", coef = 0.439, se = 0.06, nobs = 64L),
    "(7)" = list(term = "avexpr", coef = 0.375, se = 0.04, nobs = 108L),
    "(8)" = list(term = "avexpr", coef = 0.375, se = 0.06, nobs = 61L)
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

substantive_check_tab_2 <- function(object, tolerance = 0.1) {
  check_lm_table_benchmark(object, tab_2_benchmark(), tolerance = tolerance)
}