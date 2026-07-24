# tests/substantive/tab_4.R

# Published values from AJR (2001) Table 4, "avexpr" row, across IV and OLS columns.
tab_4_benchmark <- function() {
  list(
    "IV1"  = list(term = "avexpr", coef = 0.94, se = 0.16, nobs = 64L),
    "IV2"  = list(term = "avexpr", coef = 1.00, se = 0.22, nobs = 64L),
    "IV7"  = list(term = "avexpr", coef = 0.98, se = 0.3, nobs = 64L),
    "IV9"  = list(term = "avexpr", coef = 0.98, se = 0.17, nobs = 61L),
    "OLS1" = list(term = "avexpr", coef = 0.52, se = 0.06, nobs = 64L),
    "OLS7" = list(term = "avexpr", coef = 0.42, se = 0.06, nobs = 64L),
    "OLS9" = list(term = "avexpr", coef = 0.46, se = 0.06, nobs = 61L)
  )
}

check_lm_ivreg_table_benchmark <- function(models, spec, tolerance = 0.001) {
  if (!is.list(models) || length(models) == 0L) {
    stop("models must be a non-empty list of model objects.", call. = FALSE)
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
    
    if (!inherits(model, "lm") && !inherits(model, "ivreg")) {
      failures <- c(failures, paste0(col, ": not an lm/ivreg object"))
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

substantive_check_tab_4 <- function(object, tolerance = 0.01) {
  check_lm_ivreg_table_benchmark(object, tab_4_benchmark(), tolerance = tolerance)
}