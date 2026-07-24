# tests/substantive/tab_8.R

tab_8_panel_ab_benchmark <- function() {
  list(
    "IV euro"    = list(term = "avexpr", coef = 0.87, se = 0.14),
    "IV cons00a" = list(term = "avexpr", coef = 0.71, se = 0.15)
  )
}

tab_8_panel_d_benchmark <- function() {
  list(
    "2SLS euro"    = list(term = "avexpr", coef = 0.81, se = 0.23),
    "2SLS cons00a" = list(term = "avexpr", coef = 0.45, se = 0.25)
  )
}

check_lm_ivreg_table_benchmark <- function(models, spec, tolerance = 0.001, check_nobs = TRUE) {
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
    
    if (abs(actual_coef - row$coef) > tolerance) {
      failures <- c(failures, sprintf("%s %s coef: expected %.3f, got %.3f", col, term, row$coef, actual_coef))
    }
    if (abs(actual_se - row$se) > tolerance) {
      failures <- c(failures, sprintf("%s %s se: expected %.3f, got %.3f", col, term, row$se, actual_se))
    }
    if (check_nobs && !is.null(row$nobs)) {
      actual_n <- stats::nobs(model)
      if (actual_n != row$nobs) {
        failures <- c(failures, sprintf("%s N: expected %d, got %d", col, row$nobs, actual_n))
      }
    }
  }
  
  if (length(failures) > 0L) {
    stop("Published benchmark check failed:\n", paste0(" - ", failures, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

substantive_check_tab_8 <- function(object, tolerance = 0.01) {
  results <- list(
    panel_ab = tryCatch(
      { check_lm_ivreg_table_benchmark(object$panel_ab, tab_8_panel_ab_benchmark(), tolerance, check_nobs = FALSE); NULL },
      error = function(e) conditionMessage(e)
    ),
    panel_d = tryCatch(
      { check_lm_ivreg_table_benchmark(object$panel_d, tab_8_panel_d_benchmark(), tolerance, check_nobs = FALSE); NULL },
      error = function(e) conditionMessage(e)
    )
  )
  
  failures <- results[!vapply(results, is.null, logical(1))]
  if (length(failures) > 0L) {
    stop(
      "Table 8 substantive check failed:\n",
      paste(sprintf("[%s] %s", names(failures), unlist(failures)), collapse = "\n\n"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}