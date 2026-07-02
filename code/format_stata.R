# Format Stata log/SMCL output for Shiny display
# Study repo: rep-10.1257-aer.91.5.1369

`%||%` <- function(a, b) if (is.null(a)) b else a

stata_result_path_local <- function(object) {
  if (is.character(object) && length(object) == 1L && nzchar(object)) {
    return(object)
  }
  if (is.list(object) && !is.data.frame(object)) {
    path <- object$output_path %||% object$smcl_path %||% NULL
    if (!is.null(path)) {
      if (length(path) > 1L) path <- path[[1L]]
      path <- as.character(path)
      if (nzchar(path)) return(path)
    }
  }
  NULL
}

format_stata_log <- function(object) {
  path <- stata_result_path_local(object)
  if (is.null(path) || !file.exists(path)) {
    stop("Stata output not found.")
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "smcl") &&
      requireNamespace("replicateEverything", quietly = TRUE)) {
    return(replicateEverything::smcl_to_html(path))
  }
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (requireNamespace("htmltools", quietly = TRUE)) {
    text <- htmltools::htmlEscape(text)
  }
  paste0('<pre class="stata-output replication-table">', text, "</pre>")
}

format_tab_1_stata <- format_stata_log
format_tab_2_stata <- format_stata_log
format_tab_3_stata <- format_stata_log
format_tab_4_stata <- format_stata_log
format_tab_5_stata <- format_stata_log
format_tab_6_stata <- format_stata_log
format_tab_7_stata <- format_stata_log
format_tab_8_stata <- format_stata_log
