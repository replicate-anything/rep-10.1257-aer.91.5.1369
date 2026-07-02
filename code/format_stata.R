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

drop_stata_log_boilerplate <- function(lines) {
  if (!length(lines)) return(character(0))
  lines <- gsub("\r$", "", lines, fixed = TRUE)
  keep <- rep(TRUE, length(lines))
  for (i in seq_along(lines)) {
    line <- trimws(lines[[i]])
    if (grepl("^-{3,}$", line)) {
      keep[[i]] <- FALSE
      next
    }
    if (grepl("^(name:|log:|log type:|opened on:)", line)) {
      keep[[i]] <- FALSE
      next
    }
    if (grepl("^> ", line)) {
      keep[[i]] <- FALSE
      next
    }
    if (identical(line, "capture log close") || identical(line, ". capture log close")) {
      keep[[i]] <- FALSE
      next
    }
    if (grepl("^\\. (capture log close|log using)", line)) {
      keep[[i]] <- FALSE
    }
  }
  lines <- lines[keep]
  lines <- lines[!grepl("^\\s*$", lines)]
  lines
}

extract_stata_log_body <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  drop_stata_log_boilerplate(lines)
}

split_stata_pipe_row <- function(line) {
  parts <- strsplit(line, "|", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2L) {
    return(parts)
  }
  var <- parts[[1]]
  stats <- strsplit(parts[[2]], "\\s{2,}", perl = TRUE)[[1]]
  stats <- trimws(stats)
  stats <- stats[nzchar(stats)]
  c(var, stats)
}

parse_stata_table_block <- function(lines) {
  if (!length(lines)) return(NULL)
  header_idx <- grep("Variable \\|", lines)
  if (!length(header_idx)) return(NULL)
  header_idx <- header_idx[[1]]
  tail <- lines[(header_idx + 1L):length(lines)]
  sep_idx <- grep("^[-+]+$", tail)
  if (!length(sep_idx)) return(NULL)
  data_start <- header_idx + sep_idx[[1]] + 1L
  header <- split_stata_pipe_row(lines[[header_idx]])
  if (!length(header)) {
    header <- c("Variable", "Obs", "Mean", "Std. dev.", "Min", "Max")
  }
  rows <- list()
  for (i in seq.int(data_start, length(lines))) {
    line <- lines[[i]]
    if (!nzchar(trimws(line))) break
    if (grepl("^[-+]", trimws(line))) break
    if (grepl("^\\. ", line)) break
    cells <- split_stata_pipe_row(line)
    if (length(cells) >= 2L) {
      rows[[length(rows) + 1L]] <- cells
    }
  }
  if (!length(rows)) return(NULL)
  list(header = header, rows = rows)
}

stata_table_html <- function(title, block) {
  if (is.null(block)) return(NULL)
  header <- block$header
  rows <- block$rows
  ncols <- max(length(header), max(vapply(rows, length, integer(1))))
  if (length(header) < ncols) {
    header <- c(header, rep("", ncols - length(header)))
  }
  hdr <- paste0("<tr>", paste0("<th>", header, "</th>", collapse = ""), "</tr>")
  body <- vapply(rows, function(cells) {
    vals <- c(cells, rep("", max(0L, ncols - length(cells))))
    paste0("<tr>", paste0("<td>", vals, "</td>", collapse = ""), "</tr>")
  }, character(1))
  title_html <- if (!is.null(title) && nzchar(title)) {
    paste0("<h4>", title, "</h4>")
  } else {
    ""
  }
  paste0(
    title_html,
    '<table class="table table-sm replication-table stata-output-table"><thead>',
    hdr, "</thead><tbody>", paste(body, collapse = ""), "</tbody></table>"
  )
}

format_stata_log_lines <- function(lines) {
  lines <- drop_stata_log_boilerplate(lines)
  if (!length(lines)) {
    return('<p class="text-muted">No Stata output captured.</p>')
  }

  parts <- character(0)
  i <- 1L
  while (i <= length(lines)) {
    line <- lines[[i]]
    cmd <- sub("^\\. ", "", trimws(line))

    is_table_cmd <- grepl(
      "^(summ|tab |bys .+: summ|reg |ivreg|xtreg|probit|logit|areg|xi:)",
      cmd
    )
    if (is_table_cmd) {
      block <- parse_stata_table_block(lines[i:length(lines)])
      html <- stata_table_html(cmd, block)
      if (!is.null(html)) {
        parts <- c(parts, html)
        i <- i + length(block$rows) + 4L
        next
      }
    }

    if (grepl("^\\. ", line)) {
      i <- i + 1L
      next
    }

    i <- i + 1L
  }

  if (length(parts)) {
    return(paste0(parts, collapse = ""))
  }

  text <- paste(lines, collapse = "\n")
  if (requireNamespace("htmltools", quietly = TRUE)) {
    text <- htmltools::htmlEscape(text)
  }
  paste0('<pre class="stata-output replication-table">', text, "</pre>")
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
  lines <- extract_stata_log_body(path)
  format_stata_log_lines(lines)
}

format_tab_1_stata <- format_stata_log
format_tab_2_stata <- format_stata_log
format_tab_3_stata <- format_stata_log
format_tab_4_stata <- format_stata_log
format_tab_5_stata <- format_stata_log
format_tab_6_stata <- format_stata_log
format_tab_7_stata <- format_stata_log
format_tab_8_stata <- format_stata_log
