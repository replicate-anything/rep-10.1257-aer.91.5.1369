# Table 1 — Summary Statistics (R)
# Study repo: rep-10.1257-aer.91.5.1369
# R translation of stata/maketable1.do — use replication id tab_1 (not tab_1_stata).

summarize_vars <- function(data, vars) {
  out <- lapply(vars, function(v) {
    x <- data[[v]]
    x <- x[is.finite(x)]
    data.frame(
      Variable = v,
      N = length(x),
      Mean = mean(x),
      SD = sd(x),
      Min = min(x),
      Max = max(x),
      row.names = NULL,
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}

make_tab_1 <- function(data) {
  vars <- c(
    "logpgp95", "loghjypl", "avexpr", "cons00a", "cons1",
    "democ00a", "euro1900", "logem4"
  )

  whole_world <- summarize_vars(data, vars[vars %in% names(data)])

  base <- data[!is.na(data$baseco) & data$baseco == 1, , drop = FALSE]
  base_sample <- summarize_vars(base, vars[vars %in% names(base)])

  df <- data
  rank <- rank(df$extmort4, ties.method = "average")
  count <- sum(!is.na(df$extmort4))
  ptile <- rank / count
  df$q <- NA_integer_
  df$q[ptile <= 0.25] <- 1L
  df$q[ptile > 0.25 & ptile <= 0.5] <- 2L
  df$q[ptile > 0.5 & ptile <= 0.75] <- 3L
  df$q[ptile > 0.75] <- 4L

  quartiles <- lapply(1:4, function(q) {
    sub <- df[df$q == q, , drop = FALSE]
    summarize_vars(sub, vars[vars %in% names(sub)])
  })
  names(quartiles) <- paste0("Q", 1:4)

  list(
    whole_world = whole_world,
    base_sample = base_sample,
    quartiles = quartiles
  )
}

format_tab_1 <- function(object) {
  section <- function(title, df) {
    hdr <- paste0("<h4>", title, "</h4>")
    cols <- setdiff(names(df), "Variable")
    header <- paste0("<tr><th>Variable</th>", paste0("<th>", cols, "</th>", collapse = ""), "</tr>")
    rows <- apply(df, 1, function(row) {
      vals <- paste0("<td>", round(as.numeric(row[cols]), 3), "</td>", collapse = "")
      paste0("<tr><td>", row[["Variable"]], "</td>", vals, "</tr>")
    })
    tbl <- paste0(
      '<table class="table table-sm replication-table"><thead>', header,
      "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table>"
    )
    paste0(hdr, tbl)
  }

  parts <- c(
    section("Column 1 (whole world)", object$whole_world),
    section("Column 2 (base sample)", object$base_sample)
  )
  for (nm in names(object$quartiles)) {
    parts <- c(parts, section(paste("Mortality quartile", nm), object$quartiles[[nm]]))
  }

  paste0(
    '<div class="replication-table">',
    "<h3>Table 1. Summary Statistics</h3>",
    paste(parts, collapse = ""),
    "</div>"
  )
}
