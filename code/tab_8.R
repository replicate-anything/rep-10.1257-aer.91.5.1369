# Table 8 — Overidentification Tests
# Study repo: rep-10.1257-aer.91.5.1369

hausman_iv_pair <- function(consistent_fml, efficient_fml, data) {
  cons <- AER::ivreg(consistent_fml, data = data)
  eff <- AER::ivreg(efficient_fml, data = data)
  test <- tryCatch(
    lmtest::waldtest(cons, eff),
    error = function(e) NULL
  )
  list(consistent = cons, efficient = eff, hausman = test)
}

make_tab_8 <- function(data) {
  df <- data[data$baseco == 1, , drop = FALSE]
  iv <- AER::ivreg

  panel_ab <- list(
    "IV euro" = iv(logpgp95 ~ avexpr | euro1900, data = df),
    "IV euro+lat" = iv(logpgp95 ~ lat_abst + avexpr | lat_abst + euro1900, data = df),
    "IV cons00a" = iv(logpgp95 ~ avexpr | cons00a, data = df),
    "IV cons00a+lat" = iv(logpgp95 ~ lat_abst + avexpr | lat_abst + cons00a, data = df),
    "IV democ00a" = iv(logpgp95 ~ avexpr | democ00a, data = df),
    "IV democ00a+lat" = iv(logpgp95 ~ lat_abst + avexpr | lat_abst + democ00a, data = df),
    "IV cons1" = iv(logpgp95 ~ indtime + avexpr | indtime + cons1, data = df),
    "IV cons1+lat" = iv(
      logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + cons1,
      data = df
    ),
    "IV democ1" = iv(logpgp95 ~ indtime + avexpr | indtime + democ1, data = df),
    "IV democ1+lat" = iv(
      logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + democ1,
      data = df
    )
  )

  hausman_specs <- list(
  list(name = "euro1900", cons = logpgp95 ~ avexpr | euro1900, eff = logpgp95 ~ avexpr | euro1900 + logem4),
    list(
      name = "euro1900 + lat",
      cons = logpgp95 ~ lat_abst + avexpr | lat_abst + euro1900,
      eff = logpgp95 ~ lat_abst + avexpr | lat_abst + euro1900 + logem4
    ),
    list(name = "cons00a", cons = logpgp95 ~ avexpr | cons00a, eff = logpgp95 ~ avexpr | cons00a + logem4),
    list(
      name = "cons00a + lat",
      cons = logpgp95 ~ lat_abst + avexpr | lat_abst + cons00a,
      eff = logpgp95 ~ lat_abst + avexpr | lat_abst + cons00a + logem4
    ),
    list(name = "democ00a", cons = logpgp95 ~ avexpr | democ00a, eff = logpgp95 ~ avexpr | democ00a + logem4),
    list(
      name = "democ00a + lat",
      cons = logpgp95 ~ lat_abst + avexpr | lat_abst + democ00a,
      eff = logpgp95 ~ lat_abst + avexpr | lat_abst + democ00a + logem4
    ),
    list(
      name = "cons1",
      cons = logpgp95 ~ indtime + avexpr | indtime + cons1,
      eff = logpgp95 ~ indtime + avexpr | indtime + cons1 + logem4
    ),
    list(
      name = "cons1 + lat",
      cons = logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + cons1,
      eff = logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + cons1 + logem4
    ),
    list(
      name = "democ1",
      cons = logpgp95 ~ indtime + avexpr | indtime + democ1,
      eff = logpgp95 ~ indtime + avexpr | indtime + democ1 + logem4
    ),
    list(
      name = "democ1 + lat",
      cons = logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + democ1,
      eff = logpgp95 ~ lat_abst + indtime + avexpr | lat_abst + indtime + democ1 + logem4
    )
  )

  hausman <- lapply(hausman_specs, function(spec) {
    pair <- hausman_iv_pair(spec$cons, spec$eff, df)
    h <- pair$hausman
    data.frame(
      specification = spec$name,
      statistic = if (!is.null(h)) unname(h$statistic[[1]]) else NA_real_,
      df = if (!is.null(h)) unname(h$parameter[[1]]) else NA_real_,
      p_value = if (!is.null(h)) unname(h$p.value[[1]]) else NA_real_,
      row.names = NULL
    )
  })
  hausman <- do.call(rbind, hausman)

  panel_d <- list(
    "2SLS euro" = iv(logpgp95 ~ logem4 + avexpr | logem4 + euro1900, data = df),
    "2SLS euro+lat" = iv(
      logpgp95 ~ lat_abst + logem4 + avexpr | lat_abst + logem4 + euro1900,
      data = df
    ),
    "2SLS cons00a" = iv(logpgp95 ~ logem4 + avexpr | logem4 + cons00a, data = df),
    "2SLS cons00a+lat" = iv(
      logpgp95 ~ lat_abst + logem4 + avexpr | lat_abst + logem4 + cons00a,
      data = df
    ),
    "2SLS democ00a" = iv(logpgp95 ~ logem4 + avexpr | logem4 + democ00a, data = df),
    "2SLS democ00a+lat" = iv(
      logpgp95 ~ lat_abst + logem4 + avexpr | lat_abst + logem4 + democ00a,
      data = df
    ),
    "2SLS cons1" = iv(
      logpgp95 ~ indtime + logem4 + avexpr | indtime + logem4 + cons1,
      data = df
    ),
    "2SLS cons1+lat" = iv(
      logpgp95 ~ lat_abst + indtime + logem4 + avexpr |
        lat_abst + indtime + logem4 + cons1,
      data = df
    ),
    "2SLS democ1" = iv(
      logpgp95 ~ indtime + logem4 + avexpr | indtime + logem4 + democ1,
      data = df
    ),
    "2SLS democ1+lat" = iv(
      logpgp95 ~ lat_abst + indtime + logem4 + avexpr |
        lat_abst + indtime + logem4 + democ1,
      data = df
    )
  )

  list(panel_ab = panel_ab, hausman = hausman, panel_d = panel_d)
}

format_tab_8 <- function(object) {
  iv_html <- as.character(modelsummary::modelsummary(
    object$panel_ab,
    output = "html",
    stars = TRUE,
    title = "Table 8. Panels A and B — IV Regressions"
  ))
  hausman_html <- paste0(
    "<h4>Panel C — Hausman tests (consistent vs efficient IV)</h4>",
    "<table class='table table-sm replication-table'><thead><tr>",
    "<th>Specification</th><th>Statistic</th><th>df</th><th>p-value</th></tr></thead><tbody>",
    paste(apply(object$hausman, 1, function(row) {
      paste0(
        "<tr><td>", row[["specification"]], "</td><td>",
        round(as.numeric(row[["statistic"]]), 4), "</td><td>",
        round(as.numeric(row[["df"]]), 2), "</td><td>",
        round(as.numeric(row[["p_value"]]), 4), "</td></tr>"
      )
    }), collapse = ""),
    "</tbody></table>"
  )
  d_html <- as.character(modelsummary::modelsummary(
    object$panel_d,
    output = "html",
    stars = TRUE,
    title = "Panel D — Second stage with log mortality exogenous"
  ))
  paste0(
    '<div class="replication-table">',
    iv_html,
    hausman_html,
    d_html,
    "</div>"
  )
}
