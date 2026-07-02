# Table 3 — Determinants of Institutions
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_3 <- function(data) {
  df <- data
  df <- df[df$excolony == 1 & !is.na(df$extmort4), , drop = FALSE]
  df$euro1900 <- df$euro1900 / 100

  panel_a <- list(
    "A1" = lm(avexpr ~ cons00a, data = df),
    "A2" = lm(avexpr ~ lat_abst + cons00a, data = df),
    "A3" = lm(avexpr ~ democ00a, data = df),
    "A4" = lm(avexpr ~ democ00a + lat_abst, data = df),
    "A5" = lm(avexpr ~ indtime + cons1, data = df),
    "A6" = lm(avexpr ~ indtime + cons1 + lat_abst, data = df),
    "A7" = lm(avexpr ~ euro1900, data = df),
    "A8" = lm(avexpr ~ euro1900 + lat_abst, data = df),
    "A9" = lm(avexpr ~ logem4, data = subset(df, !is.na(logpgp95))),
    "A10" = lm(avexpr ~ logem4 + lat_abst, data = subset(df, !is.na(logpgp95)))
  )

  panel_b <- list(
    "B1" = lm(cons00a ~ euro1900, data = subset(df, !is.na(logpgp95))),
    "B2" = lm(cons00a ~ euro1900 + lat_abst, data = subset(df, !is.na(logpgp95))),
    "B3" = lm(cons00a ~ logem4, data = df),
    "B4" = lm(cons00a ~ lat_abst + logem4, data = df),
    "B5" = lm(democ00a ~ euro1900, data = subset(df, !is.na(logpgp95))),
    "B6" = lm(democ00a ~ lat_abst + euro1900, data = subset(df, !is.na(logpgp95))),
    "B7" = lm(democ00a ~ logem4, data = subset(df, !is.na(logpgp95))),
    "B8" = lm(democ00a ~ lat_abst + logem4, data = subset(df, !is.na(logpgp95))),
    "B9" = lm(euro1900 ~ logem4, data = subset(df, !is.na(logpgp95))),
    "B10" = lm(euro1900 ~ lat_abst + logem4, data = subset(df, !is.na(logpgp95)))
  )

  c(panel_a, panel_b)
}

format_tab_3 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    stars = TRUE,
    title = "Table 3. Determinants of Institutions"
  ))
}
