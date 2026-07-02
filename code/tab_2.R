# Table 2 — OLS Regressions
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_2 <- function(data) {
  df <- data
  list(
    "(1)" = lm(logpgp95 ~ avexpr, data = df),
    "(2)" = lm(logpgp95 ~ avexpr, data = subset(df, baseco == 1)),
    "(3)" = lm(logpgp95 ~ avexpr + lat_abst, data = df),
    "(4)" = lm(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = df),
    "(5)" = lm(logpgp95 ~ avexpr + lat_abst, data = subset(df, baseco == 1)),
    "(6)" = lm(
      logpgp95 ~ avexpr + lat_abst + africa + asia + other,
      data = subset(df, baseco == 1)
    ),
    "(7)" = lm(loghjypl ~ avexpr, data = df),
    "(8)" = lm(loghjypl ~ avexpr, data = subset(df, baseco == 1))
  )
}

format_tab_2 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    vcov = "HC1",
    stars = TRUE,
    title = "Table 2. OLS Regressions"
  ))
}
