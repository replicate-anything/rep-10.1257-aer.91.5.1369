# Table 4 — IV Regressions of Log GDP per Capita
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_4 <- function(data) {
  df <- data[data$baseco == 1, , drop = FALSE]
  df$other_cont <- ifelse(
    df$shortnam %in% c("AUS", "MLT", "NZL"),
    1,
    0
  )

  iv <- AER::ivreg
  iv_models <- list(
    "IV1" = iv(logpgp95 ~ avexpr | logem4, data = df),
    "IV2" = iv(logpgp95 ~ lat_abst + avexpr | lat_abst + logem4, data = df),
    "IV3" = iv(logpgp95 ~ avexpr | logem4, data = subset(df, rich4 != 1)),
    "IV4" = iv(
      logpgp95 ~ lat_abst + avexpr | lat_abst + logem4,
      data = subset(df, rich4 != 1)
    ),
    "IV5" = iv(logpgp95 ~ avexpr | logem4, data = subset(df, africa != 1)),
    "IV6" = iv(
      logpgp95 ~ lat_abst + avexpr | lat_abst + logem4,
      data = subset(df, africa != 1)
    ),
    "IV7" = iv(
      logpgp95 ~ africa + asia + other_cont + avexpr | africa + asia + other_cont + logem4,
      data = df
    ),
    "IV8" = iv(
      logpgp95 ~ lat_abst + africa + asia + other_cont + avexpr |
        lat_abst + africa + asia + other_cont + logem4,
      data = df
    ),
    "IV9" = iv(loghjypl ~ avexpr | logem4, data = df)
  )

  ols_models <- list(
    "OLS1" = lm(logpgp95 ~ avexpr, data = df),
    "OLS2" = lm(logpgp95 ~ lat_abst + avexpr, data = df),
    "OLS3" = lm(logpgp95 ~ avexpr, data = subset(df, rich4 != 1)),
    "OLS4" = lm(logpgp95 ~ lat_abst + avexpr, data = subset(df, rich4 != 1)),
    "OLS5" = lm(logpgp95 ~ avexpr, data = subset(df, africa != 1)),
    "OLS6" = lm(logpgp95 ~ lat_abst + avexpr, data = subset(df, africa != 1)),
    "OLS7" = lm(logpgp95 ~ avexpr + africa + asia + other_cont, data = df),
    "OLS8" = lm(logpgp95 ~ lat_abst + avexpr + africa + asia + other_cont, data = df),
    "OLS9" = lm(loghjypl ~ avexpr, data = df)
  )

  c(iv_models, ols_models)
}

format_tab_4 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    stars = TRUE,
    title = "Table 4. IV Regressions of Log GDP per Capita"
  ))
}
