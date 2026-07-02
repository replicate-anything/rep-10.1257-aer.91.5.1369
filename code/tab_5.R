# Table 5 — IV Regressions with Additional Controls
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_5 <- function(data) {
  df <- data[data$baseco == 1, , drop = FALSE]
  iv <- AER::ivreg

  iv_models <- list(
    "IV1" = iv(logpgp95 ~ f_brit + f_french + avexpr | f_brit + f_french + logem4, data = df),
    "IV2" = iv(
      logpgp95 ~ lat_abst + f_brit + f_french + avexpr |
        lat_abst + f_brit + f_french + logem4,
      data = df
    ),
    "IV3" = iv(logpgp95 ~ avexpr | logem4, data = subset(df, f_brit == 1)),
    "IV4" = iv(
      logpgp95 ~ lat_abst + avexpr | lat_abst + logem4,
      data = subset(df, f_brit == 1)
    ),
    "IV5" = iv(logpgp95 ~ sjlofr + avexpr | sjlofr + logem4, data = df),
    "IV6" = iv(
      logpgp95 ~ lat_abst + sjlofr + avexpr | lat_abst + sjlofr + logem4,
      data = df
    ),
    "IV7" = iv(
      logpgp95 ~ catho80 + muslim80 + no_cpm80 + avexpr |
        catho80 + muslim80 + no_cpm80 + logem4,
      data = df
    ),
    "IV8" = iv(
      logpgp95 ~ lat_abst + catho80 + muslim80 + no_cpm80 + avexpr |
        lat_abst + catho80 + muslim80 + no_cpm80 + logem4,
      data = df
    ),
    "IV9" = iv(
      logpgp95 ~ f_french + sjlofr + catho80 + muslim80 + no_cpm80 + avexpr |
        f_french + sjlofr + catho80 + muslim80 + no_cpm80 + logem4,
      data = df
    )
  )

  ols_models <- list(
    "OLS1" = lm(logpgp95 ~ f_brit + f_french + avexpr, data = df),
    "OLS2" = lm(logpgp95 ~ lat_abst + f_brit + f_french + avexpr, data = df),
    "OLS3" = lm(logpgp95 ~ avexpr, data = subset(df, f_brit == 1)),
    "OLS4" = lm(logpgp95 ~ lat_abst + avexpr, data = subset(df, f_brit == 1)),
    "OLS5" = lm(logpgp95 ~ sjlofr + avexpr, data = df),
    "OLS6" = lm(logpgp95 ~ lat_abst + sjlofr + avexpr, data = df),
    "OLS7" = lm(logpgp95 ~ catho80 + muslim80 + no_cpm80 + avexpr, data = df),
    "OLS8" = lm(logpgp95 ~ lat_abst + catho80 + muslim80 + no_cpm80 + avexpr, data = df),
    "OLS9" = lm(
      logpgp95 ~ lat_abst + f_french + sjlofr + catho80 + muslim80 + no_cpm80 + avexpr,
      data = df
    )
  )

  c(iv_models, ols_models)
}

format_tab_5 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    stars = TRUE,
    title = "Table 5. IV Regressions of Log GDP per Capita with Additional Controls"
  ))
}
