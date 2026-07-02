# Table 7 — Geography and Health Variables
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_7 <- function(data) {
  df <- data[data$baseco == 1, , drop = FALSE]
  df$other_cont <- ifelse(
    df$shortnam %in% c("AUS", "MLT", "NZL"),
    1,
    0
  )
  iv <- AER::ivreg

  iv_models <- list(
    "IV1" = iv(logpgp95 ~ malfal94 + avexpr | malfal94 + logem4, data = df),
    "IV2" = iv(
      logpgp95 ~ lat_abst + malfal94 + avexpr | lat_abst + malfal94 + logem4,
      data = df
    ),
    "IV3" = iv(logpgp95 ~ leb95 + avexpr | leb95 + logem4, data = df),
    "IV4" = iv(
      logpgp95 ~ lat_abst + leb95 + avexpr | lat_abst + leb95 + logem4,
      data = df
    ),
    "IV5" = iv(logpgp95 ~ imr95 + avexpr | imr95 + logem4, data = df),
    "IV6" = iv(
      logpgp95 ~ lat_abst + imr95 + avexpr | lat_abst + imr95 + logem4,
      data = df
    ),
    "IV7" = iv(
      logpgp95 ~ malfal94 + avexpr | logem4 + latabs + lt100km + meantemp,
      data = df
    ),
    "IV8" = iv(
      logpgp95 ~ leb95 + avexpr | logem4 + latabs + lt100km + meantemp,
      data = df
    ),
    "IV9" = iv(
      logpgp95 ~ imr95 + avexpr | logem4 + latabs + lt100km + meantemp,
      data = df
    ),
    "IV10" = iv(logpgp95 ~ avexpr | yellow, data = df),
    "IV11" = iv(
      logpgp95 ~ africa + asia + other_cont + avexpr | africa + asia + other_cont + yellow,
      data = df
    )
  )

  complete <- with(df, !is.na(logem4) & !is.na(latabs) & !is.na(lt100km) & !is.na(meantemp))
  ols_models <- list(
    "OLS1" = lm(logpgp95 ~ malfal94 + avexpr, data = df),
    "OLS2" = lm(logpgp95 ~ lat_abst + malfal94 + avexpr, data = df),
    "OLS3" = lm(logpgp95 ~ leb95 + avexpr, data = df),
    "OLS4" = lm(logpgp95 ~ lat_abst + leb95 + avexpr, data = df),
    "OLS5" = lm(logpgp95 ~ imr95 + avexpr, data = df),
    "OLS6" = lm(logpgp95 ~ lat_abst + imr95 + avexpr, data = df),
    "OLS7" = lm(logpgp95 ~ malfal94 + avexpr, data = df[complete, , drop = FALSE]),
    "OLS8" = lm(logpgp95 ~ leb95 + avexpr, data = df[complete, , drop = FALSE]),
    "OLS9" = lm(logpgp95 ~ imr95 + avexpr, data = df[complete, , drop = FALSE]),
    "OLS10" = lm(logpgp95 ~ avexpr, data = subset(df, !is.na(yellow))),
    "OLS11" = lm(
      logpgp95 ~ africa + asia + other_cont + avexpr,
      data = subset(df, !is.na(yellow))
    )
  )

  c(iv_models, ols_models)
}

format_tab_7 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    stars = TRUE,
    title = "Table 7. Geography and Health Variables"
  ))
}
