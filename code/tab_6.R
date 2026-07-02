# Table 6 — Robustness Checks for IV Regressions
# Study repo: rep-10.1257-aer.91.5.1369

make_tab_6 <- function(data) {
  df <- data[data$baseco == 1, , drop = FALSE]
  temp_vars <- grep("^temp", names(df), value = TRUE)
  humid_vars <- grep("^humid", names(df), value = TRUE)
  geo_vars <- c(
    "steplow", "deslow", "stepmid", "desmid", "drystep", "drywint",
    "goldm", "iron", "silv", "zinc", "oilres", "landlock"
  )
  all_controls <- c(temp_vars, humid_vars, "edes1975", "avelf", geo_vars)
  all_controls <- all_controls[all_controls %in% names(df)]

  iv <- AER::ivreg
  fml <- function(controls, lat = FALSE) {
    controls <- controls[controls %in% names(df)]
    rhs <- paste(controls, collapse = " + ")
    if (lat) {
      as.formula(paste("logpgp95 ~ lat_abst +", rhs, "+ avexpr | lat_abst +", rhs, "+ logem4"))
    } else {
      as.formula(paste("logpgp95 ~", rhs, "+ avexpr |", rhs, "+ logem4"))
    }
  }

  iv_models <- list(
    "IV1" = iv(fml(c(temp_vars, humid_vars)), data = df),
    "IV2" = iv(fml(c(temp_vars, humid_vars), lat = TRUE), data = df),
    "IV3" = iv(fml("edes1975"), data = df),
    "IV4" = iv(fml("edes1975", lat = TRUE), data = df),
    "IV5" = iv(fml(geo_vars), data = df),
    "IV6" = iv(fml(geo_vars, lat = TRUE), data = df),
    "IV7" = iv(fml("avelf"), data = df),
    "IV8" = iv(fml("avelf", lat = TRUE), data = df),
    "IV9" = iv(fml(all_controls, lat = TRUE), data = df)
  )

  ols_models <- list(
    "OLS1" = lm(as.formula(paste("logpgp95 ~", paste(c(temp_vars, humid_vars), collapse = " + "), "+ avexpr")), data = df),
    "OLS2" = lm(as.formula(paste("logpgp95 ~ lat_abst +", paste(c(temp_vars, humid_vars), collapse = " + "), "+ avexpr")), data = df),
    "OLS3" = lm(logpgp95 ~ edes1975 + avexpr, data = df),
    "OLS4" = lm(logpgp95 ~ lat_abst + edes1975 + avexpr, data = df),
    "OLS5" = lm(as.formula(paste("logpgp95 ~", paste(geo_vars, collapse = " + "), "+ avexpr")), data = df),
    "OLS6" = lm(as.formula(paste("logpgp95 ~ lat_abst +", paste(geo_vars, collapse = " + "), "+ avexpr")), data = df),
    "OLS7" = lm(logpgp95 ~ avelf + avexpr, data = df),
    "OLS8" = lm(logpgp95 ~ lat_abst + avelf + avexpr, data = df),
    "OLS9" = lm(as.formula(paste("logpgp95 ~ lat_abst +", paste(all_controls, collapse = " + "), "+ avexpr")), data = df)
  )

  c(iv_models, ols_models)
}

format_tab_6 <- function(object) {
  as.character(modelsummary::modelsummary(
    object,
    output = "html",
    stars = TRUE,
    title = "Table 6. Robustness Checks for IV Regressions with Log GDP per Capita"
  ))
}
