
#' @import dplyr
#' @export
report_glmer_effect <- function(mod, param, method = "profile") {
  parameter_stats <- mod %>%
    broom::tidy(effects = "fixed") %>%
    dplyr::filter(term == param)
  estimate <- parameter_stats$estimate
  z_value <- parameter_stats$statistic
  p_value <- parameter_stats$p.value

  interval <- confint(mod, param, method = method)
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f log-odds, 95%% CI [%.2f, %.2f], _z_ = %.4f, _p_ = %.4f",
          estimate,
          lwr,
          upr,
          z_value,
          p_value)
}

#' @import dplyr
#' @export
report_lmerTest_effect <- function(mod, param, method = "profile") {
  parameter_stats <- summary(mod)$coefficients[param, ] %>% as.data.frame
  estimate <- parameter_stats["Estimate", ]
  p_value <- parameter_stats["Pr(>|t|)", ]

  interval <- confint(mod, param, method = method)
  lwr <- interval[1]
  upr <- interval[2]

  sprintf("%.2f ms., 95%% CI [%.2f, %.2f], p = %.4f",
          estimate,
          lwr,
          upr,
          p_value)
}
