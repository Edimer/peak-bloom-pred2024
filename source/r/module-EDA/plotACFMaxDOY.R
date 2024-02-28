plotACFMaxDOY <- function(data, country_sel) {
  data_acf <-
    data |>
    filter(country == country_sel) |>
    group_by(year) |>
    reframe(max_bloom_doy = max(bloom_doy, na.rm = TRUE))
  
  g1 <-
    ggAcf(
      data_acf |> select(max_bloom_doy),
      lag.max = nrow(data_acf) - 1,
      type = "correlation"
    ) +
    labs(title = "Autocorrelation function (ACF)",
         subtitle = str_c(country_sel, " (all data)"))
  
  g2 <-
    ggAcf(
      data_acf |> select(max_bloom_doy),
      lag.max = nrow(data_acf) - 1,
      type = "partial"
    ) +
    labs(title = "Partial autocorrelation function (PACF)",
         subtitle = str_c(country_sel, " (all data)"))
  
  g3 <-
    ggAcf(data_acf |> select(max_bloom_doy),
          lag.max = 50,
          type = "partial") +
    labs(title = "Partial autocorrelation function (PACF)",
         subtitle = str_c(country_sel, " (50 years)"))
  
  return(list(
    plot1 = g1,
    plot2 = g2,
    plot3 = g3
  ))
}