lagsDOY <- function(data, country_sel, pal_colors) {
  if (is.null(country_sel)) {
    data_country <-
      data
    
    title_plot = "USA-NY"
  } else {
    data_country <-
      data |>
      filter(country == country_sel)
    
    title_plot = country_sel
  }
  
  coordinates <-
    data_country |>
    group_by(lat, long) |>
    reframe(n = length(unique(year))) |>
    filter(n >= 10) |>
    distinct(lat, long)
  
  number_coord <-
    coordinates |>
    nrow()
  
  data_lags <-
    map2(
      .x = coordinates$long,
      .y = coordinates$lat,
      .f = function(x = .x, y = .y) {
        res =
          data_country |>
          filter(long == x & lat == y) |>
          arrange(year) |>
          mutate(
            lag_year = lag(year, n = 1),
            lag1_doy = lag(bloom_doy, n = 1),
            lag2_doy = lag(bloom_doy, n = 2),
            lag3_doy = lag(bloom_doy, n = 3),
            lag4_doy = lag(bloom_doy, n = 4),
            lag5_doy = lag(bloom_doy, n = 5),
            lag6_doy = lag(bloom_doy, n = 6),
            lag7_doy = lag(bloom_doy, n = 7),
            lag8_doy = lag(bloom_doy, n = 8),
            lag9_doy = lag(bloom_doy, n = 9),
            lag10_doy = lag(bloom_doy, n = 10)
          ) |>
          mutate(flag_year = year - lag_year)   |>
          filter(flag_year == 1) |>
          select(year, bloom_doy, contains("lag"),-c(lag_year, flag_year)) |>
          pivot_longer(cols = -c(year, bloom_doy)) |>
          mutate(
            name = str_replace_all(name, "_doy", ""),
            name = str_to_sentence(name),
            name = factor(name,
                          levels = str_c("Lag", 1:10))
          ) |>
          filter(!is.na(value))
      }
    ) |>
    list_rbind()
  
  data_lags <-
    data_lags |>
    left_join(data_lags |>
                group_by(name) |>
                reframe(
                  correlation = cor(value,
                                    bloom_doy,
                                    method = "spearman",
                                    use = "pairwise.complete.obs"),
                  correlation = round(correlation, digits = 2)
                ),
              by = "name") |>
    mutate(name = str_c(name, "\nρ=", correlation)) |>
    select(-c(correlation))
  
  g1 <-
    data_lags |>
    ggplot(aes(x = value, y = bloom_doy)) +
    facet_wrap( ~ name, scales = "free", ncol = 10) +
    geom_density_2d(color = pal_colors[1], size = 0.1) +
    scale_x_log10() +
    scale_y_log10()  +
    labs(
      x = TeX(r'($\gamma_{t-i}$)'),
      y = TeX(r'($\gamma_t$)'),
      subtitle = country_sel
    )
  
  return(list(df_lags = data_lags,
              plot_lags = g1))
}
