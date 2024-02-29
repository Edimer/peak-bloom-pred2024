corrMonthlyClimate <- function(data,
                               list_coords,
                               n_lag,
                               loc_name,
                               pal_colors) {
  data_lags <-
    list_coords |>
    map(
      .f = function(x = .x) {
        res =
          data |>
          filter(lat == x) |>
          arrange(year) |>
          calculateLags(c(MEAN_1_EVLAND:TOTAL_FROST_DAYS_M12), lags = n_lag) |>
          ungroup() |>
          select(
            lat,
            long,
            year,
            bloom_date_max,
            bloom_doy,
            location_name,
            altitude,
            contains("lag")
          )
      }
    ) |>
    list_rbind()
  
  if (is.null(loc_name)) {
    data_correlation <-
      data_lags |>
      select(
        bloom_doy,
        everything(),-c(lat, long, altitude, year, bloom_date_max, location_name)
      ) |>
      corrr::correlate(method = "spearman") |>
      filter(term == "bloom_doy") |>
      select(-bloom_doy) |>
      pivot_longer(cols = -term, values_to = "correlation") |>
      arrange(desc(abs(correlation))) |>
      mutate(name = str_replace_all(name, "_lag[0-9]", ""),
             name = str_to_sentence(name))
    
    title_plot <- glue("All coordinates con p={n_lag}")
    
    
  } else {
    data_correlation <-
      data_lags |>
      filter(location_name == loc_name) |>
      select(
        bloom_doy,
        everything(),-c(lat, long, altitude, year, bloom_date_max, location_name)
      ) |>
      corrr::correlate(method = "spearman") |>
      filter(term == "bloom_doy") |>
      select(-bloom_doy) |>
      pivot_longer(cols = -term, values_to = "correlation") |>
      arrange(desc(abs(correlation))) |>
      mutate(name = str_replace_all(name, "_lag[0-9]", ""),
             name = str_to_sentence(name))
    
    title_plot <- glue("{loc_name} con p={n_lag}")
  }
  
  g1 <-
    data_correlation |>
    slice(1:50) |>
    mutate(sign = if_else(correlation > 0, true = "Pos (+)", false = "Neg (-)")) |>
    ggplot(aes(
      x = abs(correlation),
      y = reorder(name, abs(correlation)),
      color = sign,
      fill = sign
    )) +
    geom_col(alpha = 0.35) +
    scale_color_manual(values = c(pal_colors[2], pal_colors[1])) +
    scale_fill_manual(values = c(pal_colors[2], pal_colors[1])) +
    labs(
      x = "Correlation",
      y = "",
      color = "",
      fill = "",
      title = title_plot
    )
  
  return(list(df_lags = data_lags,
              plot1 = g1))
}
