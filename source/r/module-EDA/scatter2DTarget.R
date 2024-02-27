scatter2DTarget <-
  function(data,
           var_x,
           var_y,
           country_sel,
           label_x,
           label_y) {
    data |>
      filter(country == country_sel) |>
      filter(!!sym(var_x) > 0) |> 
      filter(!!sym(var_y) > 0) |> 
      group_by(lat, long) |>
      mutate(median_doy = median(bloom_doy, na.rm = TRUE)) |>
      ungroup() |>
      distinct(lat, long, .keep_all = TRUE)  |>
      ggplot(aes(
        x = !!sym(var_x),
        y = !!sym(var_y),
        color = bloom_doy
      )) +
      geom_point() +
      scale_color_viridis_c(trans = "log10",
                            breaks = trans_breaks(
                              trans = "log10",
                              inv = function(x)
                                round(10 ^ x, digits = 1)
                            )) +
      labs(
        x = label_x,
        y = label_y,
        color = "DOY (median)",
        title = country_sel
      ) +
      theme(legend.key.size = unit(1, "cm"),
            legend.key.width = unit(2, "cm")) +
      geom_smooth(
        method = "gam",
        formula = y ~ ns(x, df = 2),
        color = "red",
        size = 0.5
      )
  }
