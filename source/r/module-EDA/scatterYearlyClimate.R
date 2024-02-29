scatterYearlyClimate <-
  function(data, list_coords, n_lag, loc_name, pal_colors) {
    
    data_lags <-
      list_coords |>
      map(
        .f = function(x = .x) {
          res =
            data |>
            filter(lat == x) |>
            arrange(year) |>
            calculateLags(c(MEAN_EVLAND:TOTAL_FROST_DAYS), lags = n_lag) |>
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
            ) |>
            pivot_longer(cols = -c(
              lat,
              long,
              altitude,
              year,
              bloom_date_max,
              bloom_doy,
              location_name
            ))
        }
      ) |>
      list_rbind()
    
    if (is.null(loc_name)) {
      g1 <-
        data |>
        filter(location_name != "New York City, NY (USA)") |>
        pivot_longer(cols = -c(
          lat,
          long,
          altitude,
          year,
          bloom_date_max,
          bloom_doy,
          location_name
        )) |>
        ggplot(aes(x = value, y = bloom_doy)) +
        facet_wrap(~ name, scales = "free", ncol = 4) +
        geom_point(color = pal_colors[1],
                   size = 0.9,
                   alpha = 0.75) +
        geom_smooth(
          method = "gam",
          formula = y ~ ns(x, df = 3),
          color = pal_colors[2],
          se = TRUE,
          size = 0.7
        ) +
        scale_y_log10() +
        labs(
          x = latex2exp::TeX(r'($X_t$)'),
          y = latex2exp::TeX(r'($DOY\ (t)$)'),
          title = "DOY vs climate variables",
          subtitle = latex2exp::TeX(str_c("Evaluated in ", r'($t$)'))
        )
      
      g2 <-
        data_lags |>
        filter(location_name != "New York City, NY (USA)") |>
        ggplot(aes(x = value, y = bloom_doy)) +
        facet_wrap(~ name, scales = "free", ncol = 4) +
        geom_point(color = pal_colors[1],
                   size = 0.9,
                   alpha = 0.75) +
        geom_smooth(
          method = "gam",
          formula = y ~ ns(x, df = 3),
          color = pal_colors[2],
          se = TRUE,
          size = 0.7
        ) +
        scale_y_log10() +
        labs(
          x = latex2exp::TeX(r'($X_{t-p}$)'),
          y = latex2exp::TeX(r'($DOY\ (t)$)'),
          title = glue("DOY vs lagged (p={n_lag}) climate variables"),
          subtitle = latex2exp::TeX(str_c("Evaluated in ", r'($t-p$)'))
        )
      return(list(plot1 = g1,
                  plot2 = g2))
    } else {
      data_lags |>
        filter(location_name == loc_name) |>
        ggplot(aes(x = value, y = bloom_doy)) +
        facet_wrap(~ name, scales = "free", ncol = 4) +
        geom_point(color = pal_colors[1],
                   size = 0.9,
                   alpha = 0.75) +
        geom_smooth(
          method = "gam",
          formula = y ~ ns(x, df = 3),
          color = pal_colors[2],
          se = TRUE,
          size = 0.7
        ) +
        scale_y_log10() +
        labs(
          x = latex2exp::TeX(r'($X_{t-p}$)'),
          y = latex2exp::TeX(r'($DOY\ (t)$)'),
          title = glue("DOY vs lagged (p={n_lag}) climate variables - {loc_name}"),
          subtitle = latex2exp::TeX(str_c("Evaluated in ", r'($t-p$)'))
        )
    }
    
  }
