timeSerieDOY <- function(data, country_sel, pal_colors) {
  if (is.null(country_sel)) {
    data_ts <-
      data |>
      mutate(id_geo = str_c("long:", round(long, 4), "- lat:", round(lat, 4))) |>
      filter(year > 1950)
  } else {
    data_ts <-
      data |>
      filter(country == country_sel) |>
      mutate(id_geo = str_c("long:", round(long, 4), "- lat:", round(lat, 4))) |>
      filter(year > 1950)
  }
  
  data_ts_summary <-
    data_ts |>
    group_by(year) |>
    reframe(
      mean_doy = mean(bloom_doy, na.rm = TRUE),
      median_doy = median(bloom_doy, na.rm = TRUE),
      sd_doy = sd(bloom_doy, na.rm = TRUE),
      p5_doy = quantile(bloom_doy, na.rm = TRUE, probs = 0.05),
      p95_doy = quantile(bloom_doy, na.rm = TRUE, probs = 0.95)
    )
  
  g1 <-
    data_ts |>
    ggplot() +
    geom_line(aes(x = year, y = bloom_doy, group = id_geo),
              color = pal_colors[1],
              alpha = 0.25) +
    labs(
      x = "Year",
      y = "DOY",
      title = "DOY time series since 1950",
      subtitle = country_sel
    )
  
  g2 <-
    data_ts |>
    ggplot() +
    geom_ribbon(
      data = data_ts_summary,
      mapping = aes(x = year,
                    ymin = p5_doy,
                    ymax = p95_doy),
      fill = pal_colors[3],
      alpha = 0.27
    ) +
    geom_ribbon(
      data = data_ts_summary,
      mapping = aes(
        x = year,
        ymin = mean_doy - sd_doy,
        ymax = mean_doy + sd_doy
      ),
      fill = pal_colors[2],
      alpha = 0.22
    ) +
    geom_line(aes(x = year, y = bloom_doy, group = id_geo),
              color = pal_colors[1],
              alpha = 0.15) +
    geom_smooth(
      data = data_ts,
      mapping = aes(x = year, y = bloom_doy),
      method = "gam",
      formula = y ~ ns(x, df = 5),
      color = "firebrick",
      se = TRUE,
      size = 0.7
    ) +
    labs(
      x = "Year",
      y = "DOY",
      title = "DOY time series since 1950",
      subtitle = country_sel,
      caption = "Red line: GAM model with df = 5"
    )
  
  g3 <-
    data_ts |>
    ggplot() +
    geom_ribbon(
      data = data_ts_summary,
      mapping = aes(x = year,
                    ymin = p5_doy,
                    ymax = p95_doy),
      fill = pal_colors[3],
      alpha = 0.27
    ) +
    geom_ribbon(
      data = data_ts_summary,
      mapping = aes(
        x = year,
        ymin = mean_doy - sd_doy,
        ymax = mean_doy + sd_doy
      ),
      fill = pal_colors[2],
      alpha = 0.22
    ) +
    geom_line(
      data = data_ts_summary,
      mapping = aes(x = year,
                    y = mean_doy),
      color = pal_colors[6],
      size = 0.65
    ) +
    geom_line(
      data = data_ts_summary,
      mapping = aes(x = year,
                    y = median_doy),
      color = pal_colors[5],
      size = 0.65
    ) +
    labs(
      x = "Year",
      y = "DOY",
      title = "DOY time series since 1950",
      subtitle = country_sel,
      caption = "Orange line: median\nBlue line: mean (µ)\nYellow band: µ ± 1SD\nRed band: p5 to p95"
    )
  
  return(list(
    plot1 = g1,
    plot2 = g2,
    plot3 = g3
  ))
}
