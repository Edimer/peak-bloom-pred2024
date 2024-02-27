interactPlot <- function(data,
                         var_x,
                         var_y,
                         country_sel,
                         label_x) {
  data |>
    filter(country == country_sel) |>
    group_by(lat, long) |>
    mutate(median_doy = median(bloom_doy, na.rm = TRUE)) |>
    ungroup() |>
    distinct(lat, long, .keep_all = TRUE)  |>
    mutate(var_inter = !!sym(var_x) * !!sym(var_y)) |>
    ggplot(aes(x = var_inter, y = bloom_doy)) +
    geom_point() +
    scale_x_log10() +
    geom_smooth(
      method = "gam",
      formula = y ~ ns(x, df = 2),
      color = "red",
      size = 0.5
    ) +
    labs(x = label_x,
         y = "DOY",
         title = country_sel)
}
