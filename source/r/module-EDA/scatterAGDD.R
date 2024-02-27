scatterAGDD <- function(data, specie, var_y, lab_title) {
  g1 <-
    data |>
    filter(Species == specie) |>
    filter(AGDD > -100) |>
    ggplot(aes(
      x = AGDD,
      y = !!sym(var_y),
      color = bloom_doy
    )) +
    geom_point(alpha = 0.8) +
    geom_smooth(
      method = "gam",
      formula = y ~ ns(x, df = 3),
      color = "firebrick",
      se = FALSE,
      size = 1.5
    ) +
    scale_y_log10() +
    scale_color_viridis_c() +
    labs(
      color = "DOY:",
      title = glue("AGDD vs {lab_title} - {specie}"),
      subtitle = glue("{var_y}: logarithmic scale\nAGDD: original scale")
    ) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(2, "cm"),
      legend.title = element_text(size = 8, vjust = 0.75)
    )
  
  g2 <-
    data |>
    filter(Species == specie) |>
    filter(AGDD > -100) |>
    ggplot(aes(
      x = AGDD,
      y = !!sym(var_y),
      color = bloom_doy
    )) +
    geom_point(alpha = 0.75) +
    geom_smooth(
      method = "gam",
      formula = y ~ ns(x, df = 3),
      color = "firebrick",
      se = FALSE,
      size = 1.2
    ) +
    scale_y_log10() +
    scale_x_log10() +
    scale_color_viridis_c() +
    labs(
      color = "DOY:",
      title = glue("AGDD vs {lab_title} - {specie}"),
      subtitle = glue("{var_y}: logarithmic scale\nAGDD: logarithmic scale")
    ) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(2, "cm"),
      legend.title = element_text(size = 8, vjust = 0.75)
    )
  
  return(list(res1 = g1, res2 = g2))
}