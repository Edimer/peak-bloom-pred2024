scatterSpeciesNPN <- function(data, type, specie) {
  if (type == "Temperature") {
    data |>
      filter(Species == specie) |>
      select(bloom_doy, contains("Tm")) |>
      pivot_longer(cols = -c(bloom_doy))  |>
      filter(value > -100) |>
      ggplot(aes(x = value, y = bloom_doy)) +
      facet_wrap(~ name, scales = "free", ncol = 4) +
      geom_point()  +
      geom_smooth(
        method = "gam",
        formula = y ~ ns(x, df = 2),
        color = "red",
        size = 0.5
      ) +
      labs(x = "",
           y = "DOY",
           title = glue("{specie}")) +
      scale_y_log10()
  } else if (type == "Precipitation") {
    data |>
      filter(Species == specie) |>
      select(bloom_doy, contains("Prcp_")) |>
      pivot_longer(cols = -c(bloom_doy))  |>
      filter(value > -100) |>
      ggplot(aes(x = value, y = bloom_doy)) +
      facet_wrap( ~ name, scales = "free", ncol = 4) +
      geom_point()  +
      geom_smooth(
        method = "gam",
        formula = y ~ ns(x, df = 2),
        color = "red",
        size = 0.5
      ) +
      labs(x = "",
           y = "DOY",
           title = glue("{specie}")) +
      scale_y_log10()
  } else if (type == "Others") {
    data |>
      filter(Species == specie) |>
      select(bloom_doy, lat, long, alt, AGDD, Daylength) |>
      pivot_longer(cols = -c(bloom_doy))  |>
      filter(value > -100) |>
      ggplot(aes(x = value, y = bloom_doy)) +
      facet_wrap(~ name, scales = "free", ncol = 5) +
      geom_point()  +
      geom_smooth(
        method = "gam",
        formula = y ~ ns(x, df = 2),
        color = "red",
        size = 0.5
      ) +
      labs(x = "",
           y = "DOY",
           title = glue("{specie}")) +
      scale_y_log10()
  } else if (type == "Fixed") {
    data |>
      filter(Species == specie) |>
      select(bloom_doy, `bdod_0-5cm_mean`:water) |>
      pivot_longer(cols = -c(bloom_doy))  |>
      filter(value > -100) |>
      ggplot(aes(x = value, y = bloom_doy)) +
      facet_wrap( ~ name, scales = "free", ncol = 4) +
      geom_point()  +
      geom_smooth(
        method = "gam",
        formula = y ~ ns(x, df = 2),
        color = "red",
        size = 0.5
      ) +
      labs(x = "",
           y = "DOY",
           title = glue("{specie}")) +
      scale_y_log10()
  }
  
}