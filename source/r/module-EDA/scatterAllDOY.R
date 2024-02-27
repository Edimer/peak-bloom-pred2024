scatterAllDOY <- function(data, country_sel) {
  if (is.null(country_sel)) {
    data_graphic = data |>
      filter(location != "NPN")
    
    label_title = "All the countries"
    
  } else {
    data_graphic = data |>
      filter(country == country_sel)
    
    label_title = country_sel
  }
  
  data_graphic |>
    filter(bio10 <= 50) |>
    group_by(lat, long) |>
    mutate(bloom_doy = median(bloom_doy, na.rm = TRUE)) |>
    ungroup() |>
    select(-c(location, country,  year, bloom_date)) |>
    distinct(lat, long, .keep_all = TRUE) |>
    pivot_longer(cols = -bloom_doy) |>
    ggplot(aes(x = value, y = bloom_doy)) +
    facet_wrap( ~ name, scales = "free", ncol = 4) +
    geom_point() +
    geom_smooth(
      method = "gam",
      formula = y ~ ns(x, df = 2),
      color = "firebrick3",
      se = FALSE
    ) +
    scale_x_log10() +
    labs(y = "DOY", x = "", title = label_title)
}
