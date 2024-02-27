profileCorr <- function(data, country_sel) {
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
    select(where(is.numeric), -c(shrubs)) |>
    correlate(method = "spearman") |>
    filter(term == "bloom_doy") |>
    pivot_longer(cols = -term) |>
    filter(!is.na(value)) |>
    mutate(
      name = str_replace_all(
        name,
        "wildareas-v3-1993-human-footprint",
        "human-footprint93"
      ),
      name = str_replace_all(
        name,
        "wildareas-v3-2009-human-footprint",
        "human-footprint09"
      )
    ) |>
    ggplot(aes(
      x = reorder(name, value),
      y = term,
      fill = value
    )) +
    geom_tile() +
    scale_fill_gradient2(
      low = muted("red"),
      mid = "white",
      high = muted("dodgerblue2"),
      midpoint = 0
    ) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          legend.key.size = unit(0.5, "cm"),
          legend.key.width = unit(1, "cm")) +
    labs(
      x = "",
      y = "",
      fill = "",
      title = label_title
    ) +
    coord_flip()
}
