distSpecieStateNPN <- function(data, specie) {
  data_plot_distribution <-
    data |>
    filter(Species == specie) |>
    mutate(State = str_c(State, " - ", city)) |>
    group_by(State) |>
    mutate(flag_n = n()) |>
    ungroup() |>
    filter(flag_n >= 5)
  
  median_doy <-
    median(data_plot_distribution$bloom_doy, na.rm = TRUE)
  
  mean_doy <-
    mean(data_plot_distribution$bloom_doy, na.rm = TRUE)
  
  sd_doy <-
    sd(data_plot_distribution$bloom_doy, na.rm = TRUE)
  
  data_plot_distribution |>
    ggplot(aes(x = State, y = bloom_doy)) +
    geom_rect(
      aes(
        xmin = 0,
        xmax = length(unique(data_plot_distribution$State)) + 1,
        ymin = mean_doy - sd_doy,
        ymax = mean_doy + sd_doy
      ),
      fill = "forestgreen",
      alpha = 0.002
    ) +
    geom_boxplot(color = "gray30", size = 0.25) +
    geom_hline(
      yintercept = median_doy,
      linetype = 2,
      color = "orangered",
      size = 0.35
    ) +
    geom_hline(
      yintercept = mean_doy,
      linetype = 2,
      color = "dodgerblue3",
      size = 0.35
    ) +
    labs(
      y = "Bloom DOY",
      x = "",
      title = "DOY distribution by state",
      subtitle = glue("Logarithmic scale - {specie}"),
      caption = "Red line: median\nBlue line: mean (µ)\nGreen band: µ ± 1SD"
    ) +
    coord_flip() +
    scale_y_log10()
}
