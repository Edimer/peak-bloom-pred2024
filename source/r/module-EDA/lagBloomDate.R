lagBloomDate <- function(data, n_lag, country_sel, pal_colors) {
  lag_operator <- n_lag
  
  if (is.null(country_sel)) {
    data_country <-
      data
    
    title_plot = "USA-NY"
  } else {
    data_country <-
      data |>
      filter(country == country_sel)
    
    title_plot = country_sel
  }
  
  coordinates <-
    data_country |>
    group_by(lat, long) |>
    reframe(n = length(unique(year))) |>
    filter(n >= lag_operator + 1) |>
    distinct(lat, long)
  
  number_coord <-
    coordinates |>
    nrow()
  
  data_diff <-
    map2(
      .x = coordinates$long,
      .y = coordinates$lat,
      .f = function(x = .x, y = .y) {
        res =
          data_country |>
          filter(long == x & lat == y) |>
          arrange(year) |>
          mutate(
            lag_year = lag(year, n = lag_operator),
            lag_bloom_date = lag(bloom_date, n = lag_operator)
          ) |>
          mutate(flag_year = year - lag_year) |>
          filter(flag_year == lag_operator) |>
          mutate(diff_bloom_date = as.numeric(bloom_date - lag_bloom_date)) |>
          select(-c(lag_year, lag_bloom_date, flag_year))
        return(res)
      }
    ) |>
    list_rbind()
  
  mean_diff <-
    data_diff$diff_bloom_date |>
    mean(na.rm = TRUE) |>
    round(digits = 0)
  
  median_diff <-
    data_diff$diff_bloom_date |>
    median(na.rm = TRUE) |>
    round(digits = 0)
  
  sd_diff <-
    data_diff$diff_bloom_date |>
    sd(na.rm = TRUE) |>
    round(digits = 0)
  
  min_diff <-
    data_diff$diff_bloom_date |>
    min(na.rm = TRUE) |>
    round(digits = 0)
  
  max_diff <-
    data_diff$diff_bloom_date |>
    max(na.rm = TRUE) |>
    round(digits = 0)
  
  data_summary <-
    data.frame(
      min_diff = min_diff,
      max_diff = max_diff,
      mean_diff = mean_diff,
      median_diff = median_diff,
      sd_diff = sd_diff
    )
  
  res_plot <-
    ggplot() +
    geom_rect(
      data = data_summary,
      mapping = aes(
        x = mean_diff,
        xmin = mean_diff - (sd_diff * 2),
        xmax = mean_diff + (sd_diff * 2),
        ymin = 0,
        ymax = Inf
      ),
      fill = pal_colors[3],
      alpha = 0.27
    ) +
    geom_rect(
      data = data_summary,
      mapping = aes(
        x = mean_diff,
        xmin = mean_diff - sd_diff,
        xmax = mean_diff + sd_diff,
        ymin = 0,
        ymax = Inf
      ),
      fill = pal_colors[2],
      alpha = 0.22
    )  +
    geom_histogram(
      data_diff,
      mapping = aes(x = diff_bloom_date),
      color = pal_colors[1],
      fill = pal_colors[1],
      alpha = 0.35
    ) +
    geom_vline(
      xintercept = mean_diff,
      lty = 2,
      color = pal_colors[6],
      size = 0.65
    ) +
    geom_label(
      aes(
        x = mean_diff,
        y = nrow(data_diff) / 7,
        label = glue("μ={mean_diff} ± {sd_diff}")
      ),
      fill = pal_colors[1],
      color = "white",
      alpha = 0.8
    ) +
    scale_x_log10() +
    labs(
      title = latex2exp::TeX(
        str_c(r'($Difference\ between\ \gamma_t\ and\ \gamma_{t-p}$)')
      ),
      y = "Count",
      x = "Days",
      subtitle = glue(
        "{title_plot}: {number_coord} coordinates with p = {lag_operator}"
      )
    )
  
  data_summary_pivot <-
    data_summary |>
    pivot_longer(cols = everything(),
                 names_to = "Metric",
                 values_to = "Value")
  
  return(list(
    df_diff = data_diff,
    df_summary = data_summary_pivot,
    plot_diff = res_plot
  ))
}
