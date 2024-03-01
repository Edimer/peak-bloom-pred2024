FEPhotopSummary <-
  function(months_window, data_bloom, data_photo) {
    time_window <- months_window # months
    
    df_coord_date <-
      data_bloom |>
      select(id_coord, lat, long, bloom_date)
    
    coord_id <- df_coord_date$lat
    coord_date_bloom <- df_coord_date$bloom_date
    
    res =
      future_map2(
        .x = coord_id,
        .y = coord_date_bloom,
        .f = function(x_id = .x,
                      y_date = .y) {
          date_min_photo <- y_date - (time_window * 30)
          
          df_photo_id <-
            data_photo |>
            filter(lat %in% x_id) |>
            filter(date_photoperiod < y_date) |>
            filter(date_photoperiod >= date_min_photo)
          
          df_summary <-
            df_photo_id |>
            group_by(lat) |>
            reframe(
              avg_photo_daily = mean(photoperiod, na.rm = TRUE),
              median_photo_daily = median(photoperiod, na.rm = TRUE),
              std_photo_daily = sd(photoperiod, na.rm = TRUE)
            )  |>
            janitor::clean_names() |>
            mutate(year_bloom = year(y_date)) |>
            relocate(lat, year_bloom, everything())
          
          return(df_summary)
          
        }
      ) |>
      list_rbind()
    return(res)
  }
