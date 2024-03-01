FEWeatherSummary <-
  function(months_window, data_bloom, data_weather) {
    time_window <- months_window # months
    
    df_coord_date <-
      data_bloom |>
      select(id_coord, lat, long, bloom_date)
    
    coord_id <- df_coord_date$id_coord
    coord_date_bloom <- df_coord_date$bloom_date
    
    res =
      future_map2(
        .x = coord_id,
        .y = coord_date_bloom,
        .f = function(x_id = .x,
                      y_date = .y) {
          date_min_weather <- y_date - (time_window * 30)
          
          df_weather_id <-
            data_weather |>
            filter(id_coord %in% x_id) |>
            filter(YYYYMMDD < y_date) |>
            filter(YYYYMMDD >= date_min_weather)
          
          df_summary1 <-
            df_weather_id  |>
            select(-c(YEAR, MM, DD, DOY, YYYYMMDD, FROST_DAYS, id_coord)) |>
            pivot_longer(cols = -c(lat, long)) |>
            group_by(lat, long, name) |>
            reframe(
              avg_daily = mean(value, na.rm = TRUE),
              median_daily = median(value, na.rm = TRUE),
              std_daily = sd(value, na.rm = TRUE)
            ) |>
            pivot_wider(
              names_from = name,
              values_from = c(avg_daily, median_daily, std_daily)
            )
          
          df_summary2 <-
            df_weather_id |>
            group_by(lat, long) |>
            reframe(
              total_FROST_DAYS1 = sum(FROST_DAYS == 1),
              acumm_ts = sum(TS, na.rm = TRUE),
              acumm_ts_min = sum(TS_MIN, na.rm = TRUE),
              acumm_ts_max = sum(TS_MAX, na.rm = TRUE),
              acumm_precip = sum(PRECTOTCORR, na.rm = TRUE),
              acumm_evap = sum(EVLAND, na.rm = TRUE)
            ) |>
            mutate(prop_frost_days1 = total_FROST_DAYS1 / (30 * time_window))
          
          df_summary <-
            left_join(df_summary1, df_summary2, by = c("lat", "long")) |>
            janitor::clean_names() |>
            mutate(year_bloom = year(y_date)) |>
            relocate(lat, long, year_bloom, everything()) |>
            mutate(
              rate_acumm_precip_evap = acumm_evap / acumm_precip,
              rate_acumm_ts_precip = acumm_ts / acumm_precip
            )
          
          return(df_summary)
          
        }
      ) |>
      list_rbind()
    return(res)
  }
