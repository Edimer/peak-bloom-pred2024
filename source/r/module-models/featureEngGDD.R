calculateGDD1 <- function(t_avg, t_basal) {
  res = if_else(
    condition = t_avg <  t_basal,
    true = 0,
    false = t_avg - t_basal
  )
  return(res)
}

calculateGDD2 <- function(t_avg, t_min, t_max, t_opt) {
  res_rule1 <- (t_avg - t_min) / (t_opt - t_min)
  res_rule2 <- (t_avg - t_max) / (t_opt - t_max)
  
  res = if_else(
    condition = t_avg <  t_min,
    true = 0,
    false = if_else(
      condition = t_avg >= t_min & t_avg <= t_opt,
      true = res_rule1,
      false = if_else(
        condition = t_avg >= t_opt & t_avg < t_max,
        true = res_rule2,
        false = 0
      )
    )
  )
  return(res)
}


calculateGDD3 <- function(t_max, t_min, t_basal) {
  t_avg <- (t_max + t_min) / 2
  res = if_else(
    condition = t_avg <  t_basal,
    true = 0,
    false = t_avg - t_basal
  )
  return(res)
}


featureEngGDD <-
  function(start_date,
           t_basal,
           data_bloom,
           data_weather) {
    start_date <- start_date
    basal_temp <- t_basal
    
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
          date_min_weather <-
            str_c(year(y_date), "-", start_date) |>
            ymd()
          
          df_weather_id <-
            data_weather |>
            filter(id_coord %in% x_id) |>
            filter(YYYYMMDD < y_date) |>
            filter(YYYYMMDD >= date_min_weather)
          
          df_summary <-
            df_weather_id |>
            mutate(
              gdd1 = calculateGDD1(t_avg = TS,
                                   t_basal = basal_temp),
              gdd2 = calculateGDD2(
                t_avg = TS,
                t_min = TS_MIN,
                t_max = TS_MAX,
                t_opt = basal_temp
              ),
              gdd3 = calculateGDD3(
                t_min = TS_MIN,
                t_max = TS_MAX,
                t_basal = basal_temp
              )
            ) |>
            group_by(lat, long) |>
            reframe(
              agdd1 = sum(gdd1, na.rm = TRUE),
              agdd2 = sum(gdd2, na.rm = TRUE),
              agdd3 = sum(gdd3, na.rm = TRUE)
            ) |>
            janitor::clean_names() |>
            mutate(year_bloom = year(y_date)) |>
            relocate(lat, long, year_bloom, everything())
          
          return(df_summary)
          
        }
      ) |>
      list_rbind()
    return(res)
  }
