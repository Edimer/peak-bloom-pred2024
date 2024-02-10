# Esta función permite extraer datos metereológicos desde el proyecto POWER-NASA
# para una o más coordenadas de interés.
getWeatherPOWER <-
  function(var_climate,
           start_date,
           end_date,
           frequency_climate,
           long,
           lat) {
    df_climate =
      map2(
        .x  = long,
        .y = lat,
        .f = function(long = .x, lat = .y) {
          res = get_power(
            community = "ag",
            lonlat = c(long, lat),
            pars = var_climate,
            dates = c(start_date, end_date),
            temporal_api = frequency_climate
          )
          return(res)
        }
      ) |>
      list_rbind()
    return(df_climate)
  }