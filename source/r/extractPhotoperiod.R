extractPhotoperiod <- function(data) {
  min_date = data$bloom_date |>
    min(na.rm = TRUE) - 365.5
  
  max_date = data$bloom_date |>
    max(na.rm = TRUE)
  
  date_range =
    seq(from = min_date,
        to = max_date,
        by = 1)
  
  result =
    data |>
    pull(lat) |>
    unique() |>
    map(
      .f = function(x = .x) {
        res =
          data.frame(photoperiod = photoperiod(date_range, latitude = x),
                     lat = x)
        return(res)
      }
    ) |>
    list_rbind()
  
  return(result)
}