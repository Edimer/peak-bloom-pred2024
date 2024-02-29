# Adapted from: Pablo Cánovas
# URL: https://typethepipe.com/vizs-and-tips/how-to-create-multiple-lags-in-r/

calculateLags <- function(data, var, lags) {
  map_lag <- lags |>  map( ~ partial(lag, n = .x))
  res =
    data |>
    mutate(across(
      .cols = {
        {
          var
        }
      },
      .fns = map_lag,
      .names = "{.col}_lag{lags}"
    ))
  return(res)
}