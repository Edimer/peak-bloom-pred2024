customReactTable <- function(x) {
  x %>%
    reactable(
      searchable = TRUE,
      language = reactableLang(
        searchPlaceholder = "Search",
        noData = "No data found",
        pageInfo = "{rowStart} from {rowEnd} of {rows} rows",
        pagePrevious = "\u276e",
        pageNext = "\u276f",
        pagePreviousLabel = "Previous page",
        pageNextLabel = "Next page"
      ),
      defaultPageSize = 10,
      highlight = TRUE,
      resizable = TRUE,
      showPageSizeOptions = FALSE,
      pageSizeOptions = c(5, 10, 25, 50)
    )
}