

getLastDateAvailable <- function(listNamedByDate, asOfDate) {
  assertthat::assert_that(is.list(listNamedByDate))
  
  ratingDates.v <- names(listNamedByDate)
  is.valid.v <- ratingDates.v <= format(asOfDate, '%Y-%m-%d')
  assertthat::assert_that(any(is.valid.v))
  date <- sort(ratingDates.v[is.valid.v], decreasing = TRUE)[1]
  
  return(listNamedByDate[[date]])
}
