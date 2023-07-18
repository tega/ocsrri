
mapToRatingClass <- function(agency, rating, asOfDateRating.l) {
  info.l <- asOfDateRating.l[[agency]]
  assertthat::assert_that(is.element(rating, info.l[["rating.v"]]))
  return(names(info.l[["ratClass.l"]]) [sapply(info.l[["ratClass.l"]], is.element, el = rating)])
}
