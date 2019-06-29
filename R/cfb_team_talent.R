#' Get composite team talent rankings for all teams in a given year or
#' all rankings from 2015 - 2018
#'
#' Extracts team talent rankings as sourced from 247 rankings
#' @param year Year
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#'
#' cfb_team_talent()
#'
#' cfb_team_talent(year=2018)

cfb_team_talent <- function(year = NULL) {
  if (is.null(year)) {
    url <- "https://api.collegefootballdata.com/talent"
  } else{
    assert_that(is.numeric(year), msg = "Enter valid year")
    url <-
      paste0("https://api.collegefootballdata.com/talent?year=",
             year)
  }
  df <- fromJSON(url)
  return(df)
}
