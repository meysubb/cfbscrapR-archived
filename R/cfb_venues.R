#' Pulls all college football venues and data on capacity, grass, city/state, location,
#' elevation, dome, timezone and construction year
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'
#' cfb_venues()
#'


cfb_venues <- function(){
    base_url = "https://api.collegefootballdata.com/venues"
    df = fromJSON(base_url)
    return(df)
}
