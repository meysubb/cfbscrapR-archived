#' Get SRS historical rating data
#'
#'
#' @param year Year (optional)
#' @param team Team information (optional)
#' @param confefence Conference Abbreivation - S&P+ information by confderence
#'
#' @keywords SRS
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_srs_ranking(year=2019)
#'
#' cfb_srs_ranking(team='Texas A&M')
#'
#' cfb_srs_ranking(2019,team="Texas")
#'
#'
#' cfb_srs_ranking(year=2018,conference='SEC')


cfb_srs_ranking <- function(year=NULL,team=NULL,conference=NULL){
  if(is.null(conference )){
    base_url = "https://api.collegefootballdata.com/ratings/srs"
    full_url = paste0(base_url,"?year=",year)
    if (!is.null(team)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)
    }
    df = fromJSON(full_url)
    return(df)
  }
  if((!is.null(conference)) & !is.null(year)){
    conf_check = conference %in% cfb_conf_types_df$abbreviation
    assert_that((conf_check) == T, msg = "Please provide an appropriate conference abberivation.")
    base_url = 'https://api.collegefootballdata.com/ratings/srs'
    full_url = paste0(base_url,"?year=",year,"&conference=",conference)
    df = fromJSON(full_url)
    return(df)
  }
  warning("You must provide the right combination of parameters. Year is required is team is not specified, Team is required if year is not specified. Conference and year are required together.")
  return(NA)
}
