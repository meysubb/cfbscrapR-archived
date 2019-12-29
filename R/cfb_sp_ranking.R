#' Get S&P+ historical rating data
#'
#' If conference is provided Get average S&P+ historical rating data by conference
#'
#'
#' @param year Year (optional)
#' @param team Team information (optional)
#' @param confefence Conference Abbreivation - S&P+ information by confderence
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_sp_ranking(year=2019)
#'
#' cfb_sp_ranking(team='Texas A&M')
#'
#' cfb_sp_ranking(2019,team="Texas")
#'
#' cfb_sp_ranking(conference='SEC')
#'
#' cfb_sp_ranking(year=2018,conference='SEC')


cfb_sp_ranking <- function(year=NULL,team=NULL,conference=NULL){
  if(is.null(conference)){
    base_url = "https://api.collegefootballdata.com/ratings/sp"
    full_url = paste0(base_url,"?year=",year)
    if (!is.null(team)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)
    }
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(conference)){
    conf_check = conference %in% cfb_conf_types_df$abbreviation
    assert_that((conf_check) == T, msg = "Please provide an appropriate conference abberivation.")
    base_url = 'https://api.collegefootballdata.com/ratings/sp/conferences'
    full_url = paste0(base_url,"?year=",year,"&conference=",conference)
    df = fromJSON(full_url)
    return(df)
  }

}
