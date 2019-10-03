#' Gets CFB recruiting information for a specific year (and possibly a specific team)
#'
#' If two years are provided, get cfb recruiting information based on position groups during that
#' time period for all FBS teams.
#'
#'
#' @param year Year
#' @param team Team information (optional)
#' @param year2 Year 2 (End Year) - if looking for position group info
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'
#' cfb_recruiting(2018)
#'
#' cfb_recruiting(2018,team="Texas")
#'
#' cfb_recruiting(2016,year2=2018)


cfb_recruiting <- function(year,team=NULL,year2=NULL){
  if(!is.null(year2)){
    # by position groups
    base_url = "https://api.collegefootballdata.com/recruiting/groups?startYear="
    full_url = paste0(base_url,year,'&endYear=',year2)
    if(!is.null(team)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)
    }
    df = fromJSON(full_url)
    return(df)
  }

  base_url = paste0("https://api.collegefootballdata.com/recruiting/players?year=",year)
  if(!is.null(team)){
    team = URLencode(team, reserved = T)
    full_url = paste0(base_url,"&team=",team)
    df = fromJSON(full_url)
    return(df)
  }

  df = fromJSON(base_url)
  return(df)
}
