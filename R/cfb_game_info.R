#' Get information from games (depending on a bunch of parameters)
#' 1 - Game info for all games that year
#' 2 - Game infos for a specific week of games in a year
#' 3 - Game info for one team that year (or a specific week)
#' 4 - Game info for specific conference that year (or a specific week)
#'
#' #' @param season_type Select Season Type (regular,postseason,both)
#' @param year Year
#' @param week Week (optional, keep this numeric)
#' @param team Team (optional, D1 Team)
#' @param conference Conference (option, select an appropriate conference)
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
#' cfb_game_info(2018,week=1)
#'
#' cfb_game_info(2018,conference='SEC')
#'
#' cfb_game_info(2018,team="Texas A&M")
#'

cfb_game_info <- function(year,
                          season_type = 'regular',
                          week = NULL,
                          team = NULL,
                          conference = NULL) {
  ## check if year is numeric
  assert_that(is.numeric(year),msg='Enter valid year')
  if(!is.null(week)){
    assert_that(is.numeric(week),msg='Enter valid week')
  }
  options(stringsAsFactors = FALSE)
  play_base_url <-
    paste0("https://api.collegefootballdata.com/games?year=",
           year,"&seasonType=",season_type,"&")
  if(!is.null(team)){
    team = URLencode(team, reserved = T)
  }
  ### check for week
  ### then check for team
  ### check for conference
  param_len <- length(c(week, team, conference))

  if (param_len == 1) {
    if (!is.null(week)) {
      #week only
      url <- paste0(play_base_url, "week=", week)
    } else if (!is.null(team)) {
      # team
      url <- paste0(play_base_url, "team=", team)
    } else{
      # conference
      url <- paste0(play_base_url, "conference=", conference)
    }
  } else if(param_len == 2) {
    if (is.null(conference)) {
      ## team,week
      url <- paste0(play_base_url, "team=", team, "&week=", week)
    } else{
      ## conference,week
      url <-
        paste0(play_base_url, "week=", week, "&conference=", conference)
    }
  }else{
    url <- play_base_url
  }
  game_df <- fromJSON(url)
  return(game_df)
}
