#' Get a teams full roster
#'
#' Extracts raw game by game data.
#' @param team Team (Select a valid team, D1 football)
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'

cfb_team_roster_data <- function(team){
  options(stringsAsFactors = FALSE)
  play_base_url <- "https://api.collegefootballdata.com/roster?"
  team_encode <- URLencode(team, reserved = T)
  full_url <- paste0(play_base_url,"&team=",
                     team_encode)
  roster_df <- fromJSON(full_url)
  return(roster_df)
}

