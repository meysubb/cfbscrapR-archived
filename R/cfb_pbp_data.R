#' Extract CFB Play by Play Data - For both plays and drives
#' This is only for D1 football
#' Data comes from https://api.collegefootballdata.com
#'
#' Extracts raw game by game data.
#' @param season_type Select Season Type (regular,postseason,both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param play_type Select play type (example: see the cfb_play_type_df)
#' @param drive Enter anything, and you will get general drive infoo
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'

cfb_pbp_data <- function(year,
                         season_type = 'regular',
                         week = 1,
                         team = NULL,
                         play_type = NULL,
                         drive=NULL) {
  #require(jsonlite)
  options(stringsAsFactors = FALSE)
  if (!is.null(play_type)) {
    text <- play_type %in% cfb_play_type_df$text
    abbr <- play_type %in% cfb_play_type_df$abbreviation
    pt <-
      assert_that((text |
                     abbr) == T, msg = "Incorrect play type selected, please look at the available options in the Play Type DF.")
    if (text) {
      pt_id = cfb_play_type_df$id[which(cfb_play_type_df$text == play_type)]
    } else{
      pt_id = cfb_play_type_df$id[which(cfb_play_type_df$abbreviation == play_type)]
    }
  }
  ## Inputs
  ## Year, Week, Team
  if(is.null(drive)){
    play_base_url <- paste0("https://api.collegefootballdata.com/plays?seasonType=",season_type,'&')
  }
  else{
    play_base_url <- paste0("https://api.collegefootballdata.com/drives?seasonType=",season_type,'&')
  }

  if (is.null(play_type) & is.null(team)) {
    # no play type, no team
    full_url <- paste0(play_base_url, "year=", year, "&week=", week)
  } else{
    # no team, play_type
    if (is.null(play_type)) {
      full_url <-
        paste0(play_base_url,
               "year=",
               year,
               "&week=",
               week,
               "&team=",
               URLencode(team, reserved = T))
    } else if (is.null(team)) {
      # no team, play_type
      full_url <-
        paste0(
          play_base_url,
          "year=",
          year,
          "&week=",
          week,
          "&team=",
          URLencode(team, reserved = T)
        )
    } else{
      # team & play type
      full_url <-
        paste0(
          play_base_url,
          "year=",
          year,
          "&week=",
          week,
          "&team=",
          URLencode(team, reserved = T),
          "&playType=",
          pt_id
        )
    }
  }

  raw_play_df <- fromJSON(full_url)
  raw_play_df <- do.call(data.frame, raw_play_df)
  play_df <- raw_play_df

  return(play_df)
}
