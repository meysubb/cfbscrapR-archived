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
                         drive=NULL,
                         epa_wpa=FALSE) {
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
  if(nrow(raw_play_df)==0){
    warning("Most likely a bye week, the data pulled from the API was empty. Returning nothing
            for this one week or team.")
    return(NULL)
  }
  play_df <- raw_play_df

  ## call/drive information
  if(is.null(drive)){
    drive_info = cfb_pbp_data(year,season_type = season_type,team=team,week=week,drive=TRUE)
    clean_drive_df = clean_drive_info(drive_info)
    play_df = play_df %>% mutate(drive_id = as.numeric(drive_id)) %>% left_join(clean_drive_df,by = "drive_id",suffix=c("_play","_drive"))
    rm_cols = c('offense_conference_play','defense_conference_play','offense_drive','offense_conference_drive',
      'defense_drive','defense_conference_drive','id_drive','start_time.minutes',
      'start_time.seconds','start_period','end_period','end_yardline',
      'end_time.minutes','end_time.seconds','elapsed.seconds','elapsed.minutes',
      'plays'
    )
    play_df = play_df %>%
      select(
        setdiff(names(.), rm_cols)
      )
    if(epa_wpa){
      play_df = calculate_epa(play_df)
      play_df = create_wpa(play_df)
    }
  }
  return(play_df)
}


clean_drive_info <- function(drive_df){
  clean_drive = drive_df %>% mutate(
    pts_drive = case_when(
      str_detect(drive_result,"TD") ~ 7,
      #str_detect(drive_result,"FG") ~ 3,
      str_detect(drive_result,"SF") ~ -2,
      drive_result == 'FG GOOD' ~ 3,
      drive_result == 'FG' ~ 3,
      drive_result == 'MISSED FG TD' ~ -7,
      drive_result == 'KICKOFF RETURN TD' ~ -7,
      drive_result == 'END OF HALF TD' ~ 7,
      drive_result == "END OF GAME TD" ~ 7,
      drive_result == 'PUNT RETURN TD' ~ -7,
      drive_result == 'PUNT TD' ~ -7,
      drive_result == 'INT TD' ~ -7,
      drive_result == 'INT RETURN TOUCH' ~ -7,
      drive_result == 'FUMBLE RETURN TD' ~ -7,
      drive_result == 'FUMBLE TD' ~ -7,
      drive_result == 'DOWNS TD' ~ -7,
      TRUE ~ 0),
    scoring = ifelse(pts_drive!=0,TRUE,scoring)
  ) %>% mutate(drive_id = as.numeric(id)) %>% arrange(game_id,drive_id)
  return(clean_drive)
}

