#' Extract CFB (D1) Play by Play Data - For both plays and drives
#'
#'
#'
#' Extracts raw game by game data. Data comes from https://api.collegefootballdata.com
#' @param season_type Select Season Type (regular,postseason,both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param play_type Select play type (example: see the cfb_play_type_df)
#' @param drive Enter anything, and you will get general drive infoo
#' @keywords Play-by-Play
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'
#' cfb_pbp_data(year=2019,week=9,team='Texas A&M')
#'
#'
#' cfb_pbp_data(year=2019,week=9,team='Texas A&M',epa_wpa=T)

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
  } else{
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
    drive_info = cfb_pbp_data(
      year,
      season_type = season_type,
      team = team,
      week = week,
      drive = TRUE
    )
    clean_drive_df = clean_drive_info(drive_info)
    colnames(clean_drive_df) <- paste0("drive_",colnames(clean_drive_df))
    play_df = play_df %>% mutate(drive_id = as.numeric(drive_id)) %>% left_join(clean_drive_df,
                                                                                by = c("drive_id"="drive_drive_id"),
                                                                                suffix = c("_play", "_drive"))
    rm_cols = c(
      'drive_offense_conference',
      'drive_defense_conference',
      'drive_offense',
      'offense_conferencec',
      'drive_defense',
      'defense_conference',
      'drive_id_drive',
      'drive_start_time.minutes',
      'drive_start_time.seconds',
      'drive_start_period',
      'drive_end_period',
      'drive_end_yardline',
      'drive_end_time.minutes',
      'drive_end_time.seconds',
      'drive_elapsed.seconds',
      'drive_elapsed.minutes',
      'drive_plays'
    )
    play_df = play_df %>%
      select(
        setdiff(names(.), rm_cols)
      ) %>% rename(game_id=drive_game_id,
                   drive_pts = drive_pts_drive,
                   drive_result = drive_drive_result,
                   id_play=id,
                   offense_play=offense,
                   defense_play=defense)
    if(epa_wpa){
      if(year<=2005) {
        warning(
          "Data Quality prior to 2005 is not as consistent. This can affect the EPA/WPA values, proceed with caution."
        )
      }
      play_df = clean_pbp_dat(play_df)
      g_ids = sort(unique(play_df$game_id))
      play_df = purrr::map_dfr(g_ids,
                                     function(x) {
                                       play_df %>%
                                         filter(game_id == x) %>%
                                         add_timeout_cols()
                                     })
      play_df = calculate_epa(play_df)
      play_df = create_wpa(play_df)
      play_df = play_df %>% group_by(drive_id) %>% arrange(new_id,.by_group=T) %>% ungroup()
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

