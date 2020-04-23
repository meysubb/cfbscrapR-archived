#' Clean PBP
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param clean_pbp_dat PBP DataFrame (as pulled from cfb_pbp_dat)
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'

clean_pbp_dat <- function(raw_df) {
  #-- add change of possession to df
  raw_df <- raw_df %>%
    mutate(half = ifelse(period <= 2, 1, 2)) %>%
    group_by(game_id, half) %>%
    mutate(
      #-- ball changes hand
      change_of_poss = ifelse(offense_play == lead(offense_play, order_by = id_play), 0, 1),
      change_of_poss = ifelse(is.na(change_of_poss), 0, change_of_poss)
    ) %>% ungroup() %>% arrange(game_id, id_play)

  ## vectors
  #-- touchdowns
  td_e = str_detect(raw_df$play_text, "TD") |
    str_detect(raw_df$play_text, "Touchdown") |
    str_detect(raw_df$play_text, "TOUCHDOWN") |
    str_detect(raw_df$play_text, "touchdown")

  #-- kicks/punts
  kick_vec = str_detect(raw_df$play_text, "KICK") &
    !is.na(raw_df$play_text)
  punt_vec = (str_detect(raw_df$play_text, "Punt") |
                str_detect(raw_df$play_text, "punt")) &
    !is.na(raw_df$play_text)
  #-- fumbles
  fumble_vec = str_detect(raw_df$play_text, "fumble")
  #-- pass/rush
  rush_vec = raw_df$play_type == "Rush"
  pass_vec = raw_df$play_type == "Pass Reception"
  #-- sacks
  #- only want non-safety sacks, otherwise would be an additional condition
  sack_vec = raw_df$play_type == "Sack" |
    raw_df$play_type == "Sack Touchdown"
  #-- change of possession
  poss_change_vec = raw_df$change_of_poss == 1

  ## Fix strip-sacks to fumbles
  raw_df$play_type[fumble_vec &
                     sack_vec & poss_change_vec & !td_e] <-
    "Fumble Recovery (Opponent)"
  raw_df$play_type[fumble_vec & sack_vec & td_e] <-
    "Fumble Recovery (Opponent) Touchdown"

  ## touchdown check, want where touchdowns aren't in the play_type
  td_check = !str_detect(raw_df$play_type, "Touchdown")
  #-- fix kickoff fumble return TDs
  raw_df$play_type[kick_vec & fumble_vec & td_e & td_check] <-
    paste0(raw_df$play_type[kick_vec &
                              fumble_vec &
                              td_e & td_check], " Touchdown")
  #-- fix punt return TDs
  raw_df$play_type[punt_vec & td_e & td_check] <-
    paste0(raw_df$play_type[punt_vec &
                              td_e & td_check], " Touchdown")
  #-- fix rush/pass tds that aren't explicit
  raw_df$play_type[td_e & rush_vec] = "Rushing Touchdown"
  raw_df$play_type[td_e & pass_vec] = "Passing Touchdown"

  #-- fix duplicated TD play_type labels
  pun_td_sq = (raw_df$play_type == "Punt Touchdown Touchdown")
  raw_df$play_type[pun_td_sq] <- "Punt Touchdown"
  fum_td_sq = (raw_df$play_type == "Fumble Return Touchdown Touchdown")
  raw_df$play_type[fum_td_sq] == "Fumble Return Touchdown"
  rush_td_sq = (raw_df$play_type == "Rushing Touchdown Touchdown")
  raw_df$play_type[rush_td_sq] == "Rushing Touchdown"

  ## penalty detection
  pen_text = str_detect(raw_df$play_text, "Penalty") |
    str_detect(raw_df$play_text, "penalty") |
    str_detect(raw_df$play_text, "PENALTY")
  pen_type = raw_df$play_type == "Penalty"  | raw_df$play_type == "penalty"
  raw_df$penalty_flag = F
  raw_df$penalty_flag[pen_type] <- T
  raw_df$penalty_flag[pen_text] <- T

  ## kickoff down adjustment
  raw_df = raw_df %>% mutate(down = ifelse(down == 5 &
                                             str_detect(play_type, "Kickoff"), 1, down))


  return(raw_df)
}

add_timeout_cols <- function(play_df) {
  pbp_df <- play_df %>%
    group_by(game_id, half) %>%
    arrange(id_play) %>%
    mutate(
      timeout_called = ifelse(play_type %in% c("Timeout"), 1, 0),
      timeout_team = ifelse(
        play_type %in% c("Timeout"),
        ifelse(
          !is.na(str_extract(play_text, "timeout (.+)")),
          str_extract(play_text, "timeout (.+)"),
          str_extract(play_text, "Timeout (.+)")
        ),
        NA
      )
    ) %>%
    mutate(timeout_team = str_remove(timeout_team, ",(.+)")) %>%
    mutate(
      timeout_team = str_to_lower(str_remove(timeout_team, "Timeout ")),
      timeout_team = case_when(
        timeout_team == "af" ~ "air force",
        timeout_team == "air force academy" ~ "air force",
        timeout_team == "air force falcons" ~ "air force",
        timeout_team == "arkansas st." ~ "arkansas state",
        timeout_team == "asu" ~ "arkansas state",
        timeout_team == "ball state cardinals" ~ "ball state",
        timeout_team == "bgsu" ~ "bowling green",
        timeout_team == "brigham young" ~ "byu",
        timeout_team == "byu cougars" ~ "byu",
        timeout_team == "centrl michigan" ~ "central michigan",
        timeout_team == "cmu" ~ "central michigan",
        timeout_team == "coastl carolina" ~ "coastal carolina",
        timeout_team == "cs" ~ "colorado state",
        timeout_team == "eastrn michigan" ~ "eastern michigan",
        timeout_team == "ecu" ~ "east carolina",
        timeout_team == "emu" ~ "eastern michigan",
        timeout_team == "fau" ~ "florida atlantic",
        timeout_team == "fiu" ~ "florida international",
        timeout_team == "fla atlantic" ~ "florida atlantic",
        timeout_team == "florida intl" ~ "florida international",
        timeout_team == "floridainternational" ~ "florida international",
        timeout_team == "fresno st." ~ "fresno state",
        timeout_team == "ga southern" ~ "georgia southern",
        timeout_team == "gsu" ~ "georgia state",
        timeout_team == "hawai`i" ~ "hawai'i",
        timeout_team == "hawaii" ~ "hawai'i",
        timeout_team == "iowa hawkeyes" ~ "iowa",
        timeout_team == "las vegas" ~ "unlv",
        timeout_team == "latech" ~ "louisiana tech",
        timeout_team == "louisiana lafayette" ~ "louisiana",
        timeout_team == "louisiana state" ~ "lsu",
        timeout_team == "louisiana-lafayette" ~ "louisiana",
        timeout_team == "massachusetts" ~ "umass",
        timeout_team == "miami (fla.)" ~ "miami",
        timeout_team == "miami (ohio)" ~ "miami (oh)",
        timeout_team == "miami fl" ~ "miami",
        timeout_team == "miami florida" ~ "miami",
        timeout_team == "miami oh" ~ "miami (oh)",
        timeout_team == "miami ohio" ~ "miami (oh)",
        timeout_team == "middle tenn st" ~ "middle tennessee",
        timeout_team == "minnesota gophers" ~ "minnesota",
        timeout_team == "mississippi" ~ "ole miss",
        timeout_team == "mt" ~ "middle tennessee",
        timeout_team == "n.c. state" ~ "nc state",
        timeout_team == "NA" ~ "",
        timeout_team == "niu" ~ "northern illinois",
        timeout_team == "nm state" ~ "new mexico state",
        timeout_team == "nmsu" ~ "new mexico state",
        timeout_team == "north carolina st" ~ "nc state",
        timeout_team == "northernil" ~ "northern illinois",
        timeout_team == "ohio bobcats" ~ "ohio",
        timeout_team == "ohio university" ~ "ohio",
        timeout_team == "olddominion" ~ "old dominion",
        timeout_team == "ole ole miss" ~ "ole miss",
        timeout_team == "oregon st." ~ "oregon state",
        timeout_team == "rice owls" ~ "rice",
        timeout_team == "san jose st" ~ "san josé state",
        timeout_team == "san jose state" ~ "san josé state",
        timeout_team == "sj" ~ "san josé state",
        timeout_team == "sjsu" ~ "san josé state",
        timeout_team == "smu mustangs" ~ "smu",
        timeout_team == "southern  miss" ~ "southern mississippi",
        timeout_team == "southern cal" ~ "usc",
        timeout_team == "southern methodist university" ~ "smu",
        timeout_team == "temple owls" ~ "temple",
        timeout_team == "temple university" ~ "temple",
        timeout_team == "texas el paso" ~ "utep",
        timeout_team == "texas state university" ~ "texas state",
        timeout_team == "texassan" ~ "ut san antonio",
        timeout_team == "texas-san antonio" ~ "ut san antonio",
        timeout_team == "tls" ~ "tulsa",
        timeout_team == "troy university" ~ "troy",
        timeout_team == "tulane green wave" ~ "tulane",
        timeout_team == "uh" ~ "hawai'i",
        timeout_team == "ui" ~ "idaho",
        timeout_team == "ul" ~ "louisiana",
        timeout_team == "ul lafayette" ~ "louisiana",
        timeout_team == "ul monroe" ~ "louisiana monroe",
        timeout_team == "ull" ~ "louisiana",
        timeout_team == "ulm" ~ "louisiana monroe",
        timeout_team == "university of idaho" ~ "idaho",
        timeout_team == "usa" ~ "south alabama",
        timeout_team == "usf" ~ "south florida",
        timeout_team == "usm" ~ "southern mississippi",
        timeout_team == "usu" ~ "utah state",
        timeout_team == "utsa" ~ "ut san antonio",
        timeout_team == "washington st." ~ "washington state",
        timeout_team == "west virginia university" ~ "west virginia",
        timeout_team == "westrn kentucky" ~ "western kentucky",
        timeout_team == "westrn michigan" ~ "western michigan",
        timeout_team == "wfu" ~ "wake forest",
        timeout_team == "wku" ~ "western kentucky",
        timeout_team == "wmu" ~ "western michigan",
        timeout_team == "wsu" ~ "washington state",
        timeout_team == "wyoming cowboys" ~ "wyoming",
        TRUE ~ timeout_team
      ),
      home_timeout = ifelse(is.na(timeout_team), 0,
                            ifelse(
                              str_detect(str_to_lower(home), fixed(timeout_team)) == TRUE, 1, 0
                            )),
      away_timeout = ifelse(is.na(timeout_team), 0,
                            ifelse(
                              str_detect(str_to_lower(away), fixed(timeout_team)) == TRUE, 1, 0
                            )),
      off_timeouts_rem_before = NA,
      def_timeouts_rem_before = NA,
      off_timeouts_rem_after = NA,
      def_timeouts_rem_after = NA,
      home_timeouts_rem_before = NA,
      away_timeouts_rem_before = NA,
      home_timeouts_rem_after = NA,
      away_timeouts_rem_after = NA
    ) %>%
    mutate(
      home_timeout =
        case_when(
          timeout_called == 1 & home_timeout == 1 & away_timeout == 1 ~
            ifelse(is.na(timeout_team), 0,
                   ifelse(
                     str_detect(str_to_lower(home),
                                paste0("^", timeout_team, "$")) ==
                       TRUE, 1, 0
                   )),
          TRUE ~ home_timeout
        ),
      away_timeout =
        case_when(
          timeout_called == 1 & home_timeout == 1 & away_timeout == 1 ~
            ifelse(is.na(timeout_team), 0,
                   ifelse(
                     str_detect(str_to_lower(away),
                                paste0("^", timeout_team, "$")) ==
                       TRUE, 1, 0
                   )),
          TRUE ~ away_timeout
        )
    ) %>%
    mutate(
      home_timeouts_rem_after = 3 - cumsum(home_timeout),
      away_timeouts_rem_after = 3 - cumsum(away_timeout),
      home_timeouts_rem_before = ifelse(
        !is.na(lag(home_timeouts_rem_after, order_by = id_play)),
        lag(home_timeouts_rem_after, order_by = id_play),
        3
      ),
      away_timeouts_rem_before = ifelse(
        !is.na(lag(away_timeouts_rem_after, order_by = id_play)),
        lag(away_timeouts_rem_after, order_by = id_play),
        3
      ),
      off_timeouts_rem_after = ifelse(
        offense_play == home,
        home_timeouts_rem_after,
        away_timeouts_rem_after
      ),
      def_timeouts_rem_after = ifelse(
        defense_play == home,
        home_timeouts_rem_after,
        away_timeouts_rem_after
      ),
      off_timeouts_rem_before = ifelse(
        offense_play == home,
        home_timeouts_rem_before,
        away_timeouts_rem_before
      ),
      def_timeouts_rem_before = ifelse(
        defense_play == home,
        home_timeouts_rem_before,
        away_timeouts_rem_before
      )
    ) %>% ungroup()



  return(pbp_df)
}
