#' Add WPA calculations
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param df Clean PBP DF with EPA calculations
#' @param wp_model WPA Model
#' @keywords internal
#' @import mgcv
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'

create_wpa <- function(df, wp_model = cfbscrapR:::wp_model) {
  col_nec = c("ExpScoreDiff", "TimeSecsRem", "half", "Under_two")
  if (!all(col_nec %in% colnames(df))) {
    df = df %>% mutate(
      score_diff = offense_score - defense_score,
      home_EPA = ifelse(offense_play == home, EPA, -EPA),
      away_EPA = -home_EPA,
      ExpScoreDiff = score_diff + ep_before,
      half = as.factor(half),
      ExpScoreDiff_Time_Ratio = ExpScoreDiff / (TimeSecsRem + 1)
    )
  }

  df = df %>% arrange(game_id, new_id)
  Off_Win_Prob = as.vector(predict(wp_model, newdata = df, type = "response"))
  df$wp = Off_Win_Prob

  if (length(unique(df$game_id)) == 1) {
    df2 = wpa_calcs(df)
  }
  if (length(unique(df$game_id)) >= 1) {
    g_ids = sort(unique(df$game_id))
    df2 = purrr::map_dfr(g_ids,
                         function(x) {
                           df %>%
                             filter(game_id == x) %>%
                             wpa_calcs()
                         })
  }
  return(df2)
}


wpa_calcs <- function(df) {
  df2 = df %>% mutate(
    def_wp = 1 - wp,
    home_wp = if_else(offense_play == home,
                      wp, def_wp),
    away_wp = if_else(offense_play != home,
                      wp, def_wp)
  ) %>% group_by(half) %>%
    mutate(
      # ball changes hand
      change_of_poss = ifelse(offense_play == lead(offense_play), 0, 1),
      change_of_poss = ifelse(is.na(change_of_poss), 0, change_of_poss)
    ) %>% ungroup() %>%
    mutate(
      # base wpa
      end_of_half = ifelse(half == lead(half), 0, 1),
      lead_wp = lead(wp),
      wpa_base = lead_wp - wp,
      # account for turnover
      wpa_change = ifelse(change_of_poss == 1, (1 - lead_wp) - wp, wpa_base),
      wpa = ifelse(end_of_half == 1, 0, wpa_change),
      home_wp_post = ifelse(offense_play == home,
                            home_wp + wpa,
                            home_wp - wpa),
      away_wp_post = ifelse(offense_play != home,
                            away_wp + wpa,
                            away_wp - wpa),
      adj_TimeSecsRem = ifelse(half == 1, 1800 + TimeSecsRem, TimeSecsRem)
    )
  return(df2)
}
