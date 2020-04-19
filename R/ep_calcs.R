#' Add EPA calculations
#' This is only for D1 football
#'
#'
#' Extracts raw game by game data.
#' @param clean_pbp_dat Clean PBP DataFrame (as pulled from cfb_pbp_dat)
#' @param ep_model EP Model
#' @param fg_model FG Model
#' @keywords internal
#' @import mgcv
#' @import nnet
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#'

calculate_epa <- function(clean_pbp_dat, ep_model=cfbscrapR:::ep_model, fg_model=cfbscrapR:::fg_model) {
  # constant vectors to be used again
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown Touchdown",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )

  off_TD = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown",
    "Rushing Touchdown Touchdown"
  )
  def_TD = c(
    "Blocked Punt Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception Return Touchdown",
    "Safety",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown",
    "Blocked Field Goal Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown Touchdown",
    "Pass Interception Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )

  # if you are trying to deal with multiple games at once
  # then you have to get the after individually.
  g_ids = sort(unique(clean_pbp_dat$game_id))
  clean_pbp_dat = purrr::map_dfr(g_ids,
                                 function(x) {
                                   clean_pbp_dat %>%
                                     filter(game_id == x) %>%
                                     prep_pbp_df()
                                 })



  pred_df = clean_pbp_dat %>% group_by(drive_id) %>% arrange(new_id, .by_group =
                                                               T) %>% select(
                                                                 new_id,
                                                                 drive_id,
                                                                 game_id,
                                                                 TimeSecsRem,
                                                                 down,
                                                                 distance,
                                                                 yards_to_goal,
                                                                 log_ydstogo,
                                                                 Under_two,
                                                                 Goal_To_Go
                                                               ) %>% ungroup()

  # ep_start

  ep_start = as.data.frame(predict(ep_model, pred_df, type = 'prob'))
  colnames(ep_start) <- ep_model$lev
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,
                                 current_probs = ep_start,
                                 fg_mod = fg_model)
  weights = c(0, 3, -3, -2, -7, 2, 7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row) {
    sum(row * weights)
  })


  # if you are trying to deal with multiple games at once
  # then you have to get the after individually.
  g_ids = sort(unique(clean_pbp_dat$game_id))
  prep_df_after = purrr::map_dfr(g_ids,
                                 function(x) {
                                   clean_pbp_dat %>%
                                     filter(game_id == x) %>%
                                     prep_df_epa2()
                                 })


  ep_end = predict(ep_model, prep_df_after, type = 'prob')
  colnames(ep_end) <- ep_model$lev
  pred_df$ep_after = apply(ep_end, 1, function(row) {
    sum(row * weights)
  })

  colnames(prep_df_after)[4:12] = paste0(colnames(prep_df_after)[4:12], "_end")
  pred_df = clean_pbp_dat %>% left_join(prep_df_after,
                                        by = c("game_id","drive_id" = "drive_id_end", "new_id")) %>%
    left_join(pred_df %>% select(new_id, drive_id, game_id, ep_before, ep_after),
              by = c("game_id","drive_id", "new_id"))

  #pred_df$turnover = turnover_col
  ## kickoff plays
  ## calculate EP before at kickoff as what happens if it was a touchback
  ## 25 yard line in 2012 and onwards
  kickoff_ind = (pred_df$play_type =='Kickoff')
  if(any(kickoff_ind)){
    new_kick = pred_df[kickoff_ind,]
    new_kick["down"] = 1
    new_kick["distance"] = 10
    new_kick["yards_to_goal"] = 75
    new_kick["log_ydstogo"] = log(10)
    ep_kickoffs = as.data.frame(predict(ep_model, new_kick, type = 'prob'))
    if(table(kickoff_ind)[2] > 1){
      pred_df[kickoff_ind,"ep_before"] = apply(ep_kickoffs,1,function(row){
        sum(row*weights)
      })
    }
    else{
      pred_df[kickoff_ind,"ep_before"] = sum(ep_kickoffs * weights)
    }

  }


  turnover_plays = which(pred_df$turnover == 1 & !kickoff_ind)
  pred_df[turnover_plays, "ep_after"] = -1 * pred_df[turnover_plays, "ep_after"]

  # game end EP is 0
  pred_df[pred_df$end_half_game_end == 1, "ep_after"] = 0

  ## scoring plays from here on out
  pred_df[(pred_df$play_type %in% off_TD), "ep_after"] = 7
  pred_df[(pred_df$play_type %in% def_TD), "ep_after"] = -7
  pred_df[pred_df$play_type == "Safety", "ep_after"] = -2
  pred_df[pred_df$play_type == "Field Goal Good", "ep_after"] = 3



  pred_df = pred_df %>%
    mutate(EPA = ep_after - ep_before,
           # prep some variables for WPA
           score_diff = offense_score - defense_score,
           home_EPA = ifelse(offense_play==home,EPA,-EPA),
           away_EPA = -home_EPA,
           ExpScoreDiff = score_diff + ep_before,
           half = as.factor(half),
           ExpScoreDiff_Time_Ratio = ExpScoreDiff/ (TimeSecsRem + 1)) %>%
    select(-yard_line,
           -log_ydstogo,-log_ydstogo_end,
           -Goal_To_Go_end,-end_half_game_end,
           #-start_time.hours,-end_time.hours,
           -id_play.y,-Under_two_end) %>% select(
             game_id,
             drive_id,
             new_id,
             id_play.x,
             offense_play,
             defense_play,
             home,
             away,
             period,
             half,
             clock.minutes,
             clock.seconds,
             offense_score,
             defense_score,
             play_type,
             play_text,
             scoring,
             TimeSecsRem,
             Under_two,
             down,
             distance,
             Goal_To_Go,
             yards_to_goal,
             yards_gained,
             TimeSecsRem_end,
             down_end,
             distance_end,
             yards_to_goal_end,
             everything()
           ) %>%
    mutate(
      rz_play = ifelse((yards_to_goal <= 20), 1, 0),
      scoring_opp = ifelse((yards_to_goal <= 40), 1, 0),
      pass = if_else(
        play_type == "Pass Reception" | play_type == "Passing Touchdown" |
          play_type == "Sack" |
          play_type == "Pass Interception Return" |
          play_type == "Pass Incompletion" |
          play_type == "Sack Touchdown" |
          (play_type == "Safety" &
             str_detect(play_text, "sacked")) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "pass")
          ) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "pass")
          ),
        1,
        0
      ),
      rush = ifelse(
        play_type == "Rush" | play_type == "Rushing Touchdown" |
          (play_type == "Safety" &
             str_detect(play_text, "run")) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "run")
          ) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "run")
          ),
        1,
        0
      ),
      stuffed_run = ifelse((rush == 1 &
                              yards_gained <= 0), 1, 0),
      success = ifelse(
        yards_gained >= .5 * distance & down == 1,
        1,
        ifelse(
          yards_gained >= .7 * distance & down == 2,
          1,
          ifelse(
            yards_gained >= distance & down == 3,
            1,
            ifelse(yards_gained >= distance &
                     down == 4, 1, 0)
          )
        )
      ),
      success = ifelse(play_type %in% turnover_play_type, 0, success),
      epa_success = ifelse(EPA > 0, 1, 0)
    )

  return(pred_df)
}

prep_pbp_df <- function(df) {
  df = df %>%
    mutate(
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      Under_two = raw_secs <= 120,
      half = ifelse(period <= 2, 1, 2),
      new_id = gsub(pattern = unique(game_id), "", x = id_play),
      new_id = as.numeric(new_id),
      log_ydstogo = log(distance)
    ) %>% filter(period <= 4, down > 0) %>%
    filter(!is.na(down),!is.na(raw_secs)) %>% rename(TimeSecsRem = raw_secs)


  fg_inds = str_detect(df$play_type, "Field Goal")
  df[fg_inds, "yards_to_goal"] = df[fg_inds, "yards_to_goal"] + 17
  df[fg_inds, "log_ydstogo"] = log(df[fg_inds, "distance"])

  # kickoff_inds = str_detect(df$play_type, "Kickoff")
  # df[kickoff_inds, "adj_yd_line"] =  100 - (100 * (1 - df[kickoff_inds, "coef"]) + (2 * df[kickoff_inds, "coef"] - 1) * df[kickoff_inds, "start_yardline"])

  df = df %>% mutate(Goal_To_Go = ifelse(
    str_detect(play_type, "Field Goal"),
    distance >= (yards_to_goal - 17),
    distance >= yards_to_goal
  )) %>% filter(log_ydstogo != -Inf) %>% group_by(drive_id) %>%  arrange(new_id, .by_group =
                                                                           T) %>% ungroup()
  return(df)
}

epa_fg_probs <- function(dat, current_probs, fg_mod) {
  fg_ind = str_detect((dat$play_type), "Field Goal")
  fg_dat = dat[fg_ind, ]

  # we are setting everythign after 0 seconds to have
  # 0 probs.
  end_game_ind = which(dat$TimeSecsRem <= 0)
  current_probs[end_game_ind, ] <- 0

  make_fg_prob <- mgcv::predict.bam(fg_mod, newdata = fg_dat,
                                    type = "response")

  fg_dat<- fg_dat %>%
    # Subtract 5.065401 from TimeSecs since average time for FG att:
    mutate(
      TimeSecsRem = TimeSecsRem - 5.065401,
      # Correct the yrdline100:
      yards_to_goal = 100 - (yards_to_goal - 9),
      # Not GoalToGo:
      Goal_To_Go = rep(FALSE, n()),
      # Now first down:
      down = rep("1", n()),
      # 10 yards to go
      log_ydstogo = rep(log(10), n()),
      # Create Under_TwoMinute_Warning indicator
      Under_two = ifelse(TimeSecsRem < 120,
                         TRUE, FALSE)
    )
  # add in the fg make prob into this
  current_probs2 <- current_probs
  #current_probs2[fg_ind,] <- current_probs[fg_ind,] * (1-make_fg_prob)
  val = (1 - make_fg_prob)
  ind <- dim(current_probs2[fg_ind, ])[1]
  for (i in seq(1, ind)) {
    temp = current_probs2[fg_ind, ]
    temp[i, ] = temp[i, ] * val[i]
  }
  current_probs2[fg_ind, ] =  temp


  # now to flip all the probs,
  current_probs2[fg_ind, "FG"] <-
    make_fg_prob + current_probs[fg_ind, "Opp_FG"]
  current_probs2[fg_ind, "Opp_FG"] <- current_probs[fg_ind, "FG"]
  current_probs2[fg_ind, "TD"] <- current_probs[fg_ind, "Opp_TD"]
  current_probs2[fg_ind, "Opp_TD"] <- current_probs[fg_ind, "TD"]
  current_probs2[fg_ind, "Safety"] <-
    current_probs[fg_ind, "Opp_Safety"]
  current_probs2[fg_ind, "Opp_Safety"] <-
    current_probs[fg_ind, "Safety"]
  return(current_probs2)
}

prep_df_epa2 <- function(dat) {
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Fumble Return Touchdown Touchdown",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )

  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      yards_to_goal = as.numeric(yards_to_goal),
      distance = distance,
      yards_gained = as.numeric(yards_gained),
      start_yardline = as.numeric(start_yardline),
      start_yards_to_goal = as.numeric(start_yards_to_goal),
      end_yards_to_goal = as.numeric(end_yards_to_goal),
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      half = ifelse(period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0
      #log_ydstogo = 0
    )

  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0

  new_offense = !(dat$offense_play == lead(dat$offense_play))
  #fourth_down = dat$down == 4,  & fourth_down
  t_ind = turnover_ind | (new_offense)

  dat$turnover[t_ind] <- 1


  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Fumble Return Touchdown Touchdown",
    "Defensive 2pt Conversion",
    "Safety",
    "Sack Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown"
  )
  turnover_vec = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Fumble Return Touchdown Touchdown",
    "Defensive 2pt Conversion",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown"
  )
  normalplay = c(
    "Rush",
    "Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Sack",
    "Fumble Recovery (Own)"
  )
  score = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Punt Touchdown",
    "Rushing Touchdown Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  dat = dat %>% group_by(game_id, half) %>%
    dplyr::arrange(id_play, .by_group = TRUE) %>%
    mutate(
      turnover_indicator = ifelse(
        play_type %in% defense_score_vec | play_type %in% turnover_vec |
          play_type %in% normalplay &
          yards_gained < distance & down == 4,
        1,
        0
      ),
      down = as.numeric(down),
      down = ifelse(play_type %in% "Kickoff", 5, down),
      new_down = case_when(
        play_type %in% score ~ 1,
        play_type %in% kickoff ~ 1,
        play_type %in% turnover_vec ~ 1,
        play_type %in% defense_score_vec ~ 1,
        play_type %in% normalplay & yards_gained >= distance ~ 1,
        play_type %in% normalplay & yards_gained < distance & down <= 3 ~ down + 1,
        play_type %in% normalplay & yards_gained < distance & down == 4 ~ 1
      ),

      yards_gained = as.numeric(yards_gained),
      start_yards_to_goal = as.numeric(start_yards_to_goal),
      new_distance = as.numeric(case_when(
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal - yards_gained >= 10) ~ 10,
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal  - yards_gained <= 10) ~ yards_to_goal,
        play_type %in% normalplay &
          yards_gained < distance & down <= 3 ~ distance - yards_gained,
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 & (100 - (yards_to_goal  - yards_gained) >= 10) ~ 10,
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 &
          (100 - (yards_to_goal  - yards_gained) <= 10) ~ 100 - yards_to_goal,
        play_type %in% turnover_vec ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) >= 10) ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) <= 10) ~ 100 - (yards_to_goal  + yards_gained),
        play_type %in% defense_score_vec ~ 0,
        play_type %in% score ~ 0,
        play_type %in% kickoff ~ 10
      )),

      new_yardline = as.numeric(case_when(
        play_type %in% normalplay ~ yards_to_goal - yards_gained,
        play_type %in% score ~ 0,
        play_type %in% defense_score_vec ~ 0,
        play_type %in% kickoff ~ start_yards_to_goal,
        play_type %in% turnover_vec ~ 100 - yards_to_goal + yards_gained
      )),

      new_TimeSecsRem = ifelse(!is.na(lead(TimeSecsRem)),lead(TimeSecsRem),0),
      new_log_ydstogo = ifelse(new_distance == 0, log(0.5),log(new_distance)),
      new_Goal_To_Go = ifelse(new_yardline <= new_distance, TRUE, FALSE),
      # new under two minute warnings
      new_Under_two = new_TimeSecsRem <= 120,
      end_half_game = 0
    ) %>%
    mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0)) %>% ungroup()

  #--Punt Plays--------------------------
  punt_plays = dat$play_type == "Punt"
  touchback_punt = ifelse(!is.na(stringr::str_detect(dat$play_text,"touchback") & (punt_plays)),
                          stringr::str_detect(dat$play_text,"touchback") & (punt_plays),FALSE)
  yds_gained_more_0 = (dat$yards_gained > 0 ) & punt_plays
  dat[punt_plays,"new_down"] = 1
  dat[punt_plays,"new_distance"] = 10
  dat[punt_plays,"new_log_ydstogo"] = log(10)
  dat[punt_plays,"new_Goal_To_Go"] = FALSE
  dat[touchback_punt,"new_yardline"] = 80
  dat[yds_gained_more_0,"new_yardline"] = 100 - (with(dat[yds_gained_more_0,],yards_to_goal-yards_gained))
  punt_ind = (dat$yards_gained == 0) & punt_plays & !touchback_punt
  if(any(punt_ind)){
    punt_play = dat[punt_ind,] %>% pull(play_text)
    yds_punted = as.numeric(stringr::str_extract(
      stringi::stri_extract_last_regex(punt_play, '(?<=for)[^,]+'),
      "\\d+"
    ))
    # ball always chances hands
    punt_yd_line = dat[punt_ind,] %>% pull(yard_line)
    dat[punt_ind, "new_yardline"] = 100 - ifelse(punt_yd_line > 50,
                                                 (punt_yd_line - yds_punted),
                                                 (punt_yd_line + yds_punted))

  }


  #--End of Half Plays--------------------------
  end_of_half_plays = (dat$new_TimeSecsRem == 0)
  if (any(end_of_half_plays)) {
    dat$new_yardline[end_of_half_plays] <- 99
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 99
    dat$end_half_game[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(99)
    dat$new_Under_two[end_of_half_plays] <-
      dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }

  # missed field goal needs to be here
  # needs to go before the na check to set to 99
  dat = dat %>% mutate(new_yardline = if_else(
    is.na(new_yardline) &
      play_type %in% c("Field Goal Missed", "Blocked Field Goal"),
    100 - (yards_to_goal - 9),
    new_yardline
  ))

  #--General weird plays that don't have an easy fix----
  na_yd_line = which(is.na(dat$new_yardline) | dat$new_yardline >= 100)
  dat$new_yardline[na_yd_line] = dat$yard_line[na_yd_line+1]

  neg_distance = which(dat$new_distance < 0)
  dat$new_distance[neg_distance] = dat$distance[neg_distance+1]
  dat$new_log_ydstogo[neg_distance] = log(dat$new_distance[neg_distance])

  #--Missing yd_line Plays--------------------------
  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 99
  dat$new_log_ydstogo[missing_yd_line] = log(99)



  dat = dat %>%
    mutate(new_down = as.factor(new_down)) %>%
    select(
      game_id,
      drive_id,
      id_play,
      new_TimeSecsRem,
      new_down,
      new_distance,
      new_yardline,
      new_log_ydstogo,
      new_Goal_To_Go,
      new_Under_two,
      end_half_game,
      turnover
    ) %>% arrange(id_play)
  colnames(dat) = gsub("new_", "", colnames(dat))
  colnames(dat)[2] <- "new_id_play"
  dat = dat %>% rename("yards_to_goal"="yardline")


  return(dat)
}
