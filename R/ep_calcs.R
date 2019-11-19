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
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception',
    'Punt',
    "Field Goal Missed",
    "Blocked Field Goal"
  )

  off_TD = c(
    'Passing Touchdown',
    'Rushing Touchdown',
    "Fumble Recovery (Own) Touchdown",
    "Pass Reception Touchdown"
  )
  def_TD = c(
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Missed Field Goal Return Touchdown',
    "Fumble Return Touchdown Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Punt Return Touchdown",
    "Blocked Punt Touchdown",
    "Sack Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Pass Interception Return Touchdown",
    "Kickoff Touchdown"
  )
  if(length(unique(clean_pbp_dat$game_id))==1){
    clean_pbp_dat = prep_pbp_df(clean_pbp_dat)
  }
  if (length(unique(clean_pbp_dat$game_id)) > 1) {
    # if you are trying to deal with multiple games at once
    # then you have to get the after individually.
    g_ids = sort(unique(clean_pbp_dat$game_id))
    clean_pbp_dat = purrr::map_dfr(g_ids,
                                   function(x) {
                                     clean_pbp_dat %>%
                                       filter(game_id == x) %>%
                                       prep_pbp_df()
                                   })
  }


  pred_df = clean_pbp_dat %>% arrange(id_play) %>% select(
                                     new_id,
                                     drive_id,
                                     game_id,
                                     TimeSecsRem,
                                     down,
                                     distance,
                                     adj_yd_line,
                                     log_ydstogo,
                                     Under_two,
                                     Goal_To_Go)

  # ep_start

  ep_start = as.data.frame(predict(ep_model, pred_df, type = 'prob'))
  colnames(ep_start) <- ep_model$lev
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,
                                 current_probs = ep_start,
                                 fg_model = fg_model)
  weights = c(0, 3, -3, -2, -7, 2, 7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row) {
    sum(row * weights)
  })

  ## Prep for EP_after
  if(length(unique(clean_pbp_dat$game_id))==1){
    prep_df_after = prep_df_epa2(clean_pbp_dat)
  }
  if (length(unique(clean_pbp_dat$game_id)) > 1) {
    # if you are trying to deal with multiple games at once
    # then you have to get the after individually.
    g_ids = sort(unique(clean_pbp_dat$game_id))
    prep_df_after = purrr::map_dfr(g_ids,
                            function(x) {
                              clean_pbp_dat %>%
                                filter(game_id == x) %>%
                                prep_df_epa2()
                            })
  }

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
    new_kick["adj_yd_line"] = 75
    new_kick["log_ydstogo"] = log(75)
    ep_kickoffs = as.data.frame(predict(ep_model, new_kick, type = 'prob'))
    pred_df[kickoff_ind,"ep_before"] = apply(ep_kickoffs,1,function(row){
      sum(row*weights)
    })
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
           -coef,
           -coef2,
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
             Goal_To_Go,
             distance,
             adj_yd_line,
             yards_gained,
             TimeSecsRem_end,
             down_end,
             distance_end,
             adj_yd_line_end,
             everything()
           ) %>%
      mutate(
        rz_play = ifelse((adj_yd_line <= 20), 1, 0),
        scoring_opp = ifelse((adj_yd_line <= 40), 1, 0),
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
          play_type == "Rush" |
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

prep_pbp_df <- function(df){

  df = df %>%
    mutate(
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      Under_two = raw_secs <= 120,
      coef = home == defense_play,
      coef2 = home == offense_play,
      half = ifelse(period <= 2, 1, 2),
      new_id = gsub(pattern=unique(game_id),"",x=id_play),
      new_id = as.numeric(new_id),
      adj_yd_line = 100 * (1 - coef) + (2 * coef - 1) * yard_line,
      log_ydstogo = log(adj_yd_line)
    ) %>% filter(period <= 4, down > 0) %>%
    filter(!is.na(down), !is.na(raw_secs)) %>% rename(TimeSecsRem = raw_secs)

  bool_chk = df$year == 2019 & df$offense_play %in% c("Alabama","Duke") & df$defense_play %in% c("Alabama","Duke")
  bool_chk2 = df$year == 2019 & df$offense_play %in% c("Florida","Miami") & df$defense_play %in% c("Florida","Miami")
  bool_chk = bool_chk | bool_chk2
  if(length(bool_chk)>0){
    df$adj_yd_line[bool_chk] = 100 * (1-df$coef2[bool_chk]) + (2*df$coef2[bool_chk] - 1)*df$yard_line[bool_chk]
    df$log_ydstogo[bool_chk] = log(df$adj_yd_line[bool_chk])
  }


  fg_inds = str_detect(df$play_type,"Field Goal")
  df[fg_inds,"adj_yd_line"] = df[fg_inds,"adj_yd_line"] + 17
  df[fg_inds,"log_ydstogo"] = log(df[fg_inds,"adj_yd_line"])

  kickoff_inds = str_detect(df$play_type,"Kickoff")
  df[kickoff_inds,"adj_yd_line"] =  100 - (100 * (1 - df[kickoff_inds,"coef"]) + (2 * df[kickoff_inds,"coef"] - 1) * df[kickoff_inds,"start_yardline"])

  df = df %>% mutate(
    Goal_To_Go = ifelse(
    str_detect(play_type, "Field Goal"),
    distance >= (adj_yd_line - 17),
    distance >= adj_yd_line
  )) %>% filter(log_ydstogo != -Inf)
  return(df)
}

epa_fg_probs <- function(dat, current_probs, fg_model) {
  fg_ind = str_detect((dat$play_type), "Field Goal")
  fg_dat = dat[fg_ind, ]

  # we are setting everythign after 0 seconds to have
  # 0 probs.
  end_game_ind = which(dat$TimeSecsRem <= 0)
  current_probs[end_game_ind, ] <- 0

  make_fg_prob <- mgcv::predict.bam(fg_model, newdata = fg_dat,
                                    type = "response")

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
    make_fg_prob + current_probs[fg_ind, "Opp FG"]
  current_probs2[fg_ind, "Opp FG"] <- current_probs[fg_ind, "FG"]
  current_probs2[fg_ind, "TD"] <- current_probs[fg_ind, "Opp TD"]
  current_probs2[fg_ind, "Opp TD"] <- current_probs[fg_ind, "TD"]
  current_probs2[fg_ind, "Safety"] <-
    current_probs[fg_ind, "Opp Safety"]
  current_probs2[fg_ind, "Opp Safety"] <-
    current_probs[fg_ind, "Safety"]
  return(current_probs2)
}

prep_df_epa2 <- function(dat){
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception',
    'Punt'
  )


  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      new_id = gsub(pattern=unique(game_id),"",x=id_play),
      new_id = as.numeric(new_id),
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      log_ydstogo = log(adj_yd_line),
      half = ifelse(period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0
    )

  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0

  new_offense = !(dat$offense_play == lead(dat$offense_play))
  #fourth_down = dat$down == 4,  & fourth_down
  t_ind = turnover_ind | (new_offense)

  dat$turnover[t_ind] <- 1



  dat = dat %>% group_by(game_id,half) %>%
    dplyr::arrange(new_id,.by_group=TRUE) %>%
    mutate(
      new_down = lead(down),
      new_distance = lead(distance),
      new_yardline = lead(adj_yd_line),
      new_TimeSecsRem = lead(TimeSecsRem),
      new_log_ydstogo = log(new_yardline),
      new_Goal_To_Go = lead(Goal_To_Go),
      # new under two minute warnings
      new_Under_two = new_TimeSecsRem <= 120,
      end_half_game=0) %>% ungroup() %>%
    mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0))

  end_of_half_plays = is.na(dat$new_yardline) & (dat$new_TimeSecsRem==0)
  if(any(end_of_half_plays)){
    dat$new_yardline[end_of_half_plays] <- 99
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 99
    dat$end_half_game[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(99)
    dat$new_Under_two[end_of_half_plays] <- dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }

  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 99
  dat$new_log_ydstogo[missing_yd_line] = log(99)

  dat = dat %>% select(
    id_play,
    new_id,
    game_id,
    drive_id,
    new_TimeSecsRem,
    new_down,
    new_distance,
    new_yardline,
    new_log_ydstogo,
    new_Goal_To_Go,
    new_Under_two,
    end_half_game,
    turnover
  )
  colnames(dat) = gsub("new_","",colnames(dat))
  colnames(dat)[8] <- "adj_yd_line"
  colnames(dat)[2] <- "new_id"

  return(dat)
}
