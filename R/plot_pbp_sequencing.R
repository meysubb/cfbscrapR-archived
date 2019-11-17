#' Plot the Drive/PBP Sequencing
#'
#'
#' @param df Play-by-Play data frame (can be retrieved from cfb_pbp_data)

#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import ggrepel
#' @export
#' @examples
#'
#'
#'
#' df = cfb_pbp_data(year=2019,week=9,team='Texas A&M')
#'
#' plot_pbp_sequencing(df)
#'

plot_pbp_sequencing <- function(df){
  clean_game_df = prep_df_pbp_overview(df)
  clean_game_df$new_drive_id = as.numeric(gsub(unique(clean_game_df$game_id), "", clean_game_df$drive_id))

  clean_drive_info = clean_game_df %>% group_by(drive_id) %>%
    filter(row_number() == (n()-1)) %>% ungroup() %>%
    mutate(
      y_max = max(play_num) + 5,
      score_text = ifelse(scoring == TRUE, score_text, NA)
    )

  nd_id = clean_drive_info$new_drive_id
  off_team = clean_drive_info$offense.x

  ggplot() +
    geom_tile(
      data = clean_game_df,
      aes(
        x = new_drive_id,
        y = play_num,
        fill = clean_play_type,
        alpha = yards_gained,
        width = 0.8
      ),
      size = 1.2
    ) +
    geom_text_repel(
      data = clean_game_df,
      aes(x = new_drive_id, y = play_num, label = event),
      point.padding = NA
    ) +
    geom_label_repel(
      data = clean_drive_info,
      aes(x = new_drive_id, y = y_max, label = score_text),
      point.padding = NA
    ) +
    coord_flip() +
    scale_alpha(
      range = c(0.05, 1),
      limits = c(-20, 20),
      breaks = c(-20, -10, 0, 10, 20),
      labels = c("-20+", "-10", "0", "10", "20+"),
      name = "Yards gained"
    ) +
    scale_x_reverse(labels = off_team,
                    breaks = nd_id,
                    expand = expand_scale(add = 1.2)) +
    labs(
      x = "",
      y = "",
      fill = "Play type",
      alpha = "Yards gained",
      title = "Play-by-Play Overview",
      subtitle = paste0(unique(clean_game_df$away),"@",unique(clean_game_df$home)),
      caption = "@CFB_Data | @msubbaiah1"
    ) +
    theme_classic(base_size = 16)




}

prep_df_pbp_overview <- function(df) {
  clean_df = df %>%
    mutate(
      event = case_when(
        str_detect(play_text, "fumble") ~ "Fumble",
        str_detect(play_text, "interception") ~ "INT",
        str_detect(play_text, "sack") ~ "Sack",
        TRUE ~ "nothing"
      ),
      event = ifelse(event == "nothing", NA, event),
      clean_play_type = case_when(
        str_detect(play_type, "Pass") ~ "Pass",
        str_detect(play_type, "Rush") ~ "Rush",
        str_detect(play_type, "Field Goal") ~ "Kick",
        str_detect(play_type, "Kickoff") ~ "Kick",
        str_detect(play_type, "Punt") ~ "Punt",
        str_detect(play_type, "Penalty") ~ "Penalty",
        TRUE ~ "Other"
      ),
      score_text = paste0(offense_score, "-", defense_score)
    ) %>% group_by(drive_id) %>%
    mutate(play_num = row_number())  %>% ungroup()

}

