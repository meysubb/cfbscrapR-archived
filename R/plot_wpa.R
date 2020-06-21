#' Plot the WPA for a specific game
#'
#'
#' @param df Play-by-Play data frame (can be retrieved from cfb_pbp_data)

#' @keywords Plot WPA
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import ggrepel
#' @import tidyr
#' @export
#' @examples
#'
#'
#'
#'
#' df = cfb_pbp_data(year=2019,week=12,team='Baylor',epa_wpa = T)
#' plot_wpa(df,away_color=c(BU="#003015"),home_color=c(OU="#841617"),winner="away")


plot_wpa <- function(dat,game_id=NULL,away_color,home_color,winner="home"){
  ## check to see if game id is fed, otherwise
  ##
  away_team <- names(away_color)
  home_team <- names(home_color)
  names(away_color) <- NULL
  names(home_color) <- NULL

  plot_df <- dat %>% select(home_wp,away_wp,adj_TimeSecsRem)
  dups = duplicated(plot_df$adj_TimeSecsRem)
  if(any(dups)){
    print("Warning. Time was not recorded properly for this PBP")
    plot_df[dups,"adj_TimeSecsRem"] = plot_df[dups,"adj_TimeSecsRem"] - (cumsum(dups)*2 + 5)
  }

  plot_df <- rbind(c(0.5,0.5,3600),plot_df)
  if(winner=="away"){
    plot_df <- rbind(c(0,1,0),plot_df)
  }
  if(winner=="home"){
    plot_df <- rbind(c(1,0,0),plot_df)
  }
  plot_df <- plot_df %>% gather(team,wp,-adj_TimeSecsRem)


  p1 = ggplot(plot_df,aes(x=adj_TimeSecsRem,y=wp,color=team)) +
    geom_line(size=2) +
    geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
    scale_x_reverse(breaks = seq(0, 3600, 300)) +
    scale_color_manual(labels = c(away_team,home_team),
                       values = c(away_color,home_color),
                       guide = FALSE)  +
    annotate("text", x = 3300, y = 0.1,
             label = away_team, color = away_color, size = 8) +
    annotate("text", x = 3000, y = 0.1,
             label = paste(" @",home_team), color = home_color, size=8) +
    scale_y_continuous(limits=c(0,1)) +
    geom_vline(xintercept = 900, linetype = "dashed", color= "black") +
    geom_vline(xintercept = 1800, linetype = "dashed",color= "black") +
    geom_vline(xintercept = 2700, linetype = "dashed", color= "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color= "black") +
    labs(
      x = "Time Remaining (seconds)",
      y = "Win Probability",
      title = paste("Win Probability Chart"),
      subtitle = paste(home_team,"vs",away_team),
      caption = "Data from collegefootballdataAPI, Plot by @msubbaiah1"
    ) + theme_bw(base_size = 16)
  return(p1)
}
