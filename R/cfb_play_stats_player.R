#' Gets player info associated by play
#'
#' Information describes the players involved in the play
#' this includes passer, receiver, defensive players who
#' create sacks or picks, etc
#'
#' gameId can be extracted from cfb_game_info
#'
#'@param gameId Game ID
#'
#'#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom tidyr "pivot_wider"
#' @importFrom tidyr "unite"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_play_stats_player(401110723)


cfb_play_stats_player <- function(gameId){
  play_base_url <- "https://api.collegefootballdata.com/play/stats?gameId="
  url = paste0(play_base_url,gameId)
  raw_df = fromJSON(url)
  if(length(raw_df)==0){
    warning(paste0('There is no data in the underlying API call ',url))
    return(NA)
  }
  raw_df = raw_df[!duplicated(raw_df),]
  clean_df = pivot_wider(raw_df, names_from = statType, values_from = athleteName) %>%
    group_by(playId) %>%
    summarise_all(funs(first(na.omit(.)))) %>%
    ungroup() %>% select(-athleteId,-stat) %>%
    unite("Passer",Incompletion:Completion,remove=T,na.rm=T)

  return(clean_df)
}
