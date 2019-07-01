#' Get NCAAF quarterback ratings
#'
#' Extracts a period of NCAAF QBR rankings from ESPN.com and returns it
#' as a data frame.
#' @param x Type of the requested rankings
#' @param qlf Whether rankings should use qualified ratings
#' @param yr Year of the requested rankings
#' @return A data frame of the requested rankings
#' @import magrittr
#' @export
#' @examples
#' # Capture QBR single game rankings for 2013
#' getQBR('player-game', yr = 2013)
espn_getQBR <- function(x = c('player-season', 'player-week', 'player-game',
                         'team-season', 'alltime-season',
                         'alltime-game'),
                   qlf = TRUE,
                   yr = format(Sys.Date(), '%Y')) {

  x <- match.arg(x)

  espn <- 'http://espn.go.com/ncf/qbr/_/type/'

  if(is.null(wk)){
    df <-
      paste0(espn, x, '/qualified/', tolower(qlf), '/year/', yr) %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
  }
  if(!is.null(wk)){
    #http://www.espn.com/college-football/qbr/_/type/player-week/week/3
    df <-
      paste0(espn, x, '/qualified/', tolower(qlf), '/year/', yr) %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
  }


  df$RK <- zoo::na.locf(df$RK)
  if(grepl("player",x) | grepl("alltime",x)){

  }
  if(grepl("team-season",x)){
    df <- df %>% janitor::clean_names() %>%  mutate_at(vars(pass_epa:raw_qbr), readr::parse_number)
  }


  df

}
