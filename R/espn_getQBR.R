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
espn_getQBR <- function(x = c('player-season', 'player-week','team-season',
                              'alltime-season','alltime-game'),
                   qlf = TRUE,
                   yr = format(Sys.Date(), '%Y'),
                   wk = NULL) {

  x <- match.arg(x)

  espn <- 'http://espn.go.com/ncf/qbr/_/type/'

  if(is.null(wk)){
    url = paste0(espn, x, '/qualified/', tolower(qlf), '/year/', yr)
    pages = xml2::read_html(url) %>% html_nodes(".page-numbers") %>% html_text()
    pages =
    df <- url %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
  }
  if(!is.null(wk)){
    espn <- 'http://www.espn.com/college-football/qbr/_'
    url <= paste0(espn, '/qualified/', tolower(qlf), '/year/', yr,'/seasontype/2/type/player-week/week/',wk)
    df <- url %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
  }


  df$RK <- zoo::na.locf(df$RK)
  if(grepl("player", x) |
     grepl("alltime", x)) {
    df <- df %>% janitor::clean_names() %>%
      tidyr::separate(player, c("player", "team"), sep = ',') %>%
    mutate_at(vars(pass_epa:total_qbr), readr::parse_number)
    if (grepl("player-week", x)) {
      df = df %>%
        mutate(site = ifelse(grepl("vs", result), "H", "A"),
               result = gsub("@", "", gsub("vs", "", result))) %>% tidyr::separate(result, c("result", "team_pts", "opp_pts", "opponent")) %>%
        select(rk, player, team, opponent, site, result, everything()) %>%
        mutate_at(vars(team_pts:opp_pts), readr::parse_number)
    }
  }
  if(grepl("team-season",x)){
    df <- df %>% janitor::clean_names() %>%  mutate_at(vars(pass_epa:raw_qbr), readr::parse_number)
  }


  df

}
