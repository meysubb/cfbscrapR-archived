#' Get NCAAF quarterback ratings
#'
#' Extracts a period of NCAAF QBR rankings from ESPN.com and returns it
#' as a data frame.
#' @param x Type of the requested rankings
#' @param qlf Whether rankings should use qualified ratings
#' @param yr Year of the requested rankings
#' @return A data frame of the requested rankings
#' @import magrittr
#' @import rvest
#' @import janitor
#' @import dplyr
#' @export
#' @examples
#' # Capture QBR single game rankings for 2013
#' espn_getQBR('player-game', yr = 2013)
espn_getQBR <- function(x = c('player-season', 'player-week','team-season',
                              'alltime-season','alltime-game'),
                   qlf = TRUE,
                   yr = format(Sys.Date(), '%Y'),
                   wk = NULL) {

  x <- match.arg(x)

  espn <- 'http://espn.go.com/ncf/qbr/_/type/'

  if(is.null(wk)){
    url = paste0(espn, x, '/year/', yr)
    pages = xml2::read_html(url) %>% html_nodes(".page-numbers") %>% html_text()
    pages = as.numeric(stringr::str_extract_all(pages,"\\(?[0-9.]+\\)?")[[1]])
    max_page = max(pages)
    df <- url %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
    for(i in seq(2,max_page)){
        url2 = paste0(url,"/page/",i)
        temp_df <- url2 %>% XML::readHTMLTable(as.data.frame = TRUE,
                                               stringsAsFactors = FALSE) %>%
          .[[1]] %>%
          .[.[1] != 'RK',]
        df = rbind(df,temp_df)
    }

  }
  if(!is.null(wk)){
    espn <- 'http://www.espn.com/college-football/qbr/_'
    url <- paste0(espn, '/year/', yr,'/seasontype/2/type/player-week/week/',wk)
    pages = xml2::read_html(url) %>% html_nodes(".page-numbers") %>% html_text()
    pages = as.numeric(stringr::str_extract_all(pages,"\\(?[0-9.]+\\)?")[[1]])
    max_page = max(pages)
    df <- url %>%
      XML::readHTMLTable(as.data.frame = TRUE,
                         stringsAsFactors = FALSE) %>%
      .[[1]] %>%
      .[.[1] != 'RK', ]
    for(i in seq(2,max_page)){
      url2 = paste0(url,"/page/",i)
      temp_df <- url2 %>% XML::readHTMLTable(as.data.frame = TRUE,
                                             stringsAsFactors = FALSE) %>%
        .[[1]] %>%
        .[.[1] != 'RK',]
      df = rbind(df,temp_df)
    }
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
