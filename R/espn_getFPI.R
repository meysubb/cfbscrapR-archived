#' Get the ESPN Football Power Index
#'
#' Extracts an NCAAF Football Power Index ranking from ESPN.com and
#' returns it as a data frame.
#' @param yr Year of the requested rankings
#' @param wk Week of season through which the index should be requested
#' @return A data frame of the requested leaderboard
#' @import magrittr
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' # Capture Week 2 FPI rankings
#' espn_getFPI(2015, wk = 2)
espn_getFPI <- function(yr = format(Sys.Date(), '%Y'), wk = NULL) {

  espn <- 'http://espn.go.com/college-football/statistics/teamratings/_'

  url <- paste0(espn, '/year/', yr)

  df <-
    paste0(url, '/key/', weekKey(wk, url)) %>%
    httr::GET(.)

  df = rawToChar(df$content) %>%
    XML::readHTMLTable(as.data.frame = TRUE,
                       stringsAsFactors = FALSE) %>%
    .[[1]] %>%
    magrittr::set_names(value = .[.[1] == 'RK',][1,])

  names(df) <- gsub('[ /-]+', '.', names(df))
  names(df) <- gsub('[%]+', '.PCT', names(df))

  df[, 1] <- suppressWarnings(as.numeric(df[, 1]))


  df <- df[!is.na(df[1]), ]

  team <- strsplit(df$TEAM, ', ')
  rec  <- strsplit(df$W.L, '-')
  proj <- strsplit(df$PROJ.W.L, '[ ]*-[ ]*')

  df <- df %>% mutate(
    TEAM = sapply(team, '[[', 1),
    CONF = sapply(team, '[[', 2),
    WIN = sapply(rec,  '[[', 1),
    LOSS = sapply(rec,  '[[', 2),
    PROJ.W = sapply(proj, '[[', 1),
    PROJ.L = sapply(proj, '[[', 2)
  )

  ord <-   c('RK', 'TEAM', 'CONF', 'WIN', 'LOSS', 'PROJ.W', 'PROJ.L',
             'WIN.OUT.PCT', 'CONF.WIN.PCT', 'REM.SOS.RK', 'FPI')

  df <- df[ , ord]

  df <- df %>% mutate_at(vars(WIN:FPI), readr::parse_number) %>% janitor::clean_names()
  # for (i in c(4:length(df))) {
  #   df[ , i] <- suppressWarnings(as.numeric(df[ , i]))
  # }

  df

}

#' Scrape key attribute for FPI week
#'
#' @return Character string containing url key
#' @keywords internal
weekKey <- function(wk, url) {

  ddbx <-
    xml2::read_html(url) %>%
    rvest::html_nodes('.tablesm option') %>%
    rvest::html_attrs() %>%
    rvest::pluck(1)

  wk <- if (wk >= length(ddbx) || is.null(wk)) length(ddbx) - 1 else wk

  key <-
    ddbx %>%
    .[[length(.) - wk]] %>%
    strsplit('/key/') %>%
    sapply('[[', 2)

  key

}
