#' Gets Historical CFB poll rankings at a specific week
#'
#' Postseason polls are after Week 13
#'
#' @param year Year
#' @param week Week
#'
#'#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom purrr "flatten"
#' @importFrom tidyr "unnest"
#' @import dplyr
#' @export
#' @examples
#'
#' cfb_rankings(2018,1)
#'
#' cfb_rankings(2018,14)



cfb_rankings <- function(year,week){
  base_url = url = "https://api.collegefootballdata.com/rankings?year="

  url = paste0(base_url,year,"&week=",week)

  raw_lst = fromJSON(url) %>% purrr::flatten()

  polls_info = raw_lst$polls %>% tidyr::unnest(cols = c(ranks)) %>%
    group_by(poll) %>% arrange(rank,.by_group=TRUE) %>% ungroup()
  
  return(polls_info)
}
