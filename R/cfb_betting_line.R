#' Get betting line information
#'
#'
#'
#' @param season_type Select Season Type (regular,postseason,both)
#' @param year Select year, (example: 2018)
#' @param week Select week, this is optional (also numeric)
#' @param team Select team name (example: Texas, Texas A&M, Clemson)
#' @param conference Conference abbreviation filter
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @importFrom assertthat "assert_that"
#' @export
#' @examples
#'
cfb_betting_line <- function(year,week=1,team=NULL,conference=NULL,season_type='regular'){
  base_url = "https://api.collegefootballdata.com/lines?"
  # switch to paste 0, glue is a problem
  full_url = glue::glue(base_url,"year={year}&week={week}&seasonType={season_type}&team={team}",
                        year= year,
                        week = week,
                        season_Type = season_type,
                        team = team)


  if(!is.null(conference)){
    conf_check = conference %in% cfb_conf_types_df$abbreviation
    assert_that((conf_check) == T, msg = "Please provide an appropriate conference abberivation.")
    full_url = glue::glue(base_url,"year={year}&week={week}&seasonType={season_type}&conference={conference}")
  }
}


