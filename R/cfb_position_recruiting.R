#' CFB Recruiting Information - Position Groups
#'
#' If only start_year is provided, function will get cfb recruiting information based
#' on position groups during that year for all FBS teams.
#'
#' @param start_year Year (*Integer*, 4 digits YYYY, optional)
#' @param end_year End of time period - 2020 is max value currently (*Integer* 4 digits YYYY, optional)
#' @param team Team information (*String* optional)
#' @param conference Conference to filter on, abbreviated (*String* optional)
#'
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'
#' cfb_position_recruiting(2018)
#'
#' cfb_position_recruiting(2018, team="Texas")
#'
#' cfb_position_recruiting(2016, 2020, team="Virginia")
#'
#' cfb_position_recruiting(2015, 2020, conference = "SEC")
#'


cfb_position_recruiting <- function(start_year = NULL, end_year = NULL,
                                    team = NULL, conference = NULL){
  if(!is.null(start_year) & !is.null(end_year)){
    # by position groups
    base_url = "https://api.collegefootballdata.com/recruiting/groups?startYear="
    full_url = paste0(base_url, start_year,
                      '&endYear=', end_year)

    if(!is.null(team) & is.null(conference)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)
      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & !is.null(conference)){
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&conference=',conference)

      df = fromJSON(full_url)
      return(df)
    }
    if(!is.null(team) & !is.null(conference)){
      team = URLencode(team, reserved = T)
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&team=',team,'&conference=',conference)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & is.null(conference)){
      df = fromJSON(full_url)
      return(df)
    }
  }
  if(is.null(start_year) & !is.null(end_year)){
    # by position groups
    base_url = "https://api.collegefootballdata.com/recruiting/groups?endYear="
    full_url = paste0(base_url,
                      end_year)
    if(!is.null(team) & is.null(conference)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & !is.null(conference)){
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&conference=',conference)

      df = fromJSON(full_url)
      return(df)
    }
    if(!is.null(team) & !is.null(conference)){
      team = URLencode(team, reserved = T)
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&team=',team,'&conference=',conference)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & is.null(conference)){
      df = fromJSON(full_url)
      return(df)
    }
  }
  if(!is.null(start_year) & is.null(end_year)){
    # by position groups
    base_url = "https://api.collegefootballdata.com/recruiting/groups?startYear="
    full_url = paste0(base_url,
                      start_year,
                      '&endYear=',
                      start_year)
    if(!is.null(team) & is.null(conference)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'&team=',team)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & !is.null(conference)){
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&conference=',conference)
      df = fromJSON(full_url)
      return(df)
    }
    if(!is.null(team) & !is.null(conference)){
      team = URLencode(team, reserved = T)
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&team=',team,'&conference=',conference)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & is.null(conference)){
      df = fromJSON(full_url)
      return(df)
    }
  }
  if(is.null(start_year) & is.null(end_year)){
    # by position groups
    base_url = "https://api.collegefootballdata.com/recruiting/groups?"
    full_url = base_url
    if(!is.null(team) & is.null(conference)){
      team = URLencode(team, reserved = T)
      full_url = paste0(full_url,'team=',team)

      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & !is.null(conference)){
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&conference=',conference)
      df = fromJSON(full_url)
      return(df)
    }
    if(!is.null(team) & !is.null(conference)){
      team = URLencode(team, reserved = T)
      conference = URLencode(conference, reserved = T)
      full_url = paste0(full_url,'&team=',team,'&conference=',conference)
      df = fromJSON(full_url)
      return(df)
    }
    if(is.null(team) & is.null(conference)){
      df = fromJSON(full_url)
      return(df)
    }
  }
}
