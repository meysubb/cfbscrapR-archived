#' Gets CFB recruiting information for a single year with filters available for team,
#' recruit type, state and position.
#'
#' If two years are provided, get cfb recruiting information based on position groups during that
#' time period for all FBS teams.
#'
#'
#' @param year Year (*Integer*, 4 digits YYYY, required)
#' @param team Team information (*String* optional)
#' @param recruit_type default API return is 'HighSchool', other options include 'JUCO'
#' or 'PrepSchool' (*String* optional) - For position group information
#' @param state Two letter State abbreviation (*String* optional)
#' @param position Position Group (*String* optional) - options include:
#'  * Offense: 'PRO', 'DUAL', 'RB', 'FB', 'TE',  'OT', 'OG', 'OC', 'WR'
#'  * Defense: 'CB', 'S', 'OLB', 'ILB', 'WDE', 'SDE', 'DT'
#'  * Special Teams: 'K', 'P'
#'
#' @keywords internal
#' @importFrom jsonlite "fromJSON"
#' @export
#' @examples
#'
#' cfb_recruiting(2018)
#'
#' cfb_recruiting(2018,team="Texas")
#'
#' cfb_recruiting(2016,recruit_type = 'JUCO')
#' cfb_recruiting(2020, recruit_type = 'HighSchool', position ='OT', state = 'FL')
#'


cfb_recruiting <- function(year, team = NULL, recruit_type = NULL,
                           state = NULL, position = NULL){
  base_url = paste0("https://api.collegefootballdata.com/recruiting/players?year=", year)
  if(!is.null(team) & is.null(recruit_type) & is.null(state) & is.null(position)){
    team = URLencode(team, reserved = T)
    full_url = paste0(base_url,"&team=",team)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & !is.null(recruit_type) & is.null(state) & is.null(position)){
    recruit_type = URLencode(recruit_type, reserved = T)
    full_url = paste0(base_url,
                      "&classification=",
                      recruit_type)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & is.null(recruit_type) & !is.null(state) & is.null(position)){
    state = URLencode(state, reserved = T)
    full_url = paste0(base_url,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & is.null(recruit_type) & is.null(state) & !is.null(position)){
    position = URLencode(position, reserved = T)
    full_url = paste0(base_url,
                      "&position=",
                      position)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & !is.null(recruit_type) & is.null(state) & is.null(position)){
    team = URLencode(team, reserved = T)
    recruit_type = URLencode(recruit_type, reserved = T)
    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&classification=",
                      recruit_type)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & is.null(recruit_type) & !is.null(state) & is.null(position)){
    team = URLencode(team, reserved = T)
    state = URLencode(state, reserved = T)
    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & is.null(recruit_type) & is.null(state) & !is.null(position)){
    team = URLencode(team, reserved = T)
    position = URLencode(position, reserved = T)
    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&position=",
                      position)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & !is.null(recruit_type) & !is.null(state) & is.null(position)){
    recruit_type = URLencode(recruit_type, reserved = T)
    state = URLencode(state, reserved = T)

    full_url = paste0(base_url,
                      "&classification=",
                      recruit_type,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & !is.null(recruit_type) & is.null(state) & !is.null(position)){
    recruit_type = URLencode(recruit_type, reserved = T)
    position = URLencode(position, reserved = T)

    full_url = paste0(base_url,
                      "&classification=",
                      recruit_type,
                      "&position=",
                      position)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & is.null(recruit_type) & !is.null(state) & !is.null(position)){
    state = URLencode(state, reserved = T)
    position = URLencode(position, reserved = T)
    full_url = paste0(base_url,
                      "&position=",
                      position,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & !is.null(recruit_type) & !is.null(state) & is.null(position)){
    team = URLencode(team, reserved = T)
    recruit_type = URLencode(recruit_type, reserved = T)
    state = URLencode(position, reserved = T)

    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&classification=",
                      recruit_type,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & !is.null(recruit_type) & is.null(state) & !is.null(position)){
    team = URLencode(team, reserved = T)
    recruit_type = URLencode(recruit_type, reserved = T)
    position = URLencode(position, reserved = T)

    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&classification=",
                      recruit_type,
                      "&position=",
                      position)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & is.null(recruit_type) & !is.null(state) & !is.null(position)){
    team = URLencode(team, reserved = T)
    position = URLencode(position, reserved = T)
    state = URLencode(state, reserved = T)
    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&position=",
                      position,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(is.null(team) & !is.null(recruit_type) & !is.null(state) & !is.null(position)){
    recruit_type = URLencode(recruit_type, reserved = T)
    position = URLencode(position, reserved = T)
    state = URLencode(state, reserved = T)
    full_url = paste0(base_url,
                      "&classification=",
                      recruit_type,
                      "&position=",
                      position,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  if(!is.null(team) & !is.null(recruit_type) & !is.null(state) & !is.null(position)){
    team = URLencode(team, reserved = T)
    recruit_type = URLencode(recruit_type, reserved = T)
    position = URLencode(position, reserved = T)
    state = URLencode(state, reserved = T)
    full_url = paste0(base_url,
                      "&team=",
                      team,
                      "&classification=",
                      recruit_type,
                      "&position=",
                      position,
                      "&state=",
                      state)
    df = fromJSON(full_url)
    return(df)
  }
  df = fromJSON(base_url)
  return(df)
}
