#' College Football Mapping for Play Types
#'
#' This dataframes helps identifies all play types identified in the play by play data.
#' This can be used to filter out play types when calling functions before hand.
#'
#'
#' @format A data frame with 45 rows and 3 variables:
#' \describe{
#'   \item{id}{Referencing play id}
#'   \item{text}{play type description}
#'   \item{abberivation}{play type abberivation used for function call}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com}
"cfb_play_type_df"


#' College Football Conference
#'
#' This dataframes is used internally to identify conference names and use the abberivation in the function call for data
#'
#'
#' @format A data frame with 11 rows and 4 variables:
#' \describe{
#'   \item{id}{Referencing conference id}
#'   \item{name}{Conference name}
#'   \item{short_name}{Short name for Conference}
#'   \item{abberivation}{Conference abberivation}
#'   ...
#' }
#' @source \url{https://api.collegefootballdata.com}
"cfb_conf_types_df"
