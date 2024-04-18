#' Create New teamdata Object
#'
#' Creates an S3 object representing a subset of data for a
#' specific team and year.
#'
#' @param data_frame A data frame containing the subsetted data.
#'
#' @return A custom S3 object of class 'teamdata' containing the subsetted data.
#'
#' @export
new_teamdata <- function(data_frame) {
  stopifnot(is.data.frame(data_frame))
  structure(list(data_frame = data_frame), class = "teamdata")
}

validate_teamdata <- function(data_frame) {
}

teamdata <- function(data_frame) {
  validate_teamdata(data_frame)
  new_teamdata(data_frame)
}

#' Create New teamdata_matchup Object
#'
#' Creates an S3 object representing a list of data for two
#' specific teams and years.
#'
#' @param list_data A list containing the subsetted data.
#'
#' @return A custom S3 object of class 'teamdata_matchup'
#' containing the subsetted data.
#'
#' @export
new_teamdata_matchup <- function(list_data) {
  stopifnot(is.list(list_data))
  structure(list(list_data = list_data), class = "teamdata_matchup")
}

validate_teamdata <- function(list_data) {
}

teamdata <- function(list_data) {
  validate_teamdata(list_data)
  new_teamdata_matchup(list_data)
}
