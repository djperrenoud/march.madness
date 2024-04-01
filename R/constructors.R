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
  new_carsimr(data_frame)
}
