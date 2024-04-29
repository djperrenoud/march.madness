#' Training dataset
#'
#' The results of the 2021 NCAA tournament that we can use to train our model
#' and see which statistics historically have impacted winning the most.
#'
#' @format ## `example_results`
#' A data frame with 63 rows and 5 columns:
#' \describe{
#'   \item{Team1}{Winnig team}
#'   \item{Team2}{Losing team}
#'   \item{Year}{Year}
#'   ...
#' }
"example_results"





#' Initial Cleaning of our data
#'
#' A subset of data where we only kepy columns with numeric statistics that can
#' be used to weight each other and see which affects winning or losing the
#' most.
#'
#' @format ## `dataset`
#' A data frame with 1,079 rows and 47 columns:
#' \describe{
#'   \item{TEAM}{Team name}
#'   \item{YEAR}{Year}
#'   ...
#' }
"dataset"
