#'Team Vis Functions
#'
#'These functions create simple tables showing basic information about teams,
#'as well as important statistics that act as indicators of success.
#'
#'Team_table
#'
#'This function creates a simple table showing basic information about the team.
#' This includes team name, year, conference, seed, the round they made it to,
#' wins and losses.
#'
#' @param selected_team a team_select object representing a dataframe for a team
#' in a given year.
#'
#' @return a data table with team name, year, conference, seed, round, wins and
#' losses.
#'
#' @examples team_table(team_select("Baylor", 2023))
#'
#' @export
team_table <- function(selected_team) {
  t_table <- datatable(selected_team[, c(1:5, 13:14)])
  return(t_table)
}

#'Stat_table
#'
#'This function creates a simple table showing important success indicators
#'for a team. These include efficiency field goals, free throw rate,
#'offensive rebounds, and turnover rate.
#'
#' @param selected_team a team_select object representing a dataframe for a team
#' in a given year.
#'
#' @return a data table with the statistics specified in the description.
#'
#' @examples stat_table(team_select("Baylor", 2023))
#'
#' @export
stat_table <- function(selected_team) {
  s_table <- datatable(selected_team[, c(1, 3, 4, 16, 18, 20, 22)])
  print(s_table)
}
