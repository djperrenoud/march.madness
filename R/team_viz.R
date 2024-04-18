#Function creates a simple table showing basic information about the team.
#This includes team name, year, conference, seed, the round they made it to,
#wins and losses.
team_table <- function(selected_team) {
  t_table <- datatable(selected_team$data_frame[, c(1:5, 13:14)])
  return(t_table)
}
