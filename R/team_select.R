#' Team Select
#'
#' Creates a custom S3 object representing a subset of data
#' for a specific team and year.
#'
#' @param team A character string representing the team name.
#'
#' @param year An integer representing the year.

#' @return A custom S3 object of class 'teamdata' containing the subsetted data.
#'
#' @examples selected_team <- team_select("Baylor", 2024)
#' print(selected_team)
#'
#' @export
team_select <- function(team, year) {
  # Load the dataset
  dataset <- read.csv("initial_clean.csv")

  # Format the team name input
  format_team_name <- function(team) {
    words <- tolower(strsplit(team, " ")[[1]])
    words <- ifelse(words == "of", "of",
                    ifelse(words == "state", "St.",
                           ifelse(nchar(words) >= 3,
                                  sub("^(.)", "\\U\\1", tolower(words), perl = TRUE),
                                  sub("st$", "St.", tolower(words)))))
    paste(words, collapse = " ")
  }

  # Apply formatting to the team name
  formatted_team <- format_team_name(team)

  # Check if the year is valid
  if (year < 2008) {
    stop("Error: Data only goes back as far as 2008.")
  }

  # Check if the formatted team name exists in the dataset
  if (!formatted_team %in% as.character(dataset$TEAM)) {
    stop("Error: Team name not found. Please check spelling. Note: Do not use abreviations for schools. Package will only correct capitalization erros and will automatically abbreviate state to St.")
  }

  # Check if the selected year is available for the team
  available_years <- unique(dataset$YEAR[dataset$TEAM == formatted_team])
  if (!year %in% available_years) {
    cat("The selected year is not available for", formatted_team, "\n")
    cat("Please select an available year or choose 'Exit' to cancel:\n")
    # Adding an 'Exit' option to the menu
    available_years <- c(available_years, "Exit")
    choice <- menu(available_years, title = "Available Years")

    # Check if user chooses to exit
    if (choice == length(available_years)) {
      cat("Exiting selection.\n")
      return(NULL)
    } else {
      year <- available_years[choice]
    }
  }

  # Subset the dataset based on the provided team and year
  subset_data <- dataset[as.character(dataset$TEAM) ==
    formatted_team & dataset$YEAR == year, ]

  # Return the subsetted data
  # Also this won't be as messy bc we clean our data to only fit what we need
  return(subset_data)
}
