#' Format Team Name
#'
#' Formats the team name as inputted by the user to account for slight errors
#' that may arrise from the user input.
#'
#' @param team A character string representing the team name as inputted by the
#' user.
#'
#' @return Properly formatted teamname to fit the dataset.
#'
#' @examples formatted_team <- format_team_name("utah st")
#' print(formatted_team)
#'
#' @export
format_team_name <- function(team) {
  # Normalize space and convert to lowercase
  team <- trimws(team)
  team <- tolower(team)  # Lowercase everything for consistency

  # Handle specific cases using a lookup table
  special_cases <- list(
    "loyola md" = "Loyola MD",
    "loyola maryland" = "Loyola MD",
    "liu brooklyn" = "LIU Brooklyn",
    "ut arlington" = "UT Arlington",
    "unc asheville" = "UNC Asheville",
    "mcneese" = "McNeese St.",
    "uc davis" = "UC Davis",
    "uc irvine" = "UC Irvine",
    "uc santa barbara" = "UC Santa Barbara",
    "miami fl" = "Miami FL",
    "miami florida" = "Miami FL"
  )

  # Lookup the formatted team name if it exists
  formatted_team <- special_cases[[team]]
  if (!is.null(formatted_team)) {
    return(formatted_team)
  }

  # Split into words
  words <- strsplit(team, " ")[[1]]

  # Handle 'Saint' Schools
  if (words[1] %in% c("saint", "st", "st.")) {
    words[1] <- "Saint"
  }

  # Handle Four-Letter Abbreviations
  abbreviations <- c("unlv", "utep", "utsa", "umbc")
  if (nchar(words[1]) == 4 && words[1] %in% abbreviations) {
    return(toupper(words[1]))
  }

  # Handle Three-Letter School Names
  if (nchar(words[1]) == 3 && length(words) == 1) {
    return(toupper(words[1]))
  }

  # Handle Miami (General Case)
  if (length(words) == 1 && words[1] == "miami") {
    warning("Note: 'Miami Ohio' is not included in the dataset. Referring to 'Miami FL'.")
    return("Miami FL")
  }

  # Apply proper capitalization rules
  words <- sapply(words, function(word) {
    if (word == "of") {
      return("of")  # Keep "of" lowercase
    } else if (word == "state") {
      return("St.")  # Convert "state" to "St."
    } else {
      return(tools::toTitleCase(word))  # Capitalize each word
    }
  })

  return(paste(words, collapse = " "))
}
