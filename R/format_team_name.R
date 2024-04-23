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
  # Normalize space and convert to lowercase to handle case variations
  team <- trimws(team) # Remove any leading/trailing white space
  words <- strsplit(team, " ")[[1]]
  words <- tolower(words)  # Lowercase all to handle variations

  # Handle specific cases
  if (length(words) >= 2) {
    if (words[1] == "loyola" && (words[2] == "md" || words[2] == "maryland")) {
      return("Loyola MD")
    } else if (words[1] == "liu" && words[2] == "brooklyn") {
      return("LIU Brooklyn")
    } else if (words[1] == "ut" && words[2] == "arlington") {
      return("UT Arlington")
    } else if (words[1] == "mount" && (words[2] == "saint" || words[2] == 'st' || words[2] == 'st.') && (words[3] == "marys" || words[3] == "mary's")) {
      return("Mount St. Mary's")
    } else if (words[1] == "uc" && words[2] == "davis") {
      return("UC Davis")
    } else if (words[1] == "uc" && words[2] == "irvine") {
      return("UC Irvine")
    } else if (words[1] == "uc" && words[2] == "santa" && words[3] == "barbara") {
      return("UC Santa Barbara")
    } else if (words[1] == "miami" && (words[2] == fl || words[2] == florida)) {
      return("Miami FL")
    }
  }

  # Handle Specific Four Letter Abbreviations
  if (nchar(words[1]) == 4 && length(words) == 1){
    if (words[1] == "unlv"){
      return("UNLV")
    } else if (words[1] == "utep") {
      return("UTEP")
    } else if (words[1] == "utsa") {
      return("UTSA")
    }
  }

  # Handle three-letter school names
  if (nchar(words[1]) == 3 && length(words) == 1) {
    return(toupper(words[1]))
  }

  # Handle the Miami case
  if (length(words) == 1 && words[1] == "miami") {
    warning("Note: 'Miami Ohio' is not included in the dataset. Referring to 'Miami FL'.")
    return("Miami FL")
  }

  # Apply general capitalization rules
  words <- ifelse(words == "of", "of",
                  ifelse(words == "state", "St.",
                         ifelse(nchar(words) >= 3,
                                sub("^(.)", "\\U\\1", words, perl = TRUE),
                                sub("st$", "St.", words))))

  return(paste(words, collapse = " "))
}