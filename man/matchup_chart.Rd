% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchup_viz.R
\name{matchup_chart}
\alias{matchup_chart}
\title{Matchup Vis}
\usage{
matchup_chart(teamdata_matchup)
}
\arguments{
\item{teamdata_matchup}{A list of two dataframes, each containing the
statistics for that team and year}
}
\value{
The bar chart described above.
}
\description{
This function takes two teams and compares four important statistics
(Efficiency Field Goals, Free Throw Rate, Offensive Rebounds, and Turnover
Rate) to give insights on which team is better. The output of the function is
a bar chart showing side by side percentages for each team's metrics.
}
