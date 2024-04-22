march.madness
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# march.madness

<!-- badges: start -->
<!-- badges: end -->

The goal of march.madness is to help you fill out your march madness
bracket. This package will help you make informed decisions on who to
pick.

## Installation

You can install the development version of march.madness from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("djperrenoud/march.madness")
```

## Example

To select a single team to view their season stats and use in functions:

``` r
library(march.madness)
selected_team <- team_select("Utah St.", 2024)
selected_team
#>    YEAR CONF     TEAM SEED ROUND  KADJ.T  KADJ.O  KADJ.D BADJ.EM BADJ.O  BADJ.D GAMES  W L
#> 61 2024  MWC Utah St.    8     0 68.5319 115.617 100.502  13.786 114.15 100.364    31 25 6
#>        WIN. EFG. EFG.D FTR FTRD TOV. TOV.D OREB. DREB. OP.OREB. OP.DREB. RAW.T X2PT. X2PT.D
#> 61 80.64516 54.1  49.6  40 30.4 15.7  16.4  28.5  74.6     25.4     71.5  69.1  57.1   53.1
#>    X3PT. X3PT.D BLK. BLKED. AST. OP.AST. X2PTR X3PTR X2PTRD X3PTRD BADJ.T AVG.HGT EFF.HGT
#> 61  32.1     29 11.1    5.9 56.4      49  66.6  33.4   63.3   36.7  68.51  77.278  80.736
#>      EXP TALENT  FT. OP.FT.  PPPO PPPD
#> 61 1.918 21.975 71.2     72 1.125 1.02
```

You can also select a matchup of two teams to compare:

``` r
selected_matchup <- matchup_select("Utah St.", 2024, "Purdue", 2024)
selected_matchup
#> $list_data
#> $list_data$team1_year1
#>    YEAR CONF     TEAM SEED ROUND  KADJ.T  KADJ.O  KADJ.D BADJ.EM BADJ.O  BADJ.D GAMES  W L
#> 61 2024  MWC Utah St.    8     0 68.5319 115.617 100.502  13.786 114.15 100.364    31 25 6
#>        WIN. EFG. EFG.D FTR FTRD TOV. TOV.D OREB. DREB. OP.OREB. OP.DREB. RAW.T X2PT. X2PT.D
#> 61 80.64516 54.1  49.6  40 30.4 15.7  16.4  28.5  74.6     25.4     71.5  69.1  57.1   53.1
#>    X3PT. X3PT.D BLK. BLKED. AST. OP.AST. X2PTR X3PTR X2PTRD X3PTRD BADJ.T AVG.HGT EFF.HGT
#> 61  32.1     29 11.1    5.9 56.4      49  66.6  33.4   63.3   36.7  68.51  77.278  80.736
#>      EXP TALENT  FT. OP.FT.  PPPO PPPD
#> 61 1.918 21.975 71.2     72 1.125 1.02
#> 
#> $list_data$team2_year2
#>    YEAR CONF   TEAM SEED ROUND  KADJ.T  KADJ.O KADJ.D BADJ.EM  BADJ.O BADJ.D GAMES  W L
#> 47 2024  B10 Purdue    1     0 67.3784 124.988  95.87  31.493 126.212 94.719    33 29 4
#>        WIN. EFG. EFG.D  FTR FTRD TOV. TOV.D OREB. DREB. OP.OREB. OP.DREB. RAW.T X2PT.
#> 47 87.87879   56  47.7 42.8   23 16.5    14  37.4  75.3     24.7     62.6    69  53.2
#>    X2PT.D X3PT. X3PT.D BLK. BLKED. AST. OP.AST. X2PTR X3PTR X2PTRD X3PTRD BADJ.T AVG.HGT
#> 47   48.1  40.8   31.4  9.6      6 64.5    55.1    65    35   62.8   37.2 67.599   78.09
#>    EFF.HGT   EXP TALENT  FT. OP.FT.  PPPO  PPPD
#> 47  83.245 1.864 52.594 72.1   72.4 1.196 1.006
#> 
#> 
#> attr(,"class")
#> [1] "teamdata_matchup"
```

To visualize the season summary of a team:

``` r
team_table(selected_team)

stat_table(selected_team)
```

To visualize the matchup of two teams:

``` r
matchup_chart(selected_matchup)
```

To view the power of each team and the probability of each team winning:

``` r
matchup_prob(selected_matchup)
#> $Team1_Power_Index
#> [1] 1190.25
#> 
#> $Team2_Power_Index
#> [1] 1335.59
#> 
#> $Probability_of_Team1_Win
#> [1] "19.56%"
#> 
#> $Probability_of_Team2_Win
#> [1] "80.44%"
#> 
#> $Prediction
#> [1] "Team 2"
```
