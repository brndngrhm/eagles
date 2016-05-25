#eagles

#packages
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(highcharter)
library(rvest)

#load 2015 data
season <- agg_player_season(2015)
names(season) <-  tolower(names(season))
season <- season %>% filter(team == "PHI")
players <- season %>% select(name, playerid)
