#eagles

#packages ----
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(highcharter)
library(rvest)
library(stringr)

#load 2015 data ----
season <- agg_player_season(2015)
names(season) <-  tolower(names(season))
season <- season %>% filter(team == "PHI")
players <- season %>% select(name, playerid)

#get roster, assign players to offense, defense, st ----
#will need to add to total when 2016 season starts
roster.2016x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/roster_2016.csv")
roster <- as.data.frame(read.csv(text=roster.2016x, strip.white = T, stringsAsFactors = F))
roster$mode <- "defense"
roster$mode[roster$pos == "C"] <- "offense"
roster$mode[roster$pos == "OG"] <- "offense"
roster$mode[roster$pos == "OT"] <- "offense"
roster$mode[roster$pos == "QB"] <- "offense"
roster$mode[roster$pos == "WR"] <- "offense"
roster$mode[roster$pos == "TE"] <- "offense"
roster$mode[roster$pos == "T"] <- "offense"
roster$mode[roster$pos == "G"] <- "offense"
roster$mode[roster$pos == "RB"] <- "offense"
roster$mode[roster$pos == "k"] <- "st"
roster$mode[roster$pos == "p"] <- "st"


  

#example ----
season %>% filter(forced.fumbs > 0) %>% group_by(name) %>% 
  summarise(forced.fumbs = sum(forced.fumbs))%>% 
  ungroup() %>% arrange(desc(forced.fumbs))