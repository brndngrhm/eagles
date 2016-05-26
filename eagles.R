#eagles

#packages ----
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(highcharter)
library(rvest)
library(stringr)

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

#load 2015 data ----
season <- agg_player_season(2015)
names(season) <-  tolower(names(season))
season$team2 <- "other"
season$team2[season$team == "PHI"] <- "PHL"
season$name <- as.character(season$name)
write.csv(season, file = "~/R Working Directory/Other/eagles/season.csv")

#ideas
#offense: 
#QB: passing yards, passing attempts, yds/passing attempt, pass completions, complretion rate, ints, passing tds, rushing yards,  yds/rushing attempt,  rushing tds, qb rating (https://en.wikipedia.org/wiki/Passer_rating)
#RB: rushing yds, rushing att, rushing yds/att, rushing tds, carries, rush yds/game, runs of 20+ yds, receptions, recieving yds, recieving tds, fumbles
#WR: receptions, recieving yds, yds/recp, rec tds, rec/game, fumbles
#TE: receptions, recieving yds, yds/recp, rec tds, rec/game, fumbles
#K: field goals made, field goals att, xpmissed
#D: sacks, tackles, forced fumbles, int, assisted tackles

#offense ----
#passing attempts
pass.att <- season %>% filter(pass.att > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Attempts = sum(pass.att)) %>% ungroup() %>% arrange(desc(Passing.Attempts))
pass.att$rank <- c(1:nrow(pass.att))

(pass.att.plot <- highchart() %>%
  hc_add_series(name="Passing Attempts", data = subset(pass.att$Passing.Attempts, pass.att$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = pass.att$name, color = pass.att$coloring) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Attempts - Top 40 QBs", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#completions
pass.comp <- season %>% filter(pass.comp > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Completion = sum(pass.comp)) %>% ungroup() %>% arrange(desc(Passing.Completion))
pass.comp$rank <- c(1:nrow(pass.comp))

(pass.comp.plot <- highchart() %>%
  hc_add_series(name="Completed Passes", data = subset(pass.att$Passing.Attempts, pass.att$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = pass.att$name, color = pass.att$coloring) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Completed Passes - Top 40 QBs", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#passing yards
pass.yds <- season %>% filter(passyds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Yards = sum(passyds)) %>% ungroup() %>% arrange(desc(Passing.Yards))
pass.yds$rank <- c(1:nrow(pass.yds))

(pass.comp.plot <- highchart() %>%
  hc_add_series(name="Passing Yards", data = subset(pass.yds$Passing.Yards, pass.yds$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = pass.yds$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Yards- Top 40 QBs", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Passing Touchdowns
pass.tds <- season %>% filter(pass.tds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.tds = sum(pass.tds)) %>% ungroup() %>% arrange(desc(Passing.tds))
pass.tds$rank <- c(1:nrow(pass.tds))

(pass.comp.plot <- highchart() %>%
  hc_add_series(name="Passing Touchdowns", data = subset(pass.tds$Passing.tds, pass.tds$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = pass.tds$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Touchdowns- Top 40 QBs", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Interceptions thrown
pass.int <- season %>% filter(pass.ints > 0) %>% group_by(team,name,team2) %>% summarise(Passing.int = sum(pass.ints)) %>% ungroup() %>% arrange(desc(Passing.int))
pass.int$rank <- c(1:nrow(pass.int))

(pass.comp.plot <- highchart() %>%
  hc_add_series(name="Interceptions Thrown", data = subset(pass.int$Passing.int, pass.int$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = pass.int$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Interceptions Thrown - Top 40 QBs", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))


