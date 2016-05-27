#eagles

#packages ----
library(dplyr)
library(ggplot2)
library(nflscrapR)
library(highcharter)
library(rvest)
library(stringr)
library(reshape2)
library(purrr)
library(lubridate)
library(xts)

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

#load historical data ----
player.game.2015 <- season_playergame(2015)
player.game.2014 <- season_playergame(2014)
player.game.2013 <- season_playergame(2013)
player.game.2012 <- season_playergame(2012)
player.game.2011 <- season_playergame(2011)
player.game.2010 <- season_playergame(2010)
player.game.2009 <- season_playergame(2009)
hist <- rbind(player.game.2015, player.game.2014, player.game.2013, player.game.2012, player.game.2011, player.game.2010, player.game.2009)
names(hist) <- tolower(names(hist))
eagles.hist <- hist %>% filter(team == "PHI")
write.csv(eagles.hist, file = "~/R Working Directory/Other/eagles/eagles.hist.csv")

hist.pass.att <- eagles.hist %>% filter(pass.att > 0) %>% group_by(date, name) %>% summarise(total  = sum(pass.att))
hist.pass.att$date <- ymd(hist.pass.att$date)

bradford.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "S.Bradford"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "S.Bradford"), frequency = 52)
vick.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Vick"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "M.Vick"), frequency = 52)
foles.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "N.Foles"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "N.Foles"), frequency = 52)
sanchez.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Sanchez"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "M.Sanchez"), frequency = 52)
barkley.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Barkley"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "M.Barkley"), frequency = 52)
smith.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "B.Smith"), order.by = subset(hist.pass.att$date,hist.pass.att$name == "B.Smith"), frequency = 52)

names(bradford.ts)[1] <- "Bradford"
names(vick.ts)[1] <- "Vick"
names(foles.ts)[1] <- "Foles"
names(sanchez.ts)[1] <- "Sanchez"
names(barkley.ts)[1] <- "Barkley"
names(smith.ts)[1] <- "Smith"

highchart(type = "chart") %>% 
  hc_add_series_xts(vick.ts, id = "Vick", name= "Vick") %>%
  hc_add_series_xts(foles.ts, id = "Foles", name = "Foles") %>%
  hc_add_series_xts(barkley.ts, id = "Barkley", name = "Barkley") %>%
  hc_add_series_xts(sanchez.ts, id = "Sanchez", name = "Sanchez") %>%
  hc_add_series_xts(bradford.ts, id = "Bradford") %>%
  hc_add_series_xts(smith.ts, id = "Smith", name = "Smith") %>%
  hc_rangeSelector(inputEnabled = T) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_gridlight()) %>% 
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T)%>%
  hc_title(text="Historical Passing Attempts", align="left") %>%
  hc_subtitle(text="2009-2015", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = T)

#ideas -----
#offense: 
#QB: passing yards, passing attempts, yds/passing attempt, pass completions, complretion rate, ints, passing tds, rushing yards,  yds/rushing attempt,  rushing tds, qb rating (https://en.wikipedia.org/wiki/Passer_rating)
#RB: rushing yds, rushing att, rushing yds/att, rushing tds, carries, rush yds/game, runs of 20+ yds, receptions, recieving yds, recieving tds, fumbles
#WR: receptions, recieving yds, yds/recp, rec tds, rec/game, fumbles
#TE: receptions, recieving yds, yds/recp, rec tds, rec/game, fumbles
#ST
#K: field goals made, field goals att, xpmissed
#DEFENSE
#D: sacks, tackles, forced fumbles, int, assisted tackles

#*!*!*! NEED TO FIGURE OUT HOW TO ADD TEAM, RANK TO TOOLTIP *!*!*!

#NFL LEVEL ----
#PLAYERS ----
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

(pass.yds.plot <- highchart() %>%
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

(pass.tds.plot <- highchart() %>%
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

(pass.int.plot <- highchart() %>%
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

#Rushing Attempts
rush.att <- season %>% filter(rush.att > 0) %>% group_by(team,name,team2) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
rush.att$rank <- c(1:nrow(rush.att))

(rush.att.plot <- highchart() %>%
  hc_add_series(name="Rushing Attempts", data = subset(rush.att$rush.att, rush.att$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = rush.att$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Attempts - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing Yards
rush.yds <- season %>% filter(rushyds > 0) %>% group_by(team,name,team2) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
rush.yds$rank <- c(1:nrow(rush.yds))

(rush.yds.plot <- highchart() %>%
  hc_add_series(name="Rushing Yards", data = subset(rush.yds$rush.yds, rush.yds$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = rush.yds$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Yards - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing Yards / Att
rush.yds.att <- season %>% select(rushyds, rush.att, team, name, team2) %>% filter(rush.att > (4*16)) %>% group_by(team,name,team2) %>% summarise(rush.yds.att = (rushyds/rush.att)) %>% ungroup() %>% arrange(desc(rush.yds.att))
rush.yds.att$rank <- c(1:nrow(rush.yds.att))
rush.yds.att$rush.yds.att <- round(rush.yds.att$rush.yds.att, 2)

(rush.yds.att.plot <- highchart() %>%
  hc_add_series(name="Rushing Yards/Attempt", data = subset(rush.yds.att$rush.yds.att, rush.yds$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = rush.yds.att$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Yards / Attempt - Top 40", align="left") %>%
  hc_subtitle(text="For players with at least 4 carriers per game, 2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing TDs
rush.tds <- season %>% filter(rushtds > 0) %>% group_by(team,name,team2) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
rush.tds$rank <- c(1:nrow(rush.tds))

(rush.tds.att.plot <- highchart() %>%
  hc_add_series(name="Rushing Touchdowns", data = subset(rush.tds$rush.tds, rush.tds$rank <=40), type = "bar")  %>%
  hc_xAxis(categories = rush.tds$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Touchdowns - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))
#defense ----
#sacks
sacks <- season %>% dplyr::filter(sacks > 0) %>% group_by(team,name,team2) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
sacks$rank <- c(1:nrow(sacks))

(sacks.plot <- highchart() %>%
  hc_add_series(name="Sacks", data = subset(sacks$sacks, sacks$rank <= 40), type = "bar")  %>%
  hc_xAxis(categories = sacks$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Sacks - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#tackles
tackles <- season %>% filter(tackles> 0) %>% group_by(team,name,team2) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
tackles$rank <- c(1:nrow(tackles))

(tackles.plot <- highchart() %>%
  hc_add_series(name="Tackles", data = subset(tackles$tackles, tackles$rank <= 40), type = "bar")  %>%
  hc_xAxis(categories = tackles$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Taclkes - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#assisted tackles
asst.tackles <- season %>% filter(asst.tackles> 0) %>% group_by(team,name,team2) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
asst.tackles$rank <- c(1:nrow(asst.tackles))

(asst.tackles.plot <- highchart() %>%
  hc_add_series(name="Assisted Tackles", data = subset(asst.tackles$asst.tackles, asst.tackles$rank <= 40), type = "bar")  %>%
  hc_xAxis(categories = asst.tackles$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Assisted Tackles - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Forced Fumbles
forced.fumbs <- season %>% filter(forced.fumbs> 0) %>% group_by(team,name,team2) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
forced.fumbs$rank <- c(1:nrow(forced.fumbs))

(tackles.plot <- highchart() %>%
  hc_add_series(name="Forced Fumbles", data = subset(forced.fumbs$forced.fumbs, forced.fumbs$rank <= 40), type = "bar")  %>%
  hc_xAxis(categories = forced.fumbs$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Forced Fumbles - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Defensive Interceptions
defints <- season %>% filter(defints > 0) %>% group_by(team,name,team2) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
defints$rank <- c(1:nrow(defints))

(def.ints.plot <- highchart() %>%
  hc_add_series(name="Defensive Interceptions", data = subset(defints$defints, defints$rank <= 40), type = "bar")  %>%
  hc_xAxis(categories = defints$name) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Defensive Interceptions - Top 40", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#TEAMS
#offense
#defense
#ST
