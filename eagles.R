# 2015 NFL & Eagles Team and Player stats
# Run "packages" code chunk then start at line 42 to replicate plots

# packages ----
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

# download 2015 data ----
season.2015 <- agg_player_season(2015)
names(season.2015) <-  tolower(names(season.2015))
season.2015$team2 <- "other"
season.2015$team2[season.2015$team == "PHI"] <- "PHL"
season.2015$name <- as.character(season.2015$name)
write.csv(season.2015, file = "~/R Working Directory/Other/eagles/season.2015.csv")

------------------------

# download historical data ----
player.game.2015 <- season_playergame(2015)
player.game.2014 <- season_playergame(2014)
player.game.2013 <- season_playergame(2013)
player.game.2012 <- season_playergame(2012)
player.game.2011 <- season_playergame(2011)
player.game.2010 <- season_playergame(2010)
player.game.2009 <- season_playergame(2009)
hist <- rbind(player.game.2015, player.game.2014, player.game.2013, player.game.2012, player.game.2011, player.game.2010, player.game.2009)
names(hist) <- tolower(names(hist))
eagles.hist <- hist %>% dplyr::filter(team == "PHI")
write.csv(eagles.hist, file = "~/R Working Directory/Other/eagles/eagles.hist.csv")

------------------------

# load data ----
season.2015.x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/season.2015.csv")
season.2015 <- as.data.frame(read.csv(text = season.2015.x, strip.white = T))
  
eagles.hist.x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/eagles.hist.csv")
eagles.hist <- as.data.frame(read.csv(text = eagles.hist.x, strip.white = T))

------------------------

# global plotting parameters ----

hc_params <- highchart() %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "NFL Data from nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE) %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ")

top.n <- 40          #how many names to show on axis
plot.type <- "bar"   #type of plot
alignment <- "left"  #subtitle alignment

--------------------------------------------------

# *!*!*! NEED TO FIGURE OUT HOW TO SHOW ALL NAMES/TEAMS ON AXIS & ADD TEAM + RANK TO TOOLTIP *!*!*!
# want to add qb rating, completions, time of possession, offensive plays, offensive/defensive time on field,  points scored, 
  

# NFL LEVEL ----
# PLAYERS 
# offense ----
#passing attempts
pass.att <- season.2015 %>% dplyr::filter(pass.att > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Attempts = sum(pass.att)) %>% ungroup() %>% arrange(desc(Passing.Attempts))
pass.att$rank <- c(1:nrow(pass.att))

(pass.att.plot <- hc_params %>%
  hc_add_series(name="Passing Attempts", data = subset(pass.att$Passing.Attempts, pass.att$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.att$name) %>%
  hc_title(text="Passing Attempts - Top 40 QBs", align= alignment))

#completions
pass.comp <- season.2015 %>% dplyr::filter(pass.comp > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Completion = sum(pass.comp)) %>% ungroup() %>% arrange(desc(Passing.Completion))
pass.comp$rank <- c(1:nrow(pass.comp))

(pass.comp.plot <- hc_params %>%
  hc_add_series(name="Completed Passes", data = subset(pass.att$Passing.Attempts, pass.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.att$name) %>%
  hc_title(text="Completed Passes - Top 40 QBs", align = alignment))

#passing yards
pass.yds <- season.2015 %>% dplyr::filter(passyds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Yards = sum(passyds)) %>% ungroup() %>% arrange(desc(Passing.Yards))
pass.yds$rank <- c(1:nrow(pass.yds))

(pass.yds.plot <- hc_params %>%
  hc_add_series(name="Passing Yards", data = subset(pass.yds$Passing.Yards, pass.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.yds$name) %>% 
  hc_title(text="Passing Yards - Top 40 QBs", align = alignment))

#Passing Touchdowns
pass.tds <- season.2015 %>% dplyr::filter(pass.tds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.tds = sum(pass.tds)) %>% ungroup() %>% arrange(desc(Passing.tds))
pass.tds$rank <- c(1:nrow(pass.tds))

(pass.tds.plot <- hc_params %>%
  hc_add_series(name="Passing Touchdowns", data = subset(pass.tds$Passing.tds, pass.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.tds$name) %>%
  hc_title(text="Passing Touchdowns- Top 40 QBs", align=alignment))

#Interceptions thrown
pass.int <- season.2015 %>% dplyr::filter(pass.ints > 0) %>% group_by(team,name,team2) %>% summarise(Passing.int = sum(pass.ints)) %>% ungroup() %>% arrange(desc(Passing.int))
pass.int$rank <- c(1:nrow(pass.int))

(pass.int.plot <- hc_params%>%
  hc_add_series(name="Interceptions Thrown", data = subset(pass.int$Passing.int, pass.int$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.int$name) %>%
  hc_title(text="Interceptions Thrown - Top 40 QBs", align=alignment))

#receptions
recept <- season.2015 %>% dplyr::filter(recept > 0) %>% group_by(team,name,team2) %>% summarise(recept = sum(recept)) %>% ungroup() %>% arrange(desc(recept))
recept$rank <- c(1:nrow(recept))

(recept.plot <- hc_params %>%
  hc_add_series(name="Receptions", data = subset(recept$recept, recept$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = recept$name) %>%
  hc_title(text="Receptions - Top 40", align= alignment))

#reception yards
rec.yds <- season.2015 %>% dplyr::filter(recyds > 0) %>% group_by(team,name,team2) %>% summarise(rec.yds = sum(recyds)) %>% ungroup() %>% arrange(desc(rec.yds))
rec.yds$rank <- c(1:nrow(rec.yds))

(rec.yds.plot <- hc_params %>%
  hc_add_series(name="Reception Yards", data = subset(rec.yds$rec.yds, rec.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.yds$name) %>%
  hc_title(text="Reception Yards - Top 40", align= alignment))

#reception tds
rec.tds <- season.2015 %>% dplyr::filter(rec.tds > 0) %>% group_by(team,name,team2) %>% summarise(rec.tds = sum(rec.tds)) %>% ungroup() %>% arrange(desc(rec.tds))
rec.tds$rank <- c(1:nrow(rec.tds))

(rec.tds.plot <- hc_params %>%
  hc_add_series(name="Reception TDs", data = subset(rec.tds$rec.tds, rec.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.tds$name) %>%
  hc_title(text="Reception Touchdowns - Top 40", align= alignment))

#fumbles
fumbs <- season.2015 %>% dplyr::filter(totalfumbs > 0) %>% group_by(team,name,team2) %>% summarise(fumbs= sum(totalfumbs)) %>% ungroup() %>% arrange(desc(fumbs))
fumbs$rank <- c(1:nrow(fumbs))

(fumbs.plot <- hc_params %>%
  hc_add_series(name="Fumbles", data = subset(fumbs$fumbs, fumbs$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs$name) %>%
  hc_title(text="Total Fumbles - Top 40", align= alignment))

#fumbles lost
fumbs.lost <- season.2015 %>% dplyr::filter(fumbslost > 0) %>% group_by(team,name,team2) %>% summarise(fumbs.lost = sum(fumbslost)) %>% ungroup() %>% arrange(desc(fumbs.lost))
fumbs.lost$rank <- c(1:nrow(fumbs.lost))

(fumbs.lost.plot <- hc_params %>%
  hc_add_series(name="Lost Fumbles", data = subset(fumbs.lost$fumbs.lost, fumbs.lost$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs.lost$name) %>%
  hc_title(text="Lost Fumbles - Top 40", align= alignment))

#Rushing Attempts
rush.att <- season.2015 %>% dplyr::filter(rush.att > 0) %>% group_by(team,name,team2) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
rush.att$rank <- c(1:nrow(rush.att))

(rush.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Attempts", data = subset(rush.att$rush.att, rush.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.att$name) %>%
  hc_title(text="Rushing Attempts - Top 40", align= alignment))

#Rushing Yards
rush.yds <- season.2015 %>% dplyr::filter(rushyds > 0) %>% group_by(team,name,team2) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
rush.yds$rank <- c(1:nrow(rush.yds))

(rush.yds.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards", data = subset(rush.yds$rush.yds, rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.yds$name) %>%
  hc_title(text="Rushing Yards - Top 40", align=alignment))

#Rushing Yards / Att
rush.yds.att <- season.2015 %>% select(rushyds, rush.att, team, name, team2) %>% dplyr::filter(rush.att > (4*16)) %>% group_by(team,name,team2) %>% summarise(rush.yds.att = (rushyds/rush.att)) %>% ungroup() %>% arrange(desc(rush.yds.att))
rush.yds.att$rank <- c(1:nrow(rush.yds.att))
rush.yds.att$rush.yds.att <- round(rush.yds.att$rush.yds.att, 2)

(rush.yds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards/Attempt", data = subset(rush.yds.att$rush.yds.att, rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.yds.att$name) %>%
  hc_title(text="Rushing Yards / Attempt - Top 40", align="left") %>%
  hc_subtitle(text="For players with at least 4 carriers per game, 2015 Season (Click & Drag to Zoom)", align=alignment))

#Rushing TDs
rush.tds <- season.2015 %>% dplyr::filter(rushtds > 0) %>% group_by(team,name,team2) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
rush.tds$rank <- c(1:nrow(rush.tds))

(rush.tds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Touchdowns", data = subset(rush.tds$rush.tds, rush.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.tds$name) %>%
  hc_title(text="Rushing Touchdowns - Top 40", align= alignment)) 
# defense ----
#sacks
sacks <- season.2015 %>% dplyr::dplyr::filter(sacks > 0) %>% group_by(team,name,team2) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
sacks$rank <- c(1:nrow(sacks))

(sacks.plot <- hc_params %>%
  hc_add_series(name="Sacks", data = subset(sacks$sacks, sacks$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = sacks$name) %>%
  hc_title(text="Sacks - Top 40", align=alignment))

#tackles
tackles <- season.2015 %>% dplyr::filter(tackles> 0) %>% group_by(team,name,team2) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
tackles$rank <- c(1:nrow(tackles))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Tackles", data = subset(tackles$tackles, tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = tackles$name) %>%
  hc_title(text="Taclkes - Top 40", align=alignment))

#assisted tackles
asst.tackles <- season.2015 %>% dplyr::filter(asst.tackles> 0) %>% group_by(team,name,team2) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
asst.tackles$rank <- c(1:nrow(asst.tackles))

(asst.tackles.plot <- hc_params %>%
  hc_add_series(name="Assisted Tackles", data = subset(asst.tackles$asst.tackles, asst.tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = asst.tackles$name) %>%
  hc_title(text="Assisted Tackles - Top 40", align=alignment))

#Forced Fumbles
forced.fumbs <- season.2015 %>% dplyr::filter(forced.fumbs> 0) %>% group_by(team,name,team2) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
forced.fumbs$rank <- c(1:nrow(forced.fumbs))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Forced Fumbles", data = subset(forced.fumbs$forced.fumbs, forced.fumbs$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = forced.fumbs$name) %>%
  hc_title(text="Forced Fumbles - Top 40", align=alignment))

#Defensive Interceptions
defints <- season.2015 %>% dplyr::filter(defints > 0) %>% group_by(team,name,team2) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
defints$rank <- c(1:nrow(defints))

(def.ints.plot <- hc_params %>%
  hc_add_series(name="Defensive Interceptions", data = subset(defints$defints, defints$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = defints$name) %>%
  hc_title(text="Defensive Interceptions - Top 40", align=alignment))
# special teams ----
#field goal percentage
fg <- season.2015 %>% select(team, name, team2, fga, fgm) %>% dplyr::filter(fga > 5) %>% group_by(team,name,team2) %>% summarise(fg = (fgm/fga)) %>% ungroup() %>% arrange(desc(fg))
fg$rank <- c(1:nrow(fg))

(fg.plot <- hc_params %>%
  hc_add_series(name="Field Goal Percentage", data = subset(fg$fg, fg$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = fg$name) %>%
  hc_title(text="Field Goal Percentage", align=alignment))

#field goal points
fg.points <- season.2015 %>% dplyr::filter(totpts.fg > 0) %>% group_by(team,name,team2) %>% summarise(fg.points = sum(totpts.fg)) %>% ungroup() %>% arrange(desc(fg.points))
fg.points$rank <- c(1:nrow(fg.points))

(fg.points.plot <- hc_params %>%
  hc_add_series(name="Field Goal Points", data = subset(fg.points$fg.points, fg.points$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = fg.points$name) %>%
  hc_title(text="Field Goal Points", align=alignment))

#extra points missed
xp.missed <- season.2015 %>% dplyr::filter(xpmissed > 0) %>% group_by(team,name,team2) %>% summarise(xp.missed = sum(xpmissed)) %>% ungroup() %>% arrange(desc(xp.missed))
xp.missed$rank <- c(1:nrow(xp.missed))

(xp.missed.plot <- hc_params %>%
  hc_add_series(name="Missed Extra Points", data = subset(xp.missed$xp.missed, xp.missed$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = xp.missed$name) %>%
  hc_title(text="Missed Extra Points", align=alignment))

#Average number of yards gained on kickoff returns
kickret.avg <- season.2015 %>% dplyr::filter(kickret.avg > 0) %>% group_by(team,name,team2) %>% summarise(kickret.avg = sum(kickret.avg)) %>% ungroup() %>% arrange(desc(kickret.avg))
kickret.avg$rank <- c(1:nrow(kickret.avg))

(kickret.avg.plot <- hc_params %>%
  hc_add_series(name="Avg Yards Gained on Kickoff Returns", data = subset(kickret.avg$kickret.avg, kickret.avg$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = kickret.avg$name) %>%
  hc_title(text="Avg Yards Gained on Kickoff Returns", align=alignment))

# TEAMS ----
# offense ----
#passing attempts
pass.att <- season.2015 %>% dplyr::filter(pass.att > 0) %>% group_by(team) %>% summarise(Passing.Attempts = sum(pass.att)) %>% ungroup() %>% arrange(desc(Passing.Attempts))
pass.att$rank <- c(1:nrow(pass.att))

(pass.att.plot <- highchart() %>%
  hc_add_series(name="Passing Attempts", data = pass.att$Passing.Attempts, type = "bar")  %>%
  hc_xAxis(categories = pass.att$team, color = pass.att$coloring) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Attempts", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "http://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#completions
pass.comp <- season.2015 %>% dplyr::filter(pass.comp > 0) %>% group_by(team) %>% summarise(Passing.Completion = sum(pass.comp)) %>% ungroup() %>% arrange(desc(Passing.Completion))
pass.comp$rank <- c(1:nrow(pass.comp))

(pass.comp.plot <- highchart() %>%
  hc_add_series(name="Completed Passes", data = pass.comp$Passing.Completion, type = "bar")  %>%
  hc_xAxis(categories = pass.att$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Completed Passes", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#passing yards
pass.yds <- season.2015 %>% dplyr::filter(passyds > 0) %>% group_by(team) %>% summarise(Passing.Yards = sum(passyds)) %>% ungroup() %>% arrange(desc(Passing.Yards))
pass.yds$rank <- c(1:nrow(pass.yds))

(pass.yds.plot <- highchart() %>%
  hc_add_series(name="Passing Yards", data = pass.yds$Passing.Yards, type = "bar")  %>%
  hc_xAxis(categories = pass.yds$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Yards", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Passing Touchdowns
pass.tds <- season.2015 %>% dplyr::filter(pass.tds > 0) %>% group_by(team) %>% summarise(Passing.tds = sum(pass.tds)) %>% ungroup() %>% arrange(desc(Passing.tds))
pass.tds$rank <- c(1:nrow(pass.tds))

(pass.tds.plot <- highchart() %>%
  hc_add_series(name="Passing Touchdowns", data = pass.tds$Passing.tds, type = "bar")  %>%
  hc_xAxis(categories = pass.tds$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Passing Touchdowns", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Interceptions thrown
pass.int <- season.2015 %>% dplyr::filter(pass.ints > 0) %>% group_by(team) %>% summarise(Passing.int = sum(pass.ints)) %>% ungroup() %>% arrange(desc(Passing.int))
pass.int$rank <- c(1:nrow(pass.int))

(pass.int.plot <- highchart() %>%
  hc_add_series(name="Interceptions Thrown", data = pass.int$Passing.int, type = "bar")  %>%
  hc_xAxis(categories = pass.int$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Interceptions Thrown", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing Attempts
rush.att <- season.2015 %>% dplyr::filter(rush.att > 0) %>% group_by(team) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
rush.att$rank <- c(1:nrow(rush.att))

(rush.att.plot <- highchart() %>%
  hc_add_series(name="Rushing Attempts", data = rush.att$rush.att, type = "bar")  %>%
  hc_xAxis(categories = rush.att$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Attempts", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing Yards
rush.yds <- season.2015 %>% dplyr::filter(rushyds > 0) %>% group_by(team) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
rush.yds$rank <- c(1:nrow(rush.yds))

(rush.yds.plot <- highchart() %>%
  hc_add_series(name="Rushing Yards", data = rush.yds$rush.yds, type = "bar")  %>%
  hc_xAxis(categories = rush.yds$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Yards", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Rushing TDs
rush.tds <- season.2015 %>% dplyr::filter(rushtds > 0) %>% group_by(team) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
rush.tds$rank <- c(1:nrow(rush.tds))

(rush.tds.att.plot <- highchart() %>%
  hc_add_series(name="Rushing Touchdowns", data = rush.tds$rush.tds, type = "bar")  %>%
  hc_xAxis(categories = rush.tds$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Rushing Touchdowns", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#"rushlng" - Most yards gained on a rush attempt
#"reclng" - Longest reception
# defense ----
#sacks
sacks <- season.2015 %>% dplyr::filter(sacks > 0) %>% group_by(team) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
sacks$rank <- c(1:nrow(sacks))

(sacks.plot <- highchart() %>%
  hc_add_series(name="Sacks", data = sacks$sacks, type = "bar")  %>%
  hc_xAxis(categories = sacks$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Sacks", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#tackles
tackles <- season.2015 %>% dplyr::filter(tackles> 0) %>% group_by(team) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
tackles$rank <- c(1:nrow(tackles))

(tackles.plot <- highchart() %>%
  hc_add_series(name="Tackles", data = tackles$tackles, type = "bar")  %>%
  hc_xAxis(categories = tackles$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Taclkes", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#assisted tackles
asst.tackles <- season.2015 %>% dplyr::filter(asst.tackles> 0) %>% group_by(team) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
asst.tackles$rank <- c(1:nrow(asst.tackles))

(asst.tackles.plot <- highchart() %>%
  hc_add_series(name="Assisted Tackles", data = asst.tackles$asst.tackles, type = "bar")  %>%
  hc_xAxis(categories = asst.tackles$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Assisted Tackles", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Forced Fumbles
forced.fumbs <- season.2015 %>% dplyr::filter(forced.fumbs> 0) %>% group_by(team) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
forced.fumbs$rank <- c(1:nrow(forced.fumbs))

(tackles.plot <- highchart() %>%
  hc_add_series(name="Forced Fumbles", data = forced.fumbs$forced.fumbs, type = "bar")  %>%
  hc_xAxis(categories = forced.fumbs$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Forced Fumbles", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#Defensive Interceptions
defints <- season.2015 %>% dplyr::filter(defints > 0) %>% group_by(team) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
defints$rank <- c(1:nrow(defints))

(def.ints.plot <- highchart() %>%
  hc_add_series(name="Defensive Interceptions", data = defints$defints, type = "bar")  %>%
  hc_xAxis(categories = defints$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Defensive Interceptions", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))
# special teams ----
#field goal points
fg.points <- season.2015 %>% dplyr::filter(totpts.fg > 0) %>% group_by(team) %>% summarise(fg.points = sum(totpts.fg)) %>% ungroup() %>% arrange(desc(fg.points))
fg.points$rank <- c(1:nrow(fg.points))

(fg.points.plot <- highchart() %>%
  hc_add_series(name="Field Goal Points", data = fg.points$fg.points, type = "bar")  %>%
  hc_xAxis(categories = fg.points$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Field Goal Points", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))

#extra points missed
xp.missed <- season.2015 %>% dplyr::filter(xpmissed > 0) %>% group_by(team) %>% summarise(xp.missed = sum(xpmissed)) %>% ungroup() %>% arrange(desc(xp.missed))
xp.missed$rank <- c(1:nrow(xp.missed))

(xp.missed.plot <- highchart() %>%
  hc_add_series(name="Missed Extra Points", data = xp.missed$xp.missed, type = "bar")  %>%
  hc_xAxis(categories = xp.missed$team) %>%
  hc_chart(zoomType = "x") %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Missed Extra Points", align="left") %>%
  hc_subtitle(text="2015 Season (Click & Drag to Zoom)", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = FALSE))




 

# EAGLES ----
# offense ----
# defense ----
# special teams ----
# time series ----
#historical attempted passes
hist.pass.att <- eagles.hist %>% dplyr::filter(pass.att > 0) %>% group_by(date, name) %>% summarise(total  = sum(pass.att))
hist.pass.att$date <- ymd(hist.pass.att$date)

bradford.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "S.Bradford"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "S.Bradford"), frequency = 52)
vick.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Vick"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Vick"), frequency = 52)
foles.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "N.Foles"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "N.Foles"), frequency = 52)
sanchez.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Sanchez"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Sanchez"), frequency = 52)
barkley.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Barkley"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Barkley"), frequency = 52)
smith.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "B.Smith"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "B.Smith"), frequency = 52)

highchart(type = "chart") %>% 
  hc_add_series_xts(vick.ts, id = "Vick", name= "Vick") %>%
  hc_add_series_xts(foles.ts, id = "Foles", name = "Foles") %>%
  hc_add_series_xts(barkley.ts, id = "Barkley", name = "Barkley") %>% 
  hc_add_series_xts(smith.ts, id = "Smith", name = "Smith")%>%
  hc_add_series_xts(sanchez.ts, id = "Sanchez", name = "Sanchez") %>%
  hc_add_series_xts(bradford.ts, id = "Bradford", name = "Bradford") %>%
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

