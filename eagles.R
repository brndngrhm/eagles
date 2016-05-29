# 2015 NFL & Eagles Team and Player stats
# Run "packages" code chunk then start at line 42 to replicate plots
# *!*!*! NEED TO FIGURE OUT HOW TO MANUALLY DEFINE PLOT SIZE, COLOR PHL, allow exporting of data & ADD TEAM + RANK TO TOOLTIP *!*!*!
# want to add qb rating, time of possession, offensive plays, offensive/defensive time on field, points scored
#great highcharter reference: http://rpackages.ianhowson.com/cran/highcharter/

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

phl <- season.2015 %>% dplyr::filter(team == "PHI")
write.csv(phl, file = "~/R Working Directory/Other/eagles/phl.csv")

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

# load data ----
season.2015.x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/season.2015.csv")
season.2015 <- as.data.frame(read.csv(text = season.2015.x, strip.white = T))
phl.x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/phl.csv")
phl <- as.data.frame(read.csv(text = phl.x, strip.white = T))
eagles.hist.x <- getURL("https://raw.githubusercontent.com/brndngrhm/eagles/master/eagles.hist.csv")
eagles.hist <- as.data.frame(read.csv(text = eagles.hist.x, strip.white = T))
rm(season.2015.x, eagles.hist.x, phl.x)

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

# NFL LEVEL ----
# PLAYERS 
# offense ----
#passing attempts
pass.att <- season.2015 %>% dplyr::filter(pass.att > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Attempts = sum(pass.att)) %>% ungroup() %>% arrange(desc(Passing.Attempts))
pass.att$rank <- c(1:nrow(pass.att))

(pass.att.plot <- hc_params %>%
  hc_add_series(name="Passing Attempts", data = subset(pass.att$Passing.Attempts, pass.att$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.att$name) %>%
  hc_title(text=paste("Passing Attempts - Top",top.n, sep=" "), align= alignment))

#completions
pass.comp <- season.2015 %>% dplyr::filter(pass.comp > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Completion = sum(pass.comp)) %>% ungroup() %>% arrange(desc(Passing.Completion))
pass.comp$rank <- c(1:nrow(pass.comp))

(pass.comp.plot <- hc_params %>%
  hc_add_series(name="Completed Passes", data = subset(pass.comp$Passing.Attempts, pass.comp$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.comp$name) %>%
  hc_title(text=paste("Completed Passes - Top",top.n, sep=" "), align= alignment))

#passing yards
pass.yds <- season.2015 %>% dplyr::filter(passyds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.Yards = sum(passyds)) %>% ungroup() %>% arrange(desc(Passing.Yards))
pass.yds$rank <- c(1:nrow(pass.yds))

(pass.yds.plot <- hc_params %>%
  hc_add_series(name="Passing Yards", data = subset(pass.yds$Passing.Yards, pass.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.yds$name) %>%
  hc_title(text=paste("Passing Yards - Top",top.n, sep=" "), align= alignment))

#Passing Touchdowns
pass.tds <- season.2015 %>% dplyr::filter(pass.tds > 0) %>% group_by(team,name,team2) %>% summarise(Passing.tds = sum(pass.tds)) %>% ungroup() %>% arrange(desc(Passing.tds))
pass.tds$rank <- c(1:nrow(pass.tds))

(pass.tds.plot <- hc_params %>%
  hc_add_series(name="Passing Touchdowns", data = subset(pass.tds$Passing.tds, pass.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.tds$name) %>%
  hc_title(text=paste("Passing Touchdowns - Top",top.n, sep=" "), align= alignment))

#Interceptions thrown
pass.int <- season.2015 %>% dplyr::filter(pass.ints > 0) %>% group_by(team,name,team2) %>% summarise(Passing.int = sum(pass.ints)) %>% ungroup() %>% arrange(desc(Passing.int))
pass.int$rank <- c(1:nrow(pass.int))

(pass.int.plot <- hc_params%>%
  hc_add_series(name="Interceptions Thrown", data = subset(pass.int$Passing.int, pass.int$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.int$name) %>%
  hc_title(text=paste("Interceptions - Top",top.n, sep=" "), align= alignment))

#receptions
recept <- season.2015 %>% dplyr::filter(recept > 0) %>% group_by(team,name,team2) %>% summarise(recept = sum(recept)) %>% ungroup() %>% arrange(desc(recept))
recept$rank <- c(1:nrow(recept))

(recept.plot <- hc_params %>%
  hc_add_series(name="Receptions", data = subset(recept$recept, recept$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = recept$name) %>%
  hc_title(text=paste("Receptions - Top",top.n, sep=" "), align= alignment))

#reception yards
rec.yds <- season.2015 %>% dplyr::filter(recyds > 0) %>% group_by(team,name,team2) %>% summarise(rec.yds = sum(recyds)) %>% ungroup() %>% arrange(desc(rec.yds))
rec.yds$rank <- c(1:nrow(rec.yds))

(rec.yds.plot <- hc_params %>%
  hc_add_series(name="Reception Yards", data = subset(rec.yds$rec.yds, rec.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.yds$name) %>%
  hc_title(text=paste("Reception Yards - Top",top.n, sep=" "), align= alignment))

#longest reception
reclng <- season.2015 %>% dplyr::filter(reclng > 0) %>% group_by(team,name,team2) %>% summarise(reclng = sum(reclng)) %>% ungroup() %>% arrange(desc(reclng))
reclng$rank <- c(1:nrow(reclng))

(reclng.plot <- hc_params %>%
  hc_add_series(name="Longest Reception", data = subset(reclng$reclng, reclng$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = reclng$name) %>%
  hc_title(text=paste("Longest Reception - Top",top.n, sep=" "), align= alignment))

#reception tds
rec.tds <- season.2015 %>% dplyr::filter(rec.tds > 0) %>% group_by(team,name,team2) %>% summarise(rec.tds = sum(rec.tds)) %>% ungroup() %>% arrange(desc(rec.tds))
rec.tds$rank <- c(1:nrow(rec.tds))

(rec.tds.plot <- hc_params %>%
  hc_add_series(name="Reception TDs", data = subset(rec.tds$rec.tds, rec.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.tds$name) %>%
  hc_title(text=paste("Reception Touchdowns - Top",top.n, sep=" "), align= alignment))

#fumbles
fumbs <- season.2015 %>% dplyr::filter(totalfumbs > 0) %>% group_by(team,name,team2) %>% summarise(fumbs= sum(totalfumbs)) %>% ungroup() %>% arrange(desc(fumbs))
fumbs$rank <- c(1:nrow(fumbs))

(fumbs.plot <- hc_params %>%
  hc_add_series(name="Fumbles", data = subset(fumbs$fumbs, fumbs$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs$name) %>%
  hc_title(text=paste("Fumbles - Top",top.n, sep=" "), align= alignment))

#fumbles lost
fumbs.lost <- season.2015 %>% dplyr::filter(fumbslost > 0) %>% group_by(team,name,team2) %>% summarise(fumbs.lost = sum(fumbslost)) %>% ungroup() %>% arrange(desc(fumbs.lost))
fumbs.lost$rank <- c(1:nrow(fumbs.lost))

(fumbs.lost.plot <- hc_params %>%
  hc_add_series(name="Lost Fumbles", data = subset(fumbs.lost$fumbs.lost, fumbs.lost$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs.lost$name) %>%
  hc_title(text=paste("Lost Fumbles - Top",top.n, sep=" "), align= alignment))

#Rushing Attempts
rush.att <- season.2015 %>% dplyr::filter(rush.att > 0) %>% group_by(team,name,team2) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
rush.att$rank <- c(1:nrow(rush.att))

(rush.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Attempts", data = subset(rush.att$rush.att, rush.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.att$name) %>%
  hc_title(text=paste("Rushing Attempts - Top",top.n, sep=" "), align= alignment))

#Rushing Yards
rush.yds <- season.2015 %>% dplyr::filter(rushyds > 0) %>% group_by(team,name,team2) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
rush.yds$rank <- c(1:nrow(rush.yds))

(rush.yds.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards", data = subset(rush.yds$rush.yds, rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.yds$name) %>%
  hc_title(text=paste("Rushing Yards - Top",top.n, sep=" "), align= alignment))

#Rushing Yards / Att
rush.yds.att <- season.2015 %>% select(rushyds, rush.att, team, name, team2) %>% dplyr::filter(rush.att > (4*16)) %>% group_by(team,name,team2) %>% summarise(rush.yds.att = (rushyds/rush.att)) %>% ungroup() %>% arrange(desc(rush.yds.att))
rush.yds.att$rank <- c(1:nrow(rush.yds.att))
rush.yds.att$rush.yds.att <- round(rush.yds.att$rush.yds.att, 2)

(rush.yds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards/Attempt", data = subset(rush.yds.att$rush.yds.att, rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.yds.att$name) %>%
  hc_title(text=paste("Rushing Yards / Attempt - Top",top.n, sep=" "), align= alignment) %>%
  hc_subtitle(text="For players with at least 4 carriers per game, 2015 Season (Click & Drag to Zoom)", align=alignment))

#Rushing TDs
rush.tds <- season.2015 %>% dplyr::filter(rushtds > 0) %>% group_by(team,name,team2) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
rush.tds$rank <- c(1:nrow(rush.tds))

(rush.tds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Touchdowns", data = subset(rush.tds$rush.tds, rush.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.tds$name) %>%
  hc_title(text=paste("Rushing Touchdowns - Top",top.n, sep=" "), align= alignment))
# defense ----
#sacks
sacks <- season.2015 %>% dplyr::filter(sacks > 0) %>% group_by(team,name,team2) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
sacks$rank <- c(1:nrow(sacks))

(sacks.plot <- hc_params %>%
  hc_add_series(name="Sacks", data = subset(sacks$sacks, sacks$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = sacks$name)  %>%
  hc_title(text=paste("Sacks - Top",top.n, sep=" "), align= alignment))

#tackles
tackles <- season.2015 %>% dplyr::filter(tackles> 0) %>% group_by(team,name,team2) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
tackles$rank <- c(1:nrow(tackles))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Tackles", data = subset(tackles$tackles, tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = tackles$name)  %>%
  hc_title(text=paste("Taclkes - Top",top.n, sep=" "), align= alignment))

#assisted tackles
asst.tackles <- season.2015 %>% dplyr::filter(asst.tackles> 0) %>% group_by(team,name,team2) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
asst.tackles$rank <- c(1:nrow(asst.tackles))

(asst.tackles.plot <- hc_params %>%
  hc_add_series(name="Assisted Tackles", data = subset(asst.tackles$asst.tackles, asst.tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = asst.tackles$name) %>%
  hc_title(text=paste("Assisted Tackles - Top",top.n, sep=" "), align= alignment))

#Forced Fumbles
forced.fumbs <- season.2015 %>% dplyr::filter(forced.fumbs> 0) %>% group_by(team,name,team2) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
forced.fumbs$rank <- c(1:nrow(forced.fumbs))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Forced Fumbles", data = subset(forced.fumbs$forced.fumbs, forced.fumbs$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = forced.fumbs$name) %>%
  hc_title(text=paste("Forced Fumbles - Top",top.n, sep=" "), align= alignment))

#Defensive Interceptions
defints <- season.2015 %>% dplyr::filter(defints > 0) %>% group_by(team,name,team2) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
defints$rank <- c(1:nrow(defints))

(def.ints.plot <- hc_params %>%
  hc_add_series(name="Defensive Interceptions", data = subset(defints$defints, defints$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = defints$name) %>%
  hc_title(text=paste("Defensive Interceptions - Top",top.n, sep=" "), align= alignment))
# special teams ----
#field goal percentage
fg <- season.2015 %>% select(team, name, team2, fga, fgm) %>% dplyr::filter(fga > 5) %>% group_by(team,name,team2) %>% summarise(fg = (fgm/fga)) %>% ungroup() %>% arrange(desc(fg))
fg$rank <- c(1:nrow(fg))

(fg.plot <- hc_params %>%
  hc_add_series(name="Field Goal Percentage", data = subset(fg$fg, fg$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = fg$name)  %>%
  hc_title(text=paste("Field Goal Percentage - Top",top.n, sep=" "), align= alignment))

#field goal points
fg.points <- season.2015 %>% dplyr::filter(totpts.fg > 0) %>% group_by(team,name,team2) %>% summarise(fg.points = sum(totpts.fg)) %>% ungroup() %>% arrange(desc(fg.points))
fg.points$rank <- c(1:nrow(fg.points))

(fg.points.plot <- hc_params %>%
  hc_add_series(name="Field Goal Points", data = subset(fg.points$fg.points, fg.points$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = fg.points$name) %>%
  hc_title(text=paste("Field Goal Points - Top",top.n, sep=" "), align= alignment))

#extra points missed
xp.missed <- season.2015 %>% dplyr::filter(xpmissed > 0) %>% group_by(team,name,team2) %>% summarise(xp.missed = sum(xpmissed)) %>% ungroup() %>% arrange(desc(xp.missed))
xp.missed$rank <- c(1:nrow(xp.missed))

(xp.missed.plot <- hc_params %>%
  hc_add_series(name="Missed Extra Points", data = subset(xp.missed$xp.missed, xp.missed$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = xp.missed$name)  %>%
  hc_title(text=paste("Missed Extra Points - Top",top.n, sep=" "), align= alignment))

# TEAM LEVEL ----
# offense ----
#passing attempts
pass.att <- season.2015 %>% dplyr::filter(pass.att > 0) %>% group_by(team) %>% summarise(Passing.Attempts = sum(pass.att)) %>% ungroup() %>% arrange(desc(Passing.Attempts))
pass.att$rank <- c(1:nrow(pass.att))

(pass.att.plot <- hc_params %>%
  hc_add_series(name="Passing Attempts", data = subset(pass.att$Passing.Attempts, pass.att$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.att$team)  %>%
  hc_title(text=paste("Passing Attempts - Top",top.n, sep=" "), align= alignment))

#completions
pass.comp <- season.2015 %>% dplyr::filter(pass.comp > 0) %>% group_by(team) %>% summarise(Passing.Completion = sum(pass.comp)) %>% ungroup() %>% arrange(desc(Passing.Completion))
pass.comp$rank <- c(1:nrow(pass.comp))

(pass.comp.plot <- hc_params %>%
  hc_add_series(name="Completed Passes", data = subset(pass.att$Passing.Attempts, pass.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.att$team) %>%
  hc_title(text=paste("Completed Passes - Top",top.n, sep=" "), align= alignment))

#passing yards
pass.yds <- season.2015 %>% dplyr::filter(passyds > 0) %>% group_by(team) %>% summarise(Passing.Yards = sum(passyds)) %>% ungroup() %>% arrange(desc(Passing.Yards))
pass.yds$rank <- c(1:nrow(pass.yds))

(pass.yds.plot <- hc_params %>%
  hc_add_series(name="Passing Yards", data = subset(pass.yds$Passing.Yards, pass.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.yds$team)  %>%
  hc_title(text=paste("Passing Yards - Top",top.n, sep=" "), align= alignment))

#Passing Touchdowns
pass.tds <- season.2015 %>% dplyr::filter(pass.tds > 0) %>% group_by(team) %>% summarise(Passing.tds = sum(pass.tds)) %>% ungroup() %>% arrange(desc(Passing.tds))
pass.tds$rank <- c(1:nrow(pass.tds))

(pass.tds.plot <- hc_params %>%
  hc_add_series(name="Passing Touchdowns", data = subset(pass.tds$Passing.tds, pass.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.tds$team)  %>%
  hc_title(text=paste("Passing Touchdowns - Top",top.n, sep=" "), align= alignment))

#Interceptions thrown
pass.int <- season.2015 %>% dplyr::filter(pass.ints > 0) %>% group_by(team) %>% summarise(Passing.int = sum(pass.ints)) %>% ungroup() %>% arrange(desc(Passing.int))
pass.int$rank <- c(1:nrow(pass.int))

(pass.int.plot <- hc_params%>%
  hc_add_series(name="Interceptions Thrown", data = subset(pass.int$Passing.int, pass.int$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = pass.int$team) %>%
  hc_title(text=paste("Interceptions Thrown - Top",top.n, sep=" "), align= alignment))

#receptions
recept <- season.2015 %>% dplyr::filter(recept > 0) %>% group_by(team) %>% summarise(recept = sum(recept)) %>% ungroup() %>% arrange(desc(recept))
recept$rank <- c(1:nrow(recept))

(recept.plot <- hc_params %>%
  hc_add_series(name="Receptions", data = subset(recept$recept, recept$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = recept$team)  %>%
  hc_title(text=paste("Receptions - Top",top.n, sep=" "), align= alignment))

#reception yards
rec.yds <- season.2015 %>% dplyr::filter(recyds > 0) %>% group_by(team) %>% summarise(rec.yds = sum(recyds)) %>% ungroup() %>% arrange(desc(rec.yds))
rec.yds$rank <- c(1:nrow(rec.yds))

(rec.yds.plot <- hc_params %>%
  hc_add_series(name="Reception Yards", data = subset(rec.yds$rec.yds, rec.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.yds$team) %>%
  hc_title(text=paste("Reception Yards - Top",top.n, sep=" "), align= alignment))

#reception tds
rec.tds <- season.2015 %>% dplyr::filter(rec.tds > 0) %>% group_by(team) %>% summarise(rec.tds = sum(rec.tds)) %>% ungroup() %>% arrange(desc(rec.tds))
rec.tds$rank <- c(1:nrow(rec.tds))

(rec.tds.plot <- hc_params %>%
  hc_add_series(name="Reception TDs", data = subset(rec.tds$rec.tds, rec.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rec.tds$team)  %>%
  hc_title(text=paste("Reception Touchdowns - Top",top.n, sep=" "), align= alignment))

#fumbles
fumbs <- season.2015 %>% dplyr::filter(totalfumbs > 0) %>% group_by(team) %>% summarise(fumbs= sum(totalfumbs)) %>% ungroup() %>% arrange(desc(fumbs))
fumbs$rank <- c(1:nrow(fumbs))

(fumbs.plot <- hc_params %>%
  hc_add_series(name="Fumbles", data = subset(fumbs$fumbs, fumbs$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs$name)  %>%
  hc_title(text=paste("Fumbles - Top",top.n, sep=" "), align= alignment))

#fumbles lost
fumbs.lost <- season.2015 %>% dplyr::filter(fumbslost > 0) %>% group_by(team) %>% summarise(fumbs.lost = sum(fumbslost)) %>% ungroup() %>% arrange(desc(fumbs.lost))
fumbs.lost$rank <- c(1:nrow(fumbs.lost))

(fumbs.lost.plot <- hc_params %>%
  hc_add_series(name="Lost Fumbles", data = subset(fumbs.lost$fumbs.lost, fumbs.lost$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = fumbs.lost$team)  %>%
  hc_title(text=paste("Lost Fumbles - Top",top.n, sep=" "), align= alignment))

#Rushing Attempts
rush.att <- season.2015 %>% dplyr::filter(rush.att > 0) %>% group_by(team) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
rush.att$rank <- c(1:nrow(rush.att))

(rush.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Attempts", data = subset(rush.att$rush.att, rush.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.att$team) %>%
  hc_title(text=paste("Rushing Attempts - Top",top.n, sep=" "), align= alignment))

#Rushing Yards
rush.yds <- season.2015 %>% dplyr::filter(rushyds > 0) %>% group_by(team) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
rush.yds$rank <- c(1:nrow(rush.yds))

(rush.yds.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards", data = subset(rush.yds$rush.yds, rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.yds$team)  %>%
  hc_title(text=paste("Rushing Yards - Top",top.n, sep=" "), align= alignment))

#Rushing TDs
rush.tds <- season.2015 %>% dplyr::filter(rushtds > 0) %>% group_by(team) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
rush.tds$rank <- c(1:nrow(rush.tds))

(rush.tds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Touchdowns", data = subset(rush.tds$rush.tds, rush.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = rush.tds$team)  %>%
  hc_title(text=paste("Rushing Touchdowns - Top",top.n, sep=" "), align= alignment))
# defense ----
#sacks
sacks <- season.2015 %>% dplyr::filter(sacks > 0) %>% group_by(team) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
sacks$rank <- c(1:nrow(sacks))

(sacks.plot <- hc_params %>%
  hc_add_series(name="Sacks", data = subset(sacks$sacks, sacks$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = sacks$team)  %>%
  hc_title(text=paste("Sacks - Top",top.n, sep=" "), align= alignment))

#tackles
tackles <- season.2015 %>% dplyr::filter(tackles> 0) %>% group_by(team) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
tackles$rank <- c(1:nrow(tackles))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Tackles", data = subset(tackles$tackles, tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = tackles$team)  %>%
  hc_title(text=paste("Tackles - Top",top.n, sep=" "), align= alignment))

#assisted tackles
asst.tackles <- season.2015 %>% dplyr::filter(asst.tackles> 0) %>% group_by(team) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
asst.tackles$rank <- c(1:nrow(asst.tackles))

(asst.tackles.plot <- hc_params %>%
  hc_add_series(name="Assisted Tackles", data = subset(asst.tackles$asst.tackles, asst.tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = asst.tackles$team)  %>%
  hc_title(text=paste("Assisted Tackles - Top",top.n, sep=" "), align= alignment))

#Forced Fumbles
forced.fumbs <- season.2015 %>% dplyr::filter(forced.fumbs> 0) %>% group_by(team) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
forced.fumbs$rank <- c(1:nrow(forced.fumbs))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Forced Fumbles", data = subset(forced.fumbs$forced.fumbs, forced.fumbs$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = forced.fumbs$team) %>%
  hc_title(text=paste("Forced Fumbles - Top",top.n, sep=" "), align= alignment))

#Defensive Interceptions
defints <- season.2015 %>% dplyr::filter(defints > 0) %>% group_by(team) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
defints$rank <- c(1:nrow(defints))

(def.ints.plot <- hc_params %>%
  hc_add_series(name="Defensive Interceptions", data = subset(defints$defints, defints$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = defints$team)  %>%
  hc_title(text=paste("Defensive Interceptions- Top",top.n, sep=" "), align= alignment))
# special teams ----
#field goal points
fg.points <- season.2015 %>% dplyr::filter(totpts.fg > 0) %>% group_by(team) %>% summarise(fg.points = sum(totpts.fg)) %>% ungroup() %>% arrange(desc(fg.points))
fg.points$rank <- c(1:nrow(fg.points))

(fg.points.plot <- hc_params %>%
  hc_add_series(name="Field Goal Points", data = subset(fg.points$fg.points, fg.points$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = fg.points$team)  %>%
  hc_title(text=paste("Field Goal Points - Top",top.n, sep=" "), align= alignment))

#extra points missed
xp.missed <- season.2015 %>% dplyr::filter(xpmissed > 0) %>% group_by(team) %>% summarise(xp.missed = sum(xpmissed)) %>% ungroup() %>% arrange(desc(xp.missed))
xp.missed$rank <- c(1:nrow(xp.missed))

(xp.missed.plot <- hc_params %>%
  hc_add_series(name="Missed Extra Points", data = subset(xp.missed$xp.missed, xp.missed$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = xp.missed$team)  %>%
  hc_title(text=paste("Missed Extra Points - Top",top.n, sep=" "), align= alignment))

# EAGLES ----
# offense ----
#QBs
qb <- phl %>% select(name, pass.att, passyds, pass.tds, pass.ints, pass.ints, fumbslost, totalfumbs, rushyds) %>% dplyr::filter(name == "S.Bradford" |  name == "M.Sanchez") %>% arrange(desc(name))
qb$yds.att <- qb$passyds/qb$pass.att
qb <- qb %>% select(name, pass.att, passyds, pass.tds, yds.att, pass.ints, rushyds, totalfumbs, fumbslost)
names(qb)[1] <- "Name"
names(qb)[2] <- "Passing Attempts"
names(qb)[3] <- "Passing Yards"
names(qb)[4] <- "Touchdown Passes"
names(qb)[5] <- "Yards Per Pass"
names(qb)[6] <- "Interceptions"
names(qb)[7] <- "Rushing Yards"
names(qb)[8] <- "Fumbless"
names(qb)[9] <- "Lost Fumbles"
qb.melt <- melt(qb, id.vars = "Name")

(bradford.plot <- hc_params %>%
  hc_add_series(name="Bradford", data = subset(qb.melt$value, qb.melt$Name == "S.Bradford" & qb.melt$variable != "Passing Yards" & qb.melt$variable != "Passing Attempts"), type = plot.type) %>%
  hc_xAxis(categories = subset(qb.melt$variable, qb.melt$Name == "S.Bradford" & qb.melt$variable != "Passing Yards"& qb.melt$variable != "Passing Attempts")) %>%
  hc_title(text = paste("Sam Bradford:",qb.melt[4,3], "Passing Yards,", qb.melt[2,3], "Passing Attempts", by =" "), align = alignment))

(sanchez.plot <- hc_params %>%
  hc_add_series(name="Sanchez", data = subset(qb.melt$value, qb.melt$Name == "M.Sanchez"& qb.melt$variable != "Passing Yards"& qb.melt$variable != "Passing Attempts"), type = plot.type)  %>%
  hc_xAxis(categories = subset(qb.melt$variable, qb.melt$Name == "S.Bradford" & qb.melt$variable != "Passing Yards"& qb.melt$variable != "Passing Attempts")) %>%
  hc_title(text = paste("Mark Sanchez:",qb.melt[3,3], "Passing Yards,", qb.melt[1,3], "Passing Attempts", by =" "), align = alignment))

#receptions
recept.phl <- phl %>% dplyr::filter(recept > 0) %>% group_by(name) %>% summarise(recept = sum(recept)) %>% ungroup() %>% arrange(desc(recept))
recept.phl$rank <- c(1:nrow(recept.phl))

(recept.phl.plot <- hc_params %>%
  hc_add_series(name="Receptions", data = subset(recept.phl$recept, recept.phl$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = recept.phl$name) %>%
  hc_title(text="Receptions", align= alignment))

#reception yards
phl.rec.yds <- phl %>% dplyr::filter(recyds > 0) %>% group_by(name) %>% summarise(rec.yds = sum(recyds)) %>% ungroup() %>% arrange(desc(rec.yds))
phl.rec.yds$rank <- c(1:nrow(phl.rec.yds))

(phl.rec.yds.plot <- hc_params %>%
  hc_add_series(name="Recieving Yards", data = subset(phl.rec.yds$rec.yds, phl.rec.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rec.yds$name) %>%
  hc_title(text="Recieving Yards", align= alignment))

#receiving yds / attempt
phl.rec.yds.att <- left_join(recept.phl, phl.rec.yds, by="name")
phl.rec.yds.att$rank.x <- NULL
phl.rec.yds.att$rank.y <- NULL
phl.rec.yds.att$yds.att <- phl.rec.yds.att$rec.yds/phl.rec.yds.att$recept
phl.rec.yds.att <- phl.rec.yds.att %>% arrange(desc(yds.att))
phl.rec.yds.att$rank <- c(1:nrow(phl.rec.yds.att))
phl.rec.yds.att$yds.att <- round(phl.rec.yds.att$yds.att, 2)

(phl.rec.yds.att.plot <- hc_params %>%
  hc_add_series(name="Yards per Catch", data = subset(phl.rec.yds.att$yds.att, phl.rec.yds.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rec.yds.att$name) %>%
  hc_title(text="Yards per Catch", align= alignment))

#longest reception
phl.reclng <- phl %>% dplyr::filter(reclng > 0) %>% group_by(name) %>% summarise(reclng = sum(reclng)) %>% ungroup() %>% arrange(desc(reclng))
phl.reclng$rank <- c(1:nrow(phl.reclng))

(reclng.plot <- hc_params %>%
  hc_add_series(name="Longest Reception", data = subset(phl.reclng$reclng, phl.reclng$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.reclng$name) %>%
  hc_title(text="Longest Reception", align= alignment))

#reception tds
phl.rec.tds <- phl %>% dplyr::filter(rec.tds > 0) %>% group_by(name) %>% summarise(rec.tds = sum(rec.tds)) %>% ungroup() %>% arrange(desc(rec.tds))
phl.rec.tds$rank <- c(1:nrow(phl.rec.tds))

(rec.tds.plot <- hc_params %>%
  hc_add_series(name="Recieving Touchdowns", data = subset(phl.rec.tds$rec.tds, phl.rec.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rec.tds$name) %>%
  hc_title(text="Recieving Touchdowns", align= alignment))

#fumbles
phl.fumbs <- phl %>% dplyr::filter(totalfumbs > 0) %>% group_by(name) %>% summarise(fumbs= sum(totalfumbs)) %>% ungroup() %>% arrange(desc(fumbs))
phl.fumbs$rank <- c(1:nrow(phl.fumbs))

(fumbs.plot <- hc_params %>%
  hc_add_series(name="Fumbles", data = subset(phl.fumbs$fumbs, phl.fumbs$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.fumbs$name) %>%
  hc_title(text="Fumbles", align= alignment))

#fumbles lost
phl.fumbs.lost <- phl %>% dplyr::filter(fumbslost > 0) %>% group_by(name) %>% summarise(fumbs.lost = sum(fumbslost)) %>% ungroup() %>% arrange(desc(fumbs.lost))
phl.fumbs.lost$rank <- c(1:nrow(phl.fumbs.lost))

(fumbs.lost.plot <- hc_params %>%
  hc_add_series(name="Lost Fumbles", data = subset(phl.fumbs.lost$fumbs.lost, phl.fumbs.lost$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.fumbs.lost$name) %>%
  hc_title(text="Lost Fumbles", align= alignment))

#Rushing Attempts
phl.rush.att <- phl %>% dplyr::filter(rush.att > 0) %>% group_by(name) %>% summarise(rush.att = sum(rush.att)) %>% ungroup() %>% arrange(desc(rush.att))
phl.rush.att$rank <- c(1:nrow(phl.rush.att))

(rush.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Attempts", data = subset(phl.rush.att$rush.att, phl.rush.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rush.att$name) %>%
  hc_title(text="Rushing Attmepts", align= alignment))

#Rushing Yards
phl.rush.yds <- phl %>% dplyr::filter(rushyds > 0) %>% group_by(name) %>% summarise(rush.yds = sum(rushyds)) %>% ungroup() %>% arrange(desc(rush.yds))
phl.rush.yds$rank <- c(1:nrow(phl.rush.yds))

(rush.yds.plot <- hc_params %>%
  hc_add_series(name="Rushing Yards", data = subset(phl.rush.yds$rush.yds, phl.rush.yds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rush.yds$name) %>%
  hc_title(text="Rushing Yards", align= alignment))

#Rushing Yards / Att
phl.rush.yds.att <- left_join(phl.rush.att, phl.rush.yds, by="name")
phl.rush.yds.att$rank.x <- NULL
phl.rush.yds.att$rank.y <- NULL
phl.rush.yds.att$yds.att <- phl.rush.yds.att$rush.yds/phl.rush.att$rush.att
phl.rush.yds.att <- phl.rush.yds.att %>% arrange(desc(yds.att))
phl.rush.yds.att$rank <- c(1:nrow(phl.rush.yds.att))
phl.rush.yds.att$yds.att <- round(phl.rush.yds.att$yds.att, 2)

(phl.rush.yds.att.plot <- hc_params %>%
  hc_add_series(name="Yards per Rush", data = subset(phl.rush.yds.att$yds.att, phl.rush.yds.att$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rush.yds.att$name) %>%
  hc_title(text="Yards per Rush", align= alignment))

#Rushing TDs
phl.rush.tds <- phl %>% dplyr::filter(rushtds > 0) %>% group_by(name) %>% summarise(rush.tds = sum(rushtds)) %>% ungroup() %>% arrange(desc(rush.tds))
phl.rush.tds$rank <- c(1:nrow(phl.rush.tds))

(rush.tds.att.plot <- hc_params %>%
  hc_add_series(name="Rushing Touchdowns", data = subset(phl.rush.tds$rush.tds, phl.rush.tds$rank <=top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.rush.tds$name) %>%
  hc_title(text="Rushing Touchdowns", align= alignment))
# defense ----
phl.sacks <- phl %>% dplyr::filter(sacks > 0) %>% group_by(name) %>% summarise(sacks = sum(sacks)) %>% ungroup() %>% arrange(desc(sacks))
phl.sacks$rank <- c(1:nrow(phl.sacks))

(sacks.plot <- hc_params %>%
  hc_add_series(name="Sacks", data = subset(phl.sacks$sacks, phl.sacks$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.sacks$name)  %>%
  hc_title(text="Sacks", align= alignment))

#tackles
phl.tackles <- phl %>% dplyr::filter(tackles> 0) %>% group_by(name) %>% summarise(tackles = sum(tackles)) %>% ungroup() %>% arrange(desc(tackles))
phl.tackles$rank <- c(1:nrow(phl.tackles))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Tackles", data = subset(phl.tackles$tackles, phl.tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.tackles$name)  %>%
  hc_title(text="Tackles", align= alignment))

#assisted tackles
phl.asst.tackles <- phl %>% dplyr::filter(asst.tackles> 0) %>% group_by(name) %>% summarise(asst.tackles = sum(asst.tackles)) %>% ungroup() %>% arrange(desc(asst.tackles))
phl.asst.tackles$rank <- c(1:nrow(phl.asst.tackles))

(asst.tackles.plot <- hc_params %>%
  hc_add_series(name="Assisted Tackles", data = subset(phl.asst.tackles$asst.tackles, phl.asst.tackles$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.asst.tackles$name) %>%
  hc_title(text="Assisted Tackles", align= alignment))

#Forced Fumbles
phl.forced.fumbs <- phl %>% dplyr::filter(forced.fumbs> 0) %>% group_by(name) %>% summarise(forced.fumbs = sum(forced.fumbs)) %>% ungroup() %>% arrange(desc(forced.fumbs))
phl.forced.fumbs$rank <- c(1:nrow(phl.forced.fumbs))

(tackles.plot <- hc_params %>%
  hc_add_series(name="Forced Fumbles", data = subset(phl.forced.fumbs$forced.fumbs, phl.forced.fumbs$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.forced.fumbs$name) %>%
  hc_title(text="Forced Fumbles", align= alignment))

#Defensive Interceptions
phl.defints <- phl %>% dplyr::filter(defints > 0) %>% group_by(name) %>% summarise(defints = sum(defints)) %>% ungroup() %>% arrange(desc(defints))
phl.defints$rank <- c(1:nrow(phl.defints))

(def.ints.plot <- hc_params %>%
  hc_add_series(name="Defensive Interceptions", data = subset(phl.defints$defints, phl.defints$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.defints$name) %>%
  hc_title(text="Defensive Interceptions", align= alignment))
# special teams ----
phl.fg <- phl %>% dplyr::filter(fga > 0) %>% select(name, fga, fgm) %>% group_by(name) %>% summarise(fg = (fgm/fga)) %>% ungroup() %>% arrange(desc(fg))
phl.fg$rank <- c(1:nrow(phl.fg))

(phl.fg.plot <- hc_params %>%
  hc_add_series(name="Field Goal Percentage", data = subset(phl.fg$fg, phl.fg$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.fg$name)  %>%
  hc_title(text="Field Goal Percentage", align= alignment))

#field goal points
phl.fg.points <- phl %>% dplyr::filter(totpts.fg > 0) %>% group_by(name) %>% summarise(fg.points = sum(totpts.fg)) %>% ungroup() %>% arrange(desc(fg.points))
phl.fg.points$rank <- c(1:nrow(phl.fg.points))

(fg.points.plot <- hc_params %>%
  hc_add_series(name="Field Goal Points", data = subset(phl.fg.points$fg.points, phl.fg.points$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.fg.points$name) %>%
  hc_title(text="Field Goal Points", align= alignment))

#extra points missed
phl.xp.missed <- phl %>% dplyr::filter(xpmissed > 0) %>% group_by(name) %>% summarise(xp.missed = sum(xpmissed)) %>% ungroup() %>% arrange(desc(xp.missed))
phl.xp.missed$rank <- c(1:nrow(phl.xp.missed))

(phl.xp.missed.plot <- hc_params %>%
  hc_add_series(name="Missed Extra Points", data = subset(phl.xp.missed$xp.missed, phl.xp.missed$rank <= top.n), type = plot.type)  %>%
  hc_xAxis(categories = phl.xp.missed$name)  %>%
  hc_title(text="Missed Extra Points", align= alignment))

# time series ----
#historical attempted passes
hist.pass.att <- eagles.hist %>% dplyr::filter(pass.att > 1) %>% group_by(date, name) %>% summarise(total  = sum(pass.att))
hist.pass.att$date <- ymd(hist.pass.att$date)

bradford.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "S.Bradford"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "S.Bradford"), frequency = 52)
vick.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Vick"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Vick"), frequency = 52)
foles.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "N.Foles"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "N.Foles"), frequency = 52)
sanchez.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Sanchez"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Sanchez"), frequency = 52)
barkley.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Barkley"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Barkley"), frequency = 52)
mcnabb.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "D.McNabb"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "D.McNabb"), frequency = 52)
kolb.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "K.Kolb"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "K.Kolb"), frequency = 52)
kafka.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "M.Kafaka"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "M.Kafaka"), frequency = 52)
edwards.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "T.Edwards"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "T.Edwards"), frequency = 52)
young.ts <- xts(subset(hist.pass.att$total, hist.pass.att$name == "V.Young"), order.by = subset(hist.pass.att$date, hist.pass.att$name == "V.Young"), frequency = 52)

highchart(type = "stock") %>% 
  hc_add_series_xts(vick.ts, id = "Vick", name= "Vick") %>%
  hc_add_series_xts(foles.ts, id = "Foles", name = "Foles") %>%
  hc_add_series_xts(barkley.ts, id = "Barkley", name = "Barkley") %>% 
  hc_add_series_xts(kafka.ts, id = "Kafka", name = "Kafka")%>%
  hc_add_series_xts(sanchez.ts, id = "Sanchez", name = "Sanchez") %>%
  hc_add_series_xts(bradford.ts, id = "Bradford", name = "Bradford") %>%
  hc_add_series_xts(mcnabb.ts, id = "McNabb", name = "McNabb") %>%
  hc_add_series_xts(kolb.ts, id = "Kolb", name = "Kolb") %>%
  hc_add_series_xts(edwards.ts, id = "Edwards", name = "Edwards") %>%
  hc_add_series_xts(young.ts, id = "Young", name = "Young") %>%
  hc_rangeSelector(inputEnabled = T) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_gridlight()) %>% 
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
  hc_title(text="Historical Passing Attempts", align="left") %>%
  hc_subtitle(text="2009-2015", align="left") %>%
  hc_yAxis(title=" ") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_credits(enabled = TRUE,
             text = "nflscrapR",
             href = "https://github.com/maksimhorowitz/nflscrapR") %>% 
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_legend(enabled = T)

