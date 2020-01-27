library(ggplot2)
library(reshape2)
library(scales)
library(tidyr)
library(xml2)
library(stringi)

rm(list=ls())


#### Web scraping ####

# Webscrape player totals
myurl <- "https://www.basketball-reference.com/leagues/NBA_2019_totals.html"
apportionment_table_complex <- read_html(myurl, "table")
aptable <- html_table(apportionment_table_complex)[[1]]
aptable <- as.data.frame(aptable)
for(i in 1:423) {
  if(aptable$Player[i] == "Player") {
    aptable <- aptable[-i,]
  }
}
player_total <- aptable
player_total <- as.data.frame(player_total)

# Webscrape standings
myurl1 <- "https://www.basketball-reference.com/leagues/NBA_2019.html"
webpage <- read_html(myurl1)
apportionment_table_complex1 <- read_html(myurl1, "table")
East_Standings <- html_table(apportionment_table_complex1)[1]
East_Standings <- as.data.frame(East_Standings)
West_Standings <- html_table(apportionment_table_complex1)[2]
West_Standings <- as.data.frame(West_Standings)
East_Divisions <- html_table(apportionment_table_complex1)[3]
East_Divisions <- as.data.frame(East_Divisions)
West_Divisions <- html_table(apportionment_table_complex1)[4]
West_Divisions <- as.data.frame(West_Divisions)

# Webscrape team stats
alt_tables <- xml2::xml_find_all(webpage,"//comment()") %>% {
  #Find only commented nodes that contain the regex for html table markup
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  # Remove the comment begin and end tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Loop through the pieces that have tables within markup and 
  # apply the same functions
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
}


#### Data cleaning ####

# Creates data frames
team_per_game <- as.data.frame(alt_tables[1])
opponent_per_game <- as.data.frame(alt_tables[2])
team_stats <- as.data.frame(alt_tables[3])
opponent_stats <- as.data.frame(alt_tables[4])
team_per_100 <- as.data.frame(alt_tables[5])
opponent_per_100 <- as.data.frame(alt_tables[6])
miscellaneous_stats <- as.data.frame(alt_tables[7])
team_shooting <- as.data.frame(alt_tables[8])
opponent_shooting <- as.data.frame(alt_tables[9])

# Fixes headers
names(miscellaneous_stats) <- miscellaneous_stats[1,]
miscellaneous_stats <- miscellaneous_stats[-1,]
names(team_shooting) <- team_shooting[2,]
team_shooting <- team_shooting[-c(1,2),]
names(opponent_shooting) <- opponent_shooting[2,]
opponent_shooting <- opponent_shooting[-c(1,2),]

# Fixes row numbering
rownames(team_per_game) <- 1:nrow(team_per_game)
rownames(opponent_per_game) <- 1:nrow(opponent_per_game)
rownames(team_stats) <- 1:nrow(team_stats)
rownames(opponent_stats) <- 1:nrow(opponent_stats)
rownames(team_per_100) <- 1:nrow(team_per_100)
rownames(opponent_per_100) <- 1:nrow(opponent_per_100)
rownames(miscellaneous_stats) <- 1:nrow(miscellaneous_stats)
rownames(team_shooting) <- 1:nrow(team_shooting)
rownames(opponent_shooting) <- 1:nrow(opponent_shooting)
rownames(player_total) <- 1:nrow(player_total)

team_shooting <- team_shooting[2:28]
names(team_shooting)[1] <- "Team"
names(team_shooting)[2] <- "GP"
names(team_shooting)[3] <- "MP"
names(team_shooting)[4] <- "FGP"
names(team_shooting)[5] <- "DPS"
names(team_shooting)[6] <- "P2"
names(team_shooting)[7] <- "P0-3"
names(team_shooting)[8] <- "P3-10"
names(team_shooting)[9] <- "P10-16"
names(team_shooting)[10] <- "P16+"
names(team_shooting)[11] <- "P3"
names(team_shooting)[12] <- "FGP2"
names(team_shooting)[13] <- "FGP0-3"
names(team_shooting)[14] <- "FGP3-10"
names(team_shooting)[15] <- "FGP10-16"
names(team_shooting)[16] <- "FGP16+"
names(team_shooting)[17] <- "FGP3"
names(team_shooting)[18] <- "PAST2"
names(team_shooting)[19] <- "PDUNK"
names(team_shooting)[20] <- "MDUNK"
names(team_shooting)[21] <- "PLAY"
names(team_shooting)[22] <- "MLAY"
names(team_shooting)[23] <- "PAST3"
names(team_shooting)[24] <- "PCORN3"
names(team_shooting)[25] <- "FGPCORN3"
names(team_shooting)[26] <- "AHEAVE"
names(team_shooting)[27] <- "MHEAVE"
names(team_shooting)


#### Exploratory data analysis ####

# Shot selection graph
FG_Distance <- team_shooting[-31,c(1,7:11)]
FG_Distance <- as.data.frame(FG_Distance)
FG_Distance_m <- melt(FG_Distance, id = "Team")
FG_Distance_m$value <- as.numeric(FG_Distance_m$value)

ggplot(FG_Distance_m, aes(x = Team, y = value, fill = variable)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") + coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Shot Selection by Distance", caption = "basketball-reference.com",
       y = "Percentage Shot Selection", fill = "Range") +
  geom_hline(yintercept = c(.25, .5, .75), col = "darkgray") +
  scale_fill_manual(values = c("red", "orange", "yellow", "green", "blue"))

# Difference between corner threes and regular threes
corner_threes <- team_shooting[,c(1,17,25)]
corner_threes$color <- ifelse(corner_threes$FGP3 < corner_threes$FGPCORN3, "Increase", "Decrease")
corner_threes$FGP3 <- as.numeric(corner_threes$FGP3)
corner_threes$FGPCORN3 <- as.numeric(corner_threes$FGPCORN3)

ggplot(corner_threes, aes(x = Team, y = FGP3)) + geom_point(aes(y = FGP3, col = "Three"), size = 3) +
  geom_point(aes(y = FGPCORN3, col = "Corner Three"), size = 3) +
  geom_segment(aes(x = Team, y = FGP3, xend = Team, yend = FGPCORN3, col = color)) +
  geom_hline(yintercept = mean(corner_threes$FGP3), col = "Red") +
  geom_hline(yintercept = mean(corner_threes$FGPCORN3), col = "dark Green") + coord_flip() +
  labs(y = "Three Point Percentage", col = "Corner Three",
       title = "Three vs. Corner Three", caption = "basketball-reference.com") +
  scale_color_manual(values = c("dark Green", "Red", "dark Green", "Red"))

# Points for and points against
team_per_game2 <- team_per_game[1:30,]
team_per_game2$Team <- factor(team_per_game2$Team, levels = team_per_game2[order(team_per_game$PTS), "Team"])
opponent_per_game2 <- opponent_per_game[1:30,]
team_per_game2$OppPTS <- opponent_per_game2$PTS

ggplot(team_per_game2, aes(x = Team, y = PTS, col = "Points For")) + geom_point(stat = "identity") +
  geom_point(aes(x = Team, y = OppPTS, col = "Points Against")) +
  labs(y = "Points", title = "Points For and Points Against", caption = "basketball-reference.com",
       color = NULL) +  coord_flip()

# Top 25 scorers
player_total2 <- player_total
player_total2 <- as.data.frame(player_total2)
player_total2$PTS <- as.numeric(player_total2$PTS)

top_25_scorers <- head(arrange(player_total2, desc(PTS)), n = 25)
top_25_scorers$Player <- factor(top_25_scorers$Player,
                                levels = top_25_scorers[order(top_25_scorers$PTS), "Player"])
top_25_scorers$Rk <- 1:nrow(top_25_scorers)
top_25_scorers$MP <- as.numeric(top_25_scorers$MP)
top_25_scorers$G <- as.numeric(top_25_scorers$G)
top_25_scorers$PPG <- top_25_scorers$PTS / top_25_scorers$G

ggplot(top_25_scorers, aes(x = Player, y = PTS)) +
  geom_point(aes(col = cut(top_25_scorers$MP, c(min(top_25_scorers$MP)-1, summary(top_25_scorers$MP)[[2]],
                       summary(top_25_scorers$MP)[[5]], max(top_25_scorers$MP)+1)))) +
  scale_color_manual(values = c("Green", "Yellow", "Red")) + labs(color = "Minutes") + coord_flip()

player_total[which.max(player_total$PTS),]



