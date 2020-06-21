## -----------------------------------------------------------------------------
#install.packages('tidyverse')
#installpackages('easypackages')
#install.packages("devtools")
#devtools::install_github("meysubb/cfbscrapR")
#install.packages('ggimage')
#install.packages('gt')

## -----------------------------------------------------------------------------
easypackages::libraries('tidyverse', 'cfbscrapR', 'ggimage', 'gt')

## -----------------------------------------------------------------------------
pbp_2019 <- cfb_pbp_data(year=2019, season_type = "regular", week=NULL, epa_wpa = TRUE)

## -----------------------------------------------------------------------------
#Game level data
games_19 <- cfb_game_info(2019)

#Join Games and Play-by-Play
plays19 <- left_join(pbp_2019, games_19, by = c("game_id" = "id"))

## -----------------------------------------------------------------------------
#Create Garbage time filter, eliminate FCS games, 
#filter for only rushes and passes, create success variable
pbp <- plays19 %>% filter(rush == 1| pass == 1) %>% 
  filter(!is.na(home_conference) & !is.na(away_conference)) %>%
  mutate(abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                   ifelse(period == 2 & abs_diff > 37, 1,
                   ifelse(period == 3 & abs_diff > 27, 1,
                   ifelse(period == 4 & abs_diff > 22, 1, 0)))),
         success = ifelse(down == 1 & yards_gained > .5*distance, 1,
                   ifelse(down == 2 & yards_gained > .7*distance, 1,
                   ifelse((down == 3 | down == 4) & yards_gained >=distance, 1, 0))))

## -----------------------------------------------------------------------------
glimpse(pbp)

## -----------------------------------------------------------------------------
plays <- pbp %>% select(offense_play, defense_play, down, distance, yards_to_goal, rush, pass, yards_gained, play_text, success, EPA, garbage)

## -----------------------------------------------------------------------------
offense <- plays %>% group_by(offense_play) %>% 
summarise(ypa = mean(yards_gained[pass==1]),
          ypr = mean(yards_gained[rush==1]))

## -----------------------------------------------------------------------------
offense %>% arrange(desc(ypr))

## -----------------------------------------------------------------------------
offense %>% arrange(ypa)

## -----------------------------------------------------------------------------
offense <- plays %>% group_by(offense_play) %>% 
  summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]))
defense <- plays %>% group_by(defense_play) %>% 
  summarise(epa.pass.def = mean(EPA[pass==1]), epa.pass.def = mean(EPA[rush==1]))

## -----------------------------------------------------------------------------
team.epa <- left_join(offense, defense, by = c("offense_play" = "defense_play")) 
head(team.epa)

## -----------------------------------------------------------------------------
offense <- plays %>% filter(down == 1) %>% group_by(offense_play) %>% 
  summarise(epa.pass.off = mean(EPA[pass==1]), epa.rush.off = mean(EPA[rush==1]))

## -----------------------------------------------------------------------------
defense <- plays %>% filter(down == 1) %>% group_by(defense_play) %>% 
  summarise(epa.pass.def = mean(EPA[pass==1]), epa.pass.def = mean(EPA[rush==1]))

## -----------------------------------------------------------------------------
firstdown.epa <- left_join(offense, defense, by = c("offense_play" = "defense_play")) 

head(firstdown.epa)

## -----------------------------------------------------------------------------
success <- plays %>% filter(garbage == 0 & down < 3) %>%
  group_by(offense_play) %>% 
  summarise(success.rte = mean(success),
            rush.rte = mean(rush))

## -----------------------------------------------------------------------------
head(success)

## -----------------------------------------------------------------------------
success %>% ggplot(aes(x=rush.rte, y=success.rte)) + geom_point()

## -----------------------------------------------------------------------------
success %>% ggplot(aes(x=rush.rte, y=success.rte)) + geom_point() + 
  geom_vline(xintercept = mean(success$rush.rte), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(success$success.rte), linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Early Downs Rush Rate", y= "Success Rate",
       title = "2019 FBS Early Downs Rush Rate and Success") +
  theme_minimal() +
	theme(axis.title = element_text(size = 12),
	axis.text = element_text(size = 10),
	plot.title = element_text(size = 20),
	plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
	panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
cfblogos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>% select(school, logo)
chartdata <- success %>% left_join(cfblogos, by = c("offense_play" = "school"))

## -----------------------------------------------------------------------------
chartdata %>% ggplot(aes(x=rush.rte, y=success.rte)) + geom_image(image = chartdata$logo, asp = 16/9) + 
  geom_vline(xintercept = mean(chartdata$rush.rte), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(chartdata$success.rte), linetype = "dashed", color = "red", alpha = 0.5) +
  labs(x = "Early Downs Rush Rate", y= "Success Rate",
       title = "2019 FBS Early Downs Rush Rate and Success") +
  theme_minimal() +
	theme(axis.title = element_text(size = 12),
	axis.text = element_text(size = 10),
	plot.title = element_text(size = 20),
	plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12),
	panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
tcu <- plays %>% filter(offense_play == "TCU") 

tcu %>%
  ggplot(aes(x=yards_to_goal, y=EPA)) +
  geom_point() +
  labs(x = "Yard Line",
	y = "EPA",
	title = "Expected Points Added by Field Position",
	subtitle = "TCU Offense 2019") +
  geom_hline(yintercept = 0, alpha = 0.5, col = "purple") +
  theme_minimal() +
	theme(axis.title = element_text(size = 12),
	axis.text = element_text(size = 10),
	plot.title = element_text(size = 20),
	plot.subtitle = element_text(size = 14),
  plot.caption = element_text(size = 12),
	panel.grid.minor = element_blank())

## -----------------------------------------------------------------------------
plays %>% filter(offense_play == "TCU" & EPA < -4) %>% select(offense_play, defense_play, play_text, down, distance, yards_to_goal)

## -----------------------------------------------------------------------------
#Passing
team.epa %>% arrange(desc(epa.pass.off)) %>% mutate(rank = dense_rank(desc(epa.pass.off))) %>% 
  filter(rank < 10) %>% gt()

## -----------------------------------------------------------------------------
team.epa %>% arrange(desc(epa.pass.off)) %>% mutate(rank = dense_rank(desc(epa.pass.off))) %>%
  select(rank, offense_play, epa.pass.off) %>% 
  filter(rank < 11) %>% gt() %>%
  tab_header(title = "Best Passing Teams") %>%
  cols_label(rank = "Rank", offense_play = "Offense", epa.pass.off = "EPA/Attempt")

