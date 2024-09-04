library(rvest)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(RSelenium)
library(stringr)

### change to your player id & player_name###
player_id <- "84349021"
player_name <- "afelka"

### games stats 
url <- paste0("https://boardgamearena.com/gamestats?player=",player_id)

### Setup Selenium with the newest chrome version ### 
### follow this answer about how to download latest chromedriver https://stackoverflow.com/a/78405621/10710995
rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever =
                            system2(command = "wmic",
                                    args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                    stdout = TRUE,
                                    stderr = TRUE) %>%
                            stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                            magrittr::extract(!is.na(.)) %>%
                            stringr::str_replace_all(pattern = "\\.",
                                                     replacement = "\\\\.") %>%
                            paste0("^",  .) %>%
                            stringr::str_subset(string =
                                                  binman::list_versions(appname = "chromedriver") %>%
                                                  dplyr::last()) %>%
                            as.numeric_version() %>%
                            max() %>%
                            as.character())

remDr <- rD$client

## set page load time out ## 
remDr$setTimeout(type="page load", milliseconds = 10000)

## go to the stats page
remDr$navigate(url)

### Connect to Boardgamearena using email and password , might ask for captcha or 
### you might need to accept cookies
user_email <- remDr$findElement("css", "#username_input")
user_password <- remDr$findElement("css", "#password_input")

## pop up box to enter email & password
user_email$sendKeysToElement(list(rstudioapi::askForPassword()))
user_password$sendKeysToElement(list(rstudioapi::askForPassword()))

## click sign_in button
sign_in_button <- remDr$findElement("css", "#submit_login_button") 
sign_in_button$clickElement()

### depending on how many games you have played change 1300 (I had played 13000 games when I 
### ran the code, 10 games per page)
for (i in 1:1300) {

### click see more button 
see_more_tables <- remDr$findElement("css", "#see_more_tables")  
see_more_tables$clickElement()
Sys.sleep(0.5)

}

### get the content as html
content <-  
  remDr$getPageSource()[[1]] %>% 
  read_html(encoding = "UTF-8") 

### Close Selenium
remDr$close()
rD$server$stop()

### get names for games
games <- content %>%
  html_nodes(xpath = '//*[@class="table_name gamename"]') %>%
  html_text() %>% data.frame() %>% rename(game = ".")

### get dates by finding HH:MM information 
pattern <- "\\b\\d{1,2}:\\d{2}\\b"

dates <- content %>%
  html_nodes(xpath = '//div[@class="smalltext"]') %>% 
  html_text() %>% data.frame() %>% rename(dates = ".") %>% 
  mutate(valid = stringr::str_count(dates, pattern)) %>%
  filter(valid == 1 | grepl("ago", dates)) %>% 
  select(dates)

### get rank of the players
rank <- content %>%
  html_nodes(xpath = '//div[@class="rank"]') %>%
  html_text()

### get player names , takes long time
player <- content %>%
  html_nodes(xpath = '//div[@class="simple-score-entry"]//a[@class="playername"]') %>%
  html_text()

### combine rank and player and filter by player_name defined on top
rank_player_combined <- data.frame(rank,player) %>% filter(player == player_name)

### combine all data
total <- cbind(games, dates, rank_player_combined)

### add game count
total$game_count <- nrow(total):1

### limit with 13000, change if you want to analyze more data. add victory column and add 
### 10 bins, again change if you want to have more bins
total <-   total %>% 
           arrange(game_count) %>%
           filter(game_count <= 13000) %>%
           mutate(dates = substr(dates,1,10)) %>%
           mutate(bin = ntile(game_count, 10)) %>% 
           mutate(victory = if_else(rank == "1st", 1,0))

### create summary tables & plots 
victory_by_bins <- total %>% group_by(bin) %>% 
                   summarise(total_victory = sum(victory),
                             game_in_bin = n()) %>% ungroup() %>%
                   mutate(victory_percentage = total_victory / game_in_bin)

ggplot(victory_by_bins, aes(x = bin)) +
  geom_point(aes(y = victory_percentage)) +
  geom_line(aes(y = victory_percentage)) +
  geom_text(aes(y = victory_percentage, label = scales::percent(victory_percentage)),
            vjust = -2,
            size = 3.5) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1) ,
    name = "Victory Percentage",
    labels = scales::percent
    ) + 
  scale_x_continuous(
    labels = as.character(victory_by_bins$bin),
    breaks =victory_by_bins$bin,
    name = "1000 Games per bin") +
  theme_classic() +
  labs(title = "Victory Percentage by 1000 Games")

number_of_played_games <- total %>% group_by(game) %>%
                          summarise(total_victory = sum(victory),
                          game_by_name = n()) %>% ungroup() %>%
                          mutate(victory_percentage = total_victory / game_by_name) %>%
                          arrange(desc(game_by_name))

datatable(number_of_played_games, options = list(pageLength = 15)) %>%
          formatPercentage(4)

top_15_games <- number_of_played_games %>% slice(1:15) %>% select(game)

total$grouped_game_name <- ifelse(total$game %in% top_15_games$game, total$game, "Other")

games_played_by_bin <- total %>% group_by(bin, grouped_game_name) %>% 
                       summarise(played = n()) %>% ungroup() %>% 
                       pivot_wider(names_from =  bin,
                                   values_from = played) %>%
                       mutate_all(~replace(., is.na(.), 0)) %>% 
                       mutate(total = rowSums(across(where(is.numeric)))) %>%
                       arrange(desc(total))

datatable(games_played_by_bin, options = list(pageLength = 16)) 
