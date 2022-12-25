install.packages("tidyverse")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("lubridate")
install.packages("zoo") #for rolling averages
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
library(zoo) 
library(tidyverse)
library(lubridate)
library(patchwork)
results <- read_csv("/Users/timwelch/Documents/GitHub/World-Cup/Data/results.csv")
WorldCups <- read_csv("/Users/timwelch/Documents/GitHub/World-Cup/Data/WorldCups.csv")
#columns for home win vs. draw vs. away win
brazil <- results %>%
  mutate(draw = home_score == away_score,
         home_win = home_score > away_score,
         away_win = away_score > home_score)%>%
  select(date, home_team, #selecting columns of interest
         home_win, away_team, 
         away_win, draw, tournament)%>%
  filter(home_team == "Brazil" | # selecting Brazil games
           away_team == "Brazil")

#creating column for game results
brazil_result <- brazil %>%
  mutate(brazil_home_result =  ifelse(home_team == "Brazil" &
                                        home_win == "TRUE", "Win", 
                                      "Not Win")) %>%
  mutate(brazil_away_result = ifelse(
    away_team == "Brazil" &
      away_win == "TRUE", "Win", "Not Win")) %>%
  mutate(result = ifelse(
    brazil_home_result == "Win" | 
      brazil_away_result == "Win", 1, 0))

#changing date format and reducing data frame 
#down to date, results, tournament
brazil_result <- brazil_result%>%
  mutate(Date = as.Date(date))%>%
  select(Date, result, tournament)

#splitting year into it's own column
brazil_result$year <- year(ymd(brazil_result$Date))

#selecting
brazil_result <- brazil_result %>%
  select(result, year, tournament)

#calculating win ratio per year
win_ratio <- brazil_result %>%
  group_by(year) %>%
  summarise(total = sum(result), n = n()) %>%
  mutate(win_ratio = total / n)

#Filtering World Cup Results into 
#wins, runnerups, and thirds to get exact years
brazil_wc_wins <- WorldCups %>%
  filter(Winner == "Brazil") %>% 
  select(Year)
brazil_wc_ru <- WorldCups %>%
  filter(`Runners-Up`== "Brazil") %>%
  select(Year)
brazil_wc_third <- WorldCups %>%
  filter(Third == "Brazil") %>%
  filter(Year > 1940) %>%
  select(Year)

#Refining visualization
plot <- win_ratio %>%
  filter(year >1940) %>%
  ggplot(aes(x = year, y = win_ratio)) +
  geom_line(color = "blue", size = .2) +
  geom_point(shape = 21, color ='black', 
             fill = '#0099ff', size = 2) +
  theme_classic()+
  labs(title = "Brazil Win Ratio All Competitions",
       x = "Year",
       y = "Win Ratio")
plot

#Adding in line segments
plot2 <- plot +
  geom_segment(x = 1958,
               y = .8,
               xend = 1958,
               yend = 1.05,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1962,
               y = .95,
               xend = 1962,
               yend = 1.05,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1970,
               y = .9,
               xend = 1970,
               yend = 1.05,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1994,
               y = .8,
               xend = 1994,
               yend = 1.05,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 2002,
               y = .85,
               xend = 2002,
               yend = 1.05,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1950,
               y = .52,
               xend = 1950,
               yend = .6,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1998,
               y = .52,
               xend = 1998,
               yend = .55,
               linetype = "dashed",
               col = "#336600")+
  geom_segment(x = 1978,
               y = .42,
               xend = 1978,
               yend = .55,
               linetype = "dashed",
               col = "#336600")
plot2
#adding annotations
plot2 +  geom_text(data = brazil_wc_wins, 
                   aes(x =Year, y = 1.07, label = "Champions"), 
                   color = '#336600', size = 2, angle = 30, fontface = "bold")+
  geom_text(data = brazil_wc_ru, aes(x = Year, y = .5, label = "Runner Ups"),
            color = '#336600',
            size = 2, angle = 30, fontface = "bold")+
  geom_text(data = brazil_wc_third, aes(x = Year, y = .4, label = "Third"),
            color = '#336600', 
            size = 2, angle = 30, fontface = "bold")        


#------------- points system + rolling average --------------#

#creating points system: 
#3 points for win, 1 for a draw, 0 for a loss
brazil_points <- brazil %>%
  mutate(brazil_home_result =  ifelse(home_team == "Brazil" &
                                        home_win == "TRUE", "Win", 
                                      "Not Win")) %>%
  mutate(brazil_away_result = ifelse(
    away_team == "Brazil" &
      away_win == "TRUE", "Win", "Not Win")) %>%
  mutate(win_points = ifelse(
    brazil_home_result == "Win" |
      brazil_away_result == "Win", 3, 0))%>%
  mutate(draw_points = ifelse(draw == "TRUE", 1, 0))%>%
  mutate(result = draw_points + win_points)%>%
  mutate(Date = as.Date(date))%>% #transforming to actual date
  select(Date, result, tournament)

#splitting year into it's own column
brazil_points$year <- year(ymd(brazil_points$Date))

#calculating rolling averages
brazil_roll <- brazil_points%>%
  mutate(win_3 = rollmean(result, k = 3, fill = NA),
         win_7 = rollmean(result, k = 7, fill = NA),
         win_11 = rollmean(result, k = 11, fill = NA),
         win_15 = rollmean(result, k = 15, fill = NA))

view(brazil_roll)


#Creating rolling visualization
spline_d <- as.data.frame(spline(brazil_roll$year, brazil_roll$win_7))
roll_plot <- spline_d %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_line(color = "blue", size = .2) +
  theme_bw() +
  labs(title = "Brazil Men's National Football Team  Success", 
  subtitle = "7 Year Rolling Point Average",
       x = "Year",
       y = "Point Average")

roll_plot

roll_plot_2 <- roll_plot + 
  geom_segment(x = 1958,
               y = 2.45,
               xend = 1958,
               yend = 2.85,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1962,
               y = 2.92,
               xend = 1961.5,
               yend = 2.98,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1969.5,
               y = 2.82,
               xend = 1970,
               yend = 2.87,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1994.5,
               y = 2.72,
               xend = 1994,
               yend = 2.87,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 2002,
               y = 2.59,
               xend = 2002,
               yend = 2.87,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1950,
               y = 1.95,
               xend = 1950,
               yend = 2.15,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1998,
               y = 1.92,
               xend = 1998,
               yend = 1.94,
               linetype = "dashed",
               col = "#336600") +
  geom_segment(x = 1978,
               y = 1.7,
               xend = 1978,
               yend = 2.1,
               linetype = "dashed",
               col = "#336600") +
  geom_text(data = brazil_wc_wins, 
            aes(x =Year, y = 2.9, label = "Champions"),
            color = '#336600', size = 2, angle = 30, fontface = "bold") +
  geom_text(data = brazil_wc_ru, aes(x = Year, y = 1.9, label = "Runner Ups"),
            color = '#336600',
            size = 2, angle = 30, fontface = "bold" ) +
  geom_text(data = brazil_wc_third, aes(x = Year, y = 1.7, label = "Third"),
            color = '#336600',
            size = 2, angle = 30, fontface = "bold")
roll_plot_2
