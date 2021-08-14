library(tidyverse)
library(ggthemes)

#df <- read.csv("soccer.csv")

Team <- c("shield", "crown")
ball_possession <- c(75, 25)
goal_attempts <- c(23, 4)
sog <- c(6, 1)
off_goal <- c(12, 1)
blocked_shots <- c(5, 2)

df <- data.frame(Team, ball_possession, goal_attempts, 
                 sog, off_goal, blocked_shots)

df %>%
  pivot_longer(!Team, names_to = "Metric") %>%
  arrange((Metric)) %>%
  group_by(Metric) %>%
  mutate(total = sum(value, na.rm = TRUE),
         pct = value / total) %>%
  mutate(new_pct = ifelse(Team == "shield", -1 * pct, pct),
         loc = ifelse(Team == "shield", -1.125, 1.125),
         min = -1, 
         max = 1,
         Metric = ifelse(Metric == "ball_possession",
                         "Ball Possession",
                         ifelse(Metric == "blocked_shots", "Blocked Shots",
                                ifelse(Metric == "goal_attempts", "Goal Attempts",
                                       ifelse(Metric == "off_goal", "Off Goal Shots",
                                              ifelse(Metric == "sog", "Shots on Goal", 
                                                     NA)))))) %>%
  ggplot(aes(x = reorder(Metric, total), y = new_pct)) +
  geom_col(aes(y = 1.25), fill = "whitesmoke", width = 0.5) +
  geom_col(aes(y = -1.25), fill = "whitesmoke", width = 0.5) +
  geom_col(aes(color = Team, fill = Team), alpha = 0.7, width = 0.5) +
  geom_abline(slope = 0, intercept = 0, color = "white", size = 1) + 
  geom_text(aes(label = value, y = loc)) +
  labs(title = "0 - 1",
       subtitle = "2nd Half - 77:29") + 
  coord_flip() +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, colour = "black",
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 13, colour = "black",
                                  face = "bold"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(),
        legend.position = "bottom") +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  ylim(-1.25, 1.25)
