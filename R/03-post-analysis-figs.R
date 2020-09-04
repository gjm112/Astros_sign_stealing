## Ryan Elmore
## 20 Aug 2020
## Random term figs

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggridges)
library(forcats)
library(ggjoy)
library(hrbrthemes)

players <- readRDS("data/top-nine-players.rds")
load("int_boot.RData")
names(int_boot)[4:503] <- paste("B", 1:500, sep = "")
int_boot$batter_first[5:6] <- c("Yuli", "Josh")
int_boot <- int_boot %>% 
  dplyr::mutate(., player = paste(batter_first, batter_last, sep = " ")) %>%
  dplyr::filter(., player %in% players) %>% 
  dplyr::select(., -batter_first, -batter_last, -batter_mlbid) %>% 
  tidyr::pivot_longer(., 1:500)

load("slope_boot.RData")
names(slope_boot)[4:503] <- paste("B", 1:500, sep = "")
slope_boot$batter_first[5:6] <- c("Yuli", "Josh")
slope_boot <- slope_boot %>% 
  dplyr::mutate(., player = paste(batter_first, batter_last, sep = " ")) %>%
  dplyr::filter(., player %in% players) %>% 
  dplyr::select(., -batter_first, -batter_last, -batter_mlbid) %>% 
  tidyr::pivot_longer(., 1:500)

int_boot %>% 
  group_by(player) %>% 
  summarize(m = mean(value)) %>% 
  arrange(desc(m))

p <- ggplot(data = int_boot %>% 
              mutate(player = fct_relevel(player, 
                                          levels = "Jose Altuve",
                                          "Brian McCann",
                                          "Josh Reddick",
                                          "Yuli Gurriel",
                                          "Carlos Beltran",
                                          "Alex Bregman",
                                          "Marwin Gonzalez",
                                          "Carlos Correa",
                                          "George Springer")),
            aes(x = value, y = player))
p + geom_density_ridges(alpha = .75, col = "#002D62", fill = "#EB6E1F") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "intercept", y = "") +
  theme_ipsum()
ggsave("fig/players-intercepts.png", hei = 8, wid = 6, units = "in")

slope_boot %>% 
  group_by(player) %>% 
  summarize(m = mean(value)) %>% 
  arrange(desc(m))

p <- ggplot(data = slope_boot %>% 
              mutate(player = fct_relevel(player, 
                                          levels = "George Springer",
                                          "Yuli Gurriel",
                                          "Josh Reddick",
                                          "Carlos Correa",
                                          "Carlos Beltran",
                                          "Alex Bregman",
                                          "Jose Altuve",
                                          "Brian McCann",
                                          "Marwin Gonzalez")),
            aes(x = value, y = player))
p + geom_density_ridges(alpha = .75, col = "#002D62", fill = "#EB6E1F") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "slope", y = "") +
  theme_ipsum()
ggsave("fig/players-slopes.png", hei = 8, wid = 6, units = "in")

## Grouped on one figure

df <- bind_rows(int_boot %>% mutate(type = "intercept"),
                slope_boot %>% mutate(type = "slope"))
p <- ggplot(data = df %>% 
              mutate(player = fct_relevel(player, 
                                          levels = "Jose Altuve",
                                          "Brian McCann",
                                          "Josh Reddick",
                                          "Yuli Gurriel",
                                          "Carlos Beltran",
                                          "Alex Bregman",
                                          "Marwin Gonzalez",
                                          "Carlos Correa",
                                          "George Springer")),
            aes(x = value, y = player, fill = type))
p + geom_density_ridges(scale = 1, alpha = .5, rel_min_height = 0.015) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual("", values = c("#002D62", "#EB6E1F")) +
  labs(x = "bootstrap estimates of slope and intercept", y = "") +
  theme_ipsum()
ggsave("fig/players-slopes-intercepts.png", hei = 8, wid = 6, units = "in")
