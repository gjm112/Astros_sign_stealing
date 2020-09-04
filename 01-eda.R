## Ryan Elmore
## 9 March 2020
## Bangs Analysis

library(dplyr)
library(ggplot2)
library(lubridate)
library(questionr)


## see Greg's all_models_for_paper.R file for prelim data manipulation

df <- readRDS("data/bangs-merged-final.rds") %>%
  mutate(has_bangs = if_else(has_bangs == "y", "Yes", "No"))

df_sum <- df %>%
  group_by(pi_pitch_group, has_bangs, swing) %>%
  summarize(n = n())
df_sum

df %>%
  group_by(has_bangs, pitch_category, swing) %>%
  summarize(n = n())

## Exit velocity by bangs

p <-  df %>% 
  ggplot(aes(x = launch_speed, 
             fill = has_bangs))
p + geom_density(alpha = 0.5) + 
  scale_fill_manual("bang", values = c("#002D62", "#EB6E1F")) +
  facet_grid(pi_pitch_group ~ .) +
  labs(x = "exit velocity (mph)") +
  theme_bw()

ggsave("fig/ev-by-bang.png", hei = 8, wid = 6, unit = "in")

# p <-  df %>% 
#   filter(pi_pitch_group %in% c("CH", "CU", "SL")) %>%
#   ggplot(aes(x = launch_speed, 
#              fill = has_bangs))
# p + geom_density(alpha = 0.5) + 
#   scale_fill_manual("bang", values = c("#002D62", "#EB6E1F")) +
#   facet_grid(pi_pitch_group ~ .) +
#   labs(x = "exit velocity (mph)") +
#   theme_bw()
# ggsave("fig/ev-by-bang-no-fa.pdf", hei = 8, wid = 6)

## Bangs by top nine players
players <- df %>%
  group_by(batter) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(9) %>%
  pull(batter) %>%
  as.character()
saveRDS(players, "data/top-nine-players.rds")

p <- df %>%
  filter(batter %in% players) %>%
  group_by(batter, has_bangs) %>%
  summarize(n = n()) %>%
  ggplot(., aes(x = reorder(batter, n), y = n, fill = has_bangs)) 

# p + geom_bar(stat = "identity", position = "dodge") +
p + geom_bar(stat = "identity") +
  scale_fill_manual("bangs", values = c("#002D62", "#EB6E1F"),
                    labels = c("no", "yes")) +
  labs(y = "number of pitches",
       x = "") +
  theme_bw() +
  coord_flip()

ggsave("fig/players-bangs-stacked.png", hei = 8, wid = 6, units = "in")

## Bangs over time
df_bangs_time <- inner_join(
  df %>%
    filter(has_bangs == "Yes") %>%
    group_by(m = month(game_date, label = TRUE)) %>%
    summarize(n = n()),
  df_bangs_time <- df %>%
    group_by(m = month(game_date, label = TRUE)) %>%
    summarize(abs = n())) 
df_bangs_time$ratio = df_bangs_time$n/df_bangs_time$abs

p <- ggplot(data = df_bangs_time,
            aes(x = m, y = n, fill = "#002D62"))
p + geom_bar(stat = "identity") +
  scale_fill_manual("bangs", values = c("#002D62")) +
  labs(x = "month",
       y = "number of bangs") +
  guides(fill = F) +
  theme_bw()

ggsave("fig/bangs-by-month.png", hei = 6, wid = 6, unit = "in")

## Chi-square tests

df %>% 
  group_by(pi_pitch_group, has_bangs) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n))

chisq.test(table(df$has_bangs, df$pi_pitch_group))

df <- df %>% 
  dplyr::mutate(., I_swing = ifelse(swing == "swing", 1, 0))

prt <- prop.table(table(df$has_bangs, df$I_swing), 1)
tt <- ((prt[2, 2]/(1-prt[2, 2])) - (prt[1, 2]/(1-prt[1, 2])))/ ((prt[2, 2]/(1-prt[2, 2])))

odds.ratio(table(df$has_bangs, df$I_swing))
grt <- table(df$has_bangs, df$I_swing)
grt
prod(diag(grt)) / prod(c(grt[1,2], grt[2,1]))  





# odds.ratio(table(df$I_swing, df$has_bangs))
# table(df$I_swing, df$has_bangs)


# top_zone <- 3.5
# bot_zone <- 1.6
# left_zone <- -0.75
# right_zone <- 0.75
# strike_zone_df <- data.frame(
#   x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
#   y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
# )
# 
# 
# 
# p <- df %>% 
#   filter(pi_pitch_group %in% c("CH", "CU", "SL", "FA")) %>%
#   ggplot(.,
#          aes(x = px,
#              y = pz,
#              col = swing))
# p + geom_point(alpha = 0.15) +
#   geom_path(data = strike_zone_df, aes(x, y), color = "black") +
#   facet_grid(pi_pitch_group ~ has_bangs) +
#   labs(x = "horizontal location (ft)",
#        y = "vertical location (ft)") +
#   xlim(-3.5, 3.5) +
#   scale_color_manual("", values = c("#002D62", "#EB6E1F")) +
#   coord_fixed() +
#   theme_bw() 
# 
# ggsave("fig/swing-by-bang-pitch-group.png")

p <- df %>% 
  filter(pi_pitch_group %in% c("CH", "CU", "SL"),
         batter == "George Springer") %>%
  ggplot(.,
         aes(x = px,
             y = pz,
             col = swing))
p + geom_point(alpha = 0.5) +
  geom_path(data = strike_zone_df, aes(x, y), color = "black") +
  facet_grid(has_bangs ~ pi_pitch_group) +
  labs(x = "horizontal location (ft)",
       y = "vertical location (ft)") +
  xlim(-3.5, 3.5) +
  scale_color_manual("", values = c("#002D62", "#EB6E1F")) +
  coord_fixed() +
  theme_bw() 

# p <- df %>% 
#   filter(pitch_category %in% c("FB")) %>%
#   ggplot(.,
#          aes(x = px,
#              y = pz,
#              col = swing))
# p + geom_point(alpha = 0.25) +
#   geom_path(data = strike_zone_df, aes(x, y), color = "black") +
#   facet_grid(~ has_bangs) +
#   labs(caption = "Data courtesy of MLBAM/Bangs",
#        x = "horizontal location (ft)",
#        y = "vertical location (ft)") +
#   xlim(-3.5, 3.5) +
#   scale_color_brewer(palette = "Set1") +
#   coord_fixed() +
#   theme_bw() 
# 
# ggsave("fig/swing-by-bang-fb.pdf", hei = 7.5, wid = 10)

# p <- ggplot(data = df_bangs_time,
#             aes(x = m, y = ratio, fill = "#002D62"))
# p + geom_bar(stat = "identity") +
#   scale_fill_manual("bangs", values = c("#002D62")) +
#   labs(x = "month",
#        y = "proportion of bangs relative to total at bats") +
#   guides(fill = F) +
#   theme_bw()
# 
# ggsave("fig/bangs-by-month-by-at-bats.png")
# 
# ## Look at CS Prob
# 
# p <- ggplot(data = df %>% mutate(swing = if_else(swing == "swing", 1, 0)),
#             aes(x = cs_prob, y = swing))
# p + geom_point(alpha = .05, col = "#002D62") +
#   facet_grid(pi_pitch_group ~ has_bangs) +
#   geom_smooth(method = "gam", formula = y ~ s(x),
#               method.args = list(family = "binomial"),
#               col = "#EB6E1F")
# 
# p <- ggplot(data = df %>% mutate(swing = if_else(swing == "swing", 1, 0)),
#             aes(x = cs_prob, y = swing, group = has_bangs))
# p + geom_point(alpha = .05, col = "#002D62") +
#   facet_grid(pi_pitch_group ~ .) +
#   geom_smooth(method = "gam", formula = y ~ s(x),
#               method.args = list(family = "binomial"),
#               col = "#EB6E1F")
# 
# p <- ggplot(data = df %>% mutate(swing = if_else(swing == "swing", 1, 0)),
#             aes(x = cs_prob, y = swing, group = has_bangs, col = has_bangs))
# p + geom_point(alpha = .05, col = "black") +
#   facet_wrap(pi_pitch_group ~ ., ncol = 2) +
#   geom_smooth(method = "glm", formula = y ~ x,
#               method.args = list(family = "binomial")) +
#   scale_color_manual("Bang", values = c("#002D62", "#EB6E1F")) +
#   labs(x = "called strike probability",
#        y = "swing probability") +
#   theme_bw()
# ggsave("fig/swing-prob-cs.png")
# 
# p <- ggplot(data = df %>% mutate(swing = if_else(swing == "swing", 1, 0)),
#             aes(x = cs_prob, y = swing, col = has_bangs))
# p + geom_point(alpha = .05) +
#   facet_wrap(pi_pitch_group ~ ., ncol = 2) +
#   geom_smooth(method = "glm", formula = y ~ x,
#               method.args = list(family = "binomial")) +
#   scale_color_manual("Bang", values = c("#002D62", "#EB6E1F")) +
#   labs(x = "called strike probability",
#        y = "swing probability") +
#   theme_bw()
# ggsave("fig/swing-prob-cs-color.png")
# 
# 
# p <- ggplot(data = df %>% mutate(swing = if_else(swing == "swing", 1, 0),
#                                  fa = if_else(pi_pitch_group == "FA", 1, 0)),
#             aes(x = cs_prob, y = swing, col = has_bangs))
# p + geom_point(alpha = .05) +
#   facet_grid(fa ~ .,
#              labeller = labeller(fa = c("0" = "off speed", 
#                                         "1" = "fastball"))) +
#   geom_smooth(method = "glm", formula = y ~ x,
#               method.args = list(family = "binomial")) +
#   scale_color_manual("Bang", values = c("#002D62", "#EB6E1F")) +
#   labs(x = "called strike probability",
#        y = "swing probability") +
#   theme_bw()
# ggsave("fig/swing-prob-cs-fa-nfa-color.png")
# 
# df <- df %>%
#   mutate(miss = ifelse(df$call_code %in% c("C","S","W"),"miss","contact"))
# group_by(df, count, swing, has_bangs, miss) %>%
#   summarize(n = n())
# 
