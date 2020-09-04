## Ryan Elmore
## 5 Aug 2020
## Merging the data

library(dplyr)
library(readr)

# load("data/pitchinfo.RData")
# pi_df <- data
# saveRDS(pi_df, "data/pitch_info.rds")
# rm(data)
# load("data/statcast_hit.RData")
# sc_df <- data
# saveRDS(sc_df, "data/statcast.rds")
# rm(data)

bangs <- read_csv("data/astros_bangs_20200127.csv") %>% 
  dplyr::filter(., !is.na(game_pitch_id))
pi_df <- readRDS("data/pitch_info.rds") %>% 
  dplyr::mutate(., sv_id = gsub("_", "", sv_id)) 
sc_df <- readRDS("data/statcast.rds") %>% 
  mutate(sv_id = gsub("_", "", sv_id))

#Convert batter_mlbid to numeric
pi_df$batter_mlbid <- as.numeric(pi_df$batter_mlbid)

## What variables do we need? 
## Bangs: 
## PI: sv_id, swing, pi_pitch_group, result, ball, strike, batter_first, batter_last
##      first, last, batter_mlbid, pitch_of_ab, mlbid
## SC: "sv_id","player_name", "description", "launch_speed",
##     "launch_angle","launch_speed_angle","barrel","events", "batter", "pitch_number")

df <- dplyr::left_join(bangs, 
                       pi_df %>% 
                         dplyr::select(., sv_id, swing, pi_pitch_group, 
                                       result, ball, strike,
                                       batter_first, batter_last, first, 
                                       last, tm_guid, pz, px, cs_prob, batter_mlbid,
                                       pitch_of_ab, mlbid), 
                       by = c("pitch_playid" = "tm_guid")) %>%
    dplyr::left_join(., sc_df %>%
                     dplyr::select(., sv_id, game_pk,
                                   player_name, description, 
                                   launch_speed, launch_angle, 
                                   launch_speed_angle, barrel, events, batter,
                                   pitch_number),
                   by = c("game_pitch_id" = "sv_id", 
                          "pitch_of_ab" = "pitch_number", 
                          "batter_mlbid" = "batter"))

## rm bunts, 
df <- df %>% 
  dplyr::filter(., 
                pi_pitch_group != "XX",
                swing != "bunt")

saveRDS(df, "data/bangs-merged-final.rds")
df <- readRDS("data/bangs-merged-final.rds")

tmp <- df %>%
  dplyr::select(., pitch_playid, game_pitch_id, sv_id, batter_mlbid, pitch_of_ab)

group_by(tmp, game_pitch_id, pitch_of_ab, batter_mlbid) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(10)
View(df %>% filter(game_pitch_id == "170715035707"))
View(df %>% filter(game_pitch_id == "170823023949"))
## Compare to Greg's Data

bangs_g <- read.csv("data/astros_bangs_20200127.csv")
bangs_g$pitch_playid <- as.character(bangs_g$pitch_playid)
bangs_g$game_pitch_id <- (as.character(bangs_g$game_pitch_id))
#Remove rows with missing game_pitch_id
bangs_g <- bangs_g[bangs_g$game_pitch_id != "",]

pitchinfo <- pi_df
statcast <- readRDS("data/statcast.rds")
merg_bangs <- merge(bangs_g, pitchinfo, 
                    by.x = "pitch_playid" , 
                    by.y = "tm_guid", all.x = TRUE)

statcast$game_pitch_id <- (gsub("_", "", statcast$sv_id))
#Remove missing game_pitch_ids
statcast <- statcast[!is.na(statcast$game_pitch_id),]
merg_bangs <- merge(merg_bangs, 
                    statcast[ ,c("game_pitch_id",
                                 "player_name",
                                 "launch_speed",
                                 "launch_angle",
                                 "launch_speed_angle",
                                 "barrel",
                                 "events")], 
                    all.x = TRUE, 
                    by.x = c("game_pitch_id", "batter"), 
                    by.y = c("game_pitch_id","player_name"))
merg_bangs_2 <- merge(merg_bangs, 
                      statcast[ ,c("game_pitch_id",
                                   "player_name",
                                   "launch_speed",
                                   "launch_angle",
                                   "launch_speed_angle",
                                   "barrel",
                                   "events")], 
                      all.x = TRUE, 
                      by.x = c("game_pitch_id"), 
                      by.y = c("game_pitch_id"))

dup_game_pitch_ids_to_remove <- merg_bangs$game_pitch_id[duplicated(merg_bangs$game_pitch_id)]
merg_bangs <- subset(merg_bangs, !(game_pitch_id%in% dup_game_pitch_ids_to_remove))

#exclude bunts
merg_bangs <- subset(merg_bangs, swing != "bunt")


merg_bangs <- merg_bangs[merg_bangs$game_pitch_id != "_",]

df <- readRDS("data/bangs-merged-final.rds")
load("data/merg_bangs.rds")


length(unique(merg_bangs$game_pitch_id))

df_sub <- df %>% 
  dplyr::filter(., !(sv_id %in% merg_bangs$game_pitch_id))
