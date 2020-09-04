## Ryan Elmore
## 5 Aug 2020
## Modeling Bangs

library(dplyr)
library(lme4)

# delete pitch_playod = 6ed53428-fd72-4e13-b1ac-3d8e851d1410
# Mother f
# df[which(grepl("*Interference", df$at_bat_event, ignore.case = T)), ]
df <- readRDS("data/bangs-merged-final.rds") %>%
  dplyr::rename(., description = description.x) %>% 
  dplyr::mutate(.,
                has_bangs = if_else(has_bangs == "y", "Yes", "No"),
                count = paste(ball, "-", strike, sep = ""),
                is_swing = ifelse(swing == "swing",1,0),
                is_miss = ifelse(call_code %in% c("S","W"), 1, 0),
                is_contact = ifelse(call_code %in% c("S","W"), 0, 1),
                is_foul = ifelse(description %in% 
                                   c("Foul", "Foul Tip", "Foul (Runner Going)"),1, 0),
                #is_fastball = ifelse(pitch_category == "FB", 1, 0), 
                is_fastball = ifelse(pi_pitch_group == "FA", 1, 0),
                batter_mlbid = as.character(batter_mlbid))
  
    
  

# sort(names(df))

# ## We agree on swing except for 630
# tmp <- df %>% 
#   mutate(., 
#          is_swing = if_else(description %in%
#                               c("Ball", "Called Strike", "Ball In Dirt", 
#                                 "Hit By Pitch"),
#                             0, 1),
#          is_foul = ifelse(df$description %in% 
#                             c("Foul", "Foul Tip", "Foul (Runner Going)"), 
#                           1, 0))
# 
# df[1:20, c("call_code", "result", "description")]
## Look at Albert's Pitch Count Effects

df_pce <- df %>% 
  distinct(., count) %>% 
  arrange(., count) %>% 
  mutate(runs_value = c(0, -0.04, -0.09, 0.03, -0.02, -.07, 
                        0.09, 0.03, -0.04, 0.19, 0.13, 0.05))

df %>% 
  dplyr::group_by(., is_swing, is_miss) %>% 
  dplyr::summarize(., n = n())

df %>% 
  dplyr::filter(., is_swing == 1) %>% 
  dplyr::group_by(., is_foul) %>% 
  dplyr::summarize(n = n()) %>% 
  dplyr::mutate(freq = n / sum(n))

## 38% of "swings" are fouls
df %>% 
  dplyr::filter(., is_swing == 1, is_miss == 0) %>% 
  dplyr::group_by(., is_foul) %>% 
  dplyr::summarize(n = n()) %>% 
  dplyr::mutate(freq = n / sum(n))
## 48% of contacts are fouls

# swings <- subset(merg_bangs, I_swing == 1 & !is.na(has_bangs) & !result %in% c("BI","CI","UN"))
## Remove bangs


distinct(select(swings, call_code, description, result))

## Swing model (I_swing = 1 if swing, I_fastball = 1 if FA)

######################################################
#Swing Model
######################################################
# Note that the interaction between bangs and count does not make much sense
swing_model <- glmer(is_swing ~ is_fastball + has_bangs + count + cs_prob + (1|batter_mlbid),family = binomial, data = df)

library(xtable)
summary(swing_model) 
xtable(summary(swing_model)$coef)

confint(swing_model, parm = "has_bangsYes")

# Computing profile confidence intervals ...
# 2.5 %     97.5 %
#   has_bangsYes -0.481831 -0.1628624


######################################################
#Contact Model
######################################################
swings <- df %>% 
  dplyr::filter(., is_swing == 1)

library(lme4)
contact_model <- glmer(is_contact ~  cs_prob + is_fastball*has_bangs + + (1 + has_bangs|batter_mlbid)  + (1|mlbid)  , data = swings, family = "binomial")

summary(contact_model)
xtable(summary(contact_model)$coef)

exp(0.5905)
confint(contact_model, "has_bangsYes")

ids <- swings[!duplicated(swings$batter_mlbid),c("batter_mlbid","batter_first","batter_last")]
r_effects <- data.frame(batter_mlbid = row.names(coef(contact_model)$batter_mlbid), int = coef(contact_model)$batter_mlbid$`(Intercept)`,slope = coef(contact_model)$batter_mlbid$has_bangsYes)
r_effects <- merge(ids,r_effects, by.x = "batter_mlbid",by.y = "batter_mlbid")
r_effects <- r_effects[order(-r_effects$slope),]
r_effects$name <- paste(r_effects$batter_first, r_effects$batter_last)
print(xtable(r_effects[,c("name","int","slope")]), include.rownames=FALSE)

# ids <- swings[!duplicated(swings$batter_mlbid),c("batter_mlbid","batter_first","batter_last")]
# r_effects <- data.frame(batter_mlbid = row.names(ranef(contact_model)$batter_mlbid), ranef(contact_model)$batter_mlbid)
# r_effects <- merge(ids,r_effects, by.x = "batter_mlbid",by.y = "batter_mlbid")
# r_effects <- r_effects[order(-r_effects$has_bangsYes),]
# r_effects$name <- paste(r_effects$batter_first, r_effects$batter_last)
# print(xtable(r_effects[,c("name","X.Intercept.","has_bangsYes")]), include.rownames=FALSE)


#Bootstrap to get CI for differences to start.  
odds <- function(p){return(p/(1-p))} #defining an odds function.
OR_offspeed <- OR_fastball <- c()
library(lme4)
set.seed(2017)
ids <- swings[!duplicated(swings$batter_mlbid),c("batter_mlbid","batter_first","batter_last")]
#Bootstrap 
nsim <- 500

# get_ind <- function(id){
#   sub <- subset(swings, batter_mlbid == id)
#   ind <- sample(1:nrow(sub),nrow(sub),replace = TRUE)
#   return(sub[ind,])
# }

for (i in 1:nsim){print(i)
  #swings_boot <- do.call(rbind,lapply(sort(unique(swings$batter_mlbid)), get_ind))
  
  ind <- sample(1:nrow(swings),nrow(swings),replace = TRUE)
  swings_boot <- swings[ind,]
  
  contact_model_boot <- glmer(is_contact ~  cs_prob + is_fastball*has_bangs + (1 + has_bangs|batter_mlbid)  + (1|mlbid)  , data = swings_boot, family = "binomial")
  
  r_effects <- data.frame(batter_mlbid = row.names(coef(contact_model_boot)$batter_mlbid), int = coef(contact_model_boot)$batter_mlbid$`(Intercept)`,slope = coef(contact_model_boot)$batter_mlbid$has_bangsYes)
  r_effects <- merge(ids,r_effects, by.x = "batter_mlbid",by.y = "batter_mlbid")
  r_effects <- r_effects[order(-r_effects$slope),]
  r_effects$name <- paste(r_effects$batter_first, r_effects$batter_last)
  
  
  if (i == 1){
    int_boot <- merge(ids, r_effects[,c("batter_mlbid","int")], by.x = "batter_mlbid", by.y = "batter_mlbid" , all.x = TRUE)
  } else {
    int_boot <- merge(int_boot, r_effects[,c("batter_mlbid","int")], by.x = "batter_mlbid", by.y = "batter_mlbid" , all.x = TRUE)
  }
  
  if (i == 1){
    slope_boot <- merge(ids, r_effects[,c("batter_mlbid","slope")], by.x = "batter_mlbid", by.y = "batter_mlbid" , all.x = TRUE)
  } else {
    slope_boot <- merge(slope_boot, r_effects[,c("batter_mlbid","slope")], by.x = "batter_mlbid", by.y = "batter_mlbid" , all.x = TRUE)
  }
  
  newdat <- data.frame(cs_prob = median(swings$cs_prob), is_fastball = c(0,0,1,1), has_bangs = c("No","Yes","No","Yes"))
  mm <- model.matrix(~ cs_prob+ is_fastball*has_bangs , newdat) ## create
  newdat$y <- mm%*%fixef(contact_model_boot)
  newdat$p <- exp(newdat$y)/(1+exp(newdat$y))
  
  predict(contact_model_boot, newdat, re.form = NA, type = "response") #would give the same results
  
  OR_offspeed[i] <- odds(newdat$p[newdat$is_fastball == 0 & newdat$has_bangs == "Yes"])/odds(newdat$p[newdat$is_fastball == 0 & newdat$has_bangs == "No"])
  
  OR_fastball[i] <- odds(newdat$p[newdat$is_fastball == 1 & newdat$has_bangs == "Yes"])/odds(newdat$p[newdat$is_fastball == 1 & newdat$has_bangs == "No"])
  
  save(int_boot, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/int_boot.RData")
  save(slope_boot, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/slope_boot.RData")
  
  save(OR_offspeed, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/OR_offspeed.RData")
  save(OR_fastball, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/OR_fastball.RData")
}

quantile(OR_offspeed, c(0.025, 0.975))
quantile(OR_fastball, c(0.025, 0.975))

quantile(log(OR_offspeed), c(0.025, 0.975))


r_effects <- data.frame(batter_mlbid = row.names(coef(contact_model)$batter_mlbid), int = coef(contact_model)$batter_mlbid$`(Intercept)`,slope = coef(contact_model)$batter_mlbid$has_bangsYes)
r_effects <- merge(ids,r_effects, by.x = "batter_mlbid",by.y = "batter_mlbid")
r_effects <- r_effects[order(-r_effects$slope),]
r_effects$name <- paste(r_effects$batter_first, r_effects$batter_last)

ci_rand_slope <- cbind(slope_boot[,1:3],t(apply(slope_boot[,-c(1:3)],1,function(x){exp(quantile(x, c(0.025, 0.975)))})))

r_effects <- merge(r_effects,ci_rand_slope, by.x = "batter_mlbid", by.y = "batter_mlbid",all.x = TRUE)

r_effects$OR_out <- paste0(round(exp(r_effects$slope),3)," (",round((r_effects$`2.5%`),3),", ",round((r_effects$`97.5%`),3),")")
r_effects <- r_effects[order(-r_effects$slope),]

print(xtable(r_effects[,c("name","OR_out")]), include.rownames=FALSE)



######################################################
#Exit Velocity Model
######################################################
contacts <- swings %>% 
  dplyr::filter(., is_contact == 1 )

speed_model <- lmer(launch_speed ~   cs_prob + is_fastball + has_bangs   + (1|batter_mlbid) + (1|mlbid)  , data = contacts)
summary(speed_model)
confint(speed_model)
coef(speed_model)

xtable(summary(speed_model)$coef)

#Computing p-values 
2*(1 - pnorm(summary(speed_model)$coef[,3]))

##########################################
#Removing low PA guys (less than 50 were removed)
##########################################
speed_model <- lmer(launch_speed ~   cs_prob + is_fastball + has_bangs   + (1|batter_mlbid) + (1|mlbid)  , data = contacts[!contacts$batter_mlbid %in% c(457727,518542,545358,605204,605233,607223, 643393),])
summary(speed_model)
coef(speed_model)

xtable(summary(speed_model)$coef)

#Computing p-values 
2*(1 - pnorm(summary(speed_model)$coef[,3]))


#mod1_speed <- lmer(launch_speed ~   cs_prob + I_fastball + has_bangs  + (1+has_bangs|batter_mlbid) + (1|mlbid)  , data = inplay)
summary(speed_model)
confint(speed_model)



######################################################################
#Summary statistics for launch speeds
######################################################################
contacts <- contacts[!is.na(contacts$launch_speed),]
as.data.frame(contacts %>% group_by(batter_mlbid, batter_first, batter_last,is_fastball, has_bangs) %>% summarize(mn_speed = mean(launch_speed), n = n()))

ggplot(aes(x = has_bangs, y = launch_speed), data = subset(contacts, batter_mlbid == 543807 & is_fastball == 0)) + geom_boxplot()



