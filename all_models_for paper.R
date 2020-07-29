#bp_pitchinfo.pitches
library(RMySQL)
#con1 <- dbConnect(MySQL(), host="50.19.97.46", user="gmatthews", password=XXXX, dbname="bp_pitchinfo")
#q <-  paste("SELECT * FROM pitches WHERE gametype = 'R' and season IN (2017, 2018, 2019)")
#data <- dbGetQuery(con1,q)
#save(data,file="/Users/gregorymatthews/Dropbox/mlb_stealing_signs/pitchinfo.RData")
load("~/Dropbox/mlb_stealing_signs/pitchinfo.RData")
pitchinfo <- data
rm(data)

#con1 <- dbConnect(MySQL(), host="50.19.97.46", user="gmatthews", password=XXXX, dbname="tech_jjudge")
#q <-  paste("SELECT * FROM statcast_hit")
#data <- dbGetQuery(con1,q)
#save(data,file="/Users/gregorymatthews/Dropbox/mlb_stealing_signs/statcast_hit.RData")
load("/Users/gregorymatthews/Dropbox/mlb_stealing_signs/statcast_hit.RData")
statcast <- data
rm(data)

#We could do this modeling and see how 2.65mph affects home run prob.  But this might be more work than we need to do.  
#a <- glm(I(events == "home_run") ~ , family = "binomial", data = statcast)

bangs <- read.csv("~/Dropbox/mlb_stealing_signs/astros_bangs_20200127.csv")
bangs$pitch_playid <- as.character(bangs$pitch_playid)
bangs$game_pitch_id <- (as.character(bangs$game_pitch_id))
#Remove rows with missing game_pitch_id
bangs <- bangs[bangs$game_pitch_id != "",]

#Now merge the bangs and the pitch_info data
merg_bangs <- merge(bangs, pitchinfo, by.x = "pitch_playid" , by.y = "tm_guid", all.x = TRUE)

#Now merge on exit velocity and launch angle from stat cast
statcast$game_pitch_id <- (gsub("_", "", statcast$sv_id))
#Remove missing game_pitch_ids
statcast <- statcast[!is.na(statcast$game_pitch_id),]

merg_bangs <- merge(merg_bangs, statcast[,c("game_pitch_id","player_name","launch_speed","launch_angle","launch_speed_angle","barrel","events")], all.x = TRUE, by.x = c("game_pitch_id","batter"), by.y = c("game_pitch_id","player_name"))


merg_bangs <- merg_bangs[merg_bangs$game_pitch_id != "_",]

dup_game_pitch_ids_to_remove <- merg_bangs$game_pitch_id[duplicated(merg_bangs$game_pitch_id)]
merg_bangs <- subset(merg_bangs, !game_pitch_id%in% dup_game_pitch_ids_to_remove )

#exclude bunts
merg_bangs <- subset(merg_bangs, swing != "bunt")


table(merg_bangs$pitch_category, merg_bangs$pi_pitch_group)
#exclude pitch_type OT
#merg_bangs <- subset(merg_bangs, pitch_category != "OT")

#Creating some new variables. 
#1 is a swing, 0 is a take
merg_bangs$I_swing <- (merg_bangs$swing == "swing") + 0

merg_bangs$I_fastball <- (merg_bangs$pitch_category == "FB") + 0 

merg_bangs$count <- factor(paste(merg_bangs$ball, merg_bangs$strike, sep = "-"))

merg_bangs$I_1b <- (merg_bangs$on_1b.x == "t") + 0
merg_bangs$I_2b <- (merg_bangs$on_2b.x == "t") + 0
merg_bangs$I_3b <- (merg_bangs$on_3b.x == "t") + 0

merg_bangs$I_OB <- (merg_bangs$I_1b + merg_bangs$I_2b + merg_bangs$I_3b > 0) + 0

merg_bangs <- merg_bangs[merg_bangs$pi_pitch_group != "XX",]
##########################
#Summary Statistics
##########################
#Don't use the stat cast categories!
#prop.table(table(merg_bangs$has_bangs,merg_bangs$pitch_category),2)
#label: table_pitch_group_vs_bang
table(merg_bangs$has_bangs,merg_bangs$pi_pitch_group)
prop.table(table(merg_bangs$has_bangs,merg_bangs$pi_pitch_group),2)
xtable(table(merg_bangs$has_bangs,merg_bangs$pi_pitch_group))
chisq.test(table(merg_bangs$has_bangs,merg_bangs$pi_pitch_group))

# nrow(merg_bangs)
# prop.table(table(merg_bangs$has_bangs,merg_bangs$pitch_category),2)
# xtable(table(merg_bangs$has_bangs,merg_bangs$pitch_category))

#prop.table(table(merg_bangs$I_has_bangs, merg_bangs$opponent, merg_bangs$I_fastball),c(2,3))

#Bivariate stuff
#Label: table_swing_vs_bang
tab <- table(merg_bangs$has_bangs, merg_bangs$I_swing)
prop.table(tab,1)
xtable(tab)
(tab[1,1]*tab[2,2])/(tab[1,2]*tab[2,1])

chisq.test(table(merg_bangs$has_bangs, merg_bangs$I_swing))
prop.table(table(merg_bangs$has_bangs, merg_bangs$I_swing), 1)
prop.table(table(merg_bangs$has_bangs, merg_bangs$I_swing), 2)


prop.table(table(merg_bangs$has_bangs, merg_bangs$opponent),2)
table(merg_bangs$has_bangs, merg_bangs$opponent, merg_bangs$I_fastball)



#Question: Should we remove balls in the dirt?  Reasoning: Because no one* is swining at these.  
#table(merg_bangs$y == 0, merg_bangs$swing)
#table(merg_bangs$y == 0, merg_bangs$has_bangs)


#What are the details on the cs_prob variable? Does called strike probability factor in umpire? 
#Include I_fastball?  

##########################
#Swing Model
##########################
#Interaction 

save(merg_bangs, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/merg_bangs.RData")
write.csv(merg_bangs, file = "/Users/gregorymatthews/Dropbox/stealing_signs_gi/merg_bangs.csv")

library(lme4)
mod1_swing <- glmer(I_swing ~ I_fastball + cs_prob + count + has_bangs + (1|batter_mlbid)  , data = merg_bangs, family = "binomial")
summary(mod1_swing)
confint(mod1_swing, maxpts = 8)
Computing profile confidence intervals ...
2.5 %     97.5 %
  .sig01       0.13318025  0.3414967
(Intercept) -2.68616808 -2.2938281
I_fastball  -0.03032336  0.1919819
cs_prob      2.36083494  2.6415920
count0-1     0.99338298  1.3584255
count0-2     1.67050343  2.1663962
count1-0     0.35110100  0.7215076
count1-1     1.29889398  1.6787542
count1-2     1.96578204  2.3772251
count2-0     0.26240058  0.8071607
count2-1     1.28611025  1.7487719
count2-2     2.26569377  2.6995342
count3-0    -1.70643258 -0.5928632
count3-1     1.04779569  1.7091598
count3-2     2.20318544  2.7410977
has_bangsy  -0.48927217 -0.1695131

summary(mod1_swing)
exp(-0.32895)
#confint(mod1_swing)
exp(c(-0.48927217 ,-0.1695131))

xtable(summary(mod1_swing)$coef)
##########################
#Contact given Swing
##########################
#Subset only pitches with swings.  #remove BI, CI, and UN
swings <- subset(merg_bangs, I_swing == 1 & !is.na(has_bangs) & !result %in% c("BI","CI","UN"))



swings$I_contact <-  0
swings$I_contact[swings$result != "S"] <- 1

#label: table_contact_model
library(lme4)
mod1_contact <- glmer(I_contact ~  cs_prob + I_fastball* has_bangs + (1|batter_mlbid) + (1|mlbid)  , data = swings, family = "binomial")

confint(mod1_contact, maxpts = 8)
Computing profile confidence intervals ...
2.5 %      97.5 %
  .sig01                 0.1679064  0.47122429
.sig02                 0.2646672  0.69529285
(Intercept)           -0.6384049 -0.04850104
cs_prob                1.5502808  1.99087417
I_fastball             0.7045289  1.09437789
has_bangsy             0.2765118  0.91054145
I_fastball:has_bangsy -1.4026661 -0.10997720

-0.3307957
1.7689493*(.75)
0.8989529
0.5902648
-0.7739796

Median cs_prob = 0.84

#no bangs, off speed
-0.3307957 + 1.7689493*(.75) = 0.9959163
exp(0.9959163)/(1+exp(0.9959163))
#no bangs, fastball
-0.3307957 + 0.8989529 + 1.7689493*(.75)  = 1.894869
exp(1.894869)/(1+exp(1.894869))
#bangs, off speed 
-0.3307957 + 0.5902648 + 1.7689493*(.75)  = 1.586181
exp(1.586181)/(1+exp(1.586181))
#bangs, fastball
-0.3307957 + 0.5902648 + 0.8989529 + -0.7739796 + 1.7689493*(.75)  = 1.711154
exp(1.711154)/
  
  
#no bangs, off speed
-0.3307957 + 1.7689493*(.84) = 1.155122
exp(1.155122)/(1+exp(1.155122))
#no bangs, fastball
-0.3307957 + 0.8989529 + 1.7689493*(.84)  = 1.894869
exp(1.894869)/(1+exp(1.894869))
#bangs, off speed 
-0.3307957 + 0.5902648 + 1.7689493*(.84)  = 1.586181
exp(1.586181)/(1+exp(1.586181))
#bangs, fastball
-0.3307957 + 0.5902648 + 0.8989529 + -0.7739796 + 1.7689493*(.84)  = 1.711154
exp(1.711154)/(1+exp(1.711154))






summary(mod1_contact)$coeff[,1]

xtable(summary(mod1_contact)$coeff)

newdat <- data.frame(cs_prob = 0.75, I_fastball = c(0,0,1,1), has_bangs = c("n","y","n","y"))
mm <- model.matrix(~ cs_prob+ I_fastball*has_bangs , newdat) ## create
newdat$y <- mm%*%fixef(mod1_contact)
newdat$p <- exp(newdat$y)/(1+exp(newdat$y))

predict(mod1_contact, newdat, re.form = NA) #would give the same results
pvar1 <- diag(mm %*% tcrossprod(vcov(mod1_contact), mm))
newdat$se <- pse <- sqrt(pvar1)


#Bootstrap to get CI for differences to start.  
odds <- function(p){return(p/(1-p))} #defining an odds function.
OR_offspeed <- OR_fastball <- c()
library(lme4)
set.seed(2017)
#Bootstrap 
nsim <- 500
for (i in 1:nsim){print(i)
ind <- sample(1:nrow(swings),nrow(swings),replace = TRUE)
    swings_boot <- swings[ind,]
mod1_contact <- glmer(I_contact ~  cs_prob + I_fastball* has_bangs + (1|batter_mlbid) + (1|mlbid)  , data = swings_boot, family = "binomial")

newdat <- data.frame(cs_prob = 0.84, I_fastball = c(0,0,1,1), has_bangs = c("n","y","n","y"))
mm <- model.matrix(~ cs_prob+ I_fastball*has_bangs , newdat) ## create
newdat$y <- mm%*%fixef(mod1_contact)
newdat$p <- exp(newdat$y)/(1+exp(newdat$y))

predict(mod1_contact, newdat, re.form = NA, type = "response") #would give the same results

OR_offspeed[i] <- odds(newdat$p[newdat$I_fastball == 0 & newdat$has_bangs == "y"])/odds(newdat$p[newdat$I_fastball == 0 & newdat$has_bangs == "n"])

OR_fastball[i] <- odds(newdat$p[newdat$I_fastball == 1 & newdat$has_bangs == "y"])/odds(newdat$p[newdat$I_fastball == 1 & newdat$has_bangs == "n"])

save(OR_offspeed, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/OR_offspeed.RData")
save(OR_fastball, file = "/Users/gregorymatthews/Dropbox/stealing_signs_git/OR_fastball.RData")
}

quantile(OR_offspeed, c(0.025, 0.975))


swings$I_inplay <-  0
swings$I_inplay[!swings$result %in% c("S","F")] <- 1

#Check 
#table(swings$I_inplay,swings$result)


library(lme4)
mod1_inplay <- glmer(I_inplay ~  cs_prob + I_fastball +  has_bangs + (1|batter_mlbid) + (1|mlbid)  , data = swings, family = "binomial")

summary(mod1_inplay)


##########################
#Exit Velocity Model
##########################
#pull out only pitches where ball was put in play
inplay <- subset(swings, I_inplay == 1 & !is.na(has_bangs))
contacts <- subset(swings, I_contact == 1 & !is.na(has_bangs))

library(lme4)
#label: table_exit_velo_model
mod1_speed <- lmer(launch_speed ~   cs_prob + I_fastball + has_bangs + gtemp + (1|batter_mlbid) + (1|mlbid)  , data = contacts)

summary(mod1_speed)
xtable(summary(mod1_speed)$coeff)
confint(mod1_speed)
2.5 %     97.5 %
  .sig01       0.7074887  2.6560654
.sig02       0.4725554  2.3434300
.sigma      14.1687423 15.0390081
(Intercept) 47.3516533 85.2273384
cs_prob      6.5488867 10.1266764
I_fastball   0.9618762  3.8643119
has_bangsy   0.5890083  4.7180200
gtemp       -0.1274015  0.3838781
table(contacts$I_fastball, contacts$has_bangs)

#How big of a deal is 2.5 MPH extra?  


table(inplay$launch_speed_angle)

summary(inplay$launch_angle)

inplay

#####A quick test: 
ev_la <- read.csv("/Users/gregorymatthews/Dropbox/stealing_signs_git/ev_la_hr.csv")
ev_la <- ev_la[order(ev_la$launch_angle_avg,-ev_la$launch_angle_avg),]
ev_la$diff_ev <- c(diff(ev_la$exit_velocity_avg),0)
ev_la$diff_la <- c(diff(ev_la$launch_angle_avg),0)

a <- lm(b_home_run ~ launch_angle_avg + exit_velocity_avg, data = ev_la)
a$coefficients%*%c(1,13.5,89.4 + 2.65) - a$coefficients%*%c(1,13.5,89.4)



#############################################
#Scrap below here.  
#############################################
library(lme4)
mod1_speed_angle <- lmer(launch_speed_angle ~ cs_prob + I_fastball +  has_bangs  + (1|batter_mlbid) + (1|mlbid)  , data = contacts)

summary(mod1_speed_angle)


library(lme4)
mod1_barrel <- glmer(barrel ~ cs_prob + I_fastball +  has_bangs  + (1|batter_mlbid) + (1|mlbid)  , data = contacts, family = "binomial")

summary(mod1_barrel)