#############################################
# Springer plots with season results
#############################################
library(ggplot2)
gs <- data.frame(year = 2014:2019, contact_pct = c(.61, .695, .739, .787, .782, .75), sw_strike = c(.186, .136, .123, .095, .096, .11))
gs$diff <- c(NA, diff(gs$contact_pct))
library(teamcolors)
ggplot(aes(x = year, y = 100*contact_pct), data = gs) + geom_line(col = teamcolors$primary[teamcolors$name == "Houston Astros"]) + geom_point(col = teamcolors$secondary[teamcolors$name == "Houston Astros"]) + ylab("Contact %") + xlab("Year")

library(teamcolors)
ggplot(aes(x = year, y = sw_strike), data = gs) + geom_line(col = teamcolors$primary[teamcolors$name == "Houston Astros"]) + geom_point(col = teamcolors$secondary[teamcolors$name == "Houston Astros"])+ xlab("Year") + ylab("Swinging Strike %")

#############################################
#Springer pitchinfo stuff
#############################################

# con1 <- dbConnect(MySQL(), host="50.19.97.46", user="gmatthews", password="XXXX", dbname="bp_pitchinfo")
# q <-  paste("SELECT * FROM pitches WHERE gametype = 'R' and batter_mlbid = 543807")
# data <- dbGetQuery(con1,q)
# save(data,file="/Users/gregorymatthews/Dropbox/mlb_stealing_signs/pitchinfo_springer.RData")

load("/Users/gregorymatthews/Dropbox/mlb_stealing_signs/pitchinfo_springer.RData")
)
pi_springer <- data
rm(data)

pi_springer <- pi_springer %>% mutate(
  is_contact = ifelse(outcome %in% c("S"), 0, 1), 
  is_fastball = ifelse(pi_pitch_group == "FA", "FA", "OS")) %>% 
  subset(swing == "swing" ) %>% group_by(substring(date,1,4), is_fastball)

pi_springer <- pi_springer[!is.na(pi_springer$is_fastball),]

df <- pi_springer %>% summarize(contact_rate = sum(is_contact)/n())
names(df)[1] <- c("Year")

ggplot(aes(x = Year, y = contact_rate, col = factor(is_fastball)), data = subset(df, Year > 2013)) + geom_point()

pi_springer$Year <- substring(pi_springer$date, 1, 4)

fret <- table(pi_springer$is_contact, pi_springer$is_fastball, pi_springer$Year)

OR <- function(x){
  (x[1,2]*x[2,1])/(x[1,1]*x[2,2])
}

apply(fret, 3, OR)


sub <- subset(pi_springer, season == 2017)
table(sub$date > as.Date("2017-05-19"))
