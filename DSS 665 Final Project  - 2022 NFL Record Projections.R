#Import dplyr
library(dplyr)

#Import 2021 NFL Raw Data
Raw_NFL <- read.csv("C:/Users/cyl76/OneDrive/Documents/My Documents/DSS 665 NFL Final Project/2021 NFL Raw Data.csv",header=T)


head(Raw_NFL)


attach(Raw_NFL)


#Start by cleaning up the raw data (we will be utilizing the dplyr package for this)
df <- data.frame(Team, pass_cmp, pass_att, pass_yds, pass_td, interceptions, X._of_times_sacked,
                 pass_long, rush_yds, rush_td, rush_long, fumbles_lost, Home.or.Away., own_score,
                 opp_score, opp_team, Surface, game_date, team_ranking, opp_ranking)

head(df)


#using dplyr to group_by & summarize we can aggregate the data from a player to week view to a team by week view
df2 <- df %>% group_by(game_date,Team ,Home.or.Away.)

df2 <- df2 %>% summarise(
  pass_completions = sum(pass_cmp),
  pass_attempts = sum(pass_att),
  pass_yds = sum(pass_yds),
  pass_td = sum(pass_td),
  interceptions = sum(interceptions),
  times_sacked  = sum(X._of_times_sacked),
  longest_pass = sum(pass_long),
  rush_yds = sum(rush_yds),
  rush_td = sum(rush_td),
  longest_rush = sum(rush_long),
  lost_fumbles = sum(fumbles_lost),
  own_score = mean(own_score),
  opp_score = mean(opp_score),
  team_ranking = mean(team_ranking),
  opp_ranking = mean(opp_ranking)
)

df2


#Create a win loss column
df2 <- mutate(df2, win_loss = ifelse(own_score>opp_score, 'Win', 'Loss'))
df2 <- mutate(df2, win_loss_binary = ifelse(win_loss=="Win", 1, 0))

#Create a passing % column
df2$passing_perc <- df2$pass_completions/df2$pass_attempts
df2$counter <- 1
df2


#Let's look at some basic statistics to get an overall understanding of the grouped dataset before going any further with
#the analysis

hist(df2$passing_perc, col="coral", xlab="Pass %", 
     main="Distribution of Pass %", breaks=12,
     xlim=c(0,1), ylim=c(0,150))


hist(df2$interceptions, col="coral", xlab="Interceptions", 
     main="Distribution of Interceptions", breaks=12,
     xlim=c(0,7), ylim=c(0,300))


hist(df2$lost_fumbles, col="coral", xlab="Lost Fumbles", 
     main="Distribution of Lost Fumbles", breaks=12,
     xlim=c(0,7), ylim=c(0,350))


boxplot(df2$rush_yds, col="coral", xlab="Rush Yards", 
        main="Distribution of Rush Yards", ylim=c(0,300))



#Logistics Regression Equation with ALL Variables to see the best model to that can be used to predict a win or loss
Model1 <- glm(df2$win_loss_binary ~ df2$pass_completions + df2$pass_attempts + df2$passing_perc + 
               df2$pass_yds + df2$pass_td + df2$interceptions + df2$times_sacked + df2$longest_pass 
             + df2$longest_rush + df2$rush_yds +df2$rush_td + df2$lost_fumbles + df2$opp_score +
             + df2$Home.or.Away. + df2$team_ranking + df2$opp_ranking, family = "binomial")

summary(Model1)


#Take out the highest p-value variable - pass attempts (p-values of .87)
Model2 <- glm(df2$win_loss_binary ~ df2$pass_completions + df2$passing_perc + 
                df2$pass_yds + df2$pass_td + df2$interceptions + df2$times_sacked + df2$longest_pass 
              + df2$longest_rush + df2$rush_yds + df2$rush_td + df2$lost_fumbles + df2$opp_score + df2$Home.or.Away.
              + df2$team_ranking + df2$opp_ranking, family = "binomial")

summary(Model2)


#Take out the highest p-value variables - Home or Away (p-value of .78)
Model3 <- glm(df2$win_loss_binary ~ df2$pass_completions + df2$passing_perc + 
                df2$pass_yds + df2$pass_td + df2$interceptions + df2$times_sacked + df2$longest_pass 
              + df2$longest_rush + df2$rush_yds + df2$rush_td + df2$lost_fumbles + df2$opp_score
              + df2$team_ranking + df2$opp_ranking, family = "binomial")

summary(Model3)


#Take out the highest p-value variables - Longest Rush (p-value of .35)
Model4 <- glm(df2$win_loss_binary ~ df2$pass_completions + df2$passing_perc +
                df2$pass_yds + df2$pass_td + df2$interceptions + df2$times_sacked + df2$longest_pass 
              + df2$rush_yds + df2$rush_td + df2$lost_fumbles + df2$opp_score
              + df2$team_ranking + df2$opp_ranking, family = "binomial")

summary(Model4)


#The individual variable p-values are all below .20 which is acceptable and and we can use this model going forward

coef(Model4)
exp(coef(Model4))

#Examining the odds ratio we can see that
#for each additional pass completion the odds that a team will win decreases by 9.4%
#for each additional passing percentage the odds that a team will win increases by 2900%
#for each additional passing yard the odds that a team will win increases by 1.8%
#for each additional passing TD the odds that a team will win increases by 958%
#for each additional interception the odds that a team will win decreases by 33.5%
#for each additional times the quarterback is sacked the odds that a team will win decreases by 25.4%
#For each additional yard within the longest pass play the odds that a team will win decreases by 2.2%
#For each additional rush yard the odds that a team will win increases by 1.5%
#for each additional rushing TD the odds that a team will win increases by 930%
#for each additional lost fumble the odds that a team will win decreases by 52.2%
#for each additional point that the opponent scores the odds that a team will win decreases by 37.3%
#for each additional increment up in the rankings for your team will decrease the odds of winning by 2.8%
#for each additional increment up for the opponent ranking will increase the odds of winning by 3.2%



#let's look at some of the independent variables and how they are distributed based on games that are won and games that are lost


#pass % for games that are won vs. loss
par(mfrow=c(2,1))

hist(df2$passing_perc[df2$win_loss=="Win"], col="coral", xlab="Pass %", 
     main="Distribution of Pass % for games resulting in a win", breaks=12,
     xlim=c(0,1), ylim=c(0,100))

hist(df2$passing_perc[df2$win_loss=="Loss"], col="lightblue", xlab="Pass %", 
     main="Distribution of Pass % for games resulting in a loss", breaks=12,
     xlim=c(0,1), ylim=c(0,100))



#Interceptions for games that are won vs. loss
par(mfrow=c(2,1))

hist(df2$interceptions[df2$win_loss=="Win"], col="coral", xlab="Interceptions", 
     main="Distribution of Interceptions in games resulting in a win", breaks=12,
     xlim=c(0,7), ylim=c(0,200))

hist(df2$interceptions[df2$win_loss=="Loss"], col="lightblue", xlab="Interceptions", 
     main="Distribution of Interceptions in games resulting in a loss", breaks=12,
     xlim=c(0,7), ylim=c(0,200))


#Lost Fumbles for games that are won vs. loss
par(mfrow=c(2,1))

hist(df2$lost_fumbles[df2$win_loss=="Win"], col="coral", xlab="Lost Fumbles", 
     main="Distribution of Lost Fumbles in games resulting in a win", breaks=12,
     xlim=c(0,7), ylim=c(0,200))

hist(df2$lost_fumbles[df2$win_loss=="Loss"], col="lightblue", xlab="Lost Fumbles", 
     main="Distribution of Lost Fumbles in games resulting in a loss", breaks=12,
     xlim=c(0,7), ylim=c(0,200))


summary(df2$lost_fumbles[df2$win_loss=="Win"])

summary(df2$lost_fumbles[df2$win_loss=="Loss"])



#Looking at total rushing yards for games that are won vs. loss
par(mfrow=c(1,2))

boxplot(df2$rush_yds[df2$win_loss=="Win"], col="coral", xlab="Rush Yards", 
        main="Distribution of Rush Yards in games resulting in a win", ylim=c(0,300))

boxplot(df2$rush_yds[df2$win_loss=="Loss"], col="lightblue", xlab="Rush Yards", 
        main="Distribution of Rush Yards in games resulting in a loss",ylim=c(0,300))


#We will use the predict function to calculate the chance of winning for each match up against the historical data
predict(Model4, type="response")

predict.outcome <- predict(Model4, type="response")

#look at wins
predict_table <-table(predict.outcome[df2$win_loss_binary=="1"]>.5, df2$counter[df2$win_loss_binary=="1"])
predict_table

#look at losses
predict_table2 <-table(predict.outcome[df2$win_loss_binary=="0"]<.5, df2$counter[df2$win_loss_binary=="0"])
predict_table2


#sensitivity (models ability to successfully predict a 1)
247/(247+24)
#91.1% sensitivity

#specificity (models ability to successfully predict a 0)
247/(247+26)
#90.4% specificity 



#we will want to pull df2 and use dplyr to pull the averages of the variables that need to predict the
#2022 season based on the logistics regression model

df3 <- df2 %>% group_by(Team) %>% 
  summarise(
  pass_completions = mean(pass_cmp),
  passing_perc = mean(passing_perc),
  pass_yds = mean(pass_yds),
  pass_td = mean(pass_td),
  interceptions = mean(interceptions),
  times_sacked = mean(times_sacked),
  longest_pass = mean(longest_pass),
  rush_yds = mean(rush_yds),
  rush_td = mean(rush_td),
  lost_fumbles = mean(lost_fumbles)
)

df3


#Need to bring in opponent's team expected score
df4 <- df %>% group_by(opp_team)%>% 
  summarise(opp_score = mean(opp_score))

df4

#bring in 2022 NFL Match Ups
NFL_Games_2022 <- read.csv("C:/Users/cyl76/OneDrive/Documents/My Documents/DSS 665 NFL Final Project/2022_NFL_Match_Ups.csv",header=T)

NFL_Games_2022

df_2022 <- data.frame(Team = NFL_Games_2022$team, 
                      opp_team = NFL_Games_2022$opp_team, 
                      team_ranking = NFL_Games_2022$Team_Ranking, 
                      Opp_ranking = NFL_Games_2022$Opp_Ranking,
                      Division = NFL_Games_2022$Division,
                      Conference = NFL_Games_2022$Conference)

head(df_2022)


#using dplyr full_join we will merge the 2022 match ups with the averages from the 2021 season
df_2022 <- full_join(df_2022, df3)
head(df_2022, 30)

#we will now need to bring in the opponents expected score based on average number of points from 2021 season
df_2022 <- full_join(df_2022, df4)
head(df_2022, 20)


#Rearrange columns to coincide with Logistics regression model #dplyr function
df_2022 = df_2022 %>% select(Team, opp_team, Division, Conference, team_ranking, Opp_ranking, pass_completions, passing_perc, pass_yds, pass_td, interceptions, times_sacked, longest_pass, rush_yds, 
                                     rush_td, lost_fumbles, opp_score)

head(df_2022, 20)

coef(Model4)

#calculate the logit using the logistics regression model. The logit will be used to then calculate the probability
df_2022$logit <- 0.86475270 + df_2022$pass_completions * -0.09785600 +
  df_2022$passing_perc * 3.39595587 + df_2022$pass_yds * 0.01881354 +
  df_2022$pass_td * 2.26032448 + df_2022$interceptions * -0.40768158 +
  df_2022$times_sacked * -0.29268405 + df_2022$longest_pass * -0.02221898 +
  df_2022$rush_yds * 0.01578406 + df_2022$rush_td * 2.23033090 +
  df_2022$lost_fumbles * -0.73661376 + df_2022$opp_score * -0.46674357+
  df_2022$team_ranking * -0.02756720 + df_2022$Opp_ranking * 0.03216100
head(df_2022, 20)

#create a column to show win probability
df_2022$win_probability <- 1/(1+exp(-df_2022$logit))
df_2022

#concat team and opp team columns to get unique identifier
df_2022$concat <- paste(df_2022$Team, df_2022$opp_team)
df_2022

#set a counter for number of game
df_2022$games <- 1
df_2022


#We will using merge later and we must use DPLYR to group the categorical variables together to take away any duplicates in the data set in order to do so.
#Duplicates occur when two teams play each other twice. The games will be included and summed up in the game column
df_2022 <- df_2022 %>% group_by(Team, Division, Conference , opp_team, concat) %>% 
  summarise(
    pass_completions = mean(pass_completions),
    passing_perc = mean(passing_perc),
    pass_yds = mean(pass_yds),
    pass_td = mean(pass_td),
    interceptions = mean(interceptions),
    times_sacked = mean(times_sacked),
    longest_pass = mean(longest_pass),
    rush_yds = mean(rush_yds),
    rush_td = mean(rush_td),
    lost_fumbles = mean(lost_fumbles),
    opp_score = mean(opp_score),
    team_ranking = mean(team_ranking),
    opp_ranking = mean(Opp_ranking),
    win_probability = mean(win_probability),
    games = sum(games))

head(df_2022, 20)

#we will create a separate data set to identify our opponents probability of winning and bring this in to our df_2022 data set to compare our teams chance of winning vs. our opponents chance of winning
df_opp_match_ups <- df_2022

#we will match up the teams and opponents chance of winning through the concat column only for the opponents data set we will reverse the columns to concat opponent team first and then our team
#this will allow us to bring in the opponents chance of winning 
df_opp_match_ups$concat1 <- paste(df_opp_match_ups$opp_team, df_opp_match_ups$Team)
head(df_opp_match_ups)

#match up the column names to appropriately merge the two datasets
df_opp_match_ups <- data.frame(df_opp_match_ups$concat1, df_opp_match_ups$win_probability)
names(df_opp_match_ups)[1] <- 'concat'
names(df_opp_match_ups)[2] <- 'Opp_win_perc'
df_opp_match_ups

#merge the two datasets, adding the opponent win probability to the df_2022 data set 
df_2022 <- merge(df_2022, df_opp_match_ups,by="concat")

df_2022


#create new columns identifying which win percentage is higher and assigning a win to that team and a loss to the other team
df_2022 <- mutate(df_2022, win_loss = ifelse(df_2022$win_probability > df_2022$Opp_win_perc, 'Win', 'Loss'))
df_2022 <- mutate(df_2022, win = ifelse(df_2022$win_loss == "Win", 1*games, 0))
df_2022 <- mutate(df_2022, loss = ifelse(df_2022$win_loss == "Loss", 1*games, 0))

#remove historical statistics to just focus on 2022 data
df_2022 = df_2022 %>% select(Team, opp_team, Division, Conference, team_ranking, opp_ranking, win_probability, Opp_win_perc, win_loss, win, loss, games)

head(df_2022)

#group and summaries by team based on win and losses
df_2022_breakdown <- df_2022 %>% group_by(Team, Division, Conference)
df_2022_breakdown <- df_2022_breakdown %>% summarise(
  wins = sum(win),
  losses = sum(loss)
)

View(df_2022_breakdown)

