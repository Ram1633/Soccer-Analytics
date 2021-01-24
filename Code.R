################################ Importing data and pre-processing ####################
events <- read.csv("D:/OneDrive/Desktop/MGSC 661/Final Project/Data/events.csv")
ginf <- read.csv("D:/OneDrive/Desktop/MGSC 661/Final Project/Data/ginf.csv")
library(DataCombine)
require(rms)
library(dplyr)
library(TTR)
library(randomForest)
library(gbm)
library(car)
require(lmtest)
require(plm)
require(visreg)
require(psych)
require(ggplot2)
require(ggpubr)
require(methods)
require(caTools)
library(boot)
library(stargazer)
library(RColorBrewer)
library(reshape2)
## Extracting country and season from 'ginf' dataset

league = ginf[c(1,6,7)]

## Merging events and leagues

events = merge(events,league,by='id_odsp')

## Extracting shots events and eliminating Own goals

attach(events)
shot_events <- events[ which(event_type== '1' & (event_type2=='12' | is.na(event_type2))),]

## Remove unawnted columns

detach(events)
attach(shot_events)
shot_events <- shot_events[-c(2,5,6,13,14)]
shot_events$event_type2[is.na(shot_events$event_type2)] <- 16
shot_events$player2[is.na(shot_events$player2)] <- 'none'
shot_events = na.omit(shot_events)
shot_events = shot_events[which(shot_events$location!= '19'),]

## Filtering out the top shot takers and assist providers

sort(table(player),decreasing = TRUE)
top_player = c('cristiano ronaldo','lionel messi','zlatan ibrahimovic','robert lewandowski','edinson cavani','pierreemerick aubameyang',
                  'karim benzema','luis suarez','roberto firmino','antoine griezmann','gonzalo higuain ','sergio aguero','alexis sanchez',
                  'olivier giroud','diego costa','philippe coutinho')


shot_events = dplyr::mutate(shot_events,top_shot_takers=ifelse(player %in% top_player, 1,0))
shot_events$top_shot_takers=as.factor(shot_events$top_shot_takers)
attach(shot_events)
table(top_shot_takers)

sort(table(player2),decreasing = TRUE)
top_player2 = c('dimitri payet','mesut ozil','david silva','cristiano ronaldo','marek hamsik','lionel messi',
               'kevin de bruyne','thomas muller','miralem pjanic','eden hazard','christian eriksen ','toni kroos','angel di maria',
               'marco reus','andrea pirlo','cesc fabregas','isco')


shot_events = dplyr::mutate(shot_events,top_assist_providers=ifelse(player2 %in% top_player2, 1,0))
shot_events$top_shot_takers=as.factor(shot_events$top_shot_takers)
attach(shot_events)
table(top_assist_providers)

## Filtering out the top teams

sort(table(event_team),decreasing = TRUE)
top_team = c('Real Madrid','Barcelona','Atletico Madrid','Liverpool','Tottenham','Juventus',
                'Napoli','AS Roma','Internazionale','AC Milan','Bayern Munich','Borussia Dortmund','Chelsea',
                'Arsenal','Paris Saint-Germain','Marseille','Lyon','Manchester Utd','Manchester City')


shot_events = dplyr::mutate(shot_events,top_event_team=ifelse(event_team %in% top_team, 1,0))
shot_events$top_event_team=as.factor(shot_events$top_event_team)
attach(shot_events)

shot_events = dplyr::mutate(shot_events,top_opponent=ifelse(opponent %in% top_team, 1,0))
shot_events$top_opponent=as.factor(shot_events$top_opponent)
attach(shot_events)


##Converting to factor for replacing values

shot_events$event_type2=as.factor(shot_events$event_type2)
shot_events$side=as.factor(shot_events$side)
shot_events$shot_place=as.factor(shot_events$shot_place)
shot_events$shot_outcome=as.factor(shot_events$shot_outcome)
shot_events$location=as.factor(shot_events$location)
shot_events$bodypart=as.factor(shot_events$bodypart)
shot_events$assist_method=as.factor(shot_events$assist_method)
shot_events$situation=as.factor(shot_events$situation)
shot_events$fast_break=as.factor(shot_events$fast_break)

## Replace numbers with events

Replaces <- data.frame(from = c("12", "16"), to = c("Key Pass", "Other"))
shot_events <- FindReplace(data = shot_events, Var = "event_type2", replaceData = Replaces,
                       from = "from", to = "to", exact = FALSE)

Replaces <- data.frame(from = c("1", "2"), to = c("Home", "Away"))
shot_events <- FindReplace(data = shot_events, Var = "side", replaceData = Replaces,
                           from = "from", to = "to", exact = FALSE)

Replaces <- data.frame(from = c(1:13), to = c("Bit too high", "Blocked", "Bottom left corner",
                                              "Bottom right corner","Centre of the goal","High and wide",
                                              "Hits the bar","Misses to the left",
                                              "Misses to the right","Too high","Top centre of the goal",
                                              "Top left corner","Top right corner"))
shot_events <- FindReplace(data = shot_events, Var = "shot_place", replaceData = Replaces,
                           from = "from", to = "to", exact = TRUE)

Replaces <- data.frame(from = c(1:4), to = c("On Target","Off Target",
                                             "Blocked","Hit the bar"))
shot_events <- FindReplace(data = shot_events, Var = "shot_outcome", replaceData = Replaces,
                           from = "from", to = "to", exact = FALSE)

Replaces <- data.frame(from = c(1:18), to = c("Attacking Half","Defensive Half",
                                              "Centre of the box","Left Wing","Right Wing",
                                              "Difficult angle and long range",
                                              "Difficult angle on the left",
                                              "Difficult angle on the right",
                                              "Left side of the box",
                                              "Left side of the six yard box",
                                              "Right side of the box",
                                              "Right side of the six yard box",
                                              "Very close range","Penalty spot",
                                              "Outside the box","Long range",
                                              "More than 35 yards","More than 40 yards"))

shot_events <- FindReplace(data = shot_events, Var = "location", replaceData = Replaces,
                           from = "from", to = "to", exact = TRUE)


Replaces <- data.frame(from = c(1:3), to = c("Right Foot","Left Foot",
                                             "Head"))
shot_events <- FindReplace(data = shot_events, Var = "bodypart", replaceData = Replaces,
                           from = "from", to = "to", exact = FALSE)

Replaces <- data.frame(from = c(0:4), to = c("None","Pass","Cross",
                                             "Headed Pass","Through Ball"))
shot_events <- FindReplace(data = shot_events, Var = "assist_method", replaceData = Replaces,
                           from = "from", to = "to", exact = FALSE)

Replaces <- data.frame(from = c(1:4), to = c("Open Play","Set Piece",
                                             "Corner","Free Kick"))
shot_events <- FindReplace(data = shot_events, Var = "situation", replaceData = Replaces,
                           from = "from", to = "to", exact = FALSE)


##Factorizing replaced values and other categorical variables

shot_events$event_type2=as.factor(shot_events$event_type2)
shot_events$side=as.factor(shot_events$side)
shot_events$event_team=as.factor(shot_events$event_team)
shot_events$opponent=as.factor(shot_events$opponent)
shot_events$player=as.factor(shot_events$player)
shot_events$player2=as.factor(shot_events$player2)
shot_events$shot_place=as.factor(shot_events$shot_place)
shot_events$shot_outcome=as.factor(shot_events$shot_outcome)
shot_events$location=as.factor(shot_events$location)
shot_events$bodypart=as.factor(shot_events$bodypart)
shot_events$assist_method=as.factor(shot_events$assist_method)
shot_events$situation=as.factor(shot_events$situation)
shot_events$fast_break=as.factor(shot_events$fast_break)
shot_events$country=as.factor(shot_events$country)
shot_events$season=as.factor(shot_events$season)

## Correlation Test

#Correlation Matrix
quantvars=shot_events[,c(2,3)]
corr_matrix=cor(quantvars)
round(corr_matrix,2)

##Removing 'sort_order' due to correlation with time

shot_events <- shot_events[-c(2)]

################################ Grophs 1 ########################

shot_events = dplyr::mutate(shot_events,goal_scored=ifelse(is_goal==1,'Goal','No Goal'))

title_theme = theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (13),hjust = 0.5),
                    axis.title = element_text(family = "Helvetica", size = (12)),
                    axis.text = element_text(family = "Courier", size = (12)))

ggplot(shot_events, aes(goal_scored, ..count..)) + geom_bar(aes(fill = goal_scored), position = "dodge") + scale_fill_brewer(palette = "Paired")+
  xlab("Goal Distribution") + ylab("Number of shots") + ggtitle("Shots vs Goals Distribution") + title_theme + 
  guides(fill=guide_legend(title="Goal Scored"))

ggplot(shot_events, aes(goal_scored, ..count..)) + geom_bar(aes(fill = location), position = "fill") + 
  xlab("Goal Distribution") + ylab("Percentage of shots") + ggtitle("Shots(%) vs Goals Distribution") + title_theme + 
  guides(fill=guide_legend(title="Location"))

ggplot(shot_events, aes(goal_scored, ..count..)) + geom_bar(aes(fill = assist_method), position = "dodge") + scale_fill_brewer(palette = "Paired") +
  xlab("Goal Distribution") + ylab("Number of shots") + ggtitle("Shots vs Assists Distribution") + title_theme + 
  guides(fill=guide_legend(title="Method of Assist"))

ggplot(shot_events, aes(goal_scored, ..count..)) + geom_bar(aes(fill = country), position = "dodge") + scale_fill_brewer(palette = "Paired") +
  xlab("Goal Distribution") + ylab("Number of shots") + ggtitle("Shots vs Goals Distribution") + title_theme + 
  guides(fill=guide_legend(title="Country"))

ggplot(shot_events, aes(goal_scored, ..count..)) + geom_bar(aes(fill = side), position = "dodge") + scale_fill_brewer(palette = "Paired") +
  xlab("Goal Distribution") + ylab("Number of shots") + ggtitle("Shots vs Goals Distribution") + title_theme + 
  guides(fill=guide_legend(title="Team Side"))

ggplot(shot_events, aes(goal_scored, time)) + geom_violin(aes(fill = shot_outcome), position = "dodge") + scale_fill_brewer(palette = "Paired") +
  xlab("Goal Distribution") + ylab("Time (minutes)") + ggtitle("Time vs Shots Distribution") + title_theme + 
  guides(fill=guide_legend(title="Shot Outcome"))

player_goals = shot_events[c(7,11)]
player_goals = player_goals %>% group_by(player) %>% dplyr::summarize(Total_Goals=sum(is_goal))
top = top_n(player_goals,n=10)

ggplot(top, aes(x=reorder(player,Total_Goals), y=Total_Goals)) + geom_col(fill='steelblue') + coord_flip() + geom_text(aes(label=Total_Goals), hjust=1.2, size=5) +
  xlab("Players") + ylab("Total Goals") + ggtitle("Top 10 Goal Scorers") + title_theme

player_goals1 = shot_events[c(8,11)]
player_goals1 = player_goals1 %>% group_by(player2) %>% dplyr::summarize(Total_Asists=sum(is_goal))
top = top_n(player_goals1,n=11)
top = top[1:10,]

ggplot(top, aes(x=reorder(player2,Total_Asists), y=Total_Asists)) + geom_col(fill='steelblue') + coord_flip() + geom_text(aes(label=Total_Asists), hjust=1.2, size=5) +
  xlab("Players") + ylab("Total Assists") + ggtitle("Top 10 Assist Providers") + title_theme

summary(ginf$odd_h)
summary(ginf$odd_a)
summary(ginf$odd_d)



################################ Logistic Regression ##########################

logit=glm(is_goal ~ time+
            location+
            side+
            bodypart+
            fast_break+
            situation+
            assist_method+
            season+
            top_shot_takers+
            top_assist_providers+
            top_event_team+
            top_opponent+
            bodypart*assist_method,
            family='binomial',
          data=shot_events)
summary(logit)

logit_r2=lrm(is_goal ~ time+
               location+
               side+
               bodypart+
               fast_break+
               situation+
               assist_method+
               season+
               top_shot_takers+
               top_assist_providers+
               top_event_team+
               top_opponent+
               bodypart*assist_method,
          data=shot_events,maxit=1000)
logit_r2

################################ Please run the below chunk of code for further analysis##########
xG = predict(logit,shot_events,type='response')
xG = data.frame(xG)
Prediction = ifelse(xG>=0.5,1,0)
class_error = mean(Prediction!=shot_events$is_goal)
shot_events$Expected_Goals = xG

################################ Cross Validation #############

cv.logit <-
  function (data, model, yname, K) 
  {
    n <- nrow(data)
    datay=data[,yname] 
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    CV=NULL
    
    for (i in 1:K) { 
      test.index <- seq_len(n)[(s == i)] 
      train.index <- seq_len(n)[(s != i)] 
      
      logit.fit=glm(model, data=data[train.index,],family = "binomial")
      logit.y <- data[test.index, yname]
      logit.predy=predict(logit.fit, data[test.index,],type='response')
      logit.predy=ifelse(logit.predy>=0.5,1,0)
      predy = data.frame(logit.predy)
      error= mean(logit.y!=logit.predy)
      CV=c(CV,error)
    }
    list(call = model, K = K, 
         logit_error_rate = mean(CV),cv=CV,pred=predy)  
  }

er_logit=cv.logit(data=shot_events,model=is_goal ~ time+
                    location+
                    side+
                    bodypart+
                    fast_break+
                    situation+
                    assist_method+
                    season+
                    top_shot_takers+
                    top_assist_providers+
                    top_event_team+
                    top_opponent+
                    bodypart*assist_method, yname="is_goal", K=5)
er_logit$logit_error_rate
er_logit$cv

################################ Data Preparation for Tree Based Models #################

## Aggregating Expected Goals for home and away teams
attach(shot_events)
Home = shot_events[ which(side== 'Home'),]
Home = Home %>% group_by(id_odsp) %>% summarise(sum(Expected_Goals))


Away = shot_events[ which(side== 'Away'),]
Away = Away %>% group_by(id_odsp) %>% summarise(sum(Expected_Goals))

match = ginf[-c(2,3,5,15,16,17,18)]

match = merge(match,Home,by='id_odsp')
names(match)[12]<-paste("ht_xG")

match = merge(match,Away,by='id_odsp')
names(match)[13]<-paste("at_xG")

##Defining wins and goal differences for each team

match = mutate(match,ht_res=ifelse(fthg>ftag,"Win",ifelse(fthg<ftag,"lose","draw")))
match = mutate(match,ht_GD=fthg-ftag)
match = mutate(match,ht_pts=ifelse(fthg>ftag,3,ifelse(fthg<ftag,0,1)))
match = mutate(match,at_pts=ifelse(ftag>fthg,3,ifelse(ftag<fthg,0,1)))

## Extracting home teams

Home_team = match[c(1,3,4,5,7,8,12,13)]
Home_team = Home_team %>% group_by(country,season,ht) %>% summarise(sum(fthg),sum(ht_xG),sum(ftag),sum(at_xG))
names(Home_team)[3]<-paste("team")
names(Home_team)[4]<-paste("goals")
names(Home_team)[5]<-paste("xG")
names(Home_team)[6]<-paste("GC")
names(Home_team)[7]<-paste("xGC")

Away_team = match[c(1,3,4,6,7,8,12,13)]
Away_team = Away_team %>% group_by(country,season,at) %>% summarise(sum(ftag),sum(at_xG),sum(fthg),sum(ht_xG))
names(Away_team)[3]<-paste("team")
names(Away_team)[4]<-paste("goals")
names(Away_team)[5]<-paste("xG")
names(Away_team)[6]<-paste("GC")
names(Away_team)[7]<-paste("xGC")

Goals_table = rbind(Home_team,Away_team)
Goals_table = Goals_table %>% group_by(country,season,team) %>% summarise(sum(goals),sum(xG),sum(GC),sum(xGC))
names(Goals_table)[4]<-paste("goals")
names(Goals_table)[5]<-paste("xG")
names(Goals_table)[6]<-paste("GC")
names(Goals_table)[7]<-paste("xGC")

Goals_table <-  mutate(Goals_table,goals_xG=goals-xG)
Goals_table <-  mutate(Goals_table,GC_xGC=GC-xGC)

##Weekly table for finding moving average Goal difference and form of each team

weekly_home = match[c(1,2,3,4,5,7,8,12,13,16)]
names(weekly_home)[5]<-paste("team")
names(weekly_home)[6]<-paste("goals")
names(weekly_home)[7]<-paste("GC")
names(weekly_home)[8]<-paste("xG")
names(weekly_home)[9]<-paste("xGC")
names(weekly_home)[10]<-paste("Pts")

weekly_away = match[c(1,2,3,4,6,7,8,12,13,17)]
names(weekly_away)[5]<-paste("team")
names(weekly_away)[6]<-paste("goals")
names(weekly_away)[7]<-paste("GC")
names(weekly_away)[8]<-paste("xG")
names(weekly_away)[9]<-paste("xGC")
names(weekly_away)[10]<-paste("Pts")

weekly = rbind(weekly_home,weekly_away)
weekly = mutate(weekly,matchweek = as.Date(date,format='%Y-%m-%d'))
weekly$matchweek = strftime(weekly$matchweek,format = '%V')
weekly$matchweek = as.numeric(weekly$matchweek)
weekly = mutate(weekly,matchweek_1=ifelse(matchweek<30,matchweek+53,matchweek))

min_day = weekly %>% group_by(season,country) %>% summarise(min_day=min(matchweek_1))
weekly = merge(weekly,min_day,by=c('season','country'))
weekly = mutate(weekly,matchday=matchweek_1-min_day+1)
weekly = weekly[-c(11,12,13)]

weekly <- weekly[order(weekly$team, weekly$matchday),]

weekly = weekly %>% group_by(season,team) %>% mutate(sum_goals = runSum(goals, 5))
weekly = weekly %>% group_by(season,team) %>% mutate(sum_GC = runSum(GC, 5))
weekly = weekly %>% group_by(season,team) %>% mutate(avg_xG = runSum(xG, 5))
weekly = weekly %>% group_by(season,team) %>% mutate(avg_xGC = runSum(xGC, 5))
weekly = weekly %>% group_by(season,team) %>% mutate(sum_Pts = runSum(Pts, 5))

weekly$sum_goals = weekly$sum_goals-weekly$goals
weekly$sum_GC = weekly$sum_GC-weekly$GC
weekly$avg_xG = (weekly$avg_xG-weekly$xG)/4
weekly$avg_xGC = (weekly$avg_xGC-weekly$xGC)/4
weekly$sum_Pts = weekly$sum_Pts-weekly$Pts

match_d = weekly[c(3,5,12,13,14,15,16)]
match_d = na.omit(match_d)

weekly_home = merge(weekly_home,match_d,by=c('id_odsp','team'))
home_performance = weekly_home[c(1,11,12,13,14,15)]
names(home_performance)[2]=paste("HT_Goals_past")
names(home_performance)[3]=paste("HT_GC_past")
names(home_performance)[4]=paste("HT_avg_xG_past")
names(home_performance)[5]=paste("HT_avg_xGC_past")
names(home_performance)[6]=paste("HT_Pts_past")

weekly_away = merge(weekly_away,match_d,by=c('id_odsp','team'))
away_performance = weekly_away[c(1,11,12,13,14,15)]
names(away_performance)[2]=paste("AT_Goals_past")
names(away_performance)[3]=paste("AT_GC_past")
names(away_performance)[4]=paste("AT_avg_xG_past")
names(away_performance)[5]=paste("AT_avg_xGC_past")
names(away_performance)[6]=paste("AT_Pts_past")

match_analysis = match[-c(2,12,13,15,16,17)]
match_analysis = merge(match_analysis,home_performance,by='id_odsp')
match_analysis = merge(match_analysis,away_performance,by='id_odsp')

match_analysis = mutate(match_analysis,HT_GD_past = HT_Goals_past-HT_GC_past)
match_analysis = mutate(match_analysis,AT_GD_past = AT_Goals_past-AT_GC_past)

## Predicting match outcome

## Correlation Test

#Correlation Matrix
quantvars=match_analysis[,c(8,9,10,12:23)]
corr_matrix=cor(quantvars)
round(corr_matrix,2)


################################ Tree Based Models ###################

##Random Forest

attach(match_analysis)
match_analysis = mutate(match_analysis,HT_Win = ifelse(fthg>ftag,1,0))
match_analysis = mutate(match_analysis,HT_Win_fact = ifelse(fthg>ftag,1,0))
match_analysis$ht_res = as.factor(match_analysis$ht_res)
match_analysis$HT_Win_fact = as.factor(match_analysis$HT_Win_fact)
match_analysis$country = as.factor(match_analysis$country)
match_analysis = dplyr::mutate(match_analysis,top_ht=ifelse(ht %in% top_team, 1,0))
match_analysis = dplyr::mutate(match_analysis,top_at=ifelse(at %in% top_team, 1,0))
match_analysis$top_ht = as.factor(match_analysis$top_ht)
match_analysis$top_at = as.factor(match_analysis$top_at)

set.seed(1)
myforest=randomForest(ht_res~odd_h+odd_d+odd_a, ntree=500, data=match_analysis, importance=TRUE, na.action = na.omit)
myforest


myforest=randomForest(ht_res~odd_h+odd_d+odd_a+
                        HT_Goals_past+AT_Goals_past+
                        HT_GC_past+AT_GC_past+
                        HT_avg_xG_past+AT_avg_xG_past+
                        HT_avg_xGC_past+AT_avg_xGC_past+
                        HT_Pts_past+AT_Pts_past+
                        HT_GD_past+AT_GD_past+
                        top_ht+top_at+
                        season+country, ntree=500, data=match_analysis, importance=TRUE, na.action = na.omit)
myforest
importance(myforest)
varImpPlot(myforest)



## Boosting

set.seed(1)
boosted=gbm(HT_Win~odd_h+odd_d+odd_a, n.tree=1000, data=match_analysis, distribution = 'bernoulli',interaction.depth = 4)
summary(boosted)

predicted_score=predict(boosted, newdata=match_analysis, n.trees=1000,type = "response")
#predicted_score = data.frame(predicted_score)
predicted_score = ifelse(predicted_score>=0.5,1,0)
class_error = mean(predicted_score!=match_analysis$HT_Win)


set.seed(1)
boosted=gbm(HT_Win~odd_h+odd_d+odd_a+
              HT_Goals_past+AT_Goals_past+
              HT_GC_past+AT_GC_past+
              HT_avg_xG_past+AT_avg_xG_past+
              HT_avg_xGC_past+AT_avg_xGC_past+
              HT_Pts_past+AT_Pts_past+
              HT_GD_past+AT_GD_past+
              top_ht+top_at+
              season+country, n.tree=1000, data=match_analysis, distribution = 'bernoulli',interaction.depth = 4)
summary(boosted)

predicted_score=predict(boosted, newdata=match_analysis, n.trees=1000,type = "response")
#predicted_score = data.frame(predicted_score)
predicted_score = ifelse(predicted_score>=0.5,1,0)
class_error = mean(predicted_score!=match_analysis$HT_Win)

##Cross Validation

cv.gbm <-
  function (data, model, yname, K,t,i) 
  {
    n <- nrow(data)
    datay=data[,yname] 
    f <- ceiling(n/K)
    s <- sample(rep(1:K, f), n)  
    
    CV=NULL
    
    for (i in 1:K) { 
      test.index <- seq_len(n)[(s == i)] 
      train.index <- seq_len(n)[(s != i)] 
      
      gbm.fit=gbm(model, data=data[train.index,], n.tree=t,distribution = 'bernoulli',interaction.depth = i)
      gbm.y <- data[test.index, yname]
      gbm.predy=predict(gbm.fit, newdata=data[test.index,], n.trees=t,type='response')
      gbm.predy=ifelse(gbm.predy>=0.5,1,0)
      predy = data.frame(gbm.predy)
      error= mean(gbm.y!=gbm.predy)
      CV=c(CV,error)
    }
    list(call = model, K = K, 
         gbm_error_rate = mean(CV),cv=CV,pred=predy)  
  }

er_gbm=cv.gbm(data=match_analysis,model=HT_Win~odd_h+odd_d+odd_a+
                HT_Goals_past+AT_Goals_past+
                HT_GC_past+AT_GC_past+
                HT_avg_xG_past+AT_avg_xG_past+
                HT_avg_xGC_past+AT_avg_xGC_past+
                HT_Pts_past+AT_Pts_past+
                HT_GD_past+AT_GD_past+
                top_ht+top_at+
                season+country, yname="HT_Win", K=5,t=1000,i=3)
er_gbm$gbm_error_rate
er_gbm$cv

################################ Graph 2 ###############

## Team Analysis

Teams = rbind(Home_team,Away_team)
Teams = Teams %>% group_by(country,season,team) %>% summarize(Goals = sum(goals),xG = sum(xG),Goals_Conceded = sum(GC),xGC=sum(xGC))
Teams = dplyr::mutate(Teams,Attack = Goals-xG)
Teams = dplyr::mutate(Teams,Defence = xGC-Goals_Conceded)
Teams = dplyr::mutate(Teams,Performance = Attack+Defence)
Perf = Teams[order(Teams$Performance),]
Perf_England = Perf[which(Perf$country== 'england'),]
Perf_Spain = Perf[which(Perf$country== 'spain'),]

##Premier League

OverPerf = tail(Perf_England,10)
UnderPerf = head(Perf_England,10)

OverPerf = transform(OverPerf,Team_Season=paste(team,season))
UnderPerf = transform(UnderPerf,Team_Season=paste(team,season))

ggplot(OverPerf, aes(x=reorder(Team_Season,Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=1.2, size=5)+
  xlab("Team (Season)") + ylab("Performance") + ggtitle("Top 10 Over-performing Teams in Premier League") + title_theme + 
  guides(fill=guide_legend(title="Season"))

ggplot(UnderPerf, aes(x=reorder(Team_Season,-Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=-0.3, size=5)+
  xlab("Team (Season)") + ylab("Performance") + ggtitle("Top 10 Under-performing Teams in Premier League") + title_theme + 
  guides(fill=guide_legend(title="Season"))

##La Liga

OverPerf = tail(Perf_Spain,10)
UnderPerf = head(Perf_Spain,10)

OverPerf = transform(OverPerf,Team_Season=paste(team,season))
UnderPerf = transform(UnderPerf,Team_Season=paste(team,season))


ggplot(OverPerf, aes(x=reorder(Team_Season,Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=1.2, size=5)+
  xlab("Team (Season)") + ylab("Performance") + ggtitle("Top 10 Over-performing Teams in La Liga") + title_theme + 
  guides(fill=guide_legend(title="Season"))

ggplot(UnderPerf, aes(x=reorder(Team_Season,-Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=-0.3, size=5)+
  xlab("Team (Season)") + ylab("Performance") + ggtitle("Top 10 Under-performing Teams in La Liga") + title_theme + 
  guides(fill=guide_legend(title="Season"))

OverPerf = tail(Perf,10)
UnderPerf = head(Perf,10)

OverPerf = transform(OverPerf,Team_Season=paste(team,season))
UnderPerf = transform(UnderPerf,Team_Season=paste(team,season))

ggplot(OverPerf, aes(x=reorder(Team_Season,Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=1.2, size=5)+
  xlab("Team (Season)") + ylab("Performance") + ggtitle("Top 10 Over-performing Teams in La Liga") + title_theme + 
  guides(fill=guide_legend(title="Season"))

ggplot(UnderPerf, aes(x=reorder(Team_Season,-Performance), y=Performance)) + geom_col(aes(fill=season)) + coord_flip() + geom_text(aes(label=season), hjust=-0.3, size=5) + xlab("Teams_Season")



# Leicester City Performance Analysis

Leicester = Teams[which(Teams$team== 'Leicester City'),]
Leicester_reshape = Leicester[-c(1,3,8,9,10)]
Leicester_reshape = reshape2::melt(Leicester_reshape,id.var='season')

ggplot(Leicester_reshape, aes(x=season, y=value, col=variable)) + geom_line(size=1.5)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+ geom_point(size=4)+
  xlab("Season") + ylab("Goal") + ggtitle("Leicester City Performance Analysis") + title_theme

# Barcelona Performance Analysis

Barca = Teams[which(Teams$team== 'Barcelona'),]
Barca_reshape = Barca[-c(1,3,8,9,10)]
Barca_reshape = reshape2::melt(Barca_reshape,id.var='season')

barcelona=ggplot(Barca_reshape, aes(x=season, y=value, col=variable)) + geom_line(size=1.5)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+ geom_point(size=5)+
  xlab("Season") + ylab("Goal") + ggtitle("Barcelona Performance Analysis") + title_theme


# Real Madrid Performance Analysis

Real = Teams[which(Teams$team== 'Real Madrid'),]
Real_reshape = Real[-c(1,3,8,9,10)]
Real_reshape = reshape2::melt(Real_reshape,id.var='season')

real_madrid=ggplot(Real_reshape, aes(x=season, y=value, col=variable)) + geom_line(size=1.5)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+ geom_point(size=5)+
  xlab("Season") + ylab("Goal") + ggtitle("Real Madrid Performance Analysis") + title_theme


ggarrange(barcelona,real_madrid)

##Player Analysis

players = shot_events[c(7,10,11,17,24)]
attach(players)
players = dplyr::mutate(players,on_Target = ifelse(shot_outcome=='On Target',1,0))
players = players[-c(2)]
Total_Shots = data.frame(players %>% count(season,player))
temp = Total_Shots$n
players = players %>% group_by(season,player) %>% summarize(Goals = sum(is_goal),xG = sum(Expected_Goals),on_Target = sum(on_Target))
players = dplyr::mutate(players,Performance = Goals-xG)
players[,'Total_Shots'] = temp
players=na.omit(players)
players = dplyr::mutate(players,Percentage_on_target = on_Target/Total_Shots)
players = players[order(players$Performance),]
Over_Perf = tail(players,10)
Over_Perf$Performance = round(Over_Perf$Performance,2)
Over_Perf = transform(Over_Perf,Player_Season=paste(player,season))

ggplot(Over_Perf, aes(x=reorder(Player_Season,Performance), y=Goals)) + geom_col(fill='steelblue') + coord_flip() + geom_text(aes(label=Performance), hjust=1.2, size=5)+
  xlab("Player (Season)") + ylab("Goals (Performance)") + ggtitle("Top 10 Over-Performing Goal Scorers") + title_theme + 
  guides(fill=guide_legend(title="Season"))

bale = players[which(players$player== 'gonzalo higuain'),]
bale_reshape = bale[-c(2,6,7,8)]
bale_reshape$season <- as.numeric(as.character(bale_reshape$season))
bale_reshape = reshape2::melt(bale_reshape,id.var='season')

ggplot(bale_reshape, aes(x=season, y=value, col=variable)) + geom_line(size=1.5)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+ geom_point(size=5)+
  xlab("Season") + ylab("Goals") + ggtitle("Player Analysis - Gonzalo Higuain") + title_theme


#Assist Providers

##Player Analysis

players = shot_events[c(8,10,11,17,24)]
attach(players)
players = players %>% group_by(season,player2) %>% summarize(Assists = sum(is_goal),xA = sum(Expected_Goals))
players = dplyr::mutate(players,Performance = Assists-xA)
players=na.omit(players)
players = players[order(players$Performance),]
Over_Perf = tail(players[which(players$player2!="none"),],10)
Over_Perf$Performance = round(Over_Perf$Performance,2)
Over_Perf = transform(Over_Perf,Player_Season=paste(player2,season))

ggplot(Over_Perf, aes(x=reorder(Player_Season,Performance), y=Assists)) + geom_col(fill='steelblue') + coord_flip() + geom_text(aes(label=Performance), hjust=1.2, size=5)+
  xlab("Player (Season)") + ylab("Assists (Performance)") + ggtitle("Top 10 Over-Performing Assist Providers") + title_theme + 
  guides(fill=guide_legend(title="Season"))

bale = players[which(players$player2== 'angel di maria'),]
bale_reshape = bale[-c(2,5)]
bale_reshape$season <- as.numeric(as.character(bale_reshape$season))
bale_reshape = reshape2::melt(bale_reshape,id.var='season')

ggplot(bale_reshape, aes(x=season, y=value, col=variable)) + geom_line(size=1.5)+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+ geom_point(size=5)+
  xlab("Season") + ylab("Assists") + ggtitle("Player Analysis - Angel Di Maria") + title_theme

