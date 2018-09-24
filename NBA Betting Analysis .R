#NBA Betting Analysis

#loading libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)

#Breakdown of data in nbateams
ggpairs(data = nbateams, columns = 3:6, title = "NBA Team Data 1")
ggpairs(data = nbateams, columns = 7:10, title = "NBA Team Data 1")
ggpairs(data = nbateams, columns = 7:10, title = "NBA Team Data 2")
ggpairs(data = nbateams, columns = 11:14, title = "NBA Team Data 3")
ggpairs(data = nbateams, columns = 15:18, title = "NBA Team Data 4")
ggpairs(data = nbateams, columns = 19:22, title = "NBA Team Data 5")
ggpairs(data = nbateams, columns = 23:26, title = "NBA Team Data 6")

#Most important grouping due to manually created columns based on external research
ggpairs(data = nbateams, columns = 27:32, title = "NBA Team Data 7")


#spread models
spreadmodel <- lm(plusminuspointspergame ~ pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + pfpergame + pfdrawnpergame + blockedfgattemptspergame, data = nbateams)
spreadmodel2 <- lm(plusminuspointspergame ~ threefgpercent + ftpercent + reboundspergame + tospergame + stealspergame, data = nbateams)
spreadmodel3 <- lm(plusminuspointspergame ~ ftpercent + reboundspergame + stealspergame, data = nbateams)
spreadmodel4 <- lm(plusminuspointspergame ~ ftpercent + reboundspergame + tospergame + stealspergame, data = nbateams)
spreadmodel5 <- lm(plusminuspointspergame ~ pointspergame + fgpercent + threefgpercent + ftpercent + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + pfdrawnpergame + blockedfgattemptspergame, data = nbateams)
spreadmodel6 <- lm(plusminuspointspergame ~ minutespergame + pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + blockedfgattemptspergame + pfpergame + pfdrawnpergame, data = nbateams)
spreadmodel7 <- lm(plusminuspointspergame ~ minutespergame + pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + blockedfgattemptspergame + pfpergame + pfdrawnpergame + efffgpercent + estimatedpossessions, data = nbateams)
spreadmodel8 <- lm(plusminuspointspergame ~ minutespergame + pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + blockedfgattemptspergame + pfpergame + pfdrawnpergame + efffgpercent + estimatedpossessions + foulsconverted, data = nbateams)
spreadmodel9 <- lm(plusminuspointspergame ~ minutespergame + pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + blockedfgattemptspergame + pfpergame + pfdrawnpergame + efffgpercent + estimatedpossessions + foulsconverted + offrebperc, data = nbateams)
spreadmodel10 <- lm(plusminuspointspergame ~ efffgpercent + estimatedpossessions + foulsconverted + offrebperc, data = nbateams)

#summary of spreadmodel9
summary(spreadmodel9)

#summary of spreadmodel10
summary(spreadmodel10)

#Histogram of spreadmodel9 Residuals
ggplot(data = nbateams, aes(spreadmodel9$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal() +
  ggtitle("Histogram for Model Residuals")

#Histogram of spreadmodel10 Residuals
ggplot(data = nbateams, aes(spreadmodel10$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal() +
  ggtitle("Histogram for Model Residuals")

#linear model fitted to data for plusminuspointspergame and wins
ggplot(data = nbateams, aes(x = wins, y = plusminuspointspergame)) + geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme_minimal() + 
  ggtitle("Linear Model Fitted to Data")

#predicted linear model spreadmodel9 fitted to data
ggplot(data = nbateams, aes(x = wins, y = predplusminus_spreadmodel9)) + geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme_minimal() + 
  ggtitle("Predicted Linear Model 9 Fitted to Data")

#predicted linear model spreadmodel10 fitted to data
ggplot(data = nbateams, aes(x = wins, y = predplusminus_spreadmodel10)) + geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme_minimal() + 
  ggtitle("Predicted Linear Model 10 Fitted to Data")

#copied dataset to new object for easier manipulation
d <- nbateams

#saving residuals and predicted from both spreadmodel9 and spreadmodel10
d$predicted9 <- predict(spreadmodel9)
d$residuals9 <- residuals(spreadmodel9)

d$predicted10 <- predict(spreadmodel10)
d$residuals10 <- residuals(spreadmodel10)

#plotting actual points
ggplot(d, aes(x = wins, y = plusminuspointspergame)) +
  geom_point()

#adding predicted values from spreadmodel9
ggplot(d, aes(x = wins, y = plusminuspointspergame)) +
  geom_point() +
  geom_point(aes(y = predicted9), shape = 1)

#connecting predicted with corresponding values
ggplot(d, aes(x = wins, y = plusminuspointspergame)) +
  geom_segment(aes(xend = wins, yend = predicted9)) +
  geom_point() +
  geom_point(aes(y = predicted9), shape = 1)

#Final for spreadmodel9 vs Actual
ggplot(d, aes(x = wins, y = plusminuspointspergame)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = wins, yend = predicted9), alpha = .2) +
  geom_point(aes(color = abs(residuals9))) +
  scale_color_continuous(low = "green", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted9), shape = 1) +
  theme_minimal() +
  ggtitle("Predicted spreadmodel9 vs Actual")

#spreadmodel10 vs Actual
ggplot(d, aes(x = wins, y = plusminuspointspergame)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = wins, yend = predicted10), alpha = .2) +
  geom_point(aes(color = abs(residuals10))) +
  scale_color_continuous(low = "green", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted10), shape = 1) +
  theme_minimal() +
  ggtitle("Predicted spreadmodel10 vs Actual")

#data defining numbers of plusminuspointspergame as well as predicted means from spreadmodel9 & spreadmodel10
max(nbateams$plusminuspointspergame)
#8.5
max(nbateams$predplusminus_spreadmodel9)
#8.84004
max(nbateams$predplusminus_spreadmodel10)
#7.977606
min(nbateams$plusminuspointspergame)
#-9.4
min(nbateams$predplusminus_spreadmodel9)
#-9.012543
min(nbateams$predplusminus_spreadmodel10)
#-7.623539
median(nbateams$plusminuspointspergame)
#.55
median(nbateams$predplusminus_spreadmodel9)
#.8245395
median(nbateams$predplusminus_spreadmodel10)
#-.4393513

#Analysis of Vegas 

#histogram of Vegas' home spread line
ggplot(all_games, aes(x = all_games$Spread_Close, fill = cut(all_games$Spread_Close, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 2) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Vegas' Spread Prediction", y = "Count") +
  ggtitle("Histogram of Vegas' Home Spread Prediction")

#histogram of actual spread line
ggplot(all_games, aes(x = all_games$actual_spread_difference, fill = cut(all_games$actual_spread_difference, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 2) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Actual Ending Spread", y = "Count") +
  ggtitle("Histogram of Actual Ending Spread")

#histogram of actual game totals
ggplot(all_games, aes(x = all_games$`Actual Game Total`, fill = cut(all_games$`Actual Game Total`, 100))) +
geom_histogram(show.legend = FALSE, binwidth = 5) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Actual Game Total", y = "Count") +
  ggtitle("Histogram of Actual Game Totals")

#histogram of Vegas' predicted game totals
ggplot(all_games, aes(x = all_games$OverUnder_Close, fill = cut(all_games$OverUnder_Close, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 2.5) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Vegas Predicted Game Total", y = "Count") +
  ggtitle("Histogram of Vegas' Predicted Game Totals")

#histogram of spread residuals of Vegas prediction vs Actual
ggplot(all_games, aes(x = all_games$final_home_spread_diff_vs_pred, fill = cut(all_games$final_home_spread_diff_vs_pred, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 2.5) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Predicted Spread and Actual Spread Difference", y = "Count") +
  ggtitle("Histogram of Gap in Prediction vs Actual Result")

#histogram of over/under residuals of Vegas prediction vs actual
ggplot(all_games, aes(x = all_games$pred_overunder_diff, fill = cut(all_games$pred_overunder_diff, 100))) +
  geom_histogram(show.legend = FALSE, binwidth = 2.5) +
  scale_y_continuous() +
  theme_minimal() +
  labs(x = "Predicted and Actual Over/Under Difference", y = "Count") +
  ggtitle("Histogram of Gap in Prediction vs Actual Result")

#predicting wins
winsmodel <- lm(wins ~ pointspergame + fgpercent + threefgpercent + ftpercent + offrebpergame + reboundspergame + assistspergame + tospergame + stealspergame + blockspergame + pfpergame + pfdrawnpergame + blockedfgattemptspergame, data = nbateams)
winsmodel2 <- lm(wins ~ pointspergame + threefgpercent + reboundspergame + assistspergame + stealspergame + blockedfgattemptspergame, data = nbateams)
winsmodel3 <- lm(wins ~ pointspergame + assistspergame + stealspergame + blockedfgattemptspergame, data = nbateams)
winsmodel4 <- lm(wins ~ efffgpercent + estimatedpossessions + foulsconverted + offrebperc, data = nbateams)

#best model
winsmodel5 <- lm(wins ~ pointspergame + assistspergame + stealspergame + efffgpercent + estimatedpossessions + foulsconverted + offrebperc, data = nbateams)

#saving residuals and predicted from winsmodel5
nbateams$predwins <- predict(winsmodel5)
nbateams$residualswins <- residuals(winsmodel5)


#predicted wins vs actual wins
ggplot(nbateams, aes(x = plusminuspointspergame, y = wins)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  geom_segment(aes(xend = plusminuspointspergame, yend = predwins), alpha = .2) +
  geom_point(aes(color = abs(residualswins))) +
  scale_color_continuous(low = "green", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predwins), shape = 1) +
  theme_minimal() +
  ggtitle("Predicted winsmodel5 vs Actual")

#important values for wins analysis
median(nbateams$wins)
#44
median(nbateams$predwins)
#40.26838

#breaking down accuracy month by month
#october_games <- slice(all_games, 1:104)
#november_games <- slice(all_games, 105:317)
#december_games <- slice(all_games, 318:544)
#january_games <- slice(all_games, 545:759)
#february_games <- slice(all_games, 760:919)
#march_games <- slice(all_games, 920:1031)
mean(october_games$final_home_spread_diff_vs_pred)
#1.288462
mean(november_games$final_home_spread_diff_vs_pred)
#-0.0657277
mean(december_games$final_home_spread_diff_vs_pred)
#1.090308
mean(january_games$final_home_spread_diff_vs_pred)
#-0.1790698
mean(february_games$final_home_spread_diff_vs_pred)
#1.1
mean(march_games$final_home_spread_diff_vs_pred)
#-0.1696429

#plot of all spread differences in Vegas Pred over time
ggplot(all_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point()

#plot month by month, October games
ggplot(october_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +
  geom_smooth() +
  ggtitle("October Vegas Spread Prediction Gaps")

#November games
ggplot(november_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +
  geom_smooth() +
  ggtitle("November Vegas Spread Prediction Gaps")

#December games
ggplot(december_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +
  geom_smooth() +
  ggtitle("December Vegas Spread Prediction Gaps")

#January games
ggplot(january_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +  
  geom_smooth() +
  ggtitle("January Vegas Spread Prediction Gaps")

#February games
ggplot(february_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +
  geom_smooth() +
  ggtitle("February Vegas Spread Prediction Gaps")

#March Games
ggplot(march_games, aes(x = Date, y = final_home_spread_diff_vs_pred)) +
  geom_point() +
  geom_smooth() +
  ggtitle("March Vegas Spread Prediction Gaps")


mean(abs(october_games$pred_overunder_diff))
#1.615385
mean(november_games$pred_overunder_diff)
#.2042254
mean(december_games$pred_overunder_diff)
#-.746696
mean(january_games$pred_overunder_diff)
#-.5860465
mean(february_games$pred_overunder_diff)
#-2.4625
mean(march_games$pred_overunder_diff)
#.6205357

#plot of all over under differences in Vegas Pred over time
ggplot(all_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  ggtitle("Over/Under Difference of Vegas Prediction vs Actual")

#plot month by month, October games
ggplot(october_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  geom_smooth() +
  ggtitle("October Vegas Over/Under Prediction Gaps")

#November games
ggplot(november_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  geom_smooth() +
  ggtitle("November Vegas Over/Under Prediction Gaps")

#December games
ggplot(december_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  geom_smooth() +
  ggtitle("December Vegas Over/Under Prediction Gaps")

#January games
ggplot(january_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +  
  geom_smooth() +
  ggtitle("January Vegas Over/Under Prediction Gaps")

#February games
ggplot(february_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  geom_smooth() +
  ggtitle("February Vegas Over/Under Prediction Gaps")

#March Games
ggplot(march_games, aes(x = Date, y = pred_overunder_diff)) +
  geom_point() +
  geom_smooth() +
  ggtitle("March Vegas Over/Under Prediction Gaps")

#creating best spread opportunities games 
bestopps <- subset(all_games, final_home_spread_diff_vs_pred >= 10 | final_home_spread_diff_vs_pred <= -10)

#creating best over/under opportunities
bestoverunderopps <- subset(all_games, pred_overunder_diff >= 15 | pred_overunder_diff <= -15)

#creating histogram for games most wrong on over/under predictions
ggplot(bestoverunderopps, aes(x = pred_overunder_diff)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal() +
  ggtitle("Histogram for Most Wrong Over/Under Predictions by Vegas")

#creating histogram for games most wrong on spread predictions
ggplot(bestopps, aes(x = final_home_spread_diff_vs_pred)) +
  geom_histogram(binwidth = 1, color = "black", fill = "blue") +
  theme_minimal() +
  ggtitle("Histogram for Most Wrong Spread Predictions by Vegas")

#creating home underdogs subset
homeunderdogs <- subset(all_games, Spread_Close > 0)



