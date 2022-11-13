#EN.625.725 Project
#Cassandra Carlson

#Packages
library(readxl)
library(caret)
library(car)

#Read in Excel file
data <- read_excel("/Users/cassandracarlson/Documents/2021_FantasyPros_Fantasy_Football_Advanced_Stats_Report_QB.xlsx") #Path will differ
#View(data)

#Variables
qb_rank <- c(data[1])
amt_of_games <- c(data[2])
comp_pass_rates <- c(data[3])
passing_yds <- c(data[4])
sacks <- c(data[5])
dropped <- c(data[6])

#Prepare variables for use
qb_rank = unlist(qb_rank)
amt_of_games = unlist(amt_of_games)
comp_pass_rates = unlist(comp_pass_rates)
passing_yds = unlist(passing_yds)
sacks = unlist(sacks)
dropped = unlist(dropped)

#Put variables into dataframe
df <- data.frame(qb_rank, amt_of_games, comp_pass_rates, passing_yds, sacks, dropped)
View(df)

##########################################################################################################

#Results for linear models that look at pressure from other players (includes sacks)

#Specify the cross-validation method
ctrl <- trainControl(method = "cv", number = 5)

#Fit a regression model and use k-fold CV to evaluate performance
model_5_folds <- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + sacks + dropped, data = df, method = "lm", trControl = ctrl)

#View summary of 5 k-fold CV  
print("Results of linear model with 5 folds:")
print(model_5_folds)

#View final model for 5 folds
print("Final model parameters for 5 folds:")
print(model_5_folds$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 5 folds):")
print(model_5_folds$resample)

#View ANOVA results
print("ANOVA results (for 5 folds):")
two_way1 <- aov(model_5_folds)
print(summary(two_way1))

#Specify the cross-validation method
ctrl1 <- trainControl(method = "cv", number = 6)

#Fit a regression model and use k-fold CV to evaluate performance
model_6_folds <- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + sacks + dropped, data = df, method = "lm", trControl = ctrl1)

#View summary of 6 k-fold CV  
print("Results of linear model with 6 folds:")
print(model_6_folds)

#View final model for 6 folds
print("Final model parameters for 6 folds:")
print(model_6_folds$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 6 folds):")
print(model_6_folds$resample)

#View ANOVA results
print("ANOVA results (for 6 folds):")
two_way2 <- aov(model_6_folds)
print(summary(two_way2))

#Specify the cross-validation method
ctrl2 <- trainControl(method = "cv", number = 7)

#Fit a regression model and use k-fold CV to evaluate performance
model_7_folds <- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + sacks + dropped, data = df, method = "lm", trControl = ctrl2)

#View summary of 7 k-fold CV  
print("Results of linear model with 7 folds:")
print(model_7_folds)

#View final model for 7 folds
print("Final model parameters for 7 folds:")
print(model_7_folds$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 7 folds):")
print(model_7_folds$resample)

#View ANOVA results
print("ANOVA results (for 7 folds):")
two_way3 <- aov(model_7_folds)
print(summary(two_way3))

##########################################################################################################

#Results for linear models that do not look at pressure from other players (does not include sacks)

#Specify the cross-validation method
ctrl3 <- trainControl(method = "cv", number = 5)

#Fit a regression model and use k-fold CV to evaluate performance
model_5_folds1 <- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + dropped, data = df, method = "lm", trControl = ctrl3)

#View summary of 5 k-fold CV  
print("Results of linear model with 5 folds:")
print(model_5_folds1)

#View final model for 5 folds
print("Final model parameters for 5 folds:")
print(model_5_folds1$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 5 folds):")
print(model_5_folds1$resample)

#View ANOVA results
print("ANOVA results (for 5 folds):")
two_way4 <- aov(model_5_folds1)
print(summary(two_way4))

#Specify the cross-validation method
ctrl4 <- trainControl(method = "cv", number = 6)

#Fit a regression model and use k-fold CV to evaluate performance
model_6_folds1 <- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + dropped, data = df, method = "lm", trControl = ctrl4)

#View summary of 6 k-fold CV  
print("Results of linear model with 6 folds:")
print(model_6_folds1)

#View final model for 6 folds
print("Final model parameters for 6 folds:")
print(model_6_folds1$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 6 folds):")
print(model_6_folds1$resample)

#View ANOVA results
print("ANOVA results (for 6 folds):")
two_way5 <- aov(model_6_folds1)
print(summary(two_way5))

#Specify the cross-validation method
ctrl5 <- trainControl(method = "cv", number = 7)

#Fit a regression model and use k-fold CV to evaluate performance
model_7_folds1<- train(qb_rank ~ amt_of_games + comp_pass_rates + passing_yds + dropped, data = df, method = "lm", trControl = ctrl5)

#View summary of 7 k-fold CV  
print("Results of linear model with 7 folds:")
print(model_7_folds1)

#View final model for 7 folds
print("Final model parameters for 7 folds:")
print(model_7_folds1$finalModel)

#View predictions for each fold
print("Predictions for each fold (for 7 folds):")
print(model_7_folds1$resample)

#View ANOVA results
print("ANOVA results (for 7 folds):")
two_way6 <- aov(model_7_folds1)
print(summary(two_way6))





