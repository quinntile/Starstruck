#Step 1--Data Collecting
movie=read.csv("/Users/wall-e/Downloads/tmdb-5000-movie-dataset/tmdb_5000_movies_c1.csv",stringsAsFactors = FALSE)
#check the first few data to make sure it has been uploaded correctly
head(movie)
#Step 2--Data Cleaning
#For numerical variables
#check if there's unsual values in movie budget
sort(movie$budget)
#Zero values are not acceptable/allowed in the budget. 
#Mark zero values for the budget as missing values and check if zeros has been cleared
movie$budget[movie$budget==0]=NA
sort(movie$budget)
#using the mean function to make sure the 'NA's exist
mean(movie$budget)
#Repeat the data cleaning steps for revenues
#check if there is unusual revenues
sort(movie$revenue)
#Zero values shown, thus apply the same method and check to make sure the success.
movie$revenue[movie$revenue==0]=NA
sort(movie$revenue)
mean(movie$revenue)
#Repeat the data cleaning steps for vote average
sort(movie$vote_average)
#Zero values shown, thus apply the same method and check to make sure the success.
movie$vote_average[movie$vote_average==0.0]=NA
sort(movie$vote_average)
mean(movie$vote_average)
#Repeat the data cleaning steps for vote average
sort(movie$vote_count)
#Zero values shown, thus apply the same method and check to make sure the success.
movie$vote_count[movie$vote_count==0]=NA
sort(movie$vote_count)
mean(movie$vote_count)
#replace missing values as its mean for numerical variables
#for runtime
summary(movie$runtime)
ave_runtime=mean(movie$runtime,na.rm = T)
movie$runtime=ifelse(is.na(movie$runtime),ave_runtime,movie$runtime)
summary(movie$runtime)
#for budget
summary(movie$budget)
ave_budget=mean(movie$budget,na.rm = T)
movie$budget=ifelse(is.na(movie$budget),ave_budget,movie$budget)
summary(movie$budget)
#for revenue
summary(movie$revenue)
ave_revenue=mean(movie$revenue,na.rm = T)
movie$revenue=ifelse(is.na(movie$revenue),ave_revenue,movie$revenue)
summary(movie$revenue)
#calculate updated profit rate
movie$Profit=movie$revenue-movie$budget
movie$Profit.Rate=movie$Profit/movie$budget
summary(movie$Profit.Rate)
#for vote average scores
summary(movie$vote_average)
ave_v_a=mean(movie$vote_average,na.rm = T)
movie$vote_average=ifelse(is.na(movie$vote_average),ave_v_a,movie$vote_average)
summary(movie$vote_average)
#for vote counts
summary(movie$vote_count)
ave_c=mean(movie$vote_count,na.rm = T)
movie$vote_count=ifelse(is.na(movie$vote_count),ave_c,movie$vote_count)
summary(movie$vote_count)
#calculate weighted vote average
movie$weighted_vote=((movie$vote_count/sum(movie$vote_count)*movie$vote_average))
head(movie$weighted_vote)
#gain some overview of each numeric data

cor.test(movie$popularity,movie$runtime)
cor.test(movie$popularity,movie$Profit.Rate)
cor.test(movie$budget,movie$revenue)

lm1<-lm(formula=revenue~vote_average+budget+popularity+runtime,data=movie)
summary(lm1)

#k-means method
library(stats)
interests<-movie[c(1,9,22,13,14,19,20,21)]
interests_z<-as.data.frame(lapply(interests,scale))
set.seed(666)
movie_clusters<-kmeans(interests_z,4)
movie_clusters$size
centers<-movie_clusters$centers
centers

#Try Neural network Method
library("neuralnet")
#preparing data
movie_train<-interests_z[1:3603,]
movie_test<-interests_z[3604:4803,]
#training a model on the data
starstruck_model<-neuralnet(revenue~budget+popularity+vote_average,data=movie_train,rep=5,stepmax=100000)
plot(starstruck_model)
#evaluating model performance
model_results<-compute(starstruck_model,movie_test[c(1,2,5)])
predicted_profit<-model_results$net.result
cor(predicted_profit,movie_test$Profit)
#improving model performance
starstruck_model2<-neuralnet(revenue~budget+popularity+vote_average,data=movie_train,hidden=2,rep=5,stepmax=1000000)
plot(starstruck_model2)
model_results2<-compute(starstruck_model2,movie_test[c(1,2,5)])
predicted_profit2<-model_results2$net.result
cor(predicted_profit2,movie_test$Profit) 

