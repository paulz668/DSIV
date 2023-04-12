library(tidyverse)
library(lubridate)
library(naivebayes)

# set work directory
setwd('C:/Users/paulz/Documents/UNI/BBE/6. Semester/DS IV/DSIV') 

# clean the data
# dont forget to cite this paper for movielens dataset: 
# F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets:
# History and Context. ACM Transactions on Interactive Intelligent
# Systems (TiiS) 5, 4, Article 19 (December 2015), 19 pages.
# DOI=http://dx.doi.org/10.1145/2827872
ml_data <- read.delim('C:/Users/paulz/Documents/UNI/BBE/6. Semester/DS IV/DSIV/ml-100k/u.data', header=FALSE)
#set column names
colnames(ml_data) <- c('user_id', 'item_id', 'rating', 'timestamp') 
#set timestamps to human readable format
ml_data[4] <- as.POSIXct("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S") + unlist(ml_data[4]) 

#replicating the truncation of the dataset described in the paper
cutoff <- as.POSIXct("2000-12-31 00:00:00", format = "%Y-%m-%d %H:%M:%S") 
ml_data_trunc <- ml_data[ml_data$timestamp < cutoff,]

#initiate the lower and upper bounds of reviews per profile from the paper
lower_bound <- 15  
upper_bound <- 650 
ml_data_trunc <- ml_data_trunc %>% group_by(user_id) %>% filter(n() > lower_bound)
ml_data_trunc <- ml_data_trunc %>% filter(n() < upper_bound)


#implement UBCF and IBCF to get predictions for specific user-item pairs
UBCF <- function(data, user_id, item_id, k = 50){
  # returns a prediction for a user item pair using UBCF
  # data needs to be a list with 3 columns named user_id, item_id and rating
  # these columns all need to have entries of type integer
  
  # try to coerce user_id, item_id and k to integers
  user_id <- as.integer(user_id)
  item_id <- as.integer(item_id)
  k <- as.integer(k)
  
  # check if the input arguments are valid
  m <- match.arg(colnames(data), c('user_id', 'item_id', 'rating'), several.ok = TRUE)
  stopifnot("Columnnames are not fully matching  c('user_id', 'item_id', 'rating')" = length(m) == 3)
  t <- match.arg(sapply(ml_data_trunc, typeof), c('integer', 'integer', 'integer'), several.ok = TRUE)
  stopifnot("Columntypes are not fully matching  c('integer', 'integer', 'integer')" = length(t) == 3)
  stopifnot("user_id argument is not of type 'integer'" = typeof(user_id) == 'integer')
  stopifnot("item_id argument is not of type 'integer'" = typeof(item_id) == 'integer')
  stopifnot("k argument is not of type 'integer'" = typeof(k) == 'integer')
  
  # format data and find relevant users
  data_matrix <- xtabs(rating ~ user_id + item_id, data = ml_data_trunc)
  relevant_users <- which(data_matrix[, item_id]>0)
  
  #find similarities and filter out the k most similar users
  similarities <- cor(t(data_matrix))[user_id, relevant_users]
  similarities_ordered <- (similarities[order(similarities,decreasing= T)])[1:k]
  k_relevant_users <- names(similarities_ordered)
  
  # weight the ratings of the k most similar users
  relevant_ratings <- data_matrix[k_relevant_users, item_id]
  relevant_means <- rowMeans(data_matrix[k_relevant_users,])
  prediction <- mean(data_matrix[user_id,]) + (similarities_ordered %*% (relevant_ratings - relevant_means))/sum(abs(similarities_ordered))
  
  return(prediction)
}


#implement ProfileMAE algorithm
ProfileError <- function(data, error = c('MAE', 'MSE'), prediction = c('UBCF', 'IBCF', 'Naive_Bayes'){
  # data needs to be a list with 4 columns and the columnnames user_id, item_id, rating and timestamp
  # these columns need to have entries of type integer, integer, integer and double respectively
  # one can choose the profile error measure, which can be set to either MAE or MSE (default being MAE)

  #check if the required packages are loaded and load them if not
  install.packages(setdiff(c('recommenderlab', 'naivebayes'), rownames(installed.packages())))


  #check if the input arguments are valid
  stopifnot("Input data with incorrect column numbers: need 4" = ncol(data) == 4)
  m <- match.arg(colnames(data), c('user_id', 'item_id', 'rating', 'timestamp'), several.ok = TRUE)
  stopifnot("Columnnames are not fully matching  c('user_id', 'item_id', 'rating', 'timestamp')" = length(m) == 4)
  t <- match.arg(sapply(ml_data_trunc, typeof), c('integer', 'integer', 'integer', 'double'), several.ok = TRUE)
  stopifnot("Columntypes are not fully matching  c('integer', 'integer', 'integer', 'double')" = length(t) == 4)
  match.arg(error)
  match.arg(prediction)


  c <- c() #initialize vector to count the number of ratings per user
  eps <- c() #initialize vector to sum error terms for each user
  user_ids <- unique(data$user_id) #retrieve unique user ids

  for (u in user_ids) {
    p <- data[data$user_id == u]
    ps <- p[order(p$timestamp),]

    for (j in 1:length(pull(ps$user_id))) {
      e <- ps[j,]

      data_j <- data[data$timestamp < e$timestamp,][,1:3]
      if(prediction == 'UBCF'){
        q <- UBCF(data_j, u, e$item_id)
      }if(prediction == 'IBCF'){
        #q <- insert prediction here
      }else{
        #q <- insert prediction here
      }

      if(error == 'MAE'){
        eps_j <- abs(e$rating - q)
      } else{
        eps_j <- (e$rating - q)^2
      }

      if(length(eps) < j){
        eps <- append(eps, eps_j)
        c <- append(c, 1)
      } else{
        eps[j] <- eps[j] + eps_j
        c[j] <- c[j] + 1
      }
    }
  }
  res <- cbind(user_ids, eps/c)
  if(error == 'MAE'){
    colnames(res) <- c('user_id', 'ProfileMAE')
  } else{
    colnames(res) <- c('user_id', 'ProfileMSE')
  }

  return(res)
}

bayes_data <- ml_data_trunc
bayes_data$user_id <- as.factor(bayes_data$user_id)
bayes_data$item_id <- as.factor(bayes_data$item_id)
bayes_data$rating <- as.factor(bayes_data$rating) 
