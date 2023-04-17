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
  data_matrix <- xtabs(rating ~ user_id + item_id, data = data)
  relevant_users <- names(which(data_matrix[, as.character(item_id)]>0))
  
  if(length(relevant_users) > 0){
    similarities <- cor(t(data_matrix))[as.character(user_id), relevant_users]
  }
  else{return(sum(data_matrix[as.character(user_id),][data_matrix[as.character(user_id),] > 0]) / length(data_matrix[as.character(user_id),][data_matrix[as.character(user_id),] > 0]))}
  if(length(similarities) < k){
    similarities_ordered <- (similarities[order(similarities,decreasing= T)])[1:length(similarities)]
  }
  else{similarities_ordered <- (similarities[order(similarities,decreasing= T)])[1:k]}
  
  if(length(similarities) > 1){
    k_relevant_users <- names(similarities_ordered)
    relevant_means <- rowMeans(data_matrix[k_relevant_users,])
  }
  else{
    k_relevant_users <- relevant_users
    relevant_means <- mean(data_matrix[k_relevant_users,][data_matrix[k_relevant_users,] > 0])
  }
  
  
  # weight the ratings of the k most similar users
  relevant_ratings <- data_matrix[k_relevant_users, as.character(item_id)]
  prediction <- mean(data_matrix[as.character(user_id),]) + (similarities_ordered %*% (relevant_ratings - relevant_means))/sum(abs(similarities_ordered))
  
  if(prediction < 0){
    return(0)
  }
  else{return(prediction)}
}


adjusted_cosine_similarity <- function(data_matrix, item_id) {
  # Calculate the mean ratings for each user
  user_means <- rowMeans(data_matrix, na.rm = TRUE)
  
  # Center the ratings for each item by subtracting the mean rating for each user
  centered_data_mat <- t(scale(t(data_matrix), scale = FALSE))
  
  # Compute the adjusted cosine similarity between item_id and every other item
  similarities <- numeric(length = ncol(data_matrix))
  for (i in 1:ncol(data_matrix)) {
    if (i != item_id) {
      numerator <- sum(centered_data_mat[, as.character(item_id)] * centered_data_mat[, i], na.rm = TRUE)
      denominator <- sqrt(sum(centered_data_mat[, as.character(item_id)]^2, na.rm = TRUE)) * sqrt(sum(centered_data_mat[, i]^2, na.rm = TRUE))
      similarities[i] <- numerator / denominator
    }
    else{
      similarities[i] <- 1
    }
  }
  
  # Return the similarities
  names(similarities) <- colnames(data_matrix)
  return(similarities)
}

IBCF <- function(data, user_id, item_id, k = 50){
  # returns a prediction for a user item pair using IBCF
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
  
  # format data and find relevant items
  data_matrix <- xtabs(rating ~ user_id + item_id, data = data)
  relevant_items <- names(which(data_matrix[as.character(user_id), ]>0))
  
  # find similarities and filter out k most similar items
  if(length(relevant_items) > 0){
    similarities <- adjusted_cosine_similarity(data_matrix, item_id)[relevant_items]
  }
  else{return(sum(data_matrix[,as.character(item_id)][data_matrix[,as.character(item_id)] > 0]) / length(data_matrix[,as.character(item_id)][data_matrix[,as.character(item_id)] > 0]))}
  if(length(similarities) < k){
    similarities_ordered <- (similarities[order(similarities,decreasing= T)])[1:length(similarities)]
  }
  else{similarities_ordered <- (similarities[order(similarities,decreasing= T)])[1:k]}
  
  if(length(similarities) > 1){
    k_relevant_items <- names(similarities_ordered)
  }
  else{
    k_relevant_items <- relevant_items
  }
  
  prediction <- sum(similarities_ordered * data_matrix[as.character(user_id), k_relevant_items]) / sum(abs(similarities_ordered))
  
  if(prediction < 0){
    return(0)
  }
  else{return(prediction)}
}


#implement ProfileMAE algorithm
ProfileError <- function(data, error = c('MAE', 'MSE'), prediction = c('UBCF', 'IBCF', 'Naive_Bayes')){
  # data needs to be a list with 4 columns and the columnnames user_id, item_id, rating and timestamp
  # these columns need to have entries of type integer, integer, integer and double respectively
  # one can choose the profile error measure, which can be set to either MAE or MSE (default being MAE)

  #check if the required packages are loaded and load them if not
  install.packages(setdiff(c('tidyverse', 'naivebayes'), rownames(installed.packages())))


  #check if the input arguments are valid
  stopifnot("Input data with incorrect column numbers: need 4" = ncol(data) == 4)
  m <- match.arg(colnames(data), c('user_id', 'item_id', 'rating', 'timestamp'), several.ok = TRUE)
  stopifnot("Columnnames are not fully matching  c('user_id', 'item_id', 'rating', 'timestamp')" = length(m) == 4)
  t <- match.arg(sapply(ml_data_trunc, typeof), c('integer', 'integer', 'integer', 'double'), several.ok = TRUE)
  stopifnot("Columntypes are not fully matching  c('integer', 'integer', 'integer', 'double')" = length(t) == 4)
  match.arg(error)
  match.arg(prediction)


  user_ids <- unique(data$user_id) #retrieve unique user ids
  c <- numeric(length(user_ids)) #initialize vector to count the number of ratings per user
  eps <- numeric(length(user_ids)) #initialize vector to sum error terms for each user
  
  for (u in user_ids) {
    p <- data[data$user_id == u,]
    ps <- p[order(p$timestamp),]
    u_index <- match(u, user_ids)
    

    for (j in 1:length(ps$user_id)) {
      e <- ps[j,]
      data_j <- data[data$timestamp < e$timestamp,][,1:3]

      # predict the rating via average rating of the item in question when the user u has no previous ratings
      if (j == 1 | e$item_id %in% data_j$item_id == FALSE | ps[1,]$timestamp == ps[j,]$timestamp){
        q <- colMeans(data_j[data_j$item_id == e$item_id,1:3])[3]
        if(is.na(q)){
          q <- 3
        }
      }
      
      else{
        if(prediction == 'UBCF'){
          q <- UBCF(data_j, u, e$item_id)
        }
        if(prediction == 'IBCF'){
          q <- IBCF(data_j, u, e$item_id)
        }
        if(prediction == 'Naive_Bayes'){
          #q <- insert prediction here
          q <- 0
        } 
      }

      if(error == 'MAE'){
        eps_j <- abs(e$rating - q)
      } 
      else{
        eps_j <- (e$rating - q)^2
      }

      
      eps[u_index] <- eps[u_index] + eps_j
      c[u_index] <- c[u_index] + 1
      }
    }
  res <- cbind(user_ids, eps/c)
  if(error == 'MAE'){
    colnames(res) <- c('user_id', 'ProfileMAE')
  } 
  else{
    colnames(res) <- c('user_id', 'ProfileMSE')
  }

  return(res)
}
