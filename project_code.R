library(tidyverse)
library(lubridate)

setwd('C:/Users/paulz/Documents/UNI/BBE/6. Semester/DS IV/DSIV') #first we set our work directory

#import the u.data dataset from MovieLens manually as it a .data file and i have no idea
#how to import that
# dont forget to cite this paper for movielens dataset: 
# F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets:
# History and Context. ACM Transactions on Interactive Intelligent
# Systems (TiiS) 5, 4, Article 19 (December 2015), 19 pages.
# DOI=http://dx.doi.org/10.1145/2827872
ml_data <- u
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

