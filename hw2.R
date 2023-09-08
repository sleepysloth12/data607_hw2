library(DBI)
library(RMySQL)

#connect to the database

connect = dbConnect(RMySQL::MySQL(), 
                 dbname = "607_hw2", 
                 host = "localhost", 
                 user = "user", 
                 password = "password")

#getting table using sql query

query = "SELECT * FROM movie_rating"

#now we have our Data frame

movie_ratings = dbGetQuery(connect, query)

head(movie_ratings)

#Lets make a data frame that shows the median rating per movie

#split function to extract the ratings of each movie 

list_of_ratings = split(movie_ratings$rating, movie_ratings$movie_title)

list_of_ratings

#median rating for each movie

median_ratings = aggregate(rating ~ movie_title, data = movie_ratings, FUN = median)
median_ratings

#Barplot showing median rating using ggplot2

library(ggplot2)

# Create the bar plot
ggplot(median_ratings, aes(x = movie_title, y = rating)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Median Ratings by Movie",
       x = "Movie Title",
       y = "Median Rating")

#will now add genre to each movie

genre_key = c("fantasy", "interactive", "sci_fi", "comedy", "drama","comedy")
names(genre_key) = median_ratings$movie_title

# Convert to data frame for merging

genre_df = data.frame(movie_title = names(genre_key), genre = genre_key)

# Merge with the movie_ratings data frame

movie_ratings = merge(movie_ratings, genre_df, by = "movie_title")

head(movie_ratings)
unique(movie_ratings$movie_title)
#now ill use ggplot to plot the rating distribution of each genre

ggplot(movie_ratings, aes(x = genre, y = rating, color = ind_name)) +
  geom_jitter(width = 0.3, height = 0) +
  labs(title = "Distribution of Ratings by Genre",
       x = "Genre",
       y = "Rating") +
  theme(legend.position = "bottom")

#going to use dplyr to
#collect/aggregate every person's movie ratings and genres

library(dplyr)

#Take movie ratings data frame
#pipe it through group_by, which will group it by each individual person and genre
#pipe the grouped data into summarise which will spit out the average rating per genre
#ungroup so that it is separated by each individual person

aggrigate_ratings = movie_ratings %>%
  group_by(ind_name, genre) %>%
  summarise(avg_rating=mean(rating)) %>%
  ungroup()
  
aggrigate_ratings

#we can use this in the future to calculate stuff for each individual person

#heat map for each person and their avg rating per genre

ggplot(aggrigate_ratings, aes(x = ind_name, y = genre)) +
  geom_tile(aes(fill = avg_rating), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Average Ratings Heatmap",
       x = "Individual",
       y = "Genre")


