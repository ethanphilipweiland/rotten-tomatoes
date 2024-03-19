rm(list=ls())
library(tidyverse)

# Reading in data
movies <- read_csv("movies.csv")
movies <- movies %>% select(id, #selecting variables of interest
                            title,
                            audienceScore,
                            releaseDateTheaters,
                            runtimeMinutes,
                            originalLanguage,
                            boxOffice)
movie_reviews <- read_csv("movie_reviews.csv")
movie_reviews <- movie_reviews %>% select(id, # selecting variables of interest
                                          reviewId,
                                          isTopCritic,
                                          reviewState)
TMDB <- read_csv("TMDB.csv")
TMDB <- TMDB[,-c(1)] # deselecting index column

# audienceScore
movies <- movies %>% drop_na(audienceScore) # dropping movies with a missing audienceScore

# critic_score_mean and critic_score_mode
movie_reviews <- movie_reviews %>% filter(isTopCritic == TRUE) # only reviews from top critics
 
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="fresh", 1, movie_reviews$reviewState)
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="rotten", 0, movie_reviews$reviewState)
movie_reviews$reviewState <- as.numeric(movie_reviews$reviewState)
movie_reviews <- movie_reviews %>%
  group_by(id) %>%
  summarise(review_count = sum(!is.na(reviewState)),
            critic_score = sum(reviewState, na.rm=T) / review_count * 100) %>% # % of reviews that are "fresh"
  filter(review_count >= 2) # at least two valid isTopCritic reviews
movie_reviews$critic_score_mode <- ifelse(movie_reviews$critic_score >= 50, "Fresh", "Rotten") # if >= 50% of reviews are fresh then "fresh", otherwise "rotten"

movies <- inner_join(movies, movie_reviews, by="id") # joining to movies table (review_count, critic_score, critic_score_mode)

# originalLanguage (need as joining key)
  # Factor w/ 2 levels ("English", "Non-English")
movies$originalLanguage <- ifelse(movies$originalLanguage != "English" & !is.na(movies$originalLanguage), "Non-English", movies$originalLanguage)
movies$originalLanguage <- factor(movies$originalLanguage)

TMDB$TMDB.original_language <- ifelse(TMDB$TMDB.original_language != "en" & !is.na(TMDB$TMDB.original_language), "Non-English", TMDB$TMDB.original_language)
TMDB$TMDB.original_language <- ifelse(TMDB$TMDB.original_language == "en", "English", TMDB$TMDB.original_language)
TMDB$TMDB.original_language <- factor(TMDB$TMDB.original_language)

# Joining in TMDB data (to supplement missing values in releaseDateTheaters and boxOffice)
movies <- movies %>%
  left_join(TMDB, by=join_by(title==TMDB.title, # joining on title, run time, and language
                             runtimeMinutes==TMDB.runtime,
                             originalLanguage==TMDB.original_language)) 
duplicates <- movies %>% 
  group_by(title, runtimeMinutes, originalLanguage) %>%
  count() %>%
  filter(n > 1)
duplicates <- duplicates$title # movies with multiple matches (error in TMDB data?)
movies <- movies %>% # filtering out duplicate movies
  filter(! title %in% duplicates)

# releaseDateTheaters
sum(is.na(movies$releaseDateTheaters) / length(movies$releaseDateTheaters)) # % missing before supplementing with TMDB data
movies$releaseDateTheaters <- ifelse(is.na(movies$releaseDateTheaters) & !is.na(movies$TMDB.release_date), movies$TMDB.release_date, movies$releaseDateTheaters)
movies <- movies %>% select(-TMDB.release_date)
sum(is.na(movies$releaseDateTheaters) / length(movies$releaseDateTheaters)) # % missing after supplementing with TMDB data
movies$releaseDateTheaters <- as.Date(movies$releaseDateTheaters)
movies <- movies %>% 
  filter(!is.na(releaseDateTheaters)) # removing movies with missing values on releaseDateTheaters (independent variable)

# boxOffice
movies$boxOfficegross <- movies$boxOffice # creating boxOfficegross as temporary variable to store cleaned boxOffice values
movies$boxOfficegross <- gsub("\\$", "",  movies$boxOfficegross) # removing dollar sign
for (i in 1:length(movies$boxOfficegross)) {
  if (grepl("M", movies$boxOfficegross[i])) { # for movies that grossed in the millions 
    movies$boxOfficegross[i] <- gsub("M$", "", movies$boxOfficegross[i])
    movies$boxOfficegross[i] <- as.numeric(movies$boxOfficegross[i]) * 1000000                              
  } else if (grepl("K", movies$boxOfficegross[i])) { # for movies that grossed in the thousands
    movies$boxOfficegross[i] <- gsub("K$", "", movies$boxOfficegross[i])
    movies$boxOfficegross[i] <- as.numeric(movies$boxOfficegross[i]) *1000
  }
}
movies$boxOfficegross <- as.numeric(movies$boxOfficegross) # converting to numeric

movies <- movies %>% 
  mutate(boxOffice = boxOfficegross) %>%
  select(-boxOfficegross)

sum(is.na(movies$boxOffice)) / length(movies$boxOffice) # % missing before supplementing with TMDB data
movies$boxOffice <- ifelse(is.na(movies$boxOffice) & !is.na(movies$TMDB.revenue), movies$TMDB.revenue, movies$boxOffice)
movies <- movies %>% select(-TMDB.revenue)
sum(is.na(movies$boxOffice)) / length(movies$boxOffice) # % missing after supplementing with TMDB data

movies$boxOffice <- movies$boxOffice / 1000000 # in millions of dollars

# cleaning/standardizing column names
movies <- movies %>%
  rename(audience_score = audienceScore,
         release_date = releaseDateTheaters,
         runtime = runtimeMinutes,
         language = originalLanguage,
         box_office = boxOffice)
movies <- movies %>%
  relocate(id,
           title,
           audience_score,
           critic_score,
           critic_score_mode,
           release_date,
           box_office,
           language,
           runtime,
           review_count)

# rotten_tomatoes <- movies
# save(rotten_tomatoes, file="rotten_tomatoes.RData")
