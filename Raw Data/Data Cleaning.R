rm(list=ls())
library(tidyverse)

# Reading in data
movies <- read_csv("movies.csv")
movies <- movies %>% select(id, # selecting variables of interest
                            title,
                            audienceScore,
                            releaseDateTheaters,
                            runtimeMinutes,
                            genre,
                            originalLanguage)

movie_reviews <- read_csv("movie_reviews.csv")
movie_reviews <- movie_reviews %>% select(id, # selecting variables of interest
                                          reviewId,
                                          isTopCritic,
                                          reviewState)
TMDB <- read_csv("TMDB.csv")
TMDB <- TMDB[,-c(1)] # deselecting index column

# audienceScore
movies <- movies %>% drop_na(audienceScore) # dropping movies with a missing audienceScore

# critic_score (and review_count)
movie_reviews <- movie_reviews %>% filter(isTopCritic == TRUE) # only reviews from top critics
 
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="fresh", 1, movie_reviews$reviewState)
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="rotten", 0, movie_reviews$reviewState)
movie_reviews$reviewState <- as.numeric(movie_reviews$reviewState)
movie_reviews <- movie_reviews %>%
  group_by(id) %>%
  summarise(review_count = sum(!is.na(reviewState)),
            critic_score = sum(reviewState, na.rm=T) / review_count * 100) %>% # % of reviews that are "fresh"
  filter(review_count >= 5) # at least 5 valid isTopCritic reviews

movies <- inner_join(movies, movie_reviews, by="id") # joining to movies table

# originalLanguage (need as joining key)
  # Factor w/ 2 levels ("English", "Non-English")
movies$originalLanguage <- ifelse(movies$originalLanguage != "English" & !is.na(movies$originalLanguage), "Non-English", movies$originalLanguage)
movies$originalLanguage <- factor(movies$originalLanguage)

TMDB$TMDB.original_language <- ifelse(TMDB$TMDB.original_language != "en" & !is.na(TMDB$TMDB.original_language), "Non-English", TMDB$TMDB.original_language)
TMDB$TMDB.original_language <- ifelse(TMDB$TMDB.original_language == "en", "English", TMDB$TMDB.original_language)
TMDB$TMDB.original_language <- factor(TMDB$TMDB.original_language)

# Joining in TMDB data (to supplement missing values in releaseDateTheaters)
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
  filter(!is.na(releaseDateTheaters)) %>% # removing movies with missing values on releaseDateTheaters
  mutate(releaseDateTheaters = year(releaseDateTheaters)) # units = years

# genre indicators
movies$primarygenre <- NA # Only selecting the primary genre
for (i in 1:nrow(movies)) {
  if (!is.na(movies$genre[i])) { # for non-missing genres
    movies$primarygenre[i] <- unlist(regmatches(movies$genre[i], regexec("^[^,]+", movies$genre[i]))) # select everything before the first "," (the primary genre)
  }
}
movies <- movies %>%
  mutate(genre = primarygenre) %>%
  select(-primarygenre)

movies$Documentary <- ifelse(movies$genre=='Documentary', 1, 0)
movies$Horror <- ifelse(movies$genre=='Horror', 1, 0)
movies$Musical <- ifelse(movies$genre=='Musical', 1, 0)
movies$War <- ifelse(movies$genre=='War', 1, 0)

movies <- movies %>%
  relocate(id,
           title,
           audienceScore,
           critic_score,
           releaseDateTheaters,
           runtimeMinutes,
           genre,
           Documentary,
           Horror,
           Musical,
           War,
           originalLanguage,
           review_count)

# rotten_tomatoes <- movies
# save(rotten_tomatoes, file="rotten_tomatoes.RData")
