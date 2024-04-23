rm(list=ls()) # clearing environment

# Packages
library(tidyverse)
library(caret)

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

# critic_score (and review_count)
movie_reviews <- movie_reviews %>% filter(isTopCritic == TRUE) # only reviews from top critics
 
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="fresh", "1", movie_reviews$reviewState)
movie_reviews$reviewState <- ifelse(movie_reviews$reviewState=="rotten", "0", movie_reviews$reviewState)
movie_reviews$reviewState <- as.numeric(movie_reviews$reviewState)
movie_reviews <- movie_reviews %>%
  group_by(id) %>%
  summarise(review_count = sum(!is.na(reviewState)),
            critic_score = sum(reviewState) / review_count * 100) %>% # % of reviews that are "fresh"
  filter(review_count >= 5) # at least 5 valid isTopCritic reviews

movies <- inner_join(movies, movie_reviews, by="id") # joining to movies table

# originalLanguage (factor w/ 2 levels: "English", "Non-English")
movies$originalLanguage <- ifelse(movies$originalLanguage != "English" & !is.na(movies$originalLanguage), "Non-English", movies$originalLanguage)
movies$originalLanguage <- factor(movies$originalLanguage, levels=c("English", "Non-English"))

# releaseDateTheaters
movies <- movies %>%
  mutate(releaseDateTheaters = year(releaseDateTheaters))

# genre indicators
movies$primarygenre <- NA # Only selecting the primary genre
for (i in 1:nrow(movies)) {
  if (!is.na(movies$genre[i])) { # for non-missing genres
    movies$primarygenre[i] <- unlist(regmatches(movies$genre[i], regexec("^[^,]+", movies$genre[i]))) # select everything before the first "," (the primary genre)
  }
}

movies <- movies %>%
  select(-genre)

movies$primarygenre <- ifelse(movies$primarygenre=="Animation" |
                                movies$primarygenre=="Gay & lesbian" |
                                movies$primarygenre=="Lgbtq+" |
                                movies$primarygenre=="Music" |
                                movies$primarygenre=="Stand-up", 
                              "Other", movies$primarygenre)
movies$primarygenre <- ifelse(movies$primarygenre=="Action" | movies$primarygenre=="Adventure", "Action/Adventure", movies$primarygenre)
movies$primarygenre <- ifelse(movies$primarygenre=="Kids & family", "Kids and Family", movies$primarygenre) # so LaTex will knit properly
movies$primarygenre <- ifelse(movies$primarygenre=="Mystery & thriller", "Mystery and Thriller", movies$primarygenre) # so LaTex will knit properly
movies$primarygenre <- factor(movies$primarygenre)
movies$primarygenre <- relevel(movies$primarygenre, ref="Drama") # baseline is modal primary genre
movies <- movies %>%
  drop_na(primarygenre) # dropping movies w/ missing genre (can't impute multilevel factors)
  
# Imputation
set.seed(1895)
movies$originalLanguage<- as.numeric(movies$originalLanguage) - 1 # converting to numeric so imputation calculates probability
impute <- preProcess(select(movies, -c(id,
                                       title,
                                       primarygenre)), method=c("bagImpute"))
predictors <- predict(impute, select(movies, -c(id,
                                                title,
                                                primarygenre)))
predictors <- predictors %>%
  mutate(releaseDateTheaters = round(releaseDateTheaters)) # only whole years 
predictors$originalLanguage <- ifelse(predictors$originalLanguage >= 0.5, "English", "Non-English") # converting back to factor based on probability

movies <- predictors %>%
  select(-audienceScore) %>% # removing imputed dependent variable
  cbind(movies$id, # joining back in un-imputed columns
        movies$title,
        movies$primarygenre,
        movies$audienceScore) %>%
  rename(id="movies$id",
         title="movies$title",
         primarygenre="movies$primarygenre",
         audienceScore="movies$audienceScore") %>%
  relocate(id,
           title,
           audienceScore,
           critic_score,
           releaseDateTheaters,
           runtimeMinutes,
           primarygenre,
           originalLanguage,
           review_count)

# decade
movies$decade <- as.character(NA)
for (i in 1:nrow(movies)) {
  if (movies$releaseDateTheaters[i] < 1980) {
    movies$decade[i] <- "Pre-1980"
  } else if (movies$releaseDateTheaters[i] >= 1980 & movies$releaseDateTheaters[i] < 1990) {
    movies$decade[i] <- "1980s"
  } else if (movies$releaseDateTheaters[i] >= 1990 & movies$releaseDateTheaters[i] < 2000) {
    movies$decade[i] <- "1990s" 
  } else if (movies$releaseDateTheaters[i] >= 2000 & movies$releaseDateTheaters[i] < 2010) {
    movies$decade[i] <- "2000s"
  } else if (movies$releaseDateTheaters[i] >= 2010 & movies$releaseDateTheaters[i] < 2020) {
    movies$decade[i] <- "2010s"
  } else {
    movies$decade[i] <- "2020s"
  }
}
movies$decade <- factor(movies$decade)
movies$decade <- relevel(movies$decade, ref="2010s")

movies <- movies %>% drop_na(audienceScore) # dropping movies with a missing audienceScore (dependent variable)
movies <- movies %>% distinct()

rotten_tomatoes <- movies
save(rotten_tomatoes, file="rotten_tomatoes.RData")
