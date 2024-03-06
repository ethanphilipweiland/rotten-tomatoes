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
                                          originalScore)
TMDB <- read_csv("TMDB.csv")
TMDB <- TMDB[,-c(1)] # deselecting index column

# audienceScore
movies <- movies %>% drop_na(audienceScore) # dropping movies with a missing audienceScore

# critic_score

  movie_reviews <- movie_reviews %>% filter(isTopCritic == TRUE) # only reviews from top critics

  # Manually cleaning errors from scraping algorithm
  movie_reviews$originalScore[movie_reviews$originalScore=="6"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="1"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="Big Screen Watch"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.1/2`"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="Half Price"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="1.5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3.00"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="1.0"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.25"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="Low Big Screen Watch"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="Sid Smith"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3 1/2/4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="***"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="4.7"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="*"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="**"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="****"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="non-numerical"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="0 to 4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3.5/"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="n"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.54"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="GOAT"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="+"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3 stars"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="Catch It On Cable"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="10"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="1 star"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="1.4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore==".5 stars"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.1/2"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.00"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="5.5/5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3.5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="4.0"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2 1/2"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3.0"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="5.0"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="5.5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="3.4"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="2.0"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="4.5"] <- NA
  movie_reviews$originalScore[movie_reviews$originalScore=="35/4"] <- "3.5/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="22/5"] <- "2.2/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="3-4"] <- "3/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="1-5 stars"] <- "1/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="1/54"] <- "1/4.5"
  movie_reviews$originalScore[movie_reviews$originalScore=="3/45"] <- "3/4.5"
  movie_reviews$originalScore[movie_reviews$originalScore=="9.0"] <- "9/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="7.0"] <- "7.0/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="2/4/"] <- "2/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="8.5"] <- "8.5/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="55"] <- "55/100"
  movie_reviews$originalScore[movie_reviews$originalScore=="7.1"] <- "7.1/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="8.8"] <- "8.8/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="83.10"] <- "83.10/100"
  movie_reviews$originalScore[movie_reviews$originalScore=="35"] <- "35/100"
  movie_reviews$originalScore[movie_reviews$originalScore=="80"] <- "80/100"
  movie_reviews$originalScore[movie_reviews$originalScore=="7"] <- "7/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="6.5"] <- "6.5/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="6.3"] <- "6.3/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="6.6"] <- "6.6/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="75/10"] <- "75/100"
  movie_reviews$originalScore[movie_reviews$originalScore=="9.1"] <- "9.1/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="7.5"] <- "7.5/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="A+++"] <- "A+"
  movie_reviews$originalScore[movie_reviews$originalScore=="C +"] <- "C+"
  movie_reviews$originalScore[movie_reviews$originalScore=="B +"] <- "B+"
  movie_reviews$originalScore[movie_reviews$originalScore=="90%"] <- "9/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="5 stars"] <- "5/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="70"] <- "7/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="0"] <- "0/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="7.7"] <- "7.7/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="C -"] <- "C-"
  movie_reviews$originalScore[movie_reviews$originalScore=="B-+"] <- "B"
  movie_reviews$originalScore[movie_reviews$originalScore=="F+"] <- "F"
  movie_reviews$originalScore[movie_reviews$originalScore=="B--"] <- "B-"
  movie_reviews$originalScore[movie_reviews$originalScore=="C-+"] <- "C"
  movie_reviews$originalScore[movie_reviews$originalScore=="1.5.4"] <- "1.5/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="3.5.5"] <- "3.5/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="2.5 / 4"] <- "2.5/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="2/4 stars"] <- "2/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="C--"] <- "C-"
  movie_reviews$originalScore[movie_reviews$originalScore=="75%"] <- "7.5/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="3/4f"] <- "3/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="4/5http://www.bbc.co.uk/films/2007/08/27/a_throw_o"] <- "4/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="2/4The best that can be said about the movie is th"] <- "2/4"
  movie_reviews$originalScore[movie_reviews$originalScore=="8.5 / 10"] <- "8.5/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="4/5ao"] <- "4/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="2.5/'5"] <- "2.5/5"
  movie_reviews$originalScore[movie_reviews$originalScore=="B+`"] <- "B+"
  movie_reviews$originalScore[movie_reviews$originalScore=="D?"] <- "D"
  movie_reviews$originalScore[movie_reviews$originalScore=="80%"] <- "8/10"
  movie_reviews$originalScore[movie_reviews$originalScore=="B -"] <- "B-"
  movie_reviews$originalScore[movie_reviews$originalScore=="3/35"] <- "3.3/5"
  
  movie_reviews$critic_score <- NA # creating critic_score column (to store cleaned originalScore data)

  for (i in 1:length(movie_reviews$originalScore)) { # dealing with fraction values stored as characters (ex: "3/4")
    if (grepl("\\d.*/\\d.*", movie_reviews$originalScore[i])) {
      numerator <- regmatches(movie_reviews$originalScore[i], regexec("^[^/]+", movie_reviews$originalScore[i]))[[1]] # selecting numerator
      numerator <- as.numeric(numerator) 
      denominator <- regmatches(movie_reviews$originalScore[i], regexec("/([^/]+)$", movie_reviews$originalScore[i]))[[1]][2] # selecting denominator
      denominator <- as.numeric(denominator)
      movie_reviews$critic_score[i] <- numerator / denominator
    }
  }

  for (i in 1:nrow(movie_reviews)) { # converting critic letter grades to numeric grades (ex: "B+")
    if (movie_reviews$originalScore[i] %in% c("A+", "a+")) {
      movie_reviews$critic_score[i] <- .985
    } else if (movie_reviews$originalScore[i] %in% c("A", "a")) {
      movie_reviews$critic_score[i] <- .95
    } else if (movie_reviews$originalScore[i] %in% c("A-", "a-")) {
      movie_reviews$critic_score[i] <- .915
    } else if (movie_reviews$originalScore[i] %in% c("B+", "b+")) {
      movie_reviews$critic_score[i] <- .885
    } else if (movie_reviews$originalScore[i] %in% c("B", "b")) {
      movie_reviews$critic_score[i] <- .85
    } else if (movie_reviews$originalScore[i] %in% c("B-", "b-")) {
      movie_reviews$critic_score[i] <- .815
    } else if (movie_reviews$originalScore[i] %in% c("C+", "c+")) {
      movie_reviews$critic_score[i] <- .785
    } else if (movie_reviews$originalScore[i] %in% c("C", "c")) {
      movie_reviews$critic_score[i] <- .75
    } else if (movie_reviews$originalScore[i] %in% c("C-", "c-")) {
      movie_reviews$critic_score[i] <- .715
    } else if (movie_reviews$originalScore[i] %in% c("D+", "d+")) {
      movie_reviews$critic_score[i] <- .685
    } else if (movie_reviews$originalScore[i] %in% c("D", "d")) {
      movie_reviews$critic_score[i] <- .65
    } else if (movie_reviews$originalScore[i] %in% c("D-", "d")) {
      movie_reviews$critic_score[i] <- .615
    } else if (movie_reviews$originalScore[i] %in% c("F", "f")) {
      movie_reviews$critic_score[i] <- .585
    }
  }
  
  movie_reviews <- movie_reviews %>% # averaging reviews per movie
    group_by(id) %>%
    summarise(review_count = sum(!is.na(critic_score)),
              critic_score = mean(critic_score, na.rm=T)) %>%
    filter(review_count >= 2) %>% # at least two valid isTopCritic reviews
    mutate(critic_score = critic_score*100) # converting from decimal to %

  movies <- inner_join(movies, movie_reviews, by="id") # joining to movies table (critic_score & review_count)

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
  filter(!is.na(releaseDateTheaters), # removing movies with missing values on releaseDateTheaters (key independent variable)
         releaseDateTheaters >= '2000-01-01') # movies in the 21st century
  

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
           release_date,
           box_office,
           language,
           runtime,
           review_count)

# rotten_tomatoes <- movies
# save(rotten_tomatoes, file="rotten_tomatoes.RData")
