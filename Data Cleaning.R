rm(list=ls())
library(tidyverse)

# Reading in data
movies <- read_csv("movies.csv")
movie_reviews <- read_csv("movie_reviews.csv")

# Cleaning movie_reviews table (data on individual reviews) 

  # Manually cleaning data entry errors
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
    select(-review_count)
  
  # movie_review variables not included:
  # - reviewId = primary key of table for individual reviews
  # - creationDate
  # - criticName
  # - reviewState = rotten/fresh, captured better by critic_score
  # - publicationName
  # - scoreSentiment (of text review)
  # - reviewUrl
  
# Cleaning movies table
  
  movies <- movies %>%
    drop_na(audienceScore) %>% # dropping movies with NA for audienceScore
    select(-c(tomatoMeter, # removing tomatoMeter (using critic_score instead)
              ratingContents, # removing ratingContents (not important)
              director, # removing director (too many factor levels)
              writer, # removing writer (too many factor levels)
              soundMix, # removing soundMix (not important)
              releaseDateStreaming, # using releaseDateTheaters instead (critics more likely to review films in theaters)
              distributor)) # removing distributor (too many factor levels)
  
  # Genre variable
  movies$primarygenre <- NA # Only selecting the primary genre
  for (i in 1:nrow(movies)) {
    if (!is.na(movies$genre[i])) { # for non-missing genres
      movies$primarygenre[i] <- unlist(regmatches(movies$genre[i], regexec("^[^,]+", movies$genre[i]))) # select everything before the first "," (the primary genre)
    }
  }
  movies <- movies %>%
    mutate(genre = primarygenre) %>%
    select(-primarygenre)
    
    # Manually cleaning genre categories
    movies$genre[movies$genre=="Stand-up"] <- "Comedy"
    movies$genre[movies$genre=="Music"] <- "Musical"
    
      # Adding categories with only a few films to "Other"
      movies$genre[movies$genre=="Gay & lesbian"] <- "Other" 
      movies$genre[movies$genre=="Lgbtq+"] <- "Other"
      movies$genre[movies$genre=="Variety"] <- "Other"
      movies$genre[movies$genre=="Faith & spirituality"] <- "Other"
      movies$genre[movies$genre=="Anime"] <- "Other"
      movies$genre[movies$genre=="Sports"] <- "Other"
      movies$genre[movies$genre=="Animation"] <- "Other"
  
  # Creating boxOfficegross column (cleaned boxOffice colulmn)
  movies$boxOfficegross <- movies$boxOffice
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
  
  # Rating variable
  movies$rating[movies$rating=="TVMA"] <- "R"
  movies$rating[movies$rating=="TV14"] <- "PG-13"
  movies$rating[movies$rating=="TVPG"] <- "PG"
  movies$rating[movies$rating=="TVY7"] <- "PG" # no films have a "G" rating
  
# Joining tables
rotten_tomatoes <- inner_join(movie_reviews, movies, by="id")

# Summary of data
summary(rotten_tomatoes)
nrow(rotten_tomatoes)
table(rotten_tomatoes$rating)
table(rotten_tomatoes$genre)
table(rotten_tomatoes$originalLanguage)

# Calculating % missing
percent_missing <- function(x) {
  missing_count <- sum(is.na(x))
  output <- missing_count / length(x) * 100
  return(round(output, 2))
}
missing_data_table <- data.frame(Variable = names(rotten_tomatoes),
                                 "Percent Missing" = apply(X=rotten_tomatoes, MARGIN=2, FUN=percent_missing))
missing_data_table 

# Saving rotten_tomatoes as an .RData file due to long code run time
# save(rotten_tomatoes, file="rotten_tomatoes.RData")
