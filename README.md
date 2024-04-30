# The Golden Age of Critics? Examining the Association Between Critic Scores and Audience Scores Over Time on Rotten Tomatoes
This project uses Rotten Tomatoes data to determine the association between public opinion ("Audience Score") and critic opinion ("Tomatometer") for thousands of films. Techniques use include data visualization, numerical summaries, hypothesis testing, and statistical modeling. There are three key takeaways:

1. The opinions of critics and the general public on movies has a moderately strong linear relationship
2. This relationship did not meaningfully change over time until 2020
3. After 2020 the relationship between critic scores and audience scores weakened

This project was completed for STAT-S 690: Statistical Consulting taught by Dr. Sheya at Indiana University Bloomington in Spring 2024

## Analytical Report
The file "Report.pdf" contains a write-up of the motivation, data, steps, and results of this analysis.

## Necessary Software
You will need the following software and packages to run the code files and reproduce the analysis.

Necessary software: `R`, `Quarto`

Necessary `R` packages: `tidyverse`, `caret`, `stargazer`, `broom`

## File Descriptions
    1. /Data Cleaning/Cleaning.R : Code for cleaning raw data and imputing missing values
    2. /Data Cleaning/movie_reviews.csv : Uncleaned data file of individual critic reviews
    3. /Data Cleaning/movies.csv : Uncleaned data file of individual movies
    4. Presentation.pdf : Slide deck detailing the motivation, data, steps, and results of analysis
    5. Presentation.qmd : Dynamic document used to produce Presentation.pdf (includes code)
    6. Report.pdf : Write-up of the motivation, data, steps, and results of analysis
    7. Report.qmd : Dynamic document used to produce Report.pdf (includes code)
    8. rotten_tomatoes.RData : Cleaned Rotten Tomatoes data used for analysis

## Installation and File Execution
To begin, download the /Data Cleaning folder. Open `R` and set ~/Data Cleaning as your working directory using `setwd()`. `R` script files are executable once a working directory to the folder containing the data is set. Running Cleaning.R will produce rotten_tomatoes.RData. Download Presentation.qmd and Report.qmd and place these files in your working directory. Running Presentation.qmd or Report.qmd will generate a slide deck or write-up (respectively) with a detailed walk-through of this project.

## License
See LICENSE for licensing details for this repository. 
