# CS598 Project 4: Movie Recommendation


This movie recommendation system consists of both genre based and collaborative filtering based approaches. User can select which approach to use from the sidebar on the left of dashboard. __Note: it may take some time for data to be loaded for the first time launching the app__

- Genre-based Recommendation System:

  Recommend top10 movies based on the similarity of user preference and movie profile. For movies with the same similarity, recommend ones with higher average ratings.
  
  
- Collaborative Filtering Recommendation System:

  Recommend top10 movies based on the samples ratings gathered from user and user-based-collaborative-filtering (ubcf) model. 
<br>
__Please note: models are created by running create_model.R, then the shiny app can just load the created model system1_genre.rds for genre based recommendation and system2_ubcf.rds for ubcf based recommendation.__
<br>

__To execute the shiny app, run below commands in Rstudio:__
1. library(shiny)
2. runGitHub( "cs598_recommendation_system", "yix7", ref="main")
<br>

__Team members:__
- Yi Xu (yix7)
- Nian Jiang (nianj2)
<br>

__libraries:__
- shiny
- shinydashboard
- ShinyRatingInput
- shinyjs
- recommenderlab
- data.table
- Matrix
- slam
- dplyr
- DT
- reshape2
