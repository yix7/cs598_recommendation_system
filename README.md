# CS598 Project 4: Movie Recommendation


This movie recommendation system consists of both genre based and collaborative filtering based approaches. User can select which approach to use from the sidebar on the left of dashboard.

- Genre-based Recommendation System:

  Recommend top10 movies based on the similarity of user preference and movie profile. For movies with the same similarity, recommend ones with higher average ratings.
  
  
- Collaborative Filtering Recommendation System:

  Recommend top10 movies based on the samples ratings gathered from user and user-based-collaborative-filtering (ubcf) model. 
<br>

__To execute the shiny app, run below commands in Rstudio:__
1. library(shiny)
2. runGitHub( "cs598_recommendation_system", "yix7", ref="main")
<br>

__Team members:__
- Yi Xu (yix7)
- Nian Jiang (nianj2)
