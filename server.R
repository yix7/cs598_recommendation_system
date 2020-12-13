## server.R


library(Matrix)
library(recommenderlab)
library(slam)
library(data.table)
library(dplyr)
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

# set wd to directory of current script
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1 & grepl("select", x[[1]], ignore.case = FALSE) , x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

get_user_genre = function(value_list) {
  dat <- data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1 & grepl("genre", x[[1]], ignore.case = FALSE), x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat <- na.omit(dat)
  dat <- dat %>% mutate(dat, Rating=ifelse(Rating, 1, 0))
  dat <- dat[order(as.numeric(MovieID))]
  dat$Rating
  
}

# calculate jaccard similarity
jaccard_sim <- function(a,b) {
  # find count of both are 1
  intersect_sum <- sum(a*b)
  # find union and need to remove duplications
  union_sum <- sum(a+b)-intersect_sum
  intersect_sum/union_sum
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
# get all genres
genres <- as.data.frame(movies$Genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:ncol(genres2))
genre_list <- c()
genre_list <- c(genre_list, unique(genres2[,1]))
for (i in 1:ncol(genres2)) {
  genre_list <- c(genre_list, unique(genres2[,i]))
}
genre_list <- unique(genre_list)
genre_list <- genre_list[!is.na(genre_list)] # unique genre list

# find one movie picture for each genre
genre_movieid = c()
for (i in 1:length(genre_list)) {
  id_genre_tmp <- movies$MovieID[which(grepl(genre_list[i], movies$Genres, ignore.case = TRUE) == TRUE)][1]
  genre_movieid <- c(genre_movieid, id_genre_tmp)
}

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
# load ratings.dat
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
i = paste0('u', ratings$UserID)
new_i = "u9999"
j = paste0('m', ratings$MovieID)
x = ratings$Rating

# for those movies that are not rated, remove them from movies
missed_movies <- which(unique(movies$MovieID) %in% unique(ratings$MovieID) == FALSE)
movies <- movies[-missed_movies, ]

# get movie with ratings
movies_w_ratings = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')

# load model rds
# model.ibcf <- readRDS("system2_ibcf.rds")
model.ubcf <- readRDS("system2_ubcf.rds")
model.genre <- readRDS("system1_genre.rds")

shinyServer(function(input, output, session) {

  # show the genres to be selected
  output$genres <- renderUI({
    num_rows <- 3
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[genre_movieid[(i - 1) * num_movies + j]], height = 150)),
                 div(style = "text-align:center", checkboxInput(paste0("genre_", movies$MovieID[(i - 1) * num_movies + j]), label=strong(genre_list[(i - 1) * num_movies + j]), value = FALSE))
                 # div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", genre_list[(i - 1) * num_movies + j]), label = "", dataStop = 5))
                 )) #00c0ef
      })))
    })
  })
  
  # observeEvent(input$clearGenre, {
  #   num_rows <- 3
  #   num_movies <- 6 # movies per row
  #   
  #   for(i in 1:num_rows) {
  #     for(j in 1:num_movies) {
  #       updateCheckboxInput(session, paste0("genre_", movies$MovieID[(i - 1) * num_movies + j]), value = FALSE)
  #     }
  #   }
  # })
  # 
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # Calculate recommendations when the genre sbumbutton is clicked
  df_genre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_genre(value_list)
      # rated_count <- length(user_ratings$Rating)
      if (1 %in% user_ratings == FALSE) {
        return(data.table())
      }
      
      jaccard_sim_result <- apply(model.genre, 1, jaccard_sim, user_ratings)
      
      # add similarity result to movie_w_ratings
      movies_w_ratings_tmp <- movies_w_ratings %>%
        mutate(tmp_user_sim = jaccard_sim_result)
      
      # adopt scheme I here, for movies with same similarity, select the ones with higer average ratings.
      movies_w_ratings_tmp <- arrange(movies_w_ratings_tmp, desc(tmp_user_sim), desc(ave_ratings))
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies_w_ratings_tmp$MovieID[1:10], 
                                  Title = movies_w_ratings_tmp$Title[1:10], 
                                  Url =  movies_w_ratings_tmp$image_url[1:10])
      
    }) # still busy
    
  }) # clicked on button
  
  
  
  # Calculate recommendations when the cf sbumbutton is clicked
  df_cf <- eventReactive(input$btnCf, {
    withBusyIndicatorServer("btnCf", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      rated_count <- length(user_ratings$Rating)
      if (rated_count == 0) 
        {return(data.table())}
      
      # update all the ratings
      i <- c(i, rep(new_i, rated_count))
      j <- c(j, paste0('m', user_ratings$MovieID))
      x <- c(x, user_ratings$Rating)
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      Rmat = new('realRatingMatrix', data = Rmat)
      Rmat_newuser = Rmat[dim(Rmat)[1],]
      
      # predict ratings for other movies for this user
      p1 <- predict(model.ubcf, Rmat_newuser, type="ratings")
      pred_results <- as(p1, "matrix")[, which(!is.na(as(p1, "matrix")))]
      # sort descending, and get top10
      user_results <- sort(pred_results, decreasing = TRUE)[1:10]
      user_predicted_ids <- as.numeric(gsub("m", "", names(user_results)))
      user_predicted_idx <- match(user_predicted_ids, movies$MovieID)
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_idx], 
                                  Title = movies$Title[user_predicted_idx], 
                                  Url = movies$image_url[user_predicted_idx],
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the genre based recommendations
  output$genre_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_genre()
    
    if (nrow(recom_result) == 0) {
      h3("Please select at least one genre you like.")
    } else {
    
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", 
                  a(img(src = recom_result$Url[(i - 1) * num_movies + j], height = 150))
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(recom_result$Title[(i - 1) * num_movies + j])
              )
              
          )        
        }))) # columns
      }) # rows
    }
    
  }) # renderUI function
  
  # display the cf based recommendations
  output$cf_results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df_cf()
    
    if (nrow(recom_result) == 0) {
      h3("Please rate at least one movie.")
    } else {
      
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
              div(style = "text-align:center", 
                  a(img(src = recom_result$Url[(i - 1) * num_movies + j], height = 150))
              ),
              div(style="text-align:center; font-size: 100%", 
                  strong(recom_result$Title[(i - 1) * num_movies + j])
              )
              
          )        
        }))) # columns
      }) # rows
    }
  }) # renderUI function
  
}) # server function
