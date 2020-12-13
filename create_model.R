library(dplyr)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
set.seed(2311)

# url for data
myurl = "https://liangfgithub.github.io/MovieData/"
# load ratings.dat
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# Read movies.dat data, referenced from Rcode_W13_Movie_EDA.nb.html

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

# remove the missing movies(not in movies) from ratings 


# Read users.dat data, referenced from Rcode_W13_Movie_EDA.nb.html
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

# find all the genres
genres <- as.data.frame(movies$Genres, stringsAsFactors=FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:ncol(genres2))

# Find unique genres, total 18 unique genres
genre_list <- c()
genre_list <- c(genre_list, unique(genres2[,1]))
for (i in 1:ncol(genres2)) {
  genre_list <- c(genre_list, unique(genres2[,i]))
}
genre_list <- unique(genre_list)
genre_list <- genre_list[!is.na(genre_list)]

# Then create a matrix with columns representing every unique genre and rows representing all the movies, and then indicate whether a genre is present (mark as 1) or not (mark as 0) in each movie.
genre_matrix <- matrix(0, nrow(movies), length(genre_list)) # create all 0 matrix match dimensions of movies x genres
colnames(genre_matrix) <- genre_list
# based on data in genres2, construct genre_matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col <- which(colnames(genre_matrix) == genres2[i, c])
    genre_matrix[i, genmat_col] <- 1
  }
}

# get all the movieID that has been rated
movies_w_ratings = ratings %>% 
  group_by(MovieID) %>% 
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID')

# create genre matrix based on rated movies
genre_matrix_rated <- genre_matrix[-which((movies$MovieID %in% movies_w_ratings$MovieID) == FALSE),]

# save genre_matrix_rated to rds file, to be loaded by shiny app
saveRDS(genre_matrix_rated, "system1_genre.rds")

# Convert rating to a rating matrix for the recommenderlab library. Reference from https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)

# create ubcf model
rec.ubcf <- Recommender(Rmat, "UBCF",
                        parameter = list(normalize = "Z-score",
                                         method = "Cosine",
                                         nn = 25,
                                         weighted = TRUE))

# create ibcf model
rec.ibcf <- Recommender(Rmat, "IBCF",
                        parameter = list(normalize = "Z-score",
                                         method = "Cosine",
                                         k = 30))

# save ubcf model
saveRDS(rec.ubcf, "system2_ubcf.rds")

# save ibcf model
saveRDS(rec.ibcf, "system2_ibcf.rds")
