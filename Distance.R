spotify_songs <- read.csv("spotify_songs.csv")
# Remove Duplicate Songs
table(duplicated(spotify_songs$track_name))
spotify_songs <- spotify_songs %>% distinct(track_name,
                                            track_artist, .keep_all = TRUE)
spotify_songs <- spotify_songs %>% distinct(track_name,
                                            .keep_all = TRUE)

install.packages("dplyr")
library(dplyr)
spotify <- spotify_songs %>%
  select('track_name', 'track_artist', 'track_popularity', 'danceability',
         'energy','speechiness', 'acousticness','instrumentalness', 'liveness',
         'valence', 'loudness', 'tempo', 'duration_ms')

# NORMALIZING
normalize <- function(x, digits = 4) {
  normal <- (x - min(x)) / (max(x) - min(x))
  rounded <- round(normal, digits)
  return(rounded)
}

# Applying normalization
spotify$track_popularity <- normalize(spotify$track_popularity)
spotify$danceability <- normalize(spotify$danceability)
spotify$energy <- normalize(spotify$energy)
spotify$speechiness <- normalize(spotify$speechiness)
spotify$acousticness <- normalize(spotify$acousticness)
spotify$instrumentalness <- normalize(spotify$instrumentalness)
spotify$liveness <- normalize(spotify$liveness)
spotify$valence <- normalize(spotify$valence)
spotify$loudness <- normalize(spotify$loudness)
spotify$tempo <- normalize(spotify$tempo)
spotify$duration_ms <- normalize(spotify$duration_ms)

View(spotify)
summary(spotify)


# EUCLIDEAN DISTANCE
recommend_songs <- function(song_title, num_recommendations = 5) {
  song_row <- which(spotify$track_name == song_title)
  if(length(song_row ) == 0) {
    return("Song not found in the dataset.")
  }
  song_features <- as.numeric(spotify[song_row, -c(1, 2)])
  distances <- numeric(nrow(spotify))
  for (i in 1:nrow(spotify)) {
    other_song_features <- as.numeric(spotify[i, -c(1, 2)])
    distances[i] <- sqrt(sum((song_features - other_song_features)^2))
  }
  spotify$distance <- distances
  spotify_without_given_song <- spotify[spotify$track_name != song_title, ]
  recommendations <- spotify_without_given_song[order(spotify_without_given_song$distance), ][1:num_recommendations, ]
  recommended_titles <- recommendations$track_name
  recommended_artists <- recommendations$track_artist
  spotify$distance <- NULL
  paste(recommended_titles,"by", recommended_artists)
}

recommend_songs("Slide (feat. Frank Ocean & Migos)")

table(duplicated(spotify$track_name))  # To see if there are any

# MANHATTAN DISTANCE
recommend_songs <- function(song_title, num_recommendations = 5) {
  song_row <- which(spotify$track_name == song_title)
  if(length(song_row) == 0) {
    return("Song not found in the dataset.")
  }
  song_features <- as.numeric(spotify[song_row, -c(1, 2)])
  distances <- numeric(nrow(spotify))
  for (i in 1:nrow(spotify)) {
    other_song_features <- as.numeric(spotify[i, -c(1, 2)])
    distances[i] <- sum(abs(song_features - other_song_features))
  }
  spotify$distance <- distances
  spotify_without_given_song <- spotify[spotify$track_name != song_title, ]
  recommendations <- spotify_without_given_song[order(spotify_without_given_song$distance), ][1:num_recommendations, ]
  recommended_titles <- recommendations$track_name
  recommended_artists <- recommendations$track_artist
  spotify$distance <- NULL
  paste(recommended_titles, "by", recommended_artists)
}

recommend_songs("Feel Good Inc.")
