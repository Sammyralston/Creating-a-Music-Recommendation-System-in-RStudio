spotify_songs <- read.csv("spotify_songs.csv")
# Remove Duplicate Songs
table(duplicated(spotify_songs$track_name))
spotify_songs <- spotify_songs %>% distinct(track_name,
                                track_artist, .keep_all = TRUE)
spotify_songs <- spotify_songs %>% distinct(track_name,
                                .keep_all = TRUE)
spotify_sub <- spotify_songs[sample(nrow(spotify_songs), 1000), ]

install.packages("dplyr")
library(dplyr)
spotify <- spotify_sub %>%
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


# BUILDING THE RECOMMENDATION SYSTEM

# HIERARCHICAL CLUSTERING
install.packages("cluster")
install.packages("factoextra")
require(cluster) # for hierarchical clustering
require(factoextra) # For choosing number of clusters
h_single <- agnes(spotify, diss=FALSE, method="single")
h_complete <- agnes(spotify, diss=FALSE, method="complete")
h_average <- agnes(spotify, diss=FALSE, method="average")
h_ward <- agnes(spotify, diss=FALSE, method="ward")
h_single$ac;h_complete$ac; h_average$ac; h_ward$ac
# Looking at the agglomerative coefficients, Ward linkage produces 
# the best separation with an agglomerative coefficient of 0.9985052. 
# Deciding the number of clusters

plot(h_ward, which=2)

fviz_nbclust(spotify, hcut, "wss", hc.method="ward")
# difficult to see optimal number of clusters from this
fviz_nbclust(spotify, FUN=hcut,
             method="silhouette",
             hc.method="ward")
# from this, 4 clusters appears to be the most efficient
# so we will go with that
spotify$h_clust <- cutree(h_ward, k=2)
spotify %>%
  group_by(h_clust) %>%
  summarise(across(where(is.numeric), median))


# RECOMMENDATION SYSTEM
distance_matrix <- dist(spotify[-c(1,2)])
hc <- hclust(distance_matrix, method = "ward.D2")

clusters <- cutree(hc, k = 2)
spotify$cluster <- clusters


recommend_songs <- function(song_title, num_recommendations = 5) {
  song_index <- which(spotify$track_name == song_title)
  if(length(song_index) == 0) {
    return("Song title not found in the dataset.")
  }
  song_cluster <-spotify$cluster[song_index]
  similar_songs <- spotify[spotify$cluster == song_cluster, ]
  similar_songs <- similar_songs[similar_songs$track_name != song_title, ]
  if(nrow(similar_songs) > num_recommendations) {
    similar_songs <- similar_songs[sample(nrow(similar_songs),
                                          num_recommendations), ]
  }
  return(similar_songs[, c("track_name", "track_artist")])
}
  
recommend_songs("Roses - Imanbek Remix")
