data <- read.csv("spotify.csv", header = TRUE)

# (b)
plot_corr <- function(x, y) {
  plot(0, 0,
       main = "",
       xlab = "",
       ylab = "",
       xaxt = "n",
       yaxt = "n",
       col = "white")
  text(x = 0, y = 0,
       labels = paste(round(cor(x, y), 4)),
       col = "firebrick",
       cex = 2)
}

par(mfcol = c(3, 3), mar = c(1, 4, 4, 1))

plot_corr(data$valence, data$bpm)
title(ylab = "bpm", font.lab = 2)
plot_corr(data$valence, data$energy)
title(ylab = "energy", font.lab = 2)
plot_corr(data$danceability, data$valence)
title(ylab = "danceability", font.lab = 2)
plot_corr(data$valence, data$loudness)
title(ylab = "loudness", font.lab = 2)
plot_corr(data$valence, data$liveness)
title(ylab = "liveness", font.lab = 2)
plot_corr(data$valence, data$duration)
title(ylab = "duration", font.lab = 2)
plot_corr(data$valence, data$acousticness)
title(ylab = "acousticness", font.lab = 2)
plot_corr(data$valence, data$speechiness)
title(ylab = "speechiness", font.lab = 2)
plot_corr(data$popularity, data$valence)
title(ylab = "popularity", font.lab = 2)

# Most Strongly Related variates with valences
#- Loudness: 0.3048 (positive)
#- Acousticness: -0.2264 (negative)
#- Energy: 0.3785 (positive)
#- Danceability: 0.4128 (positive)
#- Duration: -0.2283 (negative)

#corr_all <- cor(data[, 5:14])[1:9, 10]
#print(corr_all)

# (c)
order_valence <- order(data$valence, decreasing=TRUE)
for (i in 1:5) {
  print(data$valence[order_valence[i]])
}

# (d)
highest_danceability <- max(data$danceability)
lowest_danceability <- min(data$danceability)
print(highest_danceability)
print(lowest_danceability)

# (e)
par(mfcol = c(1,1))
Delta <- abs(data$acousticness - mean(data$acousticness))/(length(data$acousticness)-1)
plot(Delta, 
     main ="Influence of songs on average acousticness",
     ylab = bquote(Delta),
     pch = 16,
     col = adjustcolor("blue", alpha.f=0.5))
# Maximum influence on acousticness
kableExtra(kable(data[c(which.max(Delta), which.max(data$acousticness)), c("title", "artist", "acousticness")]))

# (f)
par(mfcol = c(1,1))
Delta <- abs(data$speechiness - mean(data$speechiness))/(length(data$speechiness)-1)
plot(Delta, 
     main ="Influence of songs on average speechiness",
     ylab = bquote(Delta),
     pch = 16,
     col = adjustcolor("blue", alpha.f=0.5))
# Maximum influence on speechiness
kableExtra(kable(data[c(which.max(Delta), which.max(data$speechiness)), c("title", "artist", "speechiness")]))

# (g)

# (h)
# Which artist appeared at least 5 times?
top_artists <- names(which(table(data$artist) == 4))
num_appears <- rep(0, length(top_artists))
artist_pop <- rep(0, length(top_artists))
for (i in 1:length(top_artists)) {
  artist_match <- data$artist == top_artists[i]
  song_pop <- data[artist_match, ]$valence
  num_appears[i] <- length(song_pop)
  artist_pop[i] <- round(mean(song_pop))
}
kable_styling(kable(data.frame(artist = top_artists,
                               num_hits = num_appears,
                               avg_pop = artist_pop)))




