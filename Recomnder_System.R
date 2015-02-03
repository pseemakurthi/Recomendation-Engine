library("XLConnect")
options( java.parameters = "-Xmx4g" )
# To check the Amount of free memory in the Java Virtual Machine (JVM):
xlcMemoryReport()

# install.packages("recommenderlab")
library(recommenderlab)

library(ggplot2)

# Clear the existing workspace
rm(list=ls(all="TRUE"))
setwd('D:\\Music')

# Read the user song listen counts
data=read.table(file="tripletsout.txt",header=TRUE,sep=",")
colnames(data)=c("UserID","SongID","Rating")

# Convert the data into a rating matrix can be converted to real or binary matrix
(songs.rat <- as(data,"realRatingMatrix"))
# (songs.rat <- as(data,"binaryRatingMatrix"))

# display the contents of the rating matrix
# as(songs.rat, "list")
head(as(songs.rat, "data.frame"))

# image(songs.rat)

#analyze the ratings matrix
summary(getRatings(songs.rat))

#Bar chart of Users and Ratings. THis shows mist of the user did a rating from 1-7.
qplot(getRatings(songs.rat), 
      binwidth = 1, xlim = c(1,10),
      main = "Ratings Bar Chart", 
      ylab = "# of Users", xlab = "Rating")

##################  COMMENTED Z-SCORE NORMALIZATION ######################
# We can normalize the rating matrix using the z-score method if needed. 
# This will remove off high tail ratingg.

# summary(getRatings(normalize(songs.rat, method = "Z-score")))

# qplot(getRatings(normalize(songs.rat, method = "Z-score")), binwidth = 1, xlim = c(-3,5),
#      main = "Ratings Bar Chart", ylab = "# of Users", xlab = "Rating")

# songs.rat <- normalize(songs.rat, method = "Z-score")
##########################################################################

# Plot the distribution of the No of songs Rated by the users
qplot(as.vector(colCounts(songs.rat)),
      binwidth=1, xlim = c(1,30),
      main = "How Many Rated",
      xlab = "Songs",
      ylab = "No of Raters")


# This is very sparse rating with most users rating very few songs.

##################  CONSIDER HIGHLY RATED SONGS ######################

# returns the number of ratings per column (for each song) as a integer vector
colIds <- colCounts(songs.rat)

# sort the vector and finds the index of the 1000th most rated song
sort(colIdx, decreasing = TRUE)[1000]

###### For computation lets consider only 1000 most rated songs  ############
songs.rat@data <- songs.rat@data[, which(colIdx >= sort(colIdx, decreasing = TRUE)[1000])]

# analyze the ratings matrix
songs.rat
summary(rowCounts(songs.rat))
# as(songs.rat, "list")
head(as(songs.rat, "data.frame"))
##################  CONSIDER HIGHLY RATED SONGS ######################

# Also lets bring down the number of users. Consider only users that have rated 
# at least 10 songs

##################  CONSIDER USERS RATING MORE SONGS ######################
qplot(as.vector(rowCounts(songs.rat)),
      binwidth=1, xlim = c(0,30),
      main = "Avg Songs Rated",
      xlab = "Raters",
      ylab = "No of Songs")

# returns the number of ratings (by each user) per row as a integer vector
rowIdx <- rowCounts(songs.rat)
# Remove all the rows that have rated less than 20 songs
songs.rat <- songs.rat[- 1 * (which(rowIdx < 20))]

songs.rat
summary(rowCounts(songs.rat))
# as(songs.rat, "list")
head(as(songs.rat, "data.frame"))
##################  CONSIDER USERS RATING MORE SONGS ######################


###############################
## CROSS VALIDATION EVALUATION
scheme <- evaluationScheme(songs.rat, method="cross-validation", goodRating=5, k=2, given=10)
###############################

###############################
## SPLIT EVALUATION
# scheme <- evaluationScheme(songs.rat, method="split", train=0.9, given=10, goodRating=5)
###############################

algorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(method="Cosine",
                                                 nn=10, minRating=1)),
  "Item-based CF" = list(name = "IBCF", param = list(normalize="Z-score")),
  "Assoc Rules CF" = list(name="AR", param=NULL),
  "LRMF (100 categories)" = list(name = "LRMF", param = list(categories=100,
                                                             normalize="Z-score"))
)

# evaluate the results
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10))

# plot the ROC poins
plot (results, annotate=c(1,3), legend="topleft")

# plot the precision/recall poins
plot (results,"prec/rec",annotate=c(1,2,3,4))

results
names(results)
results[["user-based CF"]]
avg(results)[[1]]
# getConfusionMatrix(results)[[2]]

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

########################################
# EVALUATION OF THE PREDICETED RATINGS
########################################

scheme <- evaluationScheme(songs.rat, method="split", train=0.9, given=15, goodRating=5)
scheme

# We create two recommenders (user-based and item-based collaborative
# filtering) using the training data.

r1 <- Recommender(getData(scheme, "train"), "UBCF")
r1

r2 <- Recommender(getData(scheme, "train"), "IBCF")
r2

# Compute predicted ratings for the known part of the test data (
# 15 items for each user) using the two algorithms.

p1 <- predict(r1, getData(scheme, "known"), type="ratings")
p1
as(p1, "list")
head(as(p1, "data.frame"))

## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionError(p1, getData(scheme, "unknown"))
calcPredictionError(p1, getData(scheme, "unknown"), byUser=TRUE)
calcPredictionError(p1, songs.rat, byUser=FALSE)

p2 <- predict(r2, getData(scheme, "known"), type="ratings")
p2

# Calculate the error between the prediction and the unknown part of the test data.
error <- rbind(
  calcPredictionError(p1, getData(scheme, "unknown")),
  calcPredictionError(p2, getData(scheme, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error