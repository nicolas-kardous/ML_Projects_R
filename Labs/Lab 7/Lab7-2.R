library(caret)
library(dplyr)
library(ggplot2)

# Load the data:
airline <- read.csv("airline.csv")

# Take a look:
head(airline)

# Basic descriptive stats from the lecture:
summary(airline)

# We need to preprocess the data so that we treat each column
# equally to compute the clusters
# we use the preProcess function from Caret that does
# all the work for us

# preprocessing steps:
# First, center the data (substract the mean to each column)
# => mean becomes 0 for each column
# Then, scale the data, by dividing by the standard deviation
# => std becomes 1 for each column
# https://medium.com/@rrfd/standardize-or-normalize-examples-in-python-e3f174b65dfc

#step 1: create the pre-processor using preProcess (part of caret)
pp <- preProcess(airline, method=c("center", "scale"))
#step 2: apply it to our dataset

airline.scaled <- predict(pp, airline) # newdata?

head(airline.scaled)
summary(airline.scaled)
# What does a negative value represent?


### Clustering: K-Means

# the kmeans function is part of stats package (default in R)
# we can set an upper bound to the number of iterations of the algorithm
# here we set k=8
# setting the seed..why?
set.seed(144)
km <- kmeans(airline.scaled, iter.max=100, 8)

# Let's explore the results!

km 

# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km$withinss

# betweenss: sum of squared distances between each cluster mean and
# the data mean
km$betweenss

# number of iters
km$iter
# cluster centroids
km$centers
# cluster for each point
km$cluster

# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km$tot.withinss

# the number of observations in each cluster -- table(km$cluster) also works
km$size


# Selecting the value of K
# we want to create the scree plot
# to do this, we want to compute the cluster dissimilarity for
# all the values of k we want to try
# we store this result in the dataframe "dat"

# here we test all k from 1 to 100

dat <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat$SS <- sapply(dat$k, function(k) {
  set.seed(144)
  kmeans(airline.scaled, iter.max=100, k)$tot.withinss
})

# let's plot it using ggplot!
ggplot(dat, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 8, color = "blue")
# Was k=8 a good choice ?
# remember that we are looking for a "knee" in the curve / a place where rocks stop falling
# https://scikit-learn.org/stable/auto_examples/cluster/plot_kmeans_silhouette_analysis.html

### Hierarchical Clustering

# Compute all-pair euclidian distances between our observations
d <- dist(airline.scaled)

# Creates the Hierarchical clustering
# hclust is part of which package? stats
mod.hclust <- hclust(d, method="ward.D2")
# https://en.wikipedia.org/wiki/Ward%27s_method
# https://www.rdocumentation.org/packages/stats/versions/3.5.2/topics/hclust



# Now, we can plot the hierarchy structure / dendrogram
# labels = FALSE because we do not want to print text
# for each of the 3999 observations
plot(mod.hclust, labels = FALSE)

# We can remove the extra text for a cleaner plot
plot(mod.hclust, labels=F, xlab=NA, ylab="Dissimilarity",sub=NA, main=NA)

# To choose a good value for K, we need to create the same
# scree plot: dissimilarity for each k
# the next line puts this data in the right form to be plotted
dat.hc.airline <- data.frame(k = seq_along(mod.hclust$height),
                             dissimilarity = rev(mod.hclust$height))
#  height is a returned vector, denotes the value of the criterion for the particular agglomeration.

# Scree plot! 
ggplot(dat.hc.airline, aes(x=k, y=dissimilarity)) +
  geom_line()+
  xlab("Number of Clusters") +
  ylab("Dissimilarity")

# Let's zoom on the smallest k values:
ggplot(dat.hc.airline, aes(x=k, y=dissimilarity)) +
  geom_line()+
  xlab("Number of Clusters") +
  ylab("Dissimilarity") + 
  xlim(0,100)

# what would be a "good" k value ?

# now that we have k, we can construct the clusters
assignments <- cutree(mod.hclust, k = 5)
rev(mod.hclust$height)[5]
rev(mod.hclust$height)[4]
# we can also cut based on height
# sometiems just visually inspecting the dendrogram is enough
# but always good to validate with the scree plot
assignments2 <- cutree(mod.hclust, h = 55)

# test for the same clusters
all.equal(assignments, assignments2)

# store it into our data frame
airline.scaled$clust2 = assignments

# and finally use dplyr to have access to the centroids
# group_by groups observations in our data frame together based on which
# cluster each observation is assigned to
# very powerful and simple way to calculate important summaries of our data
# more on this and related topics in 2 weeks
airline.scaled %>% group_by(clust2) %>%
  summarize(mean(Balance), mean(BonusMiles), mean(BonusTrans))
# a shortcut to have the mean of all columns
airline.scaled %>% group_by(clust2) %>%
  summarize_all(funs(mean))

# we can also compute the size of each cluster
table(assignments)
