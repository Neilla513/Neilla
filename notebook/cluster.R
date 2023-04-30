#Load data
###UNSUPERVIED ML-CLUSTERING
###########Let us begin with a demonstration with the iris dataset. 
##LOAD THE IRIS DATA
data = iris[,-c(5)]
data = scale(data)
data

#Loading desired package
install.packages("clValid")
library(clValid)
#####Perform K-Means Clustering
##Feel free to change k values and repeat the process
data <- as.data.frame(t(as.matrix(data)))
data
kmeans(data, 3, iter.max = 10, nstart = 1)

km.res$cluster

head(km.res$cluster, 3)

km.res$centers

fviz_cluster(km.res, data,
             palette = "Set2", ggtheme = theme_minimal())

# Internal Validation
clmethods <- c("hierarchical","kmeans","pam")
internval <- clValid(data, nClust = 2:5, clMethods = clmethods, validation = "internal")

# Summary
summary(internval)
optimalScores(internval)

# Hierarchical clustering with two clusters was found the best clustering algorithm in each case (i.e., for connectivity, Dunn and Silhouette measures)
plot(internval)

# Stability measure Validation
clmethods <- c("hierarchical","kmeans","pam")
stabval <- clValid(data, nClust = 2:6, clMethods = clmethods,
                   validation = "stability")

# Display only optimal Scores
summary(stabval)
optimalScores(stabval)

# External Clustering Validation
library(fpc)

# K-Means Cluster Analysis
fit <- kmeans(data,2)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(data), species, fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand
clust_stats$vi

# k means Cluster Analysis
fit <- kmeans(data,2)

# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(data), species, fit$cluster)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats$corrected.rand
clust_stats$vi

# Same analysis for Ward Hierarchical Clustering
d2 <- dist(data, method = "euclidean")
fit2 <- hclust(d2, method="ward.D2")

# cluster assignment (members)
groups <- cutree(fit2, k=2)

# Compute cluster stats
clust_stats2 <- cluster.stats(d = dist(data), species, groups)

# Corrected Rand index and VI Score
# Rand Index should be maximized and VI score should be minimized
clust_stats2$corrected.rand
clust_stats2$vi



##########################################################################################
##########################################################################################
#Now Let us try for various real data: GDP and Temperature
################UNSUPERVISED MACHINE LEARNING#############################################
gdp = read.csv("realGDP.csv",header=T)
gdp
dim(gdp)
gdp = gdp[ ,-c(1)]
gdp
dim(gdp)
describe(gdp)
#####Clustering###################
# Loading the data set
df <- scale(gdp) # Scaling the data

dft <- as.data.frame(t(as.matrix(df)))
dft
kmeans(dft, 3, iter.max = 10, nstart = 1)

#install.packages("factoextra")
library(factoextra)
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(dft, 3, nstart = 25)

km.res$cluster

head(km.res$cluster, 3)

km.res$centers

fviz_cluster(km.res, dft,
             palette = "Set2", ggtheme = theme_minimal())

# Show text only
fviz_cluster(km.res, dft, geom = "text")

# PAM clustering
# ++++++++++++++++++++
require(cluster)
pam.res <- pam(dft, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "text", ellipse.type = "norm")

# Hierarchical clustering
# ++++++++++++++++++++++++
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(dft, k = 3, hc_method = "complete", cex=.7)
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = T, rect = TRUE, cex=.8)###Dendrogram
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

##############################################################################################
###NOW LOAD THE AFRICAN TEMPERATURE DATA AND REPEAT THE PROCESS.
##VALIDATE YOUR CLUSTERS USING THE METHODS IN SECTION ONE ABOVE.
