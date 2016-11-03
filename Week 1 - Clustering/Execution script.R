#Name:          Merlijn van Breugel
#Date:          2016-10-29
#Description:   Script for MCA and k-means clustering

######################################
## Setup of R                       ##
######################################

#Load packages and source functions
install.packages('homals')      # Package homals for nonlinear MCA
library(homals)                 
install.packages('stats')       # Package stats for k-means    
library(stats)                  
install.packages('ggplot2')     # Package ggplot2 for convenient plotting possibilities
library(ggplot2)
install.packages('ellipse')     # Package ellipse for ellipse drawing in plot  
library(ellipse)
install.packages('xlsx')        # Package xlsx for reading data i xlsx format
library(xlsx)
install.packages('cluster')     # Package xlsx for reading data i xlsx format
library(cluster)
install.packages('ggrepel')     # Package ggrepel for plotting text labels in ggplot
library(ggrepel)
install.packages('vegan')       # Package vegan for cascaded Kmeans
library(vegan)
# install.packages('CalcSegmentationTable')       # Package for cluster tables
# library(CalcSegmentationTable)


#Set directory 
setwd("C:\\Users\\Merlijn\\Documents\\GitHub\\Machine-Learning\\Week 1 - Clustering")

source("rescale.R")             # Rescales the output of homals, script written by Professor Groenen
source('plotMCA.R')               # Plot function for MCA results with labels
source('plotClusters.R')        # Plot function for k-means result
source('KmeansIC.R')            # Get AIC and BIC from k-means


######################################
## Data preparation                 ##
######################################


#Load data 
tourist             <- read.csv("TouristData_MarketingLettersPaper.csv")
underwear           <- read.xlsx("Underweardata.xlsx",1)
#Load labels for underwear data
underwear.labels    <- list(attributes = read.xlsx("Underweardata.xlsx",2)
                            ,brands    = read.xlsx("Underweardata.xlsx",3)
                            ,age       = read.xlsx("Underweardata.xlsx",4))
#Choose dataset we want to use
X           <- underwear
X.labels    <- underwear.labels

#Briefly inspect data set
#Check for Missing Values
naCol <- function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
}
naCol(X)                  # CORRECT: No missings

#Check unique values in data
uniqueValues <- function(X) apply(X, 2, unique)
uniqueValues(X)           


######################################
## MCA step                         ##
######################################

#Make screeplot to determine number of dimensions
#Get eigenvalues for max(attrinutes_i - variables) dimensions
dim.max <- max(apply(X, 2, function(x) length(unique(x)))) - ncol(X)

res.mca.dimmax <- rescale(homals(data = X, ndim = dim.max,
                          rank  = dim.max, 
                          level = "nominal",eps=1e-12))
plot(res.mca.dimmax$eigenvalues)

#Initialize
d   <- 5    #Number of dimensions

#Run standard Homals
res.mca <- rescale(homals(data = X, ndim = d,
                     rank  = d, 
                     level = "nominal",eps=1e-16))

#Append labels to category scores
#Use SQL-like merge (similar to JOIN)
catscores <- lapply(X = res.mca$catscore, function(X) cbind(Dimension = as.numeric(rownames(X)),X))


catscores.labeled <- lapply(X = 1:length(catscores), function(X)
    cbind(merge(x = as.data.frame(catscores[X],col.names = colnames(catscores[X]))
          ,y = as.data.frame(X.labels[X],col.names = colnames(X.labels[X]))
          ,by.x = "Dimension", by.y = "Dimension"),Variable = names(X.labels)[X])
    )

#Plot results
plotMCA(objectscores = res.mca$objscores, catscores.labeled = catscores.labeled, plotlabels.YN = TRUE)

######################################
## K-means clustering step          ##
######################################

#Loop over number of iterations to create an elbow plot
imax    <- 10
sse     <- matrix(0,imax,1)
BIC     <- matrix(0,imax,1)
for (i in 1:imax){
    kmeans.i <- kmeans(res.mca$objscores, centers = i, nstart = 200,
                       algorithm = "Hartigan-Wong", trace=FALSE)
    sse[i,1] <- kmeans.i$tot.withinss
    BIC[i,1] <- kmeansIC(kmeans.i)$BIC    
}
plot(sse,type="b", xlab = "Number of clusters", ylab = "Within Cluster SSE")  
plot(BIC,type="b", xlab = "Number of clusters", ylab = "BIC")
abline(v = 6, lty =2)
#Also check Calinski-Harabas method criterion
plot(cascadeKM(X, inf.gr = 2, sup.gr = 10, iter = 100, criterion = "calinski")$results[2,-1]
     ,type = "b", xlab = "Number of clusters", ylab = "Calinski-Harabasz criterion")
abline(v = 6, lty =2)



#Based on these plots, 3 clusters seem to be a good choice
#Still, interpretability will be more decisive

c       <- 4    #Number of centers    

#Plot WithinSSE's for many runs
r           <- 10000
totss       <- matrix(0,r,1)
withinss    <- matrix(0,r,1)
center.loop <- matrix(0,2*r,1+c)
gc()
for (i in 1:r){
    set.seed(i)
    centers.init<- suppressWarnings(kmeans(res.mca$objscores, centers = c, nstart = 1, iter.max = 1,
                          algorithm = "Hartigan-Wong", trace=FALSE)$centers)
    kmeans.i    <- kmeans(res.mca$objscores, centers = centers.init, nstart = 1, iter.max = 50,
                       algorithm = "Hartigan-Wong", trace=FALSE)
    withinss[i,1]  <- kmeans.i$tot.withinss
    totss[i,1]  <- kmeans.i$totss
    center.loop[((2*i)-1):(2*i),]  <- cbind(i,t(centers.init))
}
hist(withinss, main = 'Histogram of Within Cluster SSE')

#Perform full kmeans (nstart large)
res.cluster <- kmeans(res.mca$objscores, centers = c, nstart = 50,
                      algorithm = "Hartigan-Wong", trace=FALSE)

#Check whether this withinsse equals the (expected) global minimum 
cbind(global.min = min(withinss),foud.minimum = res.cluster$tot.withinss) #CORRECT

#Plot results
plotClusters(objectscores=res.mca$objscores,centroids=res.cluster$centers
             ,clusters=res.cluster$cluster,plotcentroids.YN=TRUE,labels=TRUE)
#Maybe we want to add labels as well for the categories
a <- res.cluster$cluster
cluster_profiles <- cbind(a,X)
install.packages('plyr')
library(plyr)

######################################
## Result output generation         ##
######################################

#Hardcoded for 6 clusters, may be coded more dynamic

#Number of observations within clusters
ObservationsPerCluster <- table(cluster_profiles$a)

cluster1 <- cluster_profiles[cluster_profiles$a == 1,]
cluster2 <- cluster_profiles[cluster_profiles$a == 2,]
cluster3 <- cluster_profiles[cluster_profiles$a == 3,]
cluster4 <- cluster_profiles[cluster_profiles$a == 4,]
cluster5 <- cluster_profiles[cluster_profiles$a == 5,]
cluster6 <- cluster_profiles[cluster_profiles$a == 6,]

SegmentationTable <- matrix(0,26,6)
SegmentationTable[,1] <- CalcSegmentationTable(cluster1, ObservationsPerCluster[1])
SegmentationTable[,2] <- CalcSegmentationTable(cluster2, ObservationsPerCluster[2])
SegmentationTable[,3] <- CalcSegmentationTable(cluster3, ObservationsPerCluster[3])
SegmentationTable[,4] <- CalcSegmentationTable(cluster4, ObservationsPerCluster[4])
SegmentationTable[,5] <- CalcSegmentationTable(cluster5, ObservationsPerCluster[5])
SegmentationTable[,6] <- CalcSegmentationTable(cluster6, ObservationsPerCluster[6])



