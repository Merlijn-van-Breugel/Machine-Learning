#Name:          Merlijn van Breugel
#Date:          2016-10-29
#Description:   Script for MCA and k-means clustering

######################################
## Setup of R                       ##
######################################

#Load packages and source functions
install.packages('homals')      # Install package homals for nonlinear MCA
library(homals)                 
install.packages('stats')       # Install package stats for k-means    
library(stats)                  
install.packages('ggplot2')     # Install package ggplot2 for convenient plotting possibilities
library(ggplot2)
install.packages('ellipse')     # Install package ellipse for ellipse drawing in plot  
library(ellipse)
install.packages('xlsx')        # Install package xlsx for reading data i xlsx format
library(xlsx)
install.packages('cluster')     # Install package xlsx for reading data i xlsx format
library(cluster)

#Set directory 
setwd("C:\\Users\\Merlijn\\Documents\\GitHub\\Machine-Learning\\Week 1 - Clustering")

source("plot.homals.R")         # Overrides default homals plot command
source("rescale.R")             # Rescales the output of homals, script written by Professor Groenen

######################################
## Data preparation                 ##
######################################


#Load data 
tourist     <- read.csv("TouristData_MarketingLettersPaper.csv")
underwear   <- read.xlsx("Underweardata.xlsx",1)
#Choose dataset we want to use
X           <- underwear

#Briefly inspect data set
#Check for Missing Values
naCol <- function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
}
naCol(X)                  # CORRECT: No missings

#Check whether only 0/1 values are registered
uniqueValues <- function(X) apply(X, 2, unique)
uniqueValues(X)           # CORRECT: Only 0's and 1's


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
d   <- 2    #Number of dimensions

#Run standard Homals
res.mca <- rescale(homals(data = X, ndim = d,
                     rank  = d, 
                     level = "nominal",eps=1e-12))
#Run standard Homals
res.mca <- rescale(homals(data = X, ndim = 6,
                          rank  = 6, 
                          level = "nominal",eps=1e-12))



######################################
## K-means clustering step          ##
######################################

#Loop over number of iterations to create an elbow plot
imax    <- 15
sse     <- matrix(0,imax,1)

for (i in 1:imax){
    kmeans.i <- kmeans(res.mca$objscores, centers = i, nstart = 50,
                       algorithm = "Hartigan-Wong", trace=FALSE)
    sse[i,1] <- kmeans.i$tot.withinss
}
plot(sse)  
#Based on this plot, 3 clusters seem to be a good choice
#Still, interpretability will be more decisive

#Plot WithinSSE's for many runs
r           <- 1000
totss       <- matrix(0,r,1)
withinss    <- matrix(0,r,1)
center.loop <- matrix(0,2*r,1+c)
gc()
for (i in 1:r){
    set.seed(i)
    centers.init<- kmeans(res.mca$objscores, centers = c, nstart = 1, iter.max = 1,
                          algorithm = "Hartigan-Wong", trace=FALSE)$centers
    kmeans.i    <- kmeans(res.mca$objscores, centers = centers.init, nstart = 1, iter.max = 50,
                       algorithm = "Hartigan-Wong", trace=FALSE)
    withinss[i,1]  <- sum(kmeans.i$withinss) 
    totss[i,1]  <- kmeans.i$totss
    center.loop[((2*i)-1):(2*i),]  <- cbind(i,t(centers.init))
}
hist(withinss)


c       <- 4    #Number of centers     
res.cluster <- kmeans(res.mca$objscores, centers = c, nstart = 50,
                      algorithm = "Hartigan-Wong", trace=FALSE)

test <- pam(x = res.mca$objscores, k = c)
test$objective
#Plot results
plotClusters(objectscores=res.mca$objscores,centroids=res.cluster$centers
             ,clusters=res.cluster$cluster,plotcentroidsYN=TRUE)

#Plot SSE's for many runs

ggplot() +
    geom_point(data=as.data.frame(res.mca$objscores), aes(D1,D2),size=0.1)+
    scale_shape(solid=FALSE) +
    xlab("Dimension 1")+
    ylab("Dimension 2")+
    theme_bw()+
    theme(aspect.ratio = 1)


plot(res.mca, plot.type = "joinplot", asp = 1)  # biplot of objects and categories 



#Set the number of iterations needed for convergence
iterations <- kmeans(res.homals$objscores, centers = c, iter.max = 10, nstart = 1,
       algorithm = "Hartigan-Wong", trace=FALSE)$iter




x11()
#Loop over number of iterations
for (i in 1:iterations){
    if (i == 1){center_step = c}
    kmeans_step <- suppressWarnings(kmeans(res.homals$objscores, centers = center_step, iter.max = 1, nstart = 1,
                          algorithm = "Hartigan-Wong", trace=FALSE))
    center_step <- kmeans_step$centers

    dfplot      <- as.data.frame(cbind(res.homals$objscores,cluster=kmeans_step$cluster))

    #Make centroids
    conf.rgn  <- do.call(rbind,lapply(unique(dfplot$cluster),function(t)
        data.frame(cluster=as.character(t),
                   ellipse(cov(dfplot[dfplot$cluster==t,1:2]),
                           centre=as.matrix(center_step[t,1:2]),
                           level=0.95),
                   stringsAsFactors=FALSE)))  
    
    plot.clusters <- ggplot() +
        geom_point(data=dfplot, aes(D1,D2,color = as.factor(cluster)),size=0.1)+
        scale_shape(solid=FALSE) +
        xlab("Dimension 1")+
        ylab("Dimension 2")+
        theme_bw()+
        theme(aspect.ratio = 1)+
        theme(legend.position="bottom")+
        scale_colour_discrete(name  ="Cluster",
                              labels)+
        geom_point(data=as.data.frame(cbind(center_step,cluster=as.numeric(rownames(center_step))))
                   ,aes(D1,D2,color = as.factor(cluster)),shape=18,size=3)+
        geom_path(data=conf.rgn, aes(D1,D2,color = as.factor(cluster)))+
        guides(alpha=FALSE)

    AppendPlotList(plot.clusters)
    plot(plot.clusters)
    #If you want to wait for keypress
    # cat ("Press [enter] for next iteration")
    # line <- readline()
    Sys.sleep(0.1) 
}

test <- plotlist[[1]]
plot(plotlist[[3]])
