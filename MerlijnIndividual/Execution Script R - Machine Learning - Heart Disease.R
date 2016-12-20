#Author:        Merlijn van Breugel
#Date:          19-12-2016
#Description:   Machine Learning

######################################
## Setup of R                       ##
######################################

#Load packages and source functions
# install.packages('homals')      # Package homals for nonlinear MCA
library(homals)
# install.packages('stats')       # Package stats for k-means
library(stats)
# install.packages('ggplot2')     # Package ggplot2 for convenient plotting possibilities
library(ggplot2)
# install.packages('fpc')
library(fpc)
# install.packages('MASS')
library(MASS)
# install.packages('randomForest')
library(randomForest)
# install.packages('ROCR')
library(ROCR)

#Set directory to correct folder
setwd("C:\\Users\\Merlijn\\Documents\\GitHub\\Machine-Learning\\MerlijnIndividual")

source("rescale.R")             # Rescales the output of homals, script written by Professor Groenen
source('plotMCA.R')             # Plot function for MCA results with labels
source('plotClusters.R')        # Plot function for k-means result
source('KmeansIC.R')            # Get AIC and BIC from k-means
#Load packages

rm(list=ls())

######################################
## Setup of data                    ##
######################################

#Load data from ICS Machine Learning Repository
heart.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart <- read.csv(heart.url, header=FALSE, sep = ',')
heart <- cbind(heart[,14],heart[,-14])
colnames(heart) <- c('class', 'age', 'sex','cp', 'trestbps', 
                     'chol', 'fbs', 'restecg', 
                     'thalach', 'exang',
                     'oldpeak','slope','ca','thal')

#Briefly inspect data set
#Check for Missing Values
naCol <- function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
}
naCol(heart)                    # CORRECT: No missings

#Check unique values in data
uniqueValues <- function(X) apply(X, 2, unique)
uniqueValues(heart)             # INCORRECT: Some '?' values     

#Inspect the different classes
table(heart$class)
table(heart$class>0)

#Inspect the cells with a question mark
table(heart$ca)
table(heart$thal)

#Drop the 7 observations with missing values
heart <- heart[heart$ca!='?' & heart$thal!='?', ]

#Make training and test set
X           <- heart 
X.perm      <- heart[sample(nrow(X)),]
set.seed(42)
n.train     <- round(0.7*nrow(X.perm))
X.train     <- X.perm[1:n.train,]
X.test      <- X.perm[-(1:n.train),]
n.test      <- nrow(X.test)

######################################
## K-means clustering step          ##
######################################

#Loop over number of iterations to create an elbow plot
kmin         <- 2
kmax         <- 10
kmeans.stat  <- 'ch' 
kmeans.meas  <- matrix(0,kmax-kmin+1,2)
colnames(kmeans.meas)   <- c('cluster.no','stat.value')
for (k in kmin:kmax){
    i                   <- k-kmin+1
    kmeans.meas[i,1]    <- k
    dist                <- dist(X[,-1])
    kmeans.i            <- kmeans(X[,-1], centers = k, nstart = 200,
                                  algorithm = "Hartigan-Wong", trace=FALSE)
    stats               <- cluster.stats(d = dist,kmeans.i$cluster)
    if (kmeans.stat == 'ch'){
        kmeans.meas[i,2]    <- stats$ch
        k.opt               <- which.max(kmeans.meas[,2])
    }else if (kmeans.stat == 'within.cluster.ss'){
        kmeans.meas[i,2]    <- stats$within.cluster.ss
    }else if (kmeans.stat == 'avg.silwidth'){
        kmeans.meas[i,2]    <- stats$avg.silwidth        
    }else if (kmeans.stat == 'entropy'){
        kmeans.meas[i,2]    <- stats$within.cluster.ss
    }
    
}
plot(kmeans.meas,type="b", xlab = "Number of clusters", ylab = kmeans.stat)  

kmeans.i            <- kmeans(X[,-1], centers = 2, nstart = 500,
                              algorithm = "Hartigan-Wong", trace=FALSE)
cbind(kmeans.i$cluster,X.train[,1],((kmeans.i$cluster==2)&X[,1]==0))
table(((kmeans.i$cluster==1)&X[,1]==0))
table(kmeans.i$cluster)
abline(v = 6, lty =2)




#Map the binary variables to a more logical correspondence
colnames(Husband_occ) <- c("Hb.Technical","Hb.Sales","Hb.Manual","Hb.Agriculture")
cmc <- cbind(cmc[,1:6],Wife.work=1-cmc$Wife.work,Husband_occ[,1:3],St.living=cmc[,9],Media=1-cmc$Media)
save(cmc, file="cmc.Rdata", compress=TRUE)
cmc$Method <- as.factor(cmc$Method)
classnames <- read.csv("Classnames.csv", col.names = "Classnames")
#Check for NA values
naCol <- function(x){
    y <- sapply(x, function(xx)any(is.na(xx)))
    names(y[y])
}
naCol(cmc) #None missings
#Normalize data
cmc_st <- cbind.data.frame(Method=cmc$Method,scale(cmc[,-1], center = FALSE, scale = sqrt(diag(var(cmc[-1])))))
colnames(cmc_st) <- colnames(cmc_st) 

#Test for equality of covariance matrices with BoxM test results in a rejection
#See below for more testing whether LDA is the right way to go
boxM(data=cmc_st[,-1], grouping=cmc_st[,1]) 

#Run own lda function
res_own         <- lda.own(X=cmc_st[,-1],grouping=cmc_st$Method, CV = FALSE,scale=TRUE)
#Run standard MASS package
res_standard    <- lda(cmc_st[,-1], grouping = cmc_st[,1], CV = TRUE, plot = TRUE)
#Check for equality of scaling
res_standard$scaling-res_own$LD #The second dimension is rotated, abs(coefficients) are identical 

#Standard package performs scaling on X projection
pred_standard <- predict(res_standard, newdata = cmc_st[,-1], method = "predictive")
X.proj.standard <- pred_standard$x
#If we perform the same scaling and tranpose second discriminant, results are identical!
X.proj.new      <- scale(X_mat, center = colSums(res_own$prior%*%res_own$means))%*%cbind(res_own$LD[,1],-res_own$LD[,2])
plot(X.proj.standard)
plot(X.proj.new)

#Perform LOO cross validation
res_own.CV      <- lda.own(X=cmc_st[,-1],grouping=cmc_st$Method, CV = TRUE,scale=TRUE)

#Construct confusion table
confusion <- table(cmc[, 1], res_own.CV$pred.class)
print(confusion)
confusion_perc <- t(t(confusion) / rowSums(confusion))  #Percent of row total
h   <- sum(diag(confusion)) / sum(confusion)                   #hit rate
#Test whether it outperforms random guess
p0  <- max(res_own.CV$prior)
z   <- (h-p0)/sqrt(p0*(1-p0)/n)
dnorm(z, mean = 0, sd = 1, log = FALSE)

#Test whether predictions are better with QDA method
res.qda <- qda(cmc_st[,-1], grouping = cmc_st[,1], CV = FALSE, plot = TRUE)
pred.qda<- predict(res.qda, newdata = cmc_st[,-1], method = "predictive")
confusion <- table(cmc[, 1], pred.qda$class)
print(confusion)
confusion_perc <- t(t(confusion) / rowSums(confusion))  #Percent of row total
sum(diag(confusion)) / sum(confusion)   
#We find an even lower hit rate, hence we are confident in LDA

table(pred_standard$class)

#Construct confusion table
confusion <- table(cmc[, 1], pred.class)
print(confusion)
confusion_perc <- t(t(confusion) / rowSums(confusion))  #Percent of row total
h   <- sum(diag(confusion)) / sum(confusion)            #hit rate
#Test whether it outperforms random guess
p0  <- max(res_own.CV$prior)
z   <- (h-p0)/sqrt(p0*(1-p0)/n)
dnorm(z, mean = 0, sd = 1, log = FALSE)




X.proj.dist <- as.data.frame(sqrt(rowSums(X.proj*X.proj))+4)
colnames(X.proj.dist) <- "Dist"
correct <- matrix(0,n,1)
correctc <- matrix("#d5dbe5",n,1)
for (i in 1:n){
    if (res_own$pred.class[i,1]==cmc_st[i,1]){
        correct[i,1] <- cmc_st[i,1]
        correctc[i,1] <- colors[cmc_st[i,1]]
    }
}

res_own.CV$pred.class!=cmc_st

correct <- res_own.CV$pred.class
dfplot  <- cbind(grouping=res_own$pred.clas, colorv = as.factor(correct),X.proj,centroids[,-1],X.proj.dist)
library(scales)
colors <- c(hue_pal()(3),"#d5dbe5")

plot.object5 <- ggplot() +
    geom_point(data=dfplot, aes(LD1,LD2, color = as.factor(grouping)),color = correctc)+
    scale_shape(solid=FALSE) +
    theme_bw()+
    theme(aspect.ratio = 1)+
    theme(legend.position="bottom")+
    scale_colour_discrete(name  ="Class",
                          labels=c("No-use", "Long-term", "Short-term"))


grid.arrange(plot.object1, plot.object3, plot.object4, ncol=3)+
    theme(legend.position="bottom")+
    scale_colour_discrete(name  ="Class",
                          labels=c("No-use", "Long-term", "Short-term"))

