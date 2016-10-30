#Loop over number of iterations
plotClusters <- function(objectscores,centroids,clusters){
    

    #Make centroids
    conf.rgn  <- do.call(rbind,lapply(unique(clusters),function(t)
        data.frame(cluster=as.character(t),
                   ellipse(cov(data[clusters==t,1:2]),
                           centre=as.matrix(centroids[t,1:2]),
                           level=0.95),
                   stringsAsFactors=FALSE)))  
    
    
    plotdf <- as.data.frame(cbind(objectscores,data$clusters))    
    plot.clusters <- ggplot() +
        geom_point(data=data, aes(D1,D2,color = as.factor(cluster)),size=0.1)+
        scale_shape(solid=FALSE) +
        xlab("Dimension 1")+
        ylab("Dimension 2")+
        theme_bw()+
        theme(aspect.ratio = 1)+
        theme(legend.position="bottom")+
        scale_colour_discrete(name  ="Cluster",
                              labels)+
        geom_point(data=center_step,aes(D1,D2,color = as.factor(ordering)),shape=18,size=3)+
        geom_path(data=conf.rgn, aes(D1,D2,color = as.factor(cluster)))+
        guides(alpha=FALSE)
    
    return(plot.clusters)
}