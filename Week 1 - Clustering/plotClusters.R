#Loop over number of iterations
plotClusters <- function(objectscores,centroids,clusters,plotcentroids.YN,labels){
    
    dfplot      <- as.data.frame(cbind(objectscores,clusters))
    
    if (plotcentroids.YN==TRUE){
    #Make centroids
    conf.rgn  <- as.data.frame(do.call(rbind,lapply(unique(dfplot$clusters),function(t)
        data.frame(cluster=as.character(t),
                   ellipse(cov(dfplot[dfplot$clusters==t,1:2]),
                           centre=as.matrix(centroids[t,1:2]),
                           level=0.95),
                   stringsAsFactors=FALSE))))   
    }
    plot.clusters <- ggplot() +
        geom_point(data=dfplot, aes(D1,D2,color = as.factor(clusters)),size=0.2)+
        scale_shape(solid=FALSE) +
        xlab("Dimension 1")+
        ylab("Dimension 2")+
        theme_bw()+
        theme(aspect.ratio = 1)+
        theme(legend.position="bottom")+
        scale_colour_discrete(name  ="Cluster",
                              labels)+
        geom_point(data=as.data.frame(centroids),aes(D1,D2,color = rownames(centroids)),shape=18,size=3)+
        guides(alpha=FALSE)+
        if (plotcentroids.YN==TRUE){geom_path(data=conf.rgn, aes(D1,D2,color = as.factor(cluster)))
        }
    
    return(plot.clusters)
}