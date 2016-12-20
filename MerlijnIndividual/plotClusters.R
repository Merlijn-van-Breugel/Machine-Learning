#Loop over number of iterations
plotClusters <- function(objectscores,centers,clusters,catscores.labeled,plotlabels){
    
    dfplot      <- as.data.frame(cbind(objectscores,clusters))
    
    #Make ellipses around centroids
    conf.rgn  <- do.call(rbind,lapply(unique(dfplot$cluster),function(t)
        data.frame(cluster=as.character(t),
                   ellipse(cov(dfplot[dfplot$cluster==t,1:2]),
                           centre=as.matrix(centers[t,1:2]),
                           level=0.95),
                   stringsAsFactors=FALSE)))  
    
    #Make df of labeled catscores
    if (plotlabels>1){
        
        dfplot.labels <- do.call("rbind", catscores.labeled)
    }
    
    plot.clusters <- ggplot() +
        geom_point(data=dfplot, aes(D1,D2,color = as.factor(clusters)))+
        geom_point(size=0.5)+
        scale_shape(solid=FALSE) +
        xlab("Dimension 1")+
        ylab("Dimension 2")+
        theme_bw()+
        theme(aspect.ratio = 1)+
        theme(legend.position="bottom")+
        geom_point(data=as.data.frame(centers),aes(D1,D2,color = as.factor(rownames(centers))),shape=18,size=4)+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        geom_path(data=conf.rgn, aes(D1,D2,color = as.factor(cluster)))+
        guides(colour = guide_legend(override.aes = list(size=5)))
    
    if (plotlabels==2){
        plot.clusters<- plot.clusters+
            geom_text_repel(data=dfplot.labels, aes(D1,D2,label = Label)
                            ,arrow = arrow(length = unit(0.01, 'npc')))+
            geom_point(data = dfplot.labels, aes(x = 0), 
                       y = 0, size = 0)
        
    }else if (plotlabels==3){
        plot.clusters<- plot.clusters+
            guides(colour = guide_legend(override.aes = list(size=5)))+
            geom_text_repel(data=dfplot.labels, aes(D1,D2,label = Label
                                                    ,color = Variable)
                            ,arrow = arrow(length = unit(0.01, 'npc')))
    }
    
    plot.clusters<- plot.clusters+
        if (plotlabels < 3){
            scale_colour_discrete(name  ="Clusters")
        }else{
            scale_colour_discrete(name  ="Clusters and variables")
        }
    return(plot.clusters)
}