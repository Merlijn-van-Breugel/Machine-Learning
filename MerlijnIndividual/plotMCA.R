#Loop over number of iterations
plotMCA <- function(objectscores,catscores.labeled,plotlabels.YN){
    
    dfplot      <- as.data.frame(objectscores)
    if (plotlabels.YN==TRUE){
        #Make df of labeled catscores
        dfplot.labels <- do.call("rbind", catscores.labeled)
    }
    
    plot.MCA    <- ggplot(data=dfplot, aes(D1,D2),size=0.5) +
        #geom_point()+
        geom_point(position = position_jitter(w = 0.3, h = 0.3),size=0.6)+
        #geom_jitter(position = "jitter")+
        scale_shape(solid=FALSE) +
        xlab("Dimension 1")+
        ylab("Dimension 2")+
        theme_bw()+
        theme(aspect.ratio = 1)+
        theme(legend.position="bottom")
        #geom_jitter(data=dfplot, aes(jitter(D1),jitter(D2)),size=1,alpha = 0.5)
        #geom_jitter(data=dfplot, aes(D1,D2),size=1)       
        if (plotlabels.YN==TRUE){
            plot.MCA<- plot.MCA+
                geom_text_repel(data=dfplot.labels, aes(D1,D2,label = Label
                                ,color = Variable)
                                ,arrow = arrow(length = unit(0.01, 'npc')))+
                geom_point(data = dfplot.labels, aes(x = 0, color = Variable), 
                           y = 0, size = 0)+
                scale_colour_brewer(palette = "Set1")+
                guides(colour = guide_legend(override.aes = list(size=5)))
                # theme(legend.key=element_rect(fill = "Red"))
                # facet_wrap(~ lvl) +
                # guides(colour = guide_legend(override.aes=list(size=3)))
            
        }

    return(plot.MCA)
}