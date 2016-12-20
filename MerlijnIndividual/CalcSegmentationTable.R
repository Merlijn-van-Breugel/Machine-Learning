CalcSegmentationTable <- function(clusterObs, totalObs){
    
    SegmentationTable <- matrix(0,26,1)
    
    for (i in 1:length(table(clusterObs[,2]))) {
        SegmentationTable[as.numeric(rownames(table(clusterObs[,2]))[i])] <- table(clusterObs[,2])[i] / totalObs
    }
    
    for (i in 1:length(table(clusterObs[,3]))) {
        SegmentationTable[as.numeric(rownames(table(clusterObs[,3]))[i])+15] <- table(clusterObs[,3])[i] / totalObs
    }
    
    for (i in 1:length(table(clusterObs[,4]))) {
        SegmentationTable[as.numeric(rownames(table(clusterObs[,4]))[i])+23] <- table(clusterObs[,4])[i] / totalObs
    }
    
    return(SegmentationTable)
}