kmeansIC = function(res){
    
    m = ncol(res$centers)
    n = length(res$cluster)
    k = nrow(res$centers)
    D = res$tot.withinss
    return(data.frame(AIC = D + 2*m*k,
                      BIC = D + log(n)*m*k))
}