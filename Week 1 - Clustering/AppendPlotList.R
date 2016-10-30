AppendPlotList <- function(item)
{
    if( .GlobalEnv$counter == .GlobalEnv$size )
    {
        length(.GlobalEnv$plotlist) <- .GlobalEnv$size <- .GlobalEnv$size * 2
    }
    
    .GlobalEnv$counter <- .GlobalEnv$counter + 1
    
    .GlobalEnv$plotlist[[.GlobalEnv$counter]] <- item
}