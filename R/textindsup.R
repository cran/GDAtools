textindsup <- function(resmca,supdata,axes=c(1,2),col='darkred') {
    coord <- supind(resmca,supdata)$coord
    text(coord[,axes],rownames(coord),col=col)
    }
