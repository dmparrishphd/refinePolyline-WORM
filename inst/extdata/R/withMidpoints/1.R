withMidpoints <- function ( x ) rev ( rev ( as.vector ( t ( matrix (
    ncol = 2 , data = c ( x , midpoints ( x ) , NA ) ) ) ) ) [ -1 ] )
