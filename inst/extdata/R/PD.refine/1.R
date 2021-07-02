.refine <- function ( xy , tolerance ) {
    D <- diff ( xy )
    Q <- sum ( D * D )
    if ( Q <= tolerance ) return ( xy )
    vapply (
        USE.NAMES = FALSE ,
        FUN.VALUE = double ( 3 ) ,
        X = seq_len ( ncol ( xy ) ) ,
        function ( j ) withMidpoints ( xy [ , j ] ) ) }
