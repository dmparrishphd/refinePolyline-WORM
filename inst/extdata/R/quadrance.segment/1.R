quadrance.segment <- function ( segment ) {
    D <- diff ( matrix ( nrow = 2 , data = segment ) )
    sum ( D * D ) }
