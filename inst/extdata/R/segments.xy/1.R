segments.xy <- function ( xy ) {
    s <- cbind ( xy [ - nrow ( xy ) , , drop = FALSE ] , xy [ -1 , , drop = FALSE ] )
    colnames ( s ) <- NULL
    n <- ncol ( xy )
    j <- seq_len ( n )
    j <- as.vector ( rbind ( j , j + n ) )
    s [ , j , drop = FALSE ] }
