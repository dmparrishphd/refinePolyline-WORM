refine.xy <- function ( xy , tolerance , quiet = TRUE , draw = FALSE ) {

    if ( draw ) plot ( xy [ , 1 : 2 ] )

    S <- segments.xy ( xy )

    Stack.Done <- list()

    Stack.Depth <- nrow ( S )

    Stack.ToDo <- list()
    for ( i in rev ( seq_len ( Stack.Depth ) ) ) Stack.ToDo <- list (
        S [ i , , drop = FALSE ] ,
        Stack.ToDo )

    while ( length ( Stack.ToDo ) ) {

        if ( ! quiet ) {
            cat ( "unprocessed segments:" , Stack.Depth , "\n" ) ;
            flush.console () }

        Segment <- Stack.ToDo [[ 1 ]]
        Stack.ToDo <- Stack.ToDo [[ 2 ]]
        Stack.Depth [[ 1 ]] <- Stack.Depth - 1L

        xy <- matrix ( nrow = 2 , data = Segment )
        Segments <- segments.xy ( .refine ( xy , tolerance ) )
        n <- nrow ( Segments )

        if ( n == 1 ) {
            Stack.Done <- list ( Segments , Stack.Done )
            if ( draw ) lines (
                Segment [ 1 : 2 ] ,
                Segment [ 3 : 4 ] )
            if ( ! quiet ) print ( Segment )
        } else if ( n == 2 ) {
            Stack.ToDo <- list ( Segments [ 2 , , drop = FALSE ] , Stack.ToDo )
            Stack.ToDo <- list ( Segments [ 1 , , drop = FALSE ] , Stack.ToDo )
            Stack.Depth [[ 1 ]] <- Stack.Depth + 2L
        } else stop ( "this should never happen." ) }

    R <- t ( matrix ( nrow = 4 , data = unlist ( Stack.Done ) ) )
    R <- R [ rev ( seq_len ( nrow ( R ) ) ) , , drop = FALSE ]
    cbind (
        c ( R [ , 1 ] , R [ nrow ( R ) , 2 ] ) ,
        c ( R [ , 3 ] , R [ nrow ( R ) , 4 ] ) ) }
