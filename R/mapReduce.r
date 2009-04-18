# Function: mapReduce
#   mapReduce pattern implemented on an R data structure (array data.frame matrix) 
# map: the map step evaluated on the data structure
# ...: The Reduce step can be one or more functions with optional names 
#      evaluated on the split data.  name=function.
# data: the R data structure  
# Returns a matrix or data.frame.

mapReduce <- function( map, ..., data=NULL, apply=sapply ) {

    attach(data)   # Do we really want to attach this?

  # TRAP by parameter.  Find it in data if it is missing
    if ( 
      ! exists( as.character(substitute(map) ) ) 
    ) {
      map<-eval( substitute(map), data )
    }              

  # Convert into an expression
  # The [-1] removes the 'c()' part of the call
    expr = substitute( c( ... ) )[-1]


  # Split data ... this is important since each of the features
  # will operate on the split data.  This is also the most time
  # consuming part of the process.
    if( class(data) == "list" ) {
        split.data <- data
    } else {
        split.data <- split( data, map )
    }

  # innerFun: Evaluates and expression  an expression to 
    innerFun = function( entity.data, expr ) {
        eval( expr, entity.data )
    }    

  # outerFun: Split data based on map  
    outerFun = function( expr, split.data ) {
        sapply(
            # split( data, map ) , 
            split.data  ,
            innerFun ,
            expr
        )
    }


  # RETURN:
    ret=apply( 
        expr ,  # Elimanates call
        outerFun ,  # Contains inner function
        split.data 
    )

    detach(data)
    return(ret)
}



