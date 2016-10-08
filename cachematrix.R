##^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##
## Functions to calculate/cache the inverse of an matrix.                        ##
## a) makeCacheMatrix: Function that stores the different methods i.e. get, set. ##
## b) cacheSolve: if the inverse is already found, then use the value from cache.##
##_______________________________________________________________________________##

## This function contains implementation of the get (Inverse)/set(Inverse) methods.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function searches the cache to see if the matrix is already available. if yes, returns the computed value that is cached. otherwise compute the values.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("---> Getting Cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
    ## Returning a matrix that is the inverse of 'x'
}
