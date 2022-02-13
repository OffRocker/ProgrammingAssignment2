##These two functions calculate and cache the inverse of a matrix so that
##it should not be recalculated for the same matrix again.

## The first function would create an object 
## which would hold a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL 
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function either calculates the inverse or returns the preexisting one.

cacheSolve <- function(x, ...) {
    
    i <- x$getinv()
   
    if(!is.null(i)){
        message('getting cached data')
        return(i)
    }
    
    data <- x$get()
    i <- solve(data,...)
    x$setinv(i)
    i
}
