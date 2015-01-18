## Put comments here that give an overall description of what your
## functions do

## This functions takes in a matrix and gives back special kind of 
## matrix that have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(mean) m <<- mean
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of a speacial cache matrix defined by 
## makeCacheMatrix. Repeated calls to the function with similar input object 
## result in cached output being displayed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m    
}
