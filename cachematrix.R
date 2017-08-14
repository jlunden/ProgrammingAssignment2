## This pair of functions provides the ability to cache the inverse of a matrix.
## If the inverse has not yet been calculated then it will be done and cached.
## If the inverse has been calculated and cached then that value will be returned 
## without recalculating.

# Execution steps:
# 1. create matrix
# 2. call makeCacheMatrix passing actual matrix to create cacheable matrix object
# 3. called cacheSolve passing the cacheable matrix object

## makeCacheMatrix takes a matrix as input and returns a cacheable matrix object via
## a list of functions to set and get the matrix and to set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve will return the inverse of a matrix based on makeCacheMatrix
## If the inverse has already been calculated it will return the cached value.
## If the inverse has not been calculated it will calculate the inverse, cache
## the result and return that result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    message("calculating and caching the inverse")
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
