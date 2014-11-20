## This file provides functions to optimize the computation of the inverse of a square matrix by caching.

## makeCacheMatrix creates a special matrix object, that caches the inverse of the matrix
## It returns a list of functions to handle the content of the object
## There are get and set functions for the matrix itself and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function (y) {
        x <<- y
        s <<- NULL
    }
    get <- function () x
    setinv <- function(solve) s <<- solve
    getinv <- function() s
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix object. If it does this the first time, it also caches the matrix. For all successive cases it returns the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinv()
    ## use cache if possible
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    ## compute inverse
    s <- solve(data, ...)
    ## cache it
    x$setinv(s)
    s
}
