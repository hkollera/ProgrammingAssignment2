## This file provides functions to optimize the computation of the inverse of square matrix by caching.

## makeCacheMatrix creatas a special matrix object, that caches the inverse of the matrix

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
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
