## The following functions are used in common to calculate
## and cache the inverse of a invertible matrix


## This function creates a special "matrix" object that is able to
## cache its inverse. It returns a list with 4 callable functions to:
##  * set the matrix data
##  * get the matrix data
##  * set the inverse of te matrix
##  * get the inverse of te matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(x_) {
        x <<- x_
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(s_) s <<- s_
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed -- check line #16 above), then it
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
