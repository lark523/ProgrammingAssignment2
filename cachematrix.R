## The functions of this file are makeCacheMatrix(x) and cacheSolve(x)
## The makeCacheMatrix() creates a special matrix object that allows
## for the inverse of the matrix to be calculated. It will store 
## the calculated inverse in cache.
## The cacheSolve(x) function retrieves the inverse of a matrix x
## if it is stored in cache. If it is not stored in cache, the inverse of
## matrix x will be computed and returned.

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inversedmtx) m <<- inversedmtx
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## if the inverse already exists, it will retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    tempdata <- x$get()
    m <- solve(tempdata, ...)
    x$setinverse(m)
    m
}
