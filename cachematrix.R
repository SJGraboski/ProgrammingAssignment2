## Cache Matrix Functions: these functions together compute the inverse
## of a matrix, cache the inverse, then allow the user to retrieve
## the inverse without having to run the computation a second time.
## This code is based off of the makeVector and getmean functions
## supplied in the assignment documentation.


## This function creates a special matrix that's the inverse of
## another matrix, then returns a function called getinverse to
## retrieve the new matrix from the cache.
## (It also returns other functions that can retrieve the original
## matrix and recalculate the inverse if the cached inverse no 
## longer exists).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function accepts the cached inverse computed in makeCacheMatrix
## and then spits out that inverse. Should the user's memory no longer
## contain the cached matrix, it will instead recalculate the inverse 
## with the original matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
