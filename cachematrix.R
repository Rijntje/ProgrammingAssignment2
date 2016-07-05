## A pair of functions to cache the inverse of a matrix.

## Create special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmatrix <- function(solve) m <<- solve
        getinvmatrix <- function() m
        list(set = set, get = get, setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}


## Computes inverse of special "matrix" returned by makeCacheMatrix. If inverse
## has already been calculated (and matrix has not changed), cachesolve retrieves
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmatrix(m)
        m
}
