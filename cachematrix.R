## Caching the inverse of a matrix.
## This code caches the inverse of a matrix so as to avoid repeated expensive computation.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
               x <<- y
               i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix, 
## if inverse has been calculated then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        ## If the inverse has been calculated (cached) , retrieve it.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
