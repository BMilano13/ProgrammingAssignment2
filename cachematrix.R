## The makeCacheMatrix function creates a special matrix object
## that can cache its inverse, providing functions to set
## and retrieve the matrix as well as set and retrieve the cached inverse.
## The cacheSolve function computes the inverse of the matrix
## stored in the cache matrix object.
## If the inverse is already calculated and cached,
## it retrieves the cached value instead of recomputing it.


## The makeCacheMatrix function creates a cacheable matrix object
## with functions to manipulate the matrix and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        
        set <- function(matrix) {
                x <<- matrix
                cache <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setCache <- function(inverse) {
                cache <<- inverse
        }
        
        getCache <- function() {
                cache
        }
        
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}


## The cacheSolve function computes and caches the inverse of the matrix
## stored in the cache matrix object,
## retrieving the cached value if available,
## which avoids redundant computations.

cacheSolve <- function(x, ...) {
        cache <- x$getCache()
        
        if (!is.null(cache)) {
                message("getting cached inverse")
                return(cache)
        }
        
        data <- x$get()
        inverse <- solve(data)
        x$setCache(inverse)
        inverse
}
