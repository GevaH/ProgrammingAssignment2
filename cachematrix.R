## This is a script containing two functions that are designed to take advantage 
## of R lexical scoping. The overall code computes the inverse of a matrix or
## pulls the solution out from the cache.

## The first function creates a list of functions. The set function assigns a 
## value for x and m in the cache. get pulls the data saved in x. setInverse 
## computes the inverse of the input matrix and stores it in cache. getInverse 
## gets the value for the computes inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The next function pulls out the data from cache of it exists there, otherwise
## it calls for the inverse computation of the initial input matrix.

cacheSolve <- function(x, ...) {
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
