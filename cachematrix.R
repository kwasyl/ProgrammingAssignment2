## The following functions save time when computing the inverse of a square invertible matrix  
## by caching it so that it can be returned without repeated calculations.

## makeCacheMatrix creates a matrix that can cache its inverse. 
## The function returns a list of 4 named objects ($get, $set, $setInverse, $getInverse).

makeCacheMatrix <- function(mtx = matrix()) {
        i <- NULL
        f_set <- function(new_mtx) {
                mtx <<- new_mtx
                i <<- NULL
        }
        f_get <- function() mtx
        f_setInverse <- function(new_inv) i <<- new_inv
        f_getInverse <- function() i
        list(set = f_set, get = f_get,
             setInverse = f_setInverse,
             getInverse = f_getInverse)
}


## cacheSolve computes the inverse of a square invertible matrix if there is no  
## previously calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
