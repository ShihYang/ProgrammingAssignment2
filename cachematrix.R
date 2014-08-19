
## The pair of functions makeCacheMatrix and cacheSolve take a matrix
## and calculates its inverse if it had not been previously calculated,
## or retrieves the inverse from cache if available.

## Ang Shih-Yang 19/08/2014


## This function creates an R object (list of functions) 
## containing a matrix and a cache of its inverse if it is calculated.
makeCacheMatrix <- function(x = matrix()) {
        mi <-NULL
        set <- function (y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinv <- function(inv) mi <<- inv
        getinv <- function() mi
        list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of a matrix or retrieves it from cache.
cacheSolve <- function(x, ...) {
        mi <-x$getinv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        mx <- x$get()
        mi <- solve (mx)
        x$setinv(mi)
        mi
        
}
