## The functions in this file calculate and cache
## the inverse of a matrix. These functions are
## useful when we need the inverse of some matrix
## repeatedly and we don't want to perform
## the (time-consuming) calculation of the inverse
## every time we need it.

## This function creates a special wrapper of the given matrix.
## The returned wrapper-object can be used as an argument
## to cacheSolve function for cached calculation
## of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix
## wrapped by the call to makeCacheMatrix above.
## Do not pass to this function a regular matrix.
## This function accepts wrapper-objects created
## by function makeCacheMatrix only.
## The function calculates the inverse when it
## was not calculated before (for the given matrix) only.
## It returns the cached value otherwise.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
