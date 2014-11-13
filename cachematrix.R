## The "package" contains two functions:
## 1. makeCacheMatrix - creates a sepcial matrix that also caches its inverse
##    once it's computed and until the matrix changes
## 2. cacheSolve - creates the cached inverse of the special matrix, or
##    retrieves the existing one.

## makeCacheMatrix - creates a special matrix with a cached inverse
##                   It is assumed the matrix is invertible
## parameters: x - a matrix
## return value: a list of functions:
## set: set a new matrix.
##      parameters: newMatrix - a matrix
## get: gets the stored matrix.
##      return value: the stored matrix
## setInverse: sets the cache of the inversed matrix
##      parameters: computedInverse - a matrix
## getInverse: gets the cached inversed matrix
##      return value: the cached matrix

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(computedInverse) inverse <<- computedInverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve - returns a matrix that is the inverse of the input matrix -
##              only if the inverse is not already cached
## parameters: matrixForInverse

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    ## deliberately ignoring the additional parameters
    ## otherwise, it would make the cache moot. For example, the b parameter
    ## may result in different return values.
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse

}
