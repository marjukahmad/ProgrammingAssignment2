

# This matrix creates an inverse matrix and then creates "cache" matrix
makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invert <<- inverse
    getInverse <- function() invert
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(invert)
    invert
}


