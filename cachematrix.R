## Functions that will create a matrix
## and cache its inverse.

## Creates a special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the cached inverse of a matrix, or creates one if it has
## not yet been cached.

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
