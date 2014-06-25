## Two functions, makeCacheMatrix and cacheSolve, that invert a square matrix
## and cache the result for future retrieval.

## makeCacheMatrix prepares matrix for inversion by cacheSolve
## $get returns 'x'
## $setinvert sets cached inverted matrix value 'm' to input 'solve'
## $getinvert returns cached inverted matrix 'm'

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(get = get, setinvert = setinvert, getinvert = getinvert)
}


## Given a matrix 'x', cacheSolve first checks if the inversion of 'x' has
## already been cached and calculated, and if so, returns the cached value.
## Otherwise, it will calculate and return the inversion of 'x'.

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}