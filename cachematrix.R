makeCacheMatrix <- function(x = matrix()) {
    ## Create a matrix-like structure 
    ## with the ability to cache the inverse matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    ## and stores it in cache for the subsequent requests
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
