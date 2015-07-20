## Caches inverse of matrix to avoid unnecessary recalculation

## makeCacheMatrix accepts a matrix parameter and 
## stores the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve returns the inverse of the matrix either from 
## calculation or cache (if available)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Have we cached this already?
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # No cache hit, so solve the matrix
    data <- x$get()
    inv <- solve(data, ...)
    
    # Store the result in cache
    x$setinv(inv)
    
    # Return result on calculated code path
    inv
}
