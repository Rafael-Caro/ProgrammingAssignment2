## These functions compute the inverse of a invertible matrix,
## cache it, and retrive it from the cache.

## Returns a list of functions for the given invertible matrix x,
## and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse,
         getinverse=getinverse)
}


## Returns the inverse from the object created with the
## makeCacheMatrix, and if already computed previously
## it retrieves it from the cahe.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}