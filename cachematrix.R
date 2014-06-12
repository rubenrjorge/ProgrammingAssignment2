## Functions to create a special matrix object that caches it's inverse once it
## has been calculated. As long as the original matrix doesn't change, then any
## future calls to get its inverse will simply be returned by the cache, not
## recalculated

## Creates the special matrix object
makeCacheMatrix <- function(original = matrix()) {
     
     inverse <- NULL
     
     set <- function(y) {
          original <<- y
          inverse <<- NULL
     }
     
     get <- function() original
     
     setInverse <- function(m) inverse <<- m
     
     getInverse <- function() inverse
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix from the cache (if available) or calculates it
cacheSolve <- function(x, ...) {
     
     cached <- x$getInverse()
     
     if(!is.null(cached)) {
          message("getting cached data")
          return(cached)
     }
     
     data <- x$get()
     m <- solve(data)
     x$setInverse(m)
     m
}