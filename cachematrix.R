## This file contains two functions, makeCacheMatrix and cacheSolve, that allow for
## the caching of the matrix inversion operation.

## makeCacheMatrix returns a list containing functions to get and set a particular matrix value
## and to get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse has already been computed.  If so, it returns the cached
## value.  If not, it computes the inverse, caches it, and returns the value.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
