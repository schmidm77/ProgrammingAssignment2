## Put comments here that give an overall description of what your
## functions do

## Creates an object for caching the output of inverse of a 2x2 matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setSolve <- function(solve) s <<- solve
      getSolve <- function() s
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)
}


## Checks if the inverse has already been cached, otherwise, calls the solve function
## and stores the result in the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getSolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s
}
