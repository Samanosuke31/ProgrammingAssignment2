## Functions used to calculate the inverse of a matrix and cache the result.

## Creates a spacial matrix used for caching the result.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolved <- function(solved) s <<- solved
  getSolved <- function() s
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}


## Calculates the inverse of a special matrix and caches the result.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  s <- x$getSolved()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolved(s)
  s
}
