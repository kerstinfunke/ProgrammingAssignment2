## This pair of functions allows to calculate the inverse of a matrix and to cache the result making future computations faster

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseV <- NULL
  set <- function(y) {
    x <<- y
    inverseV <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverseV <<- solve
  getsolve <- function() inverseV
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverseV <- x$getsolve()
  if(!is.null(inverseV)) {
    message("getting cached data")
    return(inverseV)
  }
  data <- x$get()
  inverseV <- solve(data, ...)
  x$setsolve(inverseV)
  inverseV
}


