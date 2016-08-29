## Assignment: Caching the Inverse of a Matrix
## The functions below cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <- NULL
  }

  getMatrix <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
